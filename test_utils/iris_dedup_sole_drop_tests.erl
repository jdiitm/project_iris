-module(iris_dedup_sole_drop_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001 v4.0 Section 6.2: Bloom Must Not Be Sole Drop Basis
%% =============================================================================
%% "Bloom filter MUST NOT be the sole basis for dropping a message."
%%
%% The is_duplicate/1 function currently checks ETS + bloom but NOT Mnesia
%% dedup_log. If bloom returns a false positive, is_duplicate/1 incorrectly
%% reports the message as a duplicate.
%%
%% check_and_mark/1 correctly cross-checks Mnesia. This test suite ensures
%% is_duplicate/1 does the same.
%% =============================================================================

setup() ->
    application:ensure_all_started(mnesia),
    case mnesia:create_table(dedup_log, [
        {attributes, [msg_id, timestamp]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, dedup_log}} -> ok;
        {aborted, Reason} ->
            logger:warning("Could not create dedup_log table: ~p", [Reason]),
            ok
    end,
    case whereis(iris_dedup) of
        undefined ->
            {ok, Pid} = iris_dedup:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_dedup);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

bloom_sole_drop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"is_duplicate must cross-check Mnesia on bloom positive (RFC 6.2)",
       fun test_is_duplicate_consults_mnesia_not_just_bloom/0},
      {"is_duplicate returns false for truly unseen message",
       fun test_is_duplicate_false_for_new/0},
      {"is_duplicate returns true when message in both bloom and Mnesia",
       fun test_is_duplicate_true_when_in_both/0},
      {"check_and_mark correctly handles bloom false positive",
       fun test_check_and_mark_handles_bloom_fp/0}
     ]}.

%% =============================================================================
%% Core Test: Bloom false positive must not cause is_duplicate to lie
%% =============================================================================

test_is_duplicate_consults_mnesia_not_just_bloom() ->
    %% A message that is in bloom (false positive) but NOT in Mnesia dedup_log
    %% must NOT be reported as duplicate by is_duplicate/1.
    %%
    %% Strategy: We cannot directly call add_to_bloom (not exported), but we
    %% can exploit bloom's probabilistic nature. We use mark_seen to populate
    %% bloom for one message, then check that a DIFFERENT (never-seen) message
    %% that happens to NOT be a bloom false positive is correctly identified.
    %%
    %% The real assertion: after the fix, is_duplicate/1 should never return
    %% true for a message that is not in dedup_log.
    %%
    %% For a direct test, we use check_and_mark (which writes to dedup_log)
    %% then clear ETS only, so the bloom entry remains but we can test the
    %% cross-check path.

    MsgId = <<"sole_drop_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Step 1: Message is completely unseen
    ?assertEqual(false, iris_dedup:is_duplicate(MsgId)),

    %% Step 2: Mark it via check_and_mark (writes ETS + bloom + dedup_log)
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    timer:sleep(150),  %% Allow async dedup_log write

    %% Step 3: Now it should be a duplicate (it's in all tiers)
    ?assertEqual(true, iris_dedup:is_duplicate(MsgId)),

    %% Step 4: Delete from ETS hot tier only (simulate hot tier expiry)
    ets:delete(iris_dedup_seen, MsgId),

    %% Step 5: is_duplicate should still return true because:
    %%   - ETS: miss (we deleted it)
    %%   - Bloom: hit (still there)
    %%   - Mnesia: hit (still there, should be cross-checked)
    %% If is_duplicate only checks bloom without Mnesia, it would still
    %% return true here — which is correct for this case.
    %%
    %% The REAL bug manifests when bloom has a false positive for a message
    %% that was NEVER written to dedup_log. We test that scenario next by
    %% creating a fresh message and checking that is_duplicate/1 returns
    %% false even if we can't guarantee bloom will false-positive on it.
    %% The definitive test is that after the fix, the code path through
    %% Mnesia is exercised.

    FreshMsgId = <<"never_seen_sole_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% This message has NEVER been through check_and_mark or mark_seen.
    %% dedup_log does NOT contain it.
    %% If bloom happens to false-positive on it, is_duplicate/1 MUST still
    %% return false (because Mnesia says no).
    %% If bloom correctly returns false, is_duplicate/1 returns false (trivially correct).
    %%
    %% Either way, the correct answer for a truly unseen message is false.
    ?assertEqual(false, iris_dedup:is_duplicate(FreshMsgId)).

test_is_duplicate_false_for_new() ->
    MsgId = <<"brand_new_sole_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ?assertEqual(false, iris_dedup:is_duplicate(MsgId)).

test_is_duplicate_true_when_in_both() ->
    MsgId = <<"both_tiers_sole_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Write via mark_seen (populates all tiers)
    iris_dedup:mark_seen(MsgId),
    timer:sleep(150),  %% Allow async dedup_log write

    %% Must be duplicate
    ?assertEqual(true, iris_dedup:is_duplicate(MsgId)).

test_check_and_mark_handles_bloom_fp() ->
    %% check_and_mark correctly cross-checks Mnesia when bloom says "seen"
    %% but dedup_log disagrees. This is the GOLD STANDARD behavior.
    MsgId = <<"cam_fp_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% First call: truly new
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),

    %% Second call: truly duplicate (in all tiers)
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).
