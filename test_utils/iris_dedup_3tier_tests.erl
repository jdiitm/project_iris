-module(iris_dedup_3tier_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% 3-Tier Dedup Path Verification Tests (RFC-001 v4.0 Section 6.2)
%%
%% Verifies the 3-tier dedup architecture as specified in the updated RFC:
%%   Hot Tier:  ETS (5 min TTL) — fast in-memory check
%%   Warm Tier: Mnesia dedup_log — persistent, authoritative
%%   Bloom:     Optimization layer — never the sole drop decision
%%
%% Pattern: follows iris_dedup_tests.erl {setup, fun setup/0, fun cleanup/1, [...]}
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
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
%% Main Test Generator
%% =============================================================================

iris_dedup_3tier_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% 3-tier path verification
      {"New message populates all 3 tiers", fun test_all_tiers_populated/0},
      {"dedup_log is authoritative after ETS clear", fun test_dedup_log_authoritative/0},
      {"Bloom false positive does not drop messages", fun test_bloom_fp_allows_new/0},
      {"Stats expose bloom_false_positives counter", fun test_fp_counter_exposed/0},
      {"100 unique messages all accepted", fun test_no_false_drops/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_all_tiers_populated() ->
    %% After check_and_mark, the message must exist in:
    %%   1. ETS (hot tier)
    %%   2. Bloom filter (via is_duplicate which checks ETS then bloom)
    %%   3. Mnesia dedup_log (async write — wait briefly)
    MsgId = <<"3tier_all_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),

    %% Tier 1: ETS
    ?assert(ets:member(iris_dedup_seen, MsgId)),

    %% Tier 2+3: is_duplicate checks ETS first, then bloom
    ?assert(iris_dedup:is_duplicate(MsgId)),

    %% Tier 3: dedup_log (async — give 200ms for spawn to complete)
    timer:sleep(200),
    DedupLogResult = mnesia:dirty_read(dedup_log, MsgId),
    ?assertMatch([{dedup_log, MsgId, _}], DedupLogResult).

test_dedup_log_authoritative() ->
    %% Simulate post-crash state: ETS is cleared, bloom is empty,
    %% but dedup_log retains the record.
    %% check_and_mark must return 'duplicate' because dedup_log is authoritative.
    MsgId = <<"3tier_auth_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Step 1: Mark message (populates all tiers)
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    timer:sleep(200),  %% Wait for async dedup_log write

    %% Step 2: Verify dedup_log has it
    ?assertMatch([{dedup_log, MsgId, _}], mnesia:dirty_read(dedup_log, MsgId)),

    %% Step 3: Clear ETS (simulates crash — hot tier lost)
    ets:delete(iris_dedup_seen, MsgId),
    ?assertNot(ets:member(iris_dedup_seen, MsgId)),

    %% Step 4: check_and_mark again — dedup_log must catch it
    %% The path is: ets:insert_new succeeds (ETS empty) → dirty_read dedup_log → found → duplicate
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(duplicate, Result).

test_bloom_fp_allows_new() ->
    %% When bloom says "probably seen" but dedup_log says "definitely not seen",
    %% the message MUST be allowed through (bloom false positive).
    %% We verify this indirectly: if a message is truly new and only the bloom
    %% thinks it's seen, check_and_mark returns 'new'.
    %%
    %% This is hard to force directly (bloom FP is probabilistic), so we verify
    %% the counter exists and that truly new messages are never dropped.
    MsgIds = [<<"3tier_fp_", (integer_to_binary(N))/binary, "_",
               (integer_to_binary(erlang:unique_integer([positive])))/binary>>
              || N <- lists:seq(1, 50)],

    Results = [iris_dedup:check_and_mark(Id) || Id <- MsgIds],
    NewCount = length([R || R <- Results, R =:= new]),

    %% All 50 unique messages must be accepted as new
    ?assertEqual(50, NewCount).

test_fp_counter_exposed() ->
    %% The stats map must include bloom_false_positives as a non-negative integer.
    Stats = iris_dedup:get_stats(),
    ?assert(maps:is_key(bloom_false_positives, Stats)),
    FP = maps:get(bloom_false_positives, Stats),
    ?assert(is_integer(FP)),
    ?assert(FP >= 0).

test_no_false_drops() ->
    %% 100 unique messages must ALL be accepted as 'new'.
    %% Zero false drops allowed — the RFC invariant (Section 1.2).
    MsgIds = [<<"3tier_nodrop_", (integer_to_binary(N))/binary, "_",
               (integer_to_binary(erlang:unique_integer([positive])))/binary>>
              || N <- lists:seq(1, 100)],

    Results = [iris_dedup:check_and_mark(Id) || Id <- MsgIds],
    NewCount = length([R || R <- Results, R =:= new]),
    DupCount = length([R || R <- Results, R =:= duplicate]),

    ?assertEqual(100, NewCount),
    ?assertEqual(0, DupCount).
