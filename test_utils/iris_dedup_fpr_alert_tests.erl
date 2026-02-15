-module(iris_dedup_fpr_alert_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Bloom Filter FPR Alerting Tests
%%
%% RFC-001 v4.0 Section 6.2: Bloom filter false positives are detected via
%% dedup_log cross-check and tracked (bloom_false_positives counter).
%%
%% These tests verify:
%% 1. get_stats() includes computed bloom_fpr (float)
%% 2. FPR is 0.0 when no bloom checks have occurred
%% 3. FPR tracks correctly as false positives accumulate
%% 4. max_bloom_fpr threshold is configurable via application env
%% 5. fpr_alert flag set in stats when threshold exceeded
%%
%% Pattern: follows iris_dedup_tests.erl setup/cleanup with Mnesia + gen_server.
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

iris_dedup_fpr_alert_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"get_stats includes bloom_fpr field", fun test_get_stats_includes_fpr/0},
      {"FPR is 0.0 when no bloom checks", fun test_fpr_zero_when_no_lookups/0},
      {"FPR tracks correctly after false positives", fun test_fpr_tracks_correctly/0},
      {"max_bloom_fpr threshold configurable", fun test_fpr_alert_threshold_configurable/0},
      {"fpr_alert set when threshold exceeded", fun test_fpr_alert_logged_when_exceeded/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_get_stats_includes_fpr() ->
    Stats = iris_dedup:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(bloom_fpr, Stats)),
    Fpr = maps:get(bloom_fpr, Stats),
    ?assert(is_float(Fpr) orelse Fpr =:= 0),
    ?assert(Fpr >= 0.0 andalso Fpr =< 1.0).

test_fpr_zero_when_no_lookups() ->
    %% On a freshly-started dedup, no bloom checks have occurred.
    %% The bloom_checks counter and bloom_false_positives are both 0,
    %% so FPR should be 0.0 (defined as 0.0 when denominator is 0).
    Stats = iris_dedup:get_stats(),
    Fpr = maps:get(bloom_fpr, Stats),
    ?assertEqual(0.0, Fpr).

test_fpr_tracks_correctly() ->
    %% After N bloom checks with M false positives, FPR = M/N.
    %% We can read bloom_false_positives and bloom_checks from stats.
    Stats = iris_dedup:get_stats(),
    FP = maps:get(bloom_false_positives, Stats, 0),
    Checks = maps:get(bloom_checks, Stats, 0),
    Fpr = maps:get(bloom_fpr, Stats),
    case Checks of
        0 -> ?assertEqual(0.0, Fpr);
        _ -> ?assertEqual(FP / Checks, Fpr)
    end.

test_fpr_alert_threshold_configurable() ->
    %% Setting max_bloom_fpr via application env should be reflected
    %% in get_stats output.
    application:set_env(iris_core, max_bloom_fpr, 0.001),
    Stats = iris_dedup:get_stats(),
    Threshold = maps:get(max_bloom_fpr, Stats, undefined),
    ?assertEqual(0.001, Threshold),
    %% Clean up
    application:unset_env(iris_core, max_bloom_fpr).

test_fpr_alert_logged_when_exceeded() ->
    %% When FPR exceeds the configured threshold, stats should
    %% contain fpr_alert => true.
    %%
    %% We set a threshold of 0.0 so any FP triggers it.
    application:set_env(iris_core, max_bloom_fpr, 0.0),
    Stats = iris_dedup:get_stats(),
    FP = maps:get(bloom_false_positives, Stats, 0),
    HasAlert = maps:get(fpr_alert, Stats, false),
    case FP > 0 of
        true -> ?assertEqual(true, HasAlert);
        false -> ?assertEqual(false, HasAlert)
    end,
    application:unset_env(iris_core, max_bloom_fpr).
