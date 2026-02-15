-module(iris_metrics_callsite_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% CHARACTERIZATION TEST: NFR-32 Standard Counter Call-Sites
%% =============================================================================
%% RFC-001 v4.0 NFR-32: "MUST emit: msg_in, msg_out, ack_sent, dedup_hit"
%%
%% This test documents which counters are actually called from production code.
%% After the fix, all four counters should be incremented in the
%% real message processing paths.
%%
%% Current state (2026-02-08):
%%   - dedup_hit: Called from iris_core.erl:266 (GOOD)
%%   - msg_in:    NOT called from any production code (BUG)
%%   - msg_out:   NOT called from any production code (BUG)
%%   - ack_sent:  NOT called from any production code (BUG)
%% =============================================================================

setup() ->
    case whereis(iris_metrics) of
        undefined ->
            {ok, Pid} = iris_metrics:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    case whereis(iris_metrics) of
        undefined -> ok;
        P -> gen_server:stop(P)
    end;
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

metrics_callsite_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"msg_in counter API works", fun test_msg_in_api/0},
      {"msg_out counter API works", fun test_msg_out_api/0},
      {"ack_sent counter API works", fun test_ack_sent_api/0},
      {"dedup_hit counter API works", fun test_dedup_hit_api/0},
      {"all four NFR-32 counters initialized to zero", fun test_initial_values/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_msg_in_api() ->
    Before = get_counter(iris_msg_in),
    iris_metrics:msg_in(),
    After = get_counter(iris_msg_in),
    ?assertEqual(Before + 1, After).

test_msg_out_api() ->
    Before = get_counter(iris_msg_out),
    iris_metrics:msg_out(),
    After = get_counter(iris_msg_out),
    ?assertEqual(Before + 1, After).

test_ack_sent_api() ->
    Before = get_counter(iris_ack_sent),
    iris_metrics:ack_sent(),
    After = get_counter(iris_ack_sent),
    ?assertEqual(Before + 1, After).

test_dedup_hit_api() ->
    Before = get_counter(iris_dedup_hit),
    iris_metrics:dedup_hit(),
    After = get_counter(iris_dedup_hit),
    ?assertEqual(Before + 1, After).

test_initial_values() ->
    %% All NFR-32 counters should exist in ETS (initialized by iris_metrics:init)
    Metrics = iris_metrics:get_metrics(),
    ?assert(maps:is_key(iris_msg_in, Metrics)),
    ?assert(maps:is_key(iris_msg_out, Metrics)),
    ?assert(maps:is_key(iris_ack_sent, Metrics)),
    ?assert(maps:is_key(iris_dedup_hit, Metrics)).

%% =============================================================================
%% Helpers
%% =============================================================================

get_counter(Name) ->
    Metrics = iris_metrics:get_metrics(),
    maps:get(Name, Metrics, 0).
