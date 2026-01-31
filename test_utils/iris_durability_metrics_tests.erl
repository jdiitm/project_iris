-module(iris_durability_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Tests for Durability Metrics (RFC NFR-6/NFR-8)
%% =============================================================================
%% Tests cover:
%% - msg_acked counter
%% - msg_lost counter (critical alert)
%% - get_durability_metrics calculation
%% - 99.999% SLA tracking
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Setup/Teardown
%% ---------------------------------------------------------------------------

setup() ->
    %% Start metrics if not already running
    case whereis(iris_metrics) of
        undefined ->
            {ok, Pid} = iris_metrics:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, Pid}) ->
    gen_server:stop(Pid);
cleanup({existing, _}) ->
    ok.

%% ---------------------------------------------------------------------------
%% Counter Tests
%% ---------------------------------------------------------------------------

durability_counters_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"msg_acked increments counter", fun test_msg_acked_increment/0},
          {"msg_lost increments counter", fun test_msg_lost_increment/0},
          {"multiple acks accumulate", fun test_multiple_acks/0},
          {"get_durability_metrics returns correct structure", fun test_durability_metrics_structure/0},
          {"durability percentage calculation", fun test_durability_percentage/0},
          {"meets_nfr6 flag correct for 100%", fun test_nfr6_flag_pass/0}
         ]
     end}.

test_msg_acked_increment() ->
    %% Get initial count
    InitialMetrics = iris_metrics:get_durability_metrics(),
    InitialAcked = maps:get(acked, InitialMetrics),
    
    %% Increment
    ok = iris_metrics:msg_acked(),
    timer:sleep(10),  %% Allow async update
    
    %% Verify increment
    NewMetrics = iris_metrics:get_durability_metrics(),
    NewAcked = maps:get(acked, NewMetrics),
    ?assertEqual(InitialAcked + 1, NewAcked).

test_msg_lost_increment() ->
    %% Get initial count
    InitialMetrics = iris_metrics:get_durability_metrics(),
    InitialLost = maps:get(lost, InitialMetrics),
    
    %% Increment (this will log EMERGENCY - expected in test)
    ok = iris_metrics:msg_lost(),
    timer:sleep(10),
    
    %% Verify increment
    NewMetrics = iris_metrics:get_durability_metrics(),
    NewLost = maps:get(lost, NewMetrics),
    ?assertEqual(InitialLost + 1, NewLost).

test_multiple_acks() ->
    %% Get initial
    Initial = maps:get(acked, iris_metrics:get_durability_metrics()),
    
    %% Send multiple acks
    lists:foreach(fun(_) -> iris_metrics:msg_acked() end, lists:seq(1, 10)),
    timer:sleep(50),
    
    %% Verify all counted
    Final = maps:get(acked, iris_metrics:get_durability_metrics()),
    ?assert(Final >= Initial + 10).

test_durability_metrics_structure() ->
    Metrics = iris_metrics:get_durability_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(acked, Metrics)),
    ?assert(maps:is_key(lost, Metrics)),
    ?assert(maps:is_key(total, Metrics)),
    ?assert(maps:is_key(durability_percent, Metrics)),
    ?assert(maps:is_key(meets_nfr6, Metrics)).

test_durability_percentage() ->
    Metrics = iris_metrics:get_durability_metrics(),
    Acked = maps:get(acked, Metrics),
    Lost = maps:get(lost, Metrics),
    Total = maps:get(total, Metrics),
    Percent = maps:get(durability_percent, Metrics),
    
    %% Verify total is sum
    ?assertEqual(Acked + Lost, Total),
    
    %% Verify percentage is reasonable
    ?assert(Percent >= 0.0),
    ?assert(Percent =< 100.0).

test_nfr6_flag_pass() ->
    %% With mostly acks and few (or no) losses, should meet NFR-6
    %% We need at least 99.999% durability
    %% Add many acks to dilute any previous test losses
    lists:foreach(fun(_) -> iris_metrics:msg_acked() end, lists:seq(1, 100000)),
    timer:sleep(100),
    
    Metrics = iris_metrics:get_durability_metrics(),
    Percent = maps:get(durability_percent, Metrics),
    MeetsNfr6 = maps:get(meets_nfr6, Metrics),
    
    %% If percentage is high enough, flag should be true
    case Percent >= 99.999 of
        true -> ?assert(MeetsNfr6);
        false -> ?assertNot(MeetsNfr6)
    end.

%% ---------------------------------------------------------------------------
%% Failover Timeout Tests
%% ---------------------------------------------------------------------------

failover_timeout_test_() ->
    {setup,
     fun() ->
         %% Start circuit breaker if not running
         case whereis(iris_circuit_breaker) of
             undefined ->
                 {ok, Pid} = iris_circuit_breaker:start_link(),
                 {started, Pid};
             Pid ->
                 {existing, Pid}
         end
     end,
     fun({started, Pid}) ->
         gen_server:stop(Pid);
        ({existing, _}) ->
         ok
     end,
     fun(_) ->
         [
          {"default failover timeout is 30s", fun test_default_failover_timeout/0},
          {"failover timeout is configurable", fun test_configurable_failover_timeout/0}
         ]
     end}.

test_default_failover_timeout() ->
    %% Default should be 30000ms (30 seconds per RFC NFR-9)
    Timeout = iris_circuit_breaker:get_failover_timeout(),
    ?assertEqual(30000, Timeout).

test_configurable_failover_timeout() ->
    %% Set custom timeout
    OldVal = application:get_env(iris_core, failover_timeout_ms),
    application:set_env(iris_core, failover_timeout_ms, 15000),
    
    %% Verify it reads new value
    Timeout = iris_circuit_breaker:get_failover_timeout(),
    ?assertEqual(15000, Timeout),
    
    %% Restore
    case OldVal of
        undefined -> application:unset_env(iris_core, failover_timeout_ms);
        {ok, V} -> application:set_env(iris_core, failover_timeout_ms, V)
    end.
