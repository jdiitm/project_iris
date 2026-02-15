-module(iris_flow_controller_safety_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Flow Controller Safety Tests (RFC 7.4)
%%
%% Tests verify:
%% 1. CPU monitoring is not blind when iris_efficiency_monitor is absent
%%    (scheduler_wall_time must be enabled by flow controller itself)
%% 2. Cascade detection requires minimum sample size (no false triggers
%%    on a single failure)
%% 3. Cascade detection still triggers with sufficient evidence
%% =============================================================================

-define(MIN_CASCADE_SAMPLES, 10).

setup() ->
    %% Ensure iris_efficiency_monitor is NOT running (simulates standalone)
    catch gen_server:stop(iris_efficiency_monitor),
    %% Stop flow controller if running
    catch gen_server:stop(iris_flow_controller),
    %% Clean up ETS tables from previous runs
    catch ets:delete(iris_flow_controller_ets),
    catch ets:delete(iris_flow_controller_dest_ets),
    timer:sleep(50),
    {ok, Pid} = iris_flow_controller:start_link(),
    {started, Pid}.

cleanup({started, Pid}) ->
    catch gen_server:stop(Pid),
    catch ets:delete(iris_flow_controller_ets),
    catch ets:delete(iris_flow_controller_dest_ets),
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_flow_controller_safety_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"CPU monitoring not blind without efficiency monitor",
       fun test_scheduler_wall_time_enabled/0},
      {"Cascade requires minimum samples (single failure = no cascade)",
       fun test_cascade_requires_minimum_samples/0},
      {"Cascade triggers above minimum with high failure ratio",
       fun test_cascade_triggers_above_minimum/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_scheduler_wall_time_enabled() ->
    %% After flow controller init, scheduler_wall_time should be enabled
    %% so get_scheduler_utilization() doesn't return 0.0 blindly.
    %% If scheduler_wall_time is enabled, erlang:statistics(scheduler_wall_time)
    %% returns a list (not 'undefined').
    Result = erlang:statistics(scheduler_wall_time),
    ?assertNotEqual(undefined, Result),
    ?assert(is_list(Result)).

test_cascade_requires_minimum_samples() ->
    %% Record a single failure -- should NOT trigger cascade detection.
    %% With the current code (no minimum), 1 failure / 1 total = 100% ratio
    %% which exceeds 50% threshold and incorrectly triggers cascade.
    iris_flow_controller:record_failure(fake_node_1),
    timer:sleep(100),  %% Let gen_server process the cast

    Stats = iris_flow_controller:get_stats(),
    ?assertEqual(false, maps:get(cascade_detected, Stats)).

test_cascade_triggers_above_minimum() ->
    %% Record enough failures to exceed both minimum sample size AND threshold.
    %% 50 failures + 1 success = 51 total, ratio = 50/51 ≈ 0.98 > 0.50
    lists:foreach(fun(I) ->
        Node = list_to_atom("failing_node_" ++ integer_to_list(I)),
        iris_flow_controller:record_failure(Node)
    end, lists:seq(1, 50)),
    iris_flow_controller:record_success(healthy_node),
    timer:sleep(100),  %% Let gen_server process all casts

    Stats = iris_flow_controller:get_stats(),
    ?assertEqual(true, maps:get(cascade_detected, Stats)).
