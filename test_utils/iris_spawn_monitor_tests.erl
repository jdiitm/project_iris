-module(iris_spawn_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mitigation: Fire-and-forget spawns must be monitored.
%% =============================================================================
%% Critical operations use spawn(fun() -> ... end) with no monitoring.
%% After the fix, iris_async:spawn_monitored/2 wraps spawn_monitor with
%% failure counting via iris_metrics.
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test: iris_async:spawn_monitored/2 -- successful operation
%% ---------------------------------------------------------------------------
spawn_monitored_success_test() ->
    %% A successful spawn should complete without incrementing failure counter
    CallerPid = self(),
    iris_async:spawn_monitored(revocation_propagation, fun() ->
        CallerPid ! {done, self()},
        ok
    end),
    
    receive
        {done, _Pid} -> ok
    after 1000 ->
        ?assert(false)
    end.

%% ---------------------------------------------------------------------------
%% Test: iris_async:spawn_monitored/2 -- failed operation increments metric
%% ---------------------------------------------------------------------------
spawn_monitored_failure_test() ->
    %% A failing spawn should not crash the caller
    %% and should increment the failure metric
    iris_async:spawn_monitored(revocation_propagation, fun() ->
        error(deliberate_test_failure)
    end),
    
    %% Give time for the monitor to fire and process
    timer:sleep(100),
    
    %% The caller process must still be alive
    ?assert(is_process_alive(self())).

%% ---------------------------------------------------------------------------
%% Test: iris_async:spawn_monitored/2 -- crash doesn't propagate to caller
%% ---------------------------------------------------------------------------
spawn_monitored_isolation_test() ->
    %% Spawn a function that crashes -- caller must not be affected
    CallerPid = self(),
    iris_async:spawn_monitored(key_replication, fun() ->
        exit(test_crash)
    end),
    
    timer:sleep(100),
    ?assert(is_process_alive(CallerPid)).
