-module(iris_exit_handling_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-7 Mitigation: Modules that trap_exit must handle EXIT signals.
%% =============================================================================
%% iris_status_batcher, iris_flow_controller, iris_durable_batcher all call
%% process_flag(trap_exit, true) but have no handle_info({'EXIT', ...}) clause.
%% EXIT signals accumulate in the mailbox, causing memory growth.
%%
%% This test verifies that each module's gen_server survives a linked process
%% dying and logs the EXIT rather than silently accumulating it.
%%
%% NOTE: gen_server has special handling for EXIT from the parent (spawner) --
%% it calls exit(Reason) directly, bypassing handle_info. So we must send
%% EXIT from a non-parent PID (simulating a worker or linked child dying).
%% =============================================================================

%% ---------------------------------------------------------------------------
%% iris_status_batcher: EXIT handling
%% ---------------------------------------------------------------------------
status_batcher_survives_exit_test() ->
    {ok, Pid} = iris_status_batcher:start_link(99),
    unlink(Pid),
    ?assert(is_process_alive(Pid)),
    
    %% Send an EXIT signal from a non-parent PID
    FakePid = spawn(fun() -> ok end),
    timer:sleep(10),
    Pid ! {'EXIT', FakePid, test_crash},
    timer:sleep(50),
    
    %% Batcher must survive
    ?assert(is_process_alive(Pid)),
    
    %% Verify mailbox is clean (EXIT was handled, not accumulated)
    {message_queue_len, QLen} = process_info(Pid, message_queue_len),
    ?assertEqual(0, QLen),
    
    gen_server:stop(Pid).

%% ---------------------------------------------------------------------------
%% iris_flow_controller: EXIT handling
%% ---------------------------------------------------------------------------
flow_controller_survives_exit_test() ->
    OldPid = whereis(iris_flow_controller),
    Pid = case OldPid of
        undefined ->
            {ok, P} = iris_flow_controller:start_link(),
            P;
        P -> P
    end,
    unlink(Pid),
    ?assert(is_process_alive(Pid)),
    
    %% Send an EXIT signal from a non-parent PID
    FakePid = spawn(fun() -> ok end),
    timer:sleep(10),
    Pid ! {'EXIT', FakePid, test_crash},
    timer:sleep(50),
    
    ?assert(is_process_alive(Pid)),
    
    {message_queue_len, QLen} = process_info(Pid, message_queue_len),
    ?assertEqual(0, QLen),
    
    %% Only stop if we started it
    case OldPid of
        undefined -> gen_server:stop(Pid);
        _ -> ok
    end.

%% ---------------------------------------------------------------------------
%% iris_durable_batcher: EXIT handling
%% ---------------------------------------------------------------------------
durable_batcher_survives_exit_test() ->
    {ok, Pid} = iris_durable_batcher:start_link(998),
    unlink(Pid),
    ?assert(is_process_alive(Pid)),
    
    %% Send an EXIT signal from a non-parent PID
    FakePid = spawn(fun() -> ok end),
    timer:sleep(10),
    Pid ! {'EXIT', FakePid, test_crash},
    timer:sleep(50),
    
    ?assert(is_process_alive(Pid)),
    
    {message_queue_len, QLen} = process_info(Pid, message_queue_len),
    ?assertEqual(0, QLen),
    
    gen_server:stop(Pid).
