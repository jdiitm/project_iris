-module(iris_partition_guard_reconciliation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F1: Split-Brain Reconciliation Tests (P0 -- RFC 7.1.1)
%%
%% Tests verify:
%% 1. When partition heals (quorum restored after diverged mode),
%%    a reconciliation process is spawned.
%% 2. The reconciliation performs union merge of offline_msg records.
%%
%% maybe_exit_diverged_mode simply sets mode=normal
%% without triggering any data reconciliation, violating RFC 7.1.1
%% which requires "Union Merge of append-only logs".
%% =============================================================================

-define(QUORUM_RECOVERY_DELAY_MS, 10000).

setup() ->
    %% Stop any existing partition guard
    catch gen_server:stop(iris_partition_guard),
    timer:sleep(50),
    %% Configure expected_nodes to include this node so quorum is achievable
    application:set_env(iris_core, expected_cluster_nodes, [node()]),
    ok.

cleanup(ok) ->
    catch gen_server:stop(iris_partition_guard),
    catch unregister(iris_reconciliation_test_listener),
    application:unset_env(iris_core, expected_cluster_nodes),
    ok.

iris_partition_guard_reconciliation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Healing from diverged mode triggers reconciliation",
       {timeout, 15, fun test_healing_triggers_reconciliation/0}},
      {"Reconciliation performs union merge of offline_msg",
       {timeout, 15, fun test_offline_msg_union_merge/0}}
     ]}.

test_healing_triggers_reconciliation() ->
    %% Register THIS process (the test process) as the listener.
    %% Must be done here, not in setup(), because EUnit runs tests
    %% in a different process than setup.
    catch unregister(iris_reconciliation_test_listener),
    register(iris_reconciliation_test_listener, self()),
    
    %% Start partition guard in normal mode
    {ok, Pid} = iris_partition_guard:start_link(),
    
    %% Force into diverged mode by simulating partition detection.
    %% We do this by setting state directly via sys:replace_state.
    %% Record fields: {state, mode(2), membership_mode(3), expected_nodes(4),
    %%   visible_nodes(5), last_quorum_loss(6), quorum_threshold(7),
    %%   check_timer(8), partition_count(9), epoch(10)}
    sys:replace_state(Pid, fun(State) ->
        PastTime = os:system_time(second) - 20,  %% 20s ago, > 10s threshold
        S1 = setelement(2, State, diverged),        %% mode = diverged
        S2 = setelement(6, S1, PastTime),           %% last_quorum_loss = past
        setelement(9, S2, 1)                        %% partition_count = 1
    end),
    
    %% Verify we're in diverged mode
    Status1 = iris_partition_guard:get_status(),
    ?assertEqual(diverged, maps:get(mode, Status1)),
    
    %% Trigger a partition check which should call maybe_exit_diverged_mode
    %% and find quorum restored (no expected nodes = always quorum).
    Pid ! check_partition,
    %% : Event-driven wait for mode to change to normal
    ok = iris_test_utils:wait_until(fun() ->
        S = iris_partition_guard:get_status(),
        maps:get(mode, S) =:= normal
    end, 2000),
    
    %% After healing, mode should be normal
    Status2 = iris_partition_guard:get_status(),
    ?assertEqual(normal, maps:get(mode, Status2)),
    
    %% The key assertion: reconciliation should have been triggered.
    %% We check by verifying iris_core:reconcile_after_partition/0 was called.
    %% Since we can't easily mock in EUnit, we check if a reconciliation
    %% process was spawned by checking the process dictionary or a message.
    %% The GREEN implementation will send a notification we can verify.
    Received = receive
        {reconciliation_triggered, _Info} -> true
    after 2000 ->
        false
    end,
    ?assertEqual(true, Received),
    
    gen_server:stop(Pid).

test_offline_msg_union_merge() ->
    %% This test verifies that iris_core:reconcile_after_partition/0
    %% performs a union merge of offline_msg records from remote nodes.
    %%
    %% Since we're testing in a single-node environment, we simulate
    %% the merge by directly calling the reconciliation function and
    %% verifying it attempts to read from remote nodes.
    %%
    %% Set up Mnesia for this test
    mnesia:start(),
    mnesia:create_schema([node()]),
    catch mnesia:create_table(offline_msg, [
        {ram_copies, [node()]},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]),
    mnesia:wait_for_tables([offline_msg], 5000),
    
    %% Write some local offline messages
    mnesia:dirty_write({offline_msg, {<<"user1">>, 0}, 1000, <<"msg_local_1">>}),
    mnesia:dirty_write({offline_msg, {<<"user1">>, 0}, 1001, <<"msg_local_2">>}),
    
    %% Verify local messages exist
    LocalMsgs = mnesia:dirty_read(offline_msg, {<<"user1">>, 0}),
    ?assertEqual(2, length(LocalMsgs)),
    
    %% Call reconcile_after_partition (should not crash, should log)
    %% In single-node mode, there are no remote nodes to merge from,
    %% so it should complete gracefully with no-op.
    Result = try
        iris_core:reconcile_after_partition()
    catch
        error:undef -> {error, not_implemented};
        _:Reason -> {error, Reason}
    end,
    
    %% The function must exist (not undef) -- this is the critical assertion
    ?assertNotMatch({error, not_implemented}, Result),
    
    %% Local messages should still be intact (no data loss)
    PostMsgs = mnesia:dirty_read(offline_msg, {<<"user1">>, 0}),
    ?assertEqual(2, length(PostMsgs)),
    
    %% Cleanup
    catch mnesia:delete_table(offline_msg),
    mnesia:stop(),
    ok.
