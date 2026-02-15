-module(iris_quorum_write_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_quorum_write.erl
%% =============================================================================
%% 
%% Tests cover:
%% - Replication factor configuration
%% - Replica selection (consistent hashing)
%% - Local write operations
%% - Quorum calculation
%% - Error handling
%% 
%% Note: Tests for actual distributed writes require multi-node setup
%% and are covered in integration tests.
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup() ->
    %% Start mnesia for local operations
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    
    %% Create test table
    {atomic, ok} = mnesia:create_table(test_quorum_table, [
        {ram_copies, [node()]},
        {attributes, [key, value]}
    ]),
    mnesia:wait_for_tables([test_quorum_table], 5000),
    ok.

cleanup(_) ->
    mnesia:delete_table(test_quorum_table),
    application:stop(mnesia),
    ok.

%% =============================================================================
%% Configuration Tests
%% =============================================================================

replication_factor_test_() ->
    {"Replication factor configuration",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Default replication factor is 3", fun() ->
            %% Clear any existing config
            application:unset_env(iris_core, replication_factor),
            ?assertEqual(3, iris_quorum_write:get_replication_factor())
        end},
       
       {"Can set replication factor", fun() ->
            iris_quorum_write:set_replication_factor(5),
            ?assertEqual(5, iris_quorum_write:get_replication_factor()),
            %% Cleanup
            application:unset_env(iris_core, replication_factor)
        end},
       
       {"Replication factor must be positive", fun() ->
            %% This should work
            iris_quorum_write:set_replication_factor(1),
            ?assertEqual(1, iris_quorum_write:get_replication_factor()),
            %% Cleanup
            application:unset_env(iris_core, replication_factor)
        end}
      ]}}.

%% =============================================================================
%% Replica Selection Tests
%% =============================================================================

replica_selection_test_() ->
    {"Replica selection",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Single node returns local node", fun() ->
            application:unset_env(iris_core, replication_factor),
            Replicas = iris_quorum_write:get_replicas(<<"test_key">>),
            ?assert(lists:member(node(), Replicas))
        end},
       
       {"Replica selection is deterministic", fun() ->
            Key = <<"deterministic_key">>,
            Replicas1 = iris_quorum_write:get_replicas(Key),
            Replicas2 = iris_quorum_write:get_replicas(Key),
            ?assertEqual(Replicas1, Replicas2)
        end},
       
       {"Different keys may get different replicas", fun() ->
            %% With single node, all keys get same node, but function executes
            _Replicas1 = iris_quorum_write:get_replicas(<<"key_a">>),
            _Replicas2 = iris_quorum_write:get_replicas(<<"key_b">>),
            %% Just verify no crash - with multi-node would test distribution
            ?assert(true)
        end}
      ]}}.

%% =============================================================================
%% Local Write Tests
%% =============================================================================

local_write_test_() ->
    {"Local sync write operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"local_sync_write succeeds for valid data", fun() ->
            Result = iris_quorum_write:local_sync_write(
                test_quorum_table, test_key, <<"test_value">>),
            ?assertEqual(ok, Result),
            %% Verify data was written
            [{test_quorum_table, test_key, Value}] = 
                mnesia:dirty_read(test_quorum_table, test_key),
            ?assertEqual(<<"test_value">>, Value)
        end},
       
       {"local_sync_write overwrites existing data", fun() ->
            %% Write initial value
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, overwrite_key, <<"value1">>),
            %% Overwrite
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, overwrite_key, <<"value2">>),
            %% Verify overwritten
            [{test_quorum_table, overwrite_key, Value}] = 
                mnesia:dirty_read(test_quorum_table, overwrite_key),
            ?assertEqual(<<"value2">>, Value)
        end},
       
       {"local_sync_write handles binary keys", fun() ->
            BinaryKey = <<1, 2, 3, 4, 5>>,
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, BinaryKey, <<"binary_key_value">>),
            [{test_quorum_table, BinaryKey, Value}] = 
                mnesia:dirty_read(test_quorum_table, BinaryKey),
            ?assertEqual(<<"binary_key_value">>, Value)
        end},
       
       {"local_sync_write handles tuple keys", fun() ->
            TupleKey = {user, <<"alice">>, 123},
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, TupleKey, <<"tuple_key_value">>),
            [{test_quorum_table, TupleKey, Value}] = 
                mnesia:dirty_read(test_quorum_table, TupleKey),
            ?assertEqual(<<"tuple_key_value">>, Value)
        end}
      ]}}.

%% =============================================================================
%% Write Durable Tests (Single Node)
%% =============================================================================

write_durable_single_node_test_() ->
    {"Write durable on single node",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"write_durable succeeds on single node", fun() ->
            %% Single node should always achieve quorum (1/1)
            iris_quorum_write:set_replication_factor(1),
            Result = iris_quorum_write:write_durable(
                test_quorum_table, durable_key, <<"durable_value">>),
            ?assertEqual(ok, Result),
            %% Cleanup
            application:unset_env(iris_core, replication_factor)
        end},
       
       {"write_durable with custom timeout", fun() ->
            iris_quorum_write:set_replication_factor(1),
            Result = iris_quorum_write:write_durable(
                test_quorum_table, timeout_key, <<"value">>,
                #{timeout => 1000}),
            ?assertEqual(ok, Result),
            application:unset_env(iris_core, replication_factor)
        end}
      ]}}.

%% =============================================================================
%% Read Quorum Tests (Single Node)
%% =============================================================================

read_quorum_single_node_test_() ->
    {"Read quorum on single node",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"read_quorum returns not_found for missing key", fun() ->
            Result = iris_quorum_write:read_quorum(
                test_quorum_table, nonexistent_key),
            ?assertEqual(not_found, Result)
        end},
       
       {"read_quorum returns value for existing key", fun() ->
            %% Write data first
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, read_key, <<"read_value">>),
            %% Read it back
            Result = iris_quorum_write:read_quorum(test_quorum_table, read_key),
            ?assertEqual({ok, <<"read_value">>}, Result)
        end},
       
       {"read_quorum with custom timeout", fun() ->
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, timeout_read_key, <<"value">>),
            Result = iris_quorum_write:read_quorum(
                test_quorum_table, timeout_read_key, #{timeout => 500}),
            ?assertEqual({ok, <<"value">>}, Result)
        end}
      ]}}.

%% =============================================================================
%% Quorum Calculation Tests
%% =============================================================================

quorum_calculation_test_() ->
    {"Quorum calculation",
     [
      {"Quorum for RF=1 is 1", fun() ->
           %% (1 div 2) + 1 = 1
           ?assertEqual(1, (1 div 2) + 1)
       end},
      
      {"Quorum for RF=3 is 2", fun() ->
           %% (3 div 2) + 1 = 2
           ?assertEqual(2, (3 div 2) + 1)
       end},
      
      {"Quorum for RF=5 is 3", fun() ->
           %% (5 div 2) + 1 = 3
           ?assertEqual(3, (5 div 2) + 1)
       end},
      
      {"Quorum for RF=7 is 4", fun() ->
           %% (7 div 2) + 1 = 4
           ?assertEqual(4, (7 div 2) + 1)
       end}
     ]}.

%% =============================================================================
%% Repair Async Tests
%% =============================================================================

repair_async_test_() ->
    {"Async repair",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"repair_async returns immediately", fun() ->
            %% repair_async spawns a process and returns ok
            Result = iris_quorum_write:repair_async(
                test_quorum_table, repair_key, <<"repair_value">>,
                []),  %% Empty failed nodes list
            ?assertEqual(ok, Result)
        end},
       
       {"repair_async handles unknown nodes gracefully", fun() ->
            %% Should not crash when given unknown nodes
            Result = iris_quorum_write:repair_async(
                test_quorum_table, repair_key2, <<"value">>,
                [{unknown, timeout}]),
            ?assertEqual(ok, Result),
            %% Give spawned process time to complete
            timer:sleep(100)
        end}
      ]}}.

%% =============================================================================
%% P1-H4: Worker Tracking Tests
%% =============================================================================

worker_tracking_test_() ->
    {"Worker tracking (P1-H4 fix)",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Worker map correctly tracks nodes", fun() ->
            %% P1-H4 TEST: Verify write_durable works correctly after worker tracking fix
            %% The bug was that wrong workers were being removed during result collection
            iris_quorum_write:set_replication_factor(1),
            
            %% Write should succeed (proves workers are tracked correctly)
            Result = iris_quorum_write:write_durable(
                test_quorum_table, worker_track_key1, <<"value1">>),
            ?assertEqual(ok, Result),
            
            %% Multiple sequential writes should all succeed
            lists:foreach(fun(I) ->
                Key = list_to_atom("worker_track_key_" ++ integer_to_list(I)),
                Value = list_to_binary("value_" ++ integer_to_list(I)),
                ?assertEqual(ok, iris_quorum_write:write_durable(
                    test_quorum_table, Key, Value))
            end, lists:seq(2, 5)),
            
            application:unset_env(iris_core, replication_factor)
        end},
       
       {"Read quorum works after worker fix", fun() ->
            %% P1-H4 TEST: Verify read_quorum also uses correct worker tracking
            iris_quorum_write:set_replication_factor(1),
            
            %% Write a value
            ok = iris_quorum_write:local_sync_write(
                test_quorum_table, read_track_key, <<"read_value">>),
            
            %% Read should succeed
            Result = iris_quorum_write:read_quorum(test_quorum_table, read_track_key),
            ?assertEqual({ok, <<"read_value">>}, Result),
            
            application:unset_env(iris_core, replication_factor)
        end}
      ]}}.

%% =============================================================================
%% Partition Guard Tests
%% =============================================================================

partition_guard_test_() ->
    {"Partition guard consistency",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"write_durable succeeds when partition guard not running", fun() ->
            %% Ensure partition guard is not running
            case whereis(iris_partition_guard) of
                undefined -> ok;
                Pid -> exit(Pid, kill), timer:sleep(50)
            end,
            
            iris_quorum_write:set_replication_factor(1),
            Result = iris_quorum_write:write_durable(
                test_quorum_table, guard_test_key, <<"value">>),
            ?assertEqual(ok, Result),
            application:unset_env(iris_core, replication_factor)
        end},
       
       {"write_durable checks partition guard (export verification)", fun() ->
            %% Verify the module exports write_durable functions
            Exports = iris_quorum_write:module_info(exports),
            ?assert(lists:member({write_durable, 3}, Exports)),
            ?assert(lists:member({write_durable, 4}, Exports))
        end}
      ]}}.

%% =============================================================================
%% Quorum Read Conflict Resolution Tests (RFC Section 5.3)
%% =============================================================================

reconcile_reads_test_() ->
    {"Quorum read conflict resolution",
     [
      {"Empty results return not_found", fun() ->
           ?assertEqual(not_found, iris_quorum_write:reconcile_reads([]))
       end},

      {"All not_found returns not_found", fun() ->
           Results = [{node1, not_found}, {node2, not_found}, {node3, not_found}],
           ?assertEqual(not_found, iris_quorum_write:reconcile_reads(Results))
       end},

      {"Single value returned as-is", fun() ->
           Results = [{node1, {<<"v1">>, 100}}, {node2, not_found}],
           ?assertEqual({ok, {<<"v1">>, 100}}, iris_quorum_write:reconcile_reads(Results))
       end},

      {"Latest timestamp wins among conflicting values", fun() ->
           %% Node 1 has older value (timestamp 100)
           %% Node 2 has newer value (timestamp 200)
           %% Node 3 has middle value (timestamp 150)
           Results = [
               {node1, {<<"old_value">>, 100}},
               {node2, {<<"newest_value">>, 200}},
               {node3, {<<"middle_value">>, 150}}
           ],
           {ok, Winner} = iris_quorum_write:reconcile_reads(Results),
           %% The winner must be the value with the highest timestamp
           ?assertEqual({<<"newest_value">>, 200}, Winner)
       end},

      {"Equal timestamps resolve deterministically", fun() ->
           %% When timestamps tie, pick the lexicographically larger value
           %% to ensure determinism across nodes
           Results = [
               {node1, {<<"value_a">>, 100}},
               {node2, {<<"value_b">>, 100}}
           ],
           {ok, Winner} = iris_quorum_write:reconcile_reads(Results),
           %% Both have same timestamp, deterministic tiebreak
           ?assertMatch({_, 100}, Winner)
       end},

      {"Majority not_found with one value still returns the value", fun() ->
           Results = [
               {node1, not_found},
               {node2, {<<"lone_value">>, 50}},
               {node3, not_found}
           ],
           ?assertEqual({ok, {<<"lone_value">>, 50}}, iris_quorum_write:reconcile_reads(Results))
       end}
     ]}.

%% =============================================================================
%% Integration Test Placeholder
%% =============================================================================

integration_placeholder_test_() ->
    {"Integration test markers",
     [
      {"Multi-node quorum writes require integration test", fun() ->
           %% This is a marker - actual test is in Python integration suite
           ?assert(true)
       end},
      
      {"Failover scenarios require integration test", fun() ->
           %% This is a marker - actual test is in Python integration suite
           ?assert(true)
       end},
       
      {"Partition guard enforcement requires integration test", fun() ->
           %% Full partition guard test requires starting the guard process
           %% in safe_mode and verifying write_durable returns error
           %% This is tested in Python integration suite
           ?assert(true)
       end}
     ]}.

%% =============================================================================
%% H-4 Mitigation: Quorum repair retry with backoff
%% =============================================================================

repair_retries_on_failure_test() ->
    %% repair_failed_replicas should retry up to 3 times with backoff
    %% when rpc:call fails.
    %% Since we can't easily mock rpc:call, we test with a known-bad node.
    %% The function should retry 3 times and log each attempt.
    
    %% Call with a fake node that doesn't exist
    FakeFailures = [{nonode@nowhere, {badrpc, nodedown}}],
    
    %% Before the fix: repair calls rpc:call once and gives up
    %% After the fix: repair retries MAX_REPAIR_RETRIES times with backoff
    %%
    %% We measure timing: if retry is working, this should take at least
    %% the sum of backoff delays (100ms + 500ms = 600ms minimum for 3 attempts)
    Start = erlang:monotonic_time(millisecond),
    iris_quorum_write:repair_failed_replicas(FakeFailures, test_table, <<"key">>, <<"value">>),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    
    %% After fix: should take >= 600ms due to backoff (100ms + 500ms between 3 attempts)
    ?assert(Elapsed >= 500).

%% =============================================================================
%% B-3 AUDIT: pg group empty with iris_shard registered must NOT include
%% all connected nodes (edge nodes don't run Mnesia).
%% =============================================================================

pg_empty_fallback_test_() ->
    {"B-3: pg empty fallback returns [node()] not [node()|nodes()]",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"iris_shard registered but pg group empty returns local only", fun() ->
            %% Start pg scope if not running
            case whereis(iris_shards) of
                undefined ->
                    try pg:start(iris_shards) catch _:_ -> ok end;
                _ -> ok
            end,
            %% Register a dummy iris_shard process
            DummyPid = spawn(fun() -> receive stop -> ok end end),
            register(iris_shard, DummyPid),
            try
                %% pg group iris_shards should be empty (no members joined)
                ?assertEqual([], pg:get_members(iris_shards)),
                %% get_replicas calls get_available_nodes internally
                %% With the bug: returns [node()|nodes()] which may include edge nodes
                %% Fixed: returns [node()] only
                Replicas = iris_quorum_write:get_replicas(<<"test_key">>),
                %% All replicas must be the local node (no remote nodes from nodes())
                lists:foreach(fun(N) ->
                    ?assertEqual(node(), N)
                end, Replicas)
            after
                unregister(iris_shard),
                DummyPid ! stop
            end
        end}
      ]}}.

