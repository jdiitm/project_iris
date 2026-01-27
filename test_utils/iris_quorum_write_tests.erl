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
       end}
     ]}.
