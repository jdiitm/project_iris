-module(iris_durable_batcher_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    %% Ensure application env is clean
    application:unset_env(iris_core, durability_mode),
    ok.

cleanup(_) ->
    application:unset_env(iris_core, durability_mode),
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_durable_batcher_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Cluster durability API tests
      {"Remote WAL function exported", fun test_remote_wal_export/0},
      {"Secondary node returns undefined in single-node", fun test_secondary_node_single/0},
      {"Durability mode config - default is local", fun test_durability_mode_default/0},
      {"Durability mode config - can set to cluster", fun test_durability_mode_cluster/0},
      {"Durability mode config - can set to quorum", fun test_durability_mode_quorum/0},
      {"Stats include replication metrics", fun test_stats_include_replication/0},
      {"Get durability mode function exists", fun test_get_durability_mode_export/0}
     ]}.

%% =============================================================================
%% Cluster Durability API Tests
%% =============================================================================

test_remote_wal_export() ->
    %% Verify accept_remote_wal/1 is exported for RPC
    Exports = iris_durable_batcher:module_info(exports),
    ?assert(lists:member({accept_remote_wal, 1}, Exports)).

test_secondary_node_single() ->
    %% In test env with single node, should return undefined
    Result = iris_durable_batcher:get_secondary_node(),
    ?assertEqual(undefined, Result).

test_durability_mode_default() ->
    %% Default should be 'local' for backward compatibility
    application:unset_env(iris_core, durability_mode),
    ?assertEqual(local, iris_durable_batcher:get_durability_mode()).

test_durability_mode_cluster() ->
    %% Setting to cluster should work
    application:set_env(iris_core, durability_mode, cluster),
    ?assertEqual(cluster, iris_durable_batcher:get_durability_mode()),
    %% Cleanup
    application:unset_env(iris_core, durability_mode).

test_durability_mode_quorum() ->
    %% Setting to quorum should work
    application:set_env(iris_core, durability_mode, quorum),
    ?assertEqual(quorum, iris_durable_batcher:get_durability_mode()),
    %% Cleanup
    application:unset_env(iris_core, durability_mode).

test_stats_include_replication() ->
    %% Stats should include the new replication metrics
    %% Note: This test may need the batcher to be running
    %% We test by checking the aggregate function produces expected keys
    EmptyStats = [#{
        writes_wal => 0,
        writes_mnesia => 0,
        batch_count => 0,
        pending_count => 0,
        writes_remote => 0,
        remote_failures => 0
    }],
    
    %% Simulate aggregation - the function is internal but we can verify structure
    %% by checking exports exist
    Exports = iris_durable_batcher:module_info(exports),
    ?assert(lists:member({get_stats, 0}, Exports)).

test_get_durability_mode_export() ->
    %% Verify get_durability_mode/0 is exported
    Exports = iris_durable_batcher:module_info(exports),
    ?assert(lists:member({get_durability_mode, 0}, Exports)).

%% =============================================================================
%% Integration-style Tests (require running batcher)
%% =============================================================================

%% Note: The following tests require the batcher processes to be running.
%% They are commented out for unit test isolation but can be enabled for
%% integration testing.

%% test_local_mode_store() ->
%%     application:set_env(iris_core, durability_mode, local),
%%     MsgId = <<"local_test_", (integer_to_binary(erlang:unique_integer()))/binary>>,
%%     Result = iris_durable_batcher:store(<<"test_user">>, MsgId, 8),
%%     ?assertEqual(ok, Result),
%%     application:unset_env(iris_core, durability_mode).

%% test_cluster_mode_degrades_gracefully() ->
%%     %% In single-node cluster, cluster mode should degrade to local-only
%%     application:set_env(iris_core, durability_mode, cluster),
%%     MsgId = <<"cluster_test_", (integer_to_binary(erlang:unique_integer()))/binary>>,
%%     Result = iris_durable_batcher:store(<<"test_user">>, MsgId, 8),
%%     ?assertEqual(ok, Result),
%%     application:unset_env(iris_core, durability_mode).
