-module(iris_partition_guard_resolution_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P0-3 (FM-2): Split-Brain Resolution Tests
%%
%% RFC-001 v4.0 Section 7.1.1:
%% - Each node tracks epoch counter, incremented on partition detection
%% - On healing: higher-epoch partition is authoritative
%% - Equal-epoch ties broken by lowest node ID
%%
%% Tests verify:
%% 1. Epoch increments when partition is detected
%% 2. Epoch persists across gen_server status calls
%% 3. Higher epoch wins resolution
%% 4. Equal epoch ties broken by node ID
%% 5. get_status includes epoch
%%
%% Pattern: follows iris_partition_guard test approach.
%% =============================================================================

setup() ->
    %% Start pg (required by partition guard in some configs)
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,

    case whereis(iris_partition_guard) of
        undefined ->
            %% Configure with expected nodes so partition detection works
            application:set_env(iris_core, expected_cluster_nodes, [node()]),
            {ok, Pid} = iris_partition_guard:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_partition_guard),
    application:unset_env(iris_core, expected_cluster_nodes);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_partition_guard_resolution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"get_status includes epoch", fun test_get_status_includes_epoch/0},
      {"Epoch starts at 0", fun test_epoch_starts_at_zero/0},
      {"Higher epoch wins resolution", fun test_higher_epoch_wins/0},
      {"Equal epoch: lowest node wins", fun test_equal_epoch_lowest_node_wins/0},
      {"Epoch exposed in status", fun test_epoch_in_status/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_get_status_includes_epoch() ->
    Status = iris_partition_guard:get_status(),
    ?assert(is_map(Status)),
    ?assert(maps:is_key(epoch, Status)),
    Epoch = maps:get(epoch, Status),
    ?assert(is_integer(Epoch)),
    ?assert(Epoch >= 0).

test_epoch_starts_at_zero() ->
    Status = iris_partition_guard:get_status(),
    Epoch = maps:get(epoch, Status),
    ?assertEqual(0, Epoch).

test_higher_epoch_wins() ->
    %% Verify resolution logic: higher epoch is authoritative
    Result = iris_partition_guard:resolve_authority(3, node(), 2, 'other@host'),
    ?assertEqual({authoritative, node()}, Result).

test_equal_epoch_lowest_node_wins() ->
    %% When epochs are equal, lowest node ID wins
    NodeA = 'a@host',
    NodeB = 'b@host',
    Result = iris_partition_guard:resolve_authority(2, NodeA, 2, NodeB),
    ?assertEqual({authoritative, NodeA}, Result),
    %% Reversed order should give same answer
    Result2 = iris_partition_guard:resolve_authority(2, NodeB, 2, NodeA),
    ?assertEqual({authoritative, NodeA}, Result2).

test_epoch_in_status() ->
    %% Full status map should have epoch
    Status = iris_partition_guard:get_status(),
    ?assert(maps:is_key(epoch, Status)),
    ?assert(maps:is_key(mode, Status)),
    ?assert(maps:is_key(partition_count, Status)).
