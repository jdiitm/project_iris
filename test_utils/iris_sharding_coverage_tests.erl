-module(iris_sharding_coverage_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Shard Coverage Tests
%% =============================================================================
%% Written BEFORE implementation. These tests define the contract:
%%   - ALL shards must be covered by available nodes (zero gaps)
%%   - No two nodes claim the same shard (zero overlap)
%%   - Distribution is balanced (floor or ceil of N/K per node)
%%   - Node ordering in AllNodes list doesn't affect assignment
%% =============================================================================

%% Test: Single node must cover ALL 4096 shards
single_node_covers_all_shards_test() ->
    Node = 'iris@node1',
    AllNodes = ['iris@node1'],
    ShardCount = 4096,
    Shards = iris_shard:calculate_shards_for_node(Node, AllNodes, ShardCount),
    ?assertEqual(ShardCount, length(Shards)),
    ?assertEqual(lists:seq(0, ShardCount - 1), lists:sort(Shards)).

%% Test: Two nodes split shards evenly (2048 each)
two_nodes_even_split_test() ->
    AllNodes = ['iris@a', 'iris@b'],
    ShardCount = 4096,
    S1 = iris_shard:calculate_shards_for_node('iris@a', AllNodes, ShardCount),
    S2 = iris_shard:calculate_shards_for_node('iris@b', AllNodes, ShardCount),
    ?assertEqual(2048, length(S1)),
    ?assertEqual(2048, length(S2)),
    %% Union is complete
    ?assertEqual(lists:seq(0, ShardCount - 1), lists:sort(S1 ++ S2)).

%% Test: Three nodes cover all shards with no gaps
three_nodes_full_coverage_test() ->
    AllNodes = ['iris@a', 'iris@b', 'iris@c'],
    ShardCount = 4096,
    S1 = iris_shard:calculate_shards_for_node('iris@a', AllNodes, ShardCount),
    S2 = iris_shard:calculate_shards_for_node('iris@b', AllNodes, ShardCount),
    S3 = iris_shard:calculate_shards_for_node('iris@c', AllNodes, ShardCount),
    Union = lists:usort(S1 ++ S2 ++ S3),
    ?assertEqual(ShardCount, length(Union)),
    ?assertEqual(lists:seq(0, ShardCount - 1), Union).

%% Test: No two nodes claim the same shard (zero overlap)
no_shard_overlap_test() ->
    AllNodes = ['iris@node1', 'iris@node2', 'iris@node3'],
    ShardCount = 4096,
    S1 = iris_shard:calculate_shards_for_node('iris@node1', AllNodes, ShardCount),
    S2 = iris_shard:calculate_shards_for_node('iris@node2', AllNodes, ShardCount),
    S3 = iris_shard:calculate_shards_for_node('iris@node3', AllNodes, ShardCount),
    Set1 = sets:from_list(S1),
    Set2 = sets:from_list(S2),
    Set3 = sets:from_list(S3),
    ?assertEqual(0, sets:size(sets:intersection(Set1, Set2))),
    ?assertEqual(0, sets:size(sets:intersection(Set2, Set3))),
    ?assertEqual(0, sets:size(sets:intersection(Set1, Set3))).

%% Test: Balance — each node gets floor(N/K) or ceil(N/K) shards
balanced_distribution_test() ->
    AllNodes = ['iris@a', 'iris@b', 'iris@c', 'iris@d', 'iris@e'],
    ShardCount = 4096,
    NodeCount = length(AllNodes),
    MinPerNode = ShardCount div NodeCount,
    MaxPerNode = MinPerNode + 1,
    lists:foreach(fun(Node) ->
        Shards = iris_shard:calculate_shards_for_node(Node, AllNodes, ShardCount),
        Count = length(Shards),
        ?assert(Count >= MinPerNode),
        ?assert(Count =< MaxPerNode)
    end, AllNodes).

%% Test: Order of AllNodes list doesn't matter (sorted internally)
node_order_independent_test() ->
    Nodes1 = ['iris@c', 'iris@a', 'iris@b'],
    Nodes2 = ['iris@a', 'iris@b', 'iris@c'],
    ShardCount = 4096,
    S1 = iris_shard:calculate_shards_for_node('iris@a', Nodes1, ShardCount),
    S2 = iris_shard:calculate_shards_for_node('iris@a', Nodes2, ShardCount),
    ?assertEqual(S1, S2).

%% Test: Edge case — more nodes than shards
more_nodes_than_shards_test() ->
    AllNodes = ['iris@a', 'iris@b', 'iris@c', 'iris@d', 'iris@e'],
    ShardCount = 3,
    AllShards = lists:flatmap(fun(N) ->
        iris_shard:calculate_shards_for_node(N, AllNodes, ShardCount)
    end, AllNodes),
    %% All shards covered
    ?assertEqual(lists:seq(0, ShardCount - 1), lists:usort(AllShards)),
    %% Total shard assignments == ShardCount (no duplicates)
    ?assertEqual(ShardCount, length(AllShards)).

%% Test: Node not in list gets empty shard assignment
unknown_node_gets_no_shards_test() ->
    AllNodes = ['iris@a', 'iris@b'],
    ShardCount = 4096,
    Shards = iris_shard:calculate_shards_for_node('iris@unknown', AllNodes, ShardCount),
    ?assertEqual([], Shards).

%% Test: 1000 random user IDs all resolve to a valid shard range
all_users_map_to_valid_shard_test() ->
    ShardCount = 4096,
    Users = [list_to_binary("user_" ++ integer_to_list(I)) || I <- lists:seq(1, 1000)],
    lists:foreach(fun(User) ->
        Shard = iris_shard:get_shard(User),
        ?assert(Shard >= 0),
        ?assert(Shard < ShardCount)
    end, Users).
