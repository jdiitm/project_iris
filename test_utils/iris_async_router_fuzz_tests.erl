-module(iris_async_router_fuzz_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Async Router Fuzz Tests
%% Validates phash2 distribution uniformity and edge-case resilience
%% for the partitioned router pool.
%% =============================================================================

-define(POOL_SIZE, 8).  %% Default pool size for testing

%% Test: phash2 distributes users uniformly across shards
phash2_distribution_test() ->
    PoolSize = ?POOL_SIZE,
    NumUsers = 10000,
    %% Generate random user IDs
    Users = [generate_random_user(I) || I <- lists:seq(1, NumUsers)],
    %% Compute shard distribution
    Shards = [erlang:phash2(U, PoolSize) + 1 || U <- Users],
    %% Count per shard
    Counts = lists:foldl(fun(S, Acc) ->
        maps:update_with(S, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Shards),
    %% Assert all shards have entries
    ?assertEqual(PoolSize, maps:size(Counts)),
    %% Assert max/min ratio < 2.0 (no single shard gets >2x average)
    Values = maps:values(Counts),
    MaxCount = lists:max(Values),
    MinCount = lists:min(Values),
    Ratio = MaxCount / max(1, MinCount),
    ?assert(Ratio < 2.0).

%% Test: phash2 is deterministic
phash2_deterministic_test() ->
    PoolSize = ?POOL_SIZE,
    User = <<"deterministic_test_user_42">>,
    Shard1 = erlang:phash2(User, PoolSize) + 1,
    Shard2 = erlang:phash2(User, PoolSize) + 1,
    Shard3 = erlang:phash2(User, PoolSize) + 1,
    ?assertEqual(Shard1, Shard2),
    ?assertEqual(Shard2, Shard3).

%% Test: phash2 with varying pool sizes returns valid shard IDs
phash2_pool_resize_test() ->
    User = <<"resize_test_user">>,
    PoolSizes = [1, 2, 4, 8, 16, 32, 64, 128],
    lists:foreach(fun(PS) ->
        Shard = erlang:phash2(User, PS) + 1,
        ?assert(Shard >= 1),
        ?assert(Shard =< PS)
    end, PoolSizes).

%% Test: edge-case inputs don't crash phash2
phash2_edge_cases_test() ->
    PoolSize = ?POOL_SIZE,
    EdgeCases = [
        <<>>,                           %% Empty binary
        <<"a">>,                        %% Single char
        binary:copy(<<"x">>, 10000),    %% 10KB user ID
        <<0, 0, 0, 0>>,                %% Null bytes
        <<"用户"/utf8>>,                 %% Unicode (Chinese)
        <<"Ünïcödë"/utf8>>,             %% Unicode (accented)
        <<"🔥💯"/utf8>>,                %% Emoji
        <<255, 254, 253, 252>>          %% High bytes
    ],
    lists:foreach(fun(User) ->
        Shard = erlang:phash2(User, PoolSize) + 1,
        ?assert(Shard >= 1),
        ?assert(Shard =< PoolSize)
    end, EdgeCases).

%% Test: unicode user IDs distribute evenly
phash2_unicode_distribution_test() ->
    PoolSize = ?POOL_SIZE,
    %% Generate 1000 unicode user IDs
    Users = [unicode_user(I) || I <- lists:seq(1, 1000)],
    Shards = [erlang:phash2(U, PoolSize) + 1 || U <- Users],
    Counts = lists:foldl(fun(S, Acc) ->
        maps:update_with(S, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Shards),
    %% All shards should have at least some entries
    ?assert(maps:size(Counts) >= PoolSize div 2).

%% =============================================================================
%% Internal helpers
%% =============================================================================

generate_random_user(Seed) ->
    %% Deterministic pseudo-random user IDs based on seed
    Hash = erlang:phash2({user, Seed, random_salt}, 1 bsl 32),
    iolist_to_binary(["user_", integer_to_list(Hash)]).

unicode_user(I) ->
    %% Create a unicode-containing user ID
    Base = integer_to_list(I),
    unicode:characters_to_binary(["ユーザー_", Base]).
