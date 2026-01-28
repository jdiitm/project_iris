-module(iris_shard_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test Jump Consistent Hash Stability
jump_hash_stability_test() ->
    %% Test that increasing bucket count moves minimal keys
    KeyCount = 1000,
    BucketsBefore = 100,
    BucketsAfter = 101,
    
    Keys = lists:seq(1, KeyCount),
    
    %% Calculate initial mapping
    Mapping1 = [{K, iris_shard:jump_hash(K, BucketsBefore)} || K <- Keys],
    
    %% Calculate new mapping
    Mapping2 = [{K, iris_shard:jump_hash(K, BucketsAfter)} || K <- Keys],
    
    %% Count how many keys moved
    Moved = count_differences(Mapping1, Mapping2),
    
    %% Expected movement is approx 1/N = 1/101 ~= 1%
    %% Allow some variance, but should be < 5% (vs 99% for modulo)
    ?debugFmt("Moved keys: ~p/~p", [Moved, KeyCount]),
    ?assert(Moved < (KeyCount * 0.05)).

%% Test Uniform Distribution
jump_hash_distribution_test() ->
    KeyCount = 10000,
    Buckets = 10,
    Keys = [erlang:phash2(X) || X <- lists:seq(1, KeyCount)],
    
    Counts = lists:foldl(fun(K, Acc) ->
        Bucket = iris_shard:jump_hash(K, Buckets),
        maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Keys),
    
    %% Check that no bucket deviates by more than 20% from mean
    Expected = KeyCount / Buckets,
    maps:foreach(fun(_B, Count) ->
        Diff = abs(Count - Expected),
        ?assert(Diff < (Expected * 0.2))
    end, Counts).

count_differences([], []) -> 0;
count_differences([{K, V1} | T1], [{K, V2} | T2]) when V1 == V2 ->
    count_differences(T1, T2);
count_differences([{K, _V1} | T1], [{K, _V2} | T2]) ->
    1 + count_differences(T1, T2).
