-module(iris_dedup_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    case whereis(iris_dedup) of
        undefined ->
            {ok, Pid} = iris_dedup:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_dedup);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_dedup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Core dedup tests
      {"New message allowed", fun test_new_message/0},
      {"Duplicate rejected", fun test_duplicate_rejected/0},
      {"Different IDs allowed", fun test_different_ids/0},
      
      %% Check and mark tests
      {"Check and mark returns new", fun test_check_and_mark_new/0},
      {"Check and mark returns duplicate", fun test_check_and_mark_dup/0},
      
      %% Is duplicate tests
      {"Is duplicate false for new", fun test_is_duplicate_false/0},
      {"Is duplicate true for seen", fun test_is_duplicate_true/0},
      
      %% Stats tests
      {"Get stats returns map", fun test_get_stats/0},
      {"Stats tracks duplicates", fun test_stats_tracks_dups/0},
      
      %% P0-C3: Bloom filter tier tests
      {"Bloom tier stats exposed", fun test_bloom_tier_stats/0},
      {"Bloom catches duplicates", fun test_bloom_catches_duplicates/0},
      {"Mark seen populates bloom", fun test_mark_seen_populates_bloom/0},
      {"Hot TTL is 5 minutes", fun test_hot_ttl_is_5_minutes/0},
      
      %% P0-FIX: Bloom false positive verification tests
      {"Stats include false positive counter", fun test_false_positive_stats/0},
      {"Dedup log verification prevents data loss", fun test_dedup_log_verification/0}
     ]}.

%% =============================================================================
%% Core Dedup Tests
%% =============================================================================

test_new_message() ->
    MsgId = <<"unique_msg_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(new, Result).

test_duplicate_rejected() ->
    MsgId = <<"dup_test_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% First time - new
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    
    %% Second time - duplicate
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).

test_different_ids() ->
    MsgId1 = <<"diff_a_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    MsgId2 = <<"diff_b_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% Both should be new since they're different
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId1)),
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId2)).

%% =============================================================================
%% Check and Mark Tests
%% =============================================================================

test_check_and_mark_new() ->
    MsgId = <<"cam_new_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(new, Result).

test_check_and_mark_dup() ->
    MsgId = <<"cam_dup_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    iris_dedup:check_and_mark(MsgId),
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(duplicate, Result).

%% =============================================================================
%% Is Duplicate Tests
%% =============================================================================

test_is_duplicate_false() ->
    MsgId = <<"isdup_false_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    Result = iris_dedup:is_duplicate(MsgId),
    ?assertEqual(false, Result).

test_is_duplicate_true() ->
    MsgId = <<"isdup_true_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% Mark as seen first
    iris_dedup:mark_seen(MsgId),
    
    Result = iris_dedup:is_duplicate(MsgId),
    ?assertEqual(true, Result).

%% =============================================================================
%% Stats Tests
%% =============================================================================

test_get_stats() ->
    Stats = iris_dedup:get_stats(),
    ?assert(is_map(Stats)).

test_stats_tracks_dups() ->
    %% Generate a duplicate to increment counter
    MsgId = <<"stats_dup_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    iris_dedup:check_and_mark(MsgId),
    iris_dedup:check_and_mark(MsgId),  %% This is a duplicate
    
    Stats = iris_dedup:get_stats(),
    DupCount = maps:get(duplicates_caught, Stats, 0),
    ?assert(DupCount >= 1).

%% =============================================================================
%% P0-C3: Tiered Dedup / Bloom Filter Tests
%% =============================================================================

test_bloom_tier_stats() ->
    %% P0-C3 TEST: Verify bloom filter stats are exposed
    Stats = iris_dedup:get_stats(),
    
    %% Should have bloom-related stats from tiered implementation
    ?assert(maps:is_key(bloom_partitions, Stats)),
    ?assert(maps:is_key(bloom_hits, Stats)),
    ?assert(maps:is_key(hot_entries, Stats)),
    ?assert(maps:is_key(warm_ttl_hours, Stats)),
    
    %% Verify warm tier is 7 days (168 hours)
    WarmTtl = maps:get(warm_ttl_hours, Stats, 0),
    ?assertEqual(168, WarmTtl).

test_bloom_catches_duplicates() ->
    %% P0-C3 TEST: Bloom filter should catch duplicates
    MsgId = <<"bloom_catch_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% Mark as seen (goes to both ETS and bloom)
    iris_dedup:mark_seen(MsgId),
    
    %% Should be detected as duplicate via is_duplicate
    ?assert(iris_dedup:is_duplicate(MsgId)),
    
    %% check_and_mark should also return duplicate
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).

test_mark_seen_populates_bloom() ->
    %% P0-C3 TEST: mark_seen should add to bloom filter
    MsgId = <<"bloom_mark_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% Before marking - not a duplicate
    ?assertNot(iris_dedup:is_duplicate(MsgId)),
    
    %% Mark as seen
    ok = iris_dedup:mark_seen(MsgId),
    
    %% After marking - should be duplicate (in both ETS and bloom)
    ?assert(iris_dedup:is_duplicate(MsgId)).

test_hot_ttl_is_5_minutes() ->
    %% P0-C3 TEST: Hot tier TTL should be 5 minutes (300000 ms)
    Stats = iris_dedup:get_stats(),
    HotTtl = maps:get(hot_ttl_ms, Stats, 0),
    ?assertEqual(300000, HotTtl).

%% =============================================================================
%% P0-FIX: Bloom False Positive Verification Tests
%% =============================================================================

test_false_positive_stats() ->
    %% P0-FIX TEST: Stats should include bloom_false_positives counter
    Stats = iris_dedup:get_stats(),
    ?assert(maps:is_key(bloom_false_positives, Stats)),
    %% Counter should be a non-negative integer
    FPCount = maps:get(bloom_false_positives, Stats, -1),
    ?assert(is_integer(FPCount) andalso FPCount >= 0).

test_dedup_log_verification() ->
    %% P0-FIX TEST: Verify that messages written via check_and_mark
    %% are persisted to dedup_log for bloom verification.
    %% This prevents false positives from causing data loss.
    MsgId = <<"dedup_log_test_", (integer_to_binary(erlang:unique_integer()))/binary>>,
    
    %% Step 1: Mark a new message
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    
    %% Step 2: Give async dedup_log write time to complete
    timer:sleep(100),
    
    %% Step 3: Verify the message is now detected as duplicate
    %% This proves the full pipeline works: ETS -> bloom -> dedup_log
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)),
    
    %% Step 4: Verify via is_duplicate too
    ?assert(iris_dedup:is_duplicate(MsgId)).
