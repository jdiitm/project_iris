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
      {"Stats tracks duplicates", fun test_stats_tracks_dups/0}
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
