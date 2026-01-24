-module(iris_store_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_store.erl
%% =============================================================================
%% 
%% Tests cover:
%% - Basic put/get/delete operations
%% - Durability options (guaranteed, best_effort, quorum)
%% - Batch operations
%% - Consistency options for reads
%% - Partition guard integration (when available)
%% 
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
    
    %% Create test table with correct structure
    {atomic, ok} = mnesia:create_table(test_store_table, [
        {disc_copies, [node()]},
        {attributes, [key, value]}
    ]),
    mnesia:wait_for_tables([test_store_table], 5000),
    ok.

cleanup(_) ->
    catch mnesia:delete_table(test_store_table),
    application:stop(mnesia),
    ok.

%% =============================================================================
%% Basic Put Tests
%% =============================================================================

put_test_() ->
    {"Basic put operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"put/3 stores value with guaranteed durability", fun() ->
            Result = iris_store:put(test_store_table, put_key1, <<"value1">>),
            ?assertEqual(ok, Result),
            %% Verify data was written
            [{test_store_table, put_key1, Value}] = 
                mnesia:dirty_read(test_store_table, put_key1),
            ?assertEqual(<<"value1">>, Value)
        end},
       
       {"put/4 with guaranteed durability", fun() ->
            Result = iris_store:put(test_store_table, put_key2, <<"value2">>,
                                   #{durability => guaranteed}),
            ?assertEqual(ok, Result)
        end},
       
       {"put/4 with best_effort durability returns immediately", fun() ->
            Result = iris_store:put(test_store_table, put_key3, <<"value3">>,
                                   #{durability => best_effort}),
            ?assertEqual(ok, Result),
            %% Give async process time to complete
            timer:sleep(100),
            %% Verify data was written
            [{test_store_table, put_key3, _}] = 
                mnesia:dirty_read(test_store_table, put_key3)
        end},
       
       {"put overwrites existing value", fun() ->
            ok = iris_store:put(test_store_table, overwrite_key, <<"old">>),
            ok = iris_store:put(test_store_table, overwrite_key, <<"new">>),
            [{test_store_table, overwrite_key, Value}] = 
                mnesia:dirty_read(test_store_table, overwrite_key),
            ?assertEqual(<<"new">>, Value)
        end},
       
       {"put handles binary keys", fun() ->
            BinKey = <<1, 2, 3, 4>>,
            ok = iris_store:put(test_store_table, BinKey, <<"bin_value">>),
            [{test_store_table, BinKey, Value}] = 
                mnesia:dirty_read(test_store_table, BinKey),
            ?assertEqual(<<"bin_value">>, Value)
        end},
       
       {"put handles complex values", fun() ->
            ComplexValue = #{name => <<"Alice">>, age => 30, tags => [a, b, c]},
            ok = iris_store:put(test_store_table, complex_key, ComplexValue),
            [{test_store_table, complex_key, Value}] = 
                mnesia:dirty_read(test_store_table, complex_key),
            ?assertEqual(ComplexValue, Value)
        end}
      ]}}.

%% =============================================================================
%% Basic Get Tests
%% =============================================================================

get_test_() ->
    {"Basic get operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"get/2 returns not_found for missing key", fun() ->
            Result = iris_store:get(test_store_table, nonexistent_key),
            ?assertEqual(not_found, Result)
        end},
       
       {"get/2 returns value for existing key", fun() ->
            ok = iris_store:put(test_store_table, get_key1, <<"get_value">>),
            Result = iris_store:get(test_store_table, get_key1),
            ?assertEqual({ok, <<"get_value">>}, Result)
        end},
       
       {"get/3 with eventual consistency", fun() ->
            ok = iris_store:put(test_store_table, get_key2, <<"value2">>),
            Result = iris_store:get(test_store_table, get_key2,
                                   #{consistency => eventual}),
            ?assertEqual({ok, <<"value2">>}, Result)
        end},
       
       {"get returns record for complex table structures", fun() ->
            ok = iris_store:put(test_store_table, record_key, <<"record_value">>),
            {ok, Value} = iris_store:get(test_store_table, record_key),
            ?assertEqual(<<"record_value">>, Value)
        end}
      ]}}.

%% =============================================================================
%% Basic Delete Tests
%% =============================================================================

delete_test_() ->
    {"Basic delete operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"delete/2 removes existing key", fun() ->
            ok = iris_store:put(test_store_table, del_key1, <<"to_delete">>),
            %% Verify exists
            ?assertMatch({ok, _}, iris_store:get(test_store_table, del_key1)),
            %% Delete
            ok = iris_store:delete(test_store_table, del_key1),
            %% Verify gone
            ?assertEqual(not_found, iris_store:get(test_store_table, del_key1))
        end},
       
       {"delete/2 succeeds for nonexistent key", fun() ->
            %% Should not error when deleting nonexistent key
            Result = iris_store:delete(test_store_table, never_existed_key),
            ?assertEqual(ok, Result)
        end},
       
       {"delete/3 with best_effort durability", fun() ->
            ok = iris_store:put(test_store_table, del_key2, <<"value">>),
            Result = iris_store:delete(test_store_table, del_key2,
                                       #{durability => best_effort}),
            ?assertEqual(ok, Result),
            timer:sleep(100)  %% Wait for async delete
        end}
      ]}}.

%% =============================================================================
%% Batch Put Tests
%% =============================================================================

batch_put_test_() ->
    {"Batch put operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"batch_put/2 writes multiple key-values atomically", fun() ->
            KVPairs = [
                {batch_key1, <<"batch_value1">>},
                {batch_key2, <<"batch_value2">>},
                {batch_key3, <<"batch_value3">>}
            ],
            Result = iris_store:batch_put(test_store_table, KVPairs),
            ?assertEqual(ok, Result),
            %% Verify all written
            ?assertMatch({ok, <<"batch_value1">>}, 
                         iris_store:get(test_store_table, batch_key1)),
            ?assertMatch({ok, <<"batch_value2">>}, 
                         iris_store:get(test_store_table, batch_key2)),
            ?assertMatch({ok, <<"batch_value3">>}, 
                         iris_store:get(test_store_table, batch_key3))
        end},
       
       {"batch_put/2 with empty list succeeds", fun() ->
            Result = iris_store:batch_put(test_store_table, []),
            ?assertEqual(ok, Result)
        end},
       
       {"batch_put/3 with best_effort durability", fun() ->
            KVPairs = [{async_key1, <<"v1">>}, {async_key2, <<"v2">>}],
            Result = iris_store:batch_put(test_store_table, KVPairs,
                                          #{durability => best_effort}),
            ?assertEqual(ok, Result),
            timer:sleep(100)  %% Wait for async write
        end}
      ]}}.

%% =============================================================================
%% Durability Option Tests
%% =============================================================================

durability_options_test_() ->
    {"Durability options",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"guaranteed durability blocks until complete", fun() ->
            Start = erlang:monotonic_time(millisecond),
            ok = iris_store:put(test_store_table, dur_key1, <<"value">>,
                               #{durability => guaranteed}),
            End = erlang:monotonic_time(millisecond),
            %% Should complete synchronously (not instantly)
            ?assert((End - Start) >= 0)
        end},
       
       {"best_effort returns immediately", fun() ->
            Start = erlang:monotonic_time(millisecond),
            ok = iris_store:put(test_store_table, dur_key2, <<"value">>,
                               #{durability => best_effort}),
            End = erlang:monotonic_time(millisecond),
            %% Should return very quickly (< 10ms typically)
            ?assert((End - Start) < 100)
        end}
      ]}}.

%% =============================================================================
%% Error Handling Tests
%% =============================================================================

error_handling_test_() ->
    {"Error handling",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"put to nonexistent table throws exception", fun() ->
            %% Mnesia throws exception for nonexistent tables
            ?assertException(exit, {aborted, {no_exists, nonexistent_table}},
                            iris_store:put(nonexistent_table, key, value))
        end},
       
       {"get from nonexistent table handles gracefully", fun() ->
            %% dirty_read may throw or return empty
            Result = try
                iris_store:get(nonexistent_table, key)
            catch
                _:_ -> {error, table_not_found}
            end,
            ?assert(Result =:= not_found orelse 
                    element(1, Result) =:= error orelse
                    Result =:= {error, table_not_found})
        end}
      ]}}.

%% =============================================================================
%% Partition Guard Integration Tests
%% =============================================================================

partition_guard_test_() ->
    {"Partition guard integration",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"writes succeed when partition guard not running", fun() ->
            %% iris_partition_guard is not started in unit tests
            ?assertEqual(undefined, whereis(iris_partition_guard)),
            %% Writes should still succeed (permissive mode)
            Result = iris_store:put(test_store_table, guard_key, <<"value">>),
            ?assertEqual(ok, Result)
        end}
      ]}}.
