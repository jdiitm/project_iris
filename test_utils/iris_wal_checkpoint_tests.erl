-module(iris_wal_checkpoint_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% WAL Checkpoint Tests (Unbounded WAL Growth)
%%
%% Verifies that the WAL is truncated after successful Mnesia flush,
%% retained on failure, bounded under sustained load, and stats are tracked.
%% =============================================================================

-define(TEST_SHARD, 99).
-define(TEST_WAL_DIR, "/tmp/iris_wal_test").

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

setup_test_env() ->
    %% Ensure Mnesia is running with offline_msg table
    case mnesia:system_info(is_running) of
        no -> mnesia:start();
        _ -> ok
    end,
    %% Create offline_msg table if not exists
    case lists:member(offline_msg, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(offline_msg, [
                {attributes, [key, timestamp, msg]},
                {type, set}
            ]);
        true -> ok
    end,
    mnesia:wait_for_tables([offline_msg], 5000),
    %% Set environment for test WAL dir
    application:set_env(iris_core, wal_directory, ?TEST_WAL_DIR),
    application:set_env(iris_core, env, test),
    ok.

cleanup_test_env() ->
    %% Ensure offline_msg table exists (test may have deleted it)
    case lists:member(offline_msg, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(offline_msg, [
                {attributes, [key, timestamp, msg]},
                {type, set}
            ]),
            mnesia:wait_for_tables([offline_msg], 5000);
        true -> ok
    end,
    %% Stop the test batcher if running
    Name = list_to_atom("iris_durable_batcher_" ++ integer_to_list(?TEST_SHARD)),
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            try gen_server:stop(Pid, normal, 5000)
            catch _:_ -> ok
            end
    end,
    %% Clean up WAL files
    os:cmd("rm -rf " ++ ?TEST_WAL_DIR ++ "/shard_" ++ integer_to_list(?TEST_SHARD) ++ ".*"),
    ok.

start_test_batcher() ->
    {ok, Pid} = iris_durable_batcher:start_link(?TEST_SHARD),
    Pid.

get_wal_log(Pid) ->
    gen_server:call(Pid, get_wal_log).

get_wal_items(Log) ->
    case disk_log:info(Log) of
        InfoList when is_list(InfoList) ->
            proplists:get_value(no_written_items, InfoList, -1);
        _ -> -1
    end.

store_entries(N) ->
    Name = list_to_atom("iris_durable_batcher_" ++ integer_to_list(?TEST_SHARD)),
    lists:foreach(fun(I) ->
        User = list_to_binary("wal_test_user_" ++ integer_to_list(I)),
        Msg = list_to_binary("wal_test_msg_" ++ integer_to_list(I)),
        gen_server:call(Name, {store, User, Msg, 10, undefined}, 10000)
    end, lists:seq(1, N)).

force_flush_shard() ->
    Name = list_to_atom("iris_durable_batcher_" ++ integer_to_list(?TEST_SHARD)),
    gen_server:call(Name, force_flush, 30000).

get_shard_stats() ->
    Name = list_to_atom("iris_durable_batcher_" ++ integer_to_list(?TEST_SHARD)),
    gen_server:call(Name, get_stats_local, 5000).

%% ---------------------------------------------------------------------------
%% Test 1: WAL is truncated after successful flush
%% ---------------------------------------------------------------------------
wal_truncated_after_successful_flush_test_() ->
    {setup,
     fun() -> setup_test_env(), start_test_batcher() end,
     fun(_) -> cleanup_test_env() end,
     fun(Pid) ->
         [?_test(begin
             Log = get_wal_log(Pid),
             ?assert(Log =/= undefined),
             %% Write entries
             store_entries(10),
             %% WAL should have items before flush
             ItemsBefore = get_wal_items(Log),
             ?assert(ItemsBefore > 0),
             %% Flush to Mnesia
             force_flush_shard(),
             %% After flush, WAL should be truncated (0 items written since truncate)
             ItemsAfter = get_wal_items(Log),
             ?assert(ItemsAfter < ItemsBefore)
         end)]
     end}.

%% ---------------------------------------------------------------------------
%% Test 2: WAL retains entries on flush failure
%% ---------------------------------------------------------------------------
wal_retains_entries_on_flush_failure_test_() ->
    {setup,
     fun() ->
         setup_test_env(),
         Pid = start_test_batcher(),
         Pid
     end,
     fun(_) -> cleanup_test_env() end,
     fun(Pid) ->
         [?_test(begin
             Log = get_wal_log(Pid),
             ?assert(Log =/= undefined),
             %% Write entries
             store_entries(5),
             ItemsBefore = get_wal_items(Log),
             ?assert(ItemsBefore > 0),
             %% Sabotage Mnesia by deleting the table temporarily
             mnesia:delete_table(offline_msg),
             %% Attempt flush - should fail internally
             force_flush_shard(),
             %% WAL should still have items (not truncated on failure)
             ItemsAfter = get_wal_items(Log),
             ?assert(ItemsAfter >= ItemsBefore),
             %% Restore table for other tests
             mnesia:create_table(offline_msg, [
                 {attributes, [key, timestamp, msg]},
                 {type, set}
             ]),
             mnesia:wait_for_tables([offline_msg], 5000)
         end)]
     end}.

%% ---------------------------------------------------------------------------
%% Test 3: WAL bounded under sustained load (50 write-flush cycles)
%% ---------------------------------------------------------------------------
wal_bounded_under_sustained_load_test_() ->
    {timeout, 60,
     {setup,
      fun() -> setup_test_env(), start_test_batcher() end,
      fun(_) -> cleanup_test_env() end,
      fun(Pid) ->
          [?_test(begin
              Log = get_wal_log(Pid),
              ?assert(Log =/= undefined),
              %% Run 50 write-flush cycles
              MaxItems = lists:foldl(fun(_, AccMax) ->
                  store_entries(5),
                  force_flush_shard(),
                  Items = get_wal_items(Log),
                  max(Items, AccMax)
              end, 0, lists:seq(1, 50)),
              %% After each flush, WAL should be truncated
              %% Under unbounded growth, no_written_items grows monotonically
              %% With checkpoint, it resets after each flush
              FinalItems = get_wal_items(Log),
              %% Final items should be 0 (just truncated after flush)
              ?assert(FinalItems < 20),
              %% Max items at any point should be bounded
              %% Each cycle writes 5 entries + committed markers (~10 items)
              %% Without truncation, after 50 cycles: ~500+ items
              ?assert(MaxItems < 50)
          end)]
      end}}.

%% ---------------------------------------------------------------------------
%% Test 4: wal_checkpoints counter in stats
%% ---------------------------------------------------------------------------
checkpoint_count_in_stats_test_() ->
    {setup,
     fun() -> setup_test_env(), start_test_batcher() end,
     fun(_) -> cleanup_test_env() end,
     fun(_Pid) ->
         [?_test(begin
             %% Write and flush multiple times
             store_entries(3),
             force_flush_shard(),
             store_entries(3),
             force_flush_shard(),
             store_entries(3),
             force_flush_shard(),
             %% Stats should include wal_checkpoints counter
             Stats = get_shard_stats(),
             ?assert(is_map(Stats)),
             ?assert(maps:is_key(wal_checkpoints, Stats)),
             Checkpoints = maps:get(wal_checkpoints, Stats),
             ?assert(Checkpoints >= 3)
         end)]
     end}.
