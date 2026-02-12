-module(iris_durable_batcher_strict_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F4: Cluster Durability Semantics Tests (P2 Audit Finding)
%%
%% Tests verify:
%% 1. In strict mode, remote WAL failure returns {error, remote_wal_failed}
%% 2. In best_effort mode (default), remote WAL failure returns ok with warning
%%
%% Both tests use cluster durability mode with no secondary node connected,
%% so the remote write fails with {error, no_secondary}.
%% =============================================================================

-define(TEST_WAL_DIR, "/tmp/iris_strict_test_wal").

setup() ->
    %% Clean up any previous state
    cleanup_shards(),
    os:cmd("rm -rf " ++ ?TEST_WAL_DIR),
    
    %% Ensure Mnesia is running with required tables
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(offline_msg, [
        {ram_copies, [node()]},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]),
    mnesia:wait_for_tables([offline_msg], 5000),
    
    %% Configure cluster durability mode and WAL directory
    application:set_env(iris_core, durability_mode, cluster),
    application:set_env(iris_core, wal_directory, ?TEST_WAL_DIR),
    
    %% Start shard 1 (test user will be routed here via select_shard)
    {ok, Pid} = iris_durable_batcher:start_link(1),
    %% Wait for replay_wal message to be processed
    timer:sleep(100),
    {started, Pid}.

cleanup({started, _Pid}) ->
    cleanup_shards(),
    application:unset_env(iris_core, durability_mode),
    application:unset_env(iris_core, wal_directory),
    application:unset_env(iris_core, cluster_durability_strict),
    try mnesia:delete_table(offline_msg) catch _:_ -> ok end,
    application:stop(mnesia),
    os:cmd("rm -rf " ++ ?TEST_WAL_DIR),
    ok.

cleanup_shards() ->
    lists:foreach(fun(I) ->
        Name = list_to_atom("iris_durable_batcher_" ++ integer_to_list(I)),
        catch gen_server:stop(Name)
    end, lists:seq(1, 8)),
    timer:sleep(50).

iris_durable_batcher_strict_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Strict cluster mode rejects write on remote WAL failure",
       fun test_strict_cluster_rejects_on_remote_failure/0},
      {"Best-effort cluster mode allows write on remote WAL failure",
       fun test_best_effort_allows_on_remote_failure/0}
     ]}.

test_strict_cluster_rejects_on_remote_failure() ->
    %% Enable strict cluster durability
    application:set_env(iris_core, cluster_durability_strict, true),
    
    %% Route to shard 1: need a user that maps to shard 1
    %% select_shard(User) = (erlang:phash2(User, 8) + 1)
    %% Find a user that maps to shard 1
    User = find_user_for_shard(1),
    
    %% Store should fail because remote WAL is unavailable
    Result = gen_server:call(iris_durable_batcher_1, 
                             {store, User, <<"test_msg_strict">>, 1, undefined}, 10000),
    ?assertMatch({error, remote_wal_failed}, Result).

test_best_effort_allows_on_remote_failure() ->
    %% Disable strict mode (default behavior)
    application:set_env(iris_core, cluster_durability_strict, false),
    
    User = find_user_for_shard(1),
    
    %% Store should succeed despite remote WAL failure (graceful degradation)
    Result = gen_server:call(iris_durable_batcher_1, 
                             {store, User, <<"test_msg_besteffort">>, 1, undefined}, 10000),
    ?assertMatch(ok, Result).

%% Find a user binary that hashes to the given shard
find_user_for_shard(TargetShard) ->
    find_user_for_shard(TargetShard, 0).

find_user_for_shard(TargetShard, N) when N < 1000 ->
    User = <<"test_user_", (integer_to_binary(N))/binary>>,
    case (erlang:phash2(User, 8) + 1) of
        TargetShard -> User;
        _ -> find_user_for_shard(TargetShard, N + 1)
    end;
find_user_for_shard(_, _) ->
    error(could_not_find_user_for_shard).
