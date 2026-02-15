-module(iris_wal_size_limit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-2: WAL Size Limit Tests
%% =============================================================================
%% Requirement: disk_log must use wrap type with bounded size to prevent
%% unbounded disk growth. Each shard WAL is capped at ~100MB with 3 files.
%% =============================================================================

-define(TEST_WAL_DIR, "/tmp/iris_wal_test_" ++ integer_to_list(erlang:system_time(microsecond))).

setup() ->
    Dir = ?TEST_WAL_DIR,
    ok = filelib:ensure_dir(Dir ++ "/"),
    %% Configure non-production to avoid tmpfs check failures
    application:set_env(iris_core, deployment_mode, development),
    application:set_env(iris_core, env, test),
    application:set_env(iris_core, wal_directory, Dir),
    Dir.

cleanup(Dir) ->
    %% Stop any running batcher shards
    catch gen_server:stop(iris_durable_batcher_99),
    %% Clean up WAL files
    os:cmd("rm -rf " ++ Dir),
    application:unset_env(iris_core, wal_directory).

%% =============================================================================
%% Test: WAL opens as wrap type (not halt)
%% =============================================================================
wal_opens_as_wrap_type_test() ->
    Dir = setup(),
    try
        {ok, Pid} = iris_durable_batcher:start_link(99),
        %% Get the WAL log name
        LogName = iris_wal_99,
        Info = disk_log:info(LogName),
        Type = proplists:get_value(type, Info),
        ?assertEqual(wrap, Type),
        gen_server:stop(Pid)
    after
        cleanup(Dir)
    end.

%% =============================================================================
%% Test: WAL has size limit configured
%% =============================================================================
wal_has_size_limit_test() ->
    Dir = setup(),
    try
        {ok, Pid} = iris_durable_batcher:start_link(99),
        LogName = iris_wal_99,
        Info = disk_log:info(LogName),
        Size = proplists:get_value(size, Info),
        ?assertMatch({MaxBytes, MaxFiles} when MaxBytes > 0 andalso MaxFiles > 0, Size),
        gen_server:stop(Pid)
    after
        cleanup(Dir)
    end.

%% =============================================================================
%% Test: WAL wraps when full (no ENOSPC)
%% =============================================================================
wal_wraps_when_full_test() ->
    Dir = setup(),
    try
        %% Use a tiny WAL size to force rotation quickly
        application:set_env(iris_core, wal_max_bytes_per_shard, 1024),
        {ok, Pid} = iris_durable_batcher:start_link(99),
        LogName = iris_wal_99,
        %% Write enough data to exceed one WAL file
        LargePayload = list_to_binary(lists:duplicate(200, $X)),
        lists:foreach(fun(I) ->
            ok = disk_log:blog(LogName, <<I:32, LargePayload/binary>>)
        end, lists:seq(1, 20)),
        %% Check that rotation happened — no_overflows counts file wraps
        Info = disk_log:info(LogName),
        NoOverflows = proplists:get_value(no_overflows, Info),
        %% For wrap logs, no_overflows is {SinceOpen, SinceLastInfo}
        %% At least one wrap must have occurred with this volume of writes
        {SinceOpen, _SinceLastInfo} = NoOverflows,
        ?assert(SinceOpen > 0),
        gen_server:stop(Pid)
    after
        application:unset_env(iris_core, wal_max_bytes_per_shard),
        cleanup(Dir)
    end.
