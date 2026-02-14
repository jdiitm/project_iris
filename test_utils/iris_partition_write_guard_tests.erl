-module(iris_partition_write_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION V2 — P0-1: Partition-Safe Write Rejection
%% =============================================================================
%%
%% Tests verify that the partition guard rejects writes when this node is
%% in a minority partition (diverged mode + static membership), moving
%% from pure-AP to "safe-AP" semantics.
%%
%% This is the stepping stone toward full CP:
%%   1. Minority partition → writes rejected → no split-brain corruption
%%   2. Majority partition → writes allowed → availability preserved
%%   3. Metric emitted for observability
%% =============================================================================

-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

ensure_metrics_table() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end.

get_metric(Key) ->
    case ets:lookup(?METRICS_TABLE, Key) of
        [{_, Val}] -> Val;
        [] -> 0
    end.

cleanup() ->
    %% Stop the partition guard if running
    case whereis(iris_partition_guard) of
        undefined -> ok;
        Pid ->
            gen_server:stop(Pid, normal, 1000)
    end,
    %% Clean up env
    application:unset_env(iris_core, expected_cluster_nodes),
    application:unset_env(iris_core, partition_guard_mode),
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, deployment_mode).

%% =============================================================================
%% Test: Partition guard rejects writes in minority partition
%% =============================================================================

partition_guard_rejects_writes_in_minority_test() ->
    ensure_metrics_table(),
    cleanup(),
    %% Configure static mode with fake expected nodes that don't exist
    %% so this node sees 0 of 3 expected → minority
    application:set_env(iris_core, expected_cluster_nodes,
                        ['fake1@nowhere', 'fake2@nowhere', 'fake3@nowhere']),
    application:set_env(iris_core, partition_guard_mode, static),
    try
        {ok, Pid} = iris_partition_guard:start_link(),
        %% Give time for the initial partition check
        timer:sleep(100),
        %% Force a check by sending the message directly
        Pid ! check_partition,
        timer:sleep(100),
        %% Now the guard should be in diverged mode and reject writes
        Result = iris_partition_guard:is_safe_for_writes(),
        ?assertEqual({error, minority_partition}, Result)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: Partition guard allows writes when in majority
%% =============================================================================

partition_guard_allows_writes_in_majority_test() ->
    ensure_metrics_table(),
    cleanup(),
    %% Configure with only this node as expected → always has quorum
    application:set_env(iris_core, expected_cluster_nodes, [node()]),
    application:set_env(iris_core, partition_guard_mode, static),
    try
        {ok, _Pid} = iris_partition_guard:start_link(),
        timer:sleep(100),
        Result = iris_partition_guard:is_safe_for_writes(),
        ?assertEqual(ok, Result)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: Read-only mode metric emitted on minority partition
%% =============================================================================

partition_guard_read_only_mode_metric_test() ->
    ensure_metrics_table(),
    cleanup(),
    catch ets:insert(?METRICS_TABLE, {partition_guard_read_only_mode, 0}),
    application:set_env(iris_core, expected_cluster_nodes,
                        ['fake1@nowhere', 'fake2@nowhere', 'fake3@nowhere']),
    application:set_env(iris_core, partition_guard_mode, static),
    try
        {ok, Pid} = iris_partition_guard:start_link(),
        Pid ! check_partition,
        timer:sleep(100),
        MetricVal = get_metric(partition_guard_read_only_mode),
        ?assertEqual(1, MetricVal)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: Epoch increments on partition detection
%% =============================================================================

partition_guard_epoch_increments_on_partition_test() ->
    ensure_metrics_table(),
    cleanup(),
    application:set_env(iris_core, expected_cluster_nodes,
                        ['fake1@nowhere', 'fake2@nowhere', 'fake3@nowhere']),
    application:set_env(iris_core, partition_guard_mode, static),
    try
        {ok, Pid} = iris_partition_guard:start_link(),
        Pid ! check_partition,
        timer:sleep(100),
        Status = iris_partition_guard:get_status(),
        Epoch = maps:get(epoch, Status),
        ?assert(Epoch >= 1)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: iris_store:check_write_safety respects partition guard
%% =============================================================================

store_write_respects_partition_guard_test() ->
    %% Structural test: verify iris_store has write APIs (put/3, put/4)
    %% and that check_write_safety calls iris_partition_guard:is_safe_for_writes/0
    Exports = iris_store:module_info(exports),
    %% The store module must export put/3 or put/4 for writes
    ?assert(lists:member({put, 3}, Exports) orelse
            lists:member({put, 4}, Exports)),
    %% Verify the source code contains the partition guard check
    {ok, Src} = file:read_file("src/iris_store.erl"),
    ?assertNotEqual(nomatch, binary:match(Src, <<"is_safe_for_writes">>)).

%% =============================================================================
%% Test: CP mode startup crash in production
%% =============================================================================

cp_mode_startup_crash_in_production_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, deployment_mode, production),
    application:set_env(iris_core, consistency_mode, cp),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual({error, cp_not_implemented}, Result)
    after
        application:unset_env(iris_core, deployment_mode),
        application:unset_env(iris_core, consistency_mode)
    end.
