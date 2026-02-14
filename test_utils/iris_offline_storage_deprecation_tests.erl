-module(iris_offline_storage_deprecation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION — Finding 2.4: Legacy Storage Retrieval Deprecation
%% =============================================================================
%%
%% retrieve/2 uses mnesia:activity(transaction, ...) which holds global locks.
%% retrieve_cursor/3 uses dirty reads (lockfree).
%%
%% Tests enforce:
%%   1. retrieve/2 emits a deprecation metric (observable for migration)
%%   2. retrieve_cursor/3 uses dirty reads (structural verification)
%%   3. retrieve_cursor/3 returns paginated results
%%   4. retrieve_cursor/3 preserves FIFO ordering
%%   5. delete_confirmed/4 is async (spawns process)
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

setup_mnesia() ->
    mnesia:start(),
    case mnesia:create_table(offline_msg, [
        {attributes, [key, timestamp, msg]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

cleanup_mnesia() ->
    catch mnesia:delete_table(offline_msg),
    mnesia:stop().

store_test_msg(User, BucketId, Timestamp, Msg) ->
    mnesia:dirty_write({offline_msg, {User, BucketId}, Timestamp, Msg}).

%% =============================================================================
%% Test: retrieve/2 emits deprecation metric
%% =============================================================================

retrieve_legacy_emits_deprecation_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {offline_retrieve_deprecated_calls, 0}),
    setup_mnesia(),
    try
        Before = get_metric(offline_retrieve_deprecated_calls),
        _Result = iris_offline_storage:retrieve(<<"user1">>, 1),
        After = get_metric(offline_retrieve_deprecated_calls),
        ?assert(After > Before)
    after
        cleanup_mnesia()
    end.

%% =============================================================================
%% Test: retrieve_cursor/3 source uses dirty_read (not transaction)
%% =============================================================================

retrieve_cursor_uses_dirty_reads_test() ->
    {ok, Src} = file:read_file("src/iris_offline_storage.erl"),
    %% retrieve_cursor must use dirty_read (lockfree), not transaction
    ?assertNotEqual(nomatch, binary:match(Src, <<"retrieve_cursor">>)),
    ?assertNotEqual(nomatch, binary:match(Src, <<"dirty_read">>)),
    %% Verify the dirty_read is in the retrieve_cursor function region
    {CursorPos, _} = binary:match(Src, <<"retrieve_cursor">>),
    %% dirty_read must appear after retrieve_cursor definition
    {DirtyPos, _} = binary:match(Src, <<"mnesia:dirty_read(offline_msg">>),
    ?assert(DirtyPos > CursorPos).

%% =============================================================================
%% Test: retrieve_cursor/3 returns paginated results
%% =============================================================================

retrieve_cursor_paginated_test() ->
    setup_mnesia(),
    try
        User = <<"paginated_user">>,
        %% Store messages in 15 buckets
        lists:foreach(fun(I) ->
            store_test_msg(User, I, I * 100, <<"msg_", (integer_to_binary(I))/binary>>)
        end, lists:seq(0, 14)),
        %% First batch: buckets 0-9 (BatchSize capped at 10)
        {Msgs1, Cursor1} = iris_offline_storage:retrieve_cursor(User, 15, 0),
        ?assertEqual(10, length(Msgs1)),
        ?assertEqual(10, Cursor1),
        %% Second batch: buckets 10-14
        {Msgs2, Cursor2} = iris_offline_storage:retrieve_cursor(User, 15, 10),
        ?assertEqual(5, length(Msgs2)),
        ?assertEqual(done, Cursor2)
    after
        cleanup_mnesia()
    end.

%% =============================================================================
%% Test: retrieve_cursor/3 preserves FIFO ordering
%% =============================================================================

retrieve_cursor_fifo_ordering_test() ->
    setup_mnesia(),
    try
        User = <<"fifo_user">>,
        %% Store messages with timestamps out of order across buckets
        store_test_msg(User, 0, 300, <<"third">>),
        store_test_msg(User, 1, 100, <<"first">>),
        store_test_msg(User, 2, 200, <<"second">>),
        {Msgs, _Cursor} = iris_offline_storage:retrieve_cursor(User, 3, 0),
        ?assertEqual([<<"first">>, <<"second">>, <<"third">>], Msgs)
    after
        cleanup_mnesia()
    end.

%% =============================================================================
%% Test: delete_confirmed/4 source uses spawn (async)
%% =============================================================================

delete_confirmed_is_async_test() ->
    {ok, Src} = file:read_file("src/iris_offline_storage.erl"),
    {Pos, _} = binary:match(Src, <<"delete_confirmed(User, _Count, FromCursor, ToCursor) ->">>),
    Snippet = binary:part(Src, Pos, min(200, byte_size(Src) - Pos)),
    ?assertNotEqual(nomatch, binary:match(Snippet, <<"spawn">>)).
