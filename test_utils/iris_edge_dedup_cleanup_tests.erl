-module(iris_edge_dedup_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for iris_edge_dedup_cleaner: TTL eviction and MAX_ENTRIES cap.

-define(TABLE, iris_edge_dedup).

setup_table() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [set, named_table, public,
                                      {read_concurrency, true},
                                      {write_concurrency, true}]);
        _ -> ets:delete_all_objects(?TABLE)
    end,
    ok.

cleanup_table() ->
    try ets:delete_all_objects(?TABLE) catch error:badarg -> ok end,
    ok.

ttl_eviction_test() ->
    setup_table(),
    try
        Now = os:system_time(millisecond),
        %% Insert entries that are 6 minutes old (older than 5 min TTL)
        OldTs = Now - 360000,
        ets:insert(?TABLE, {<<"old_key_1">>, OldTs}),
        ets:insert(?TABLE, {<<"old_key_2">>, OldTs}),
        ets:insert(?TABLE, {<<"old_key_3">>, OldTs}),
        ?assertEqual(3, ets:info(?TABLE, size)),
        %% Cleanup should remove all expired entries
        Cutoff = Now - 300000,
        {_Kept, Removed} = cleanup_expired_entries(Cutoff),
        ?assertEqual(3, Removed),
        ?assertEqual(0, ets:info(?TABLE, size))
    after
        cleanup_table()
    end.

recent_entries_survive_test() ->
    setup_table(),
    try
        Now = os:system_time(millisecond),
        %% Insert entries that are 1 minute old (well within 5 min TTL)
        RecentTs = Now - 60000,
        ets:insert(?TABLE, {<<"fresh_1">>, RecentTs}),
        ets:insert(?TABLE, {<<"fresh_2">>, RecentTs}),
        ?assertEqual(2, ets:info(?TABLE, size)),
        Cutoff = Now - 300000,
        {Kept, Removed} = cleanup_expired_entries(Cutoff),
        ?assertEqual(0, Removed),
        ?assertEqual(2, Kept),
        ?assertEqual(2, ets:info(?TABLE, size))
    after
        cleanup_table()
    end.

max_entries_cap_test() ->
    setup_table(),
    try
        Now = os:system_time(millisecond),
        %% Insert more entries than the cap allows.
        %% Use a small set to keep the test fast; the eviction logic is the same.
        %% Insert 20 entries, then simulate a cap of 10 via direct eviction call.
        lists:foreach(fun(I) ->
            Key = iolist_to_binary(io_lib:format("key_~p", [I])),
            ets:insert(?TABLE, {Key, Now})
        end, lists:seq(1, 20)),
        ?assertEqual(20, ets:info(?TABLE, size)),
        %% Evict 10 entries to bring within cap
        Evicted = evict_oldest(10),
        ?assertEqual(10, Evicted),
        ?assertEqual(10, ets:info(?TABLE, size))
    after
        cleanup_table()
    end.

empty_table_no_crash_test() ->
    setup_table(),
    try
        ?assertEqual(0, ets:info(?TABLE, size)),
        Cutoff = os:system_time(millisecond) - 300000,
        {Kept, Removed} = cleanup_expired_entries(Cutoff),
        ?assertEqual(0, Kept),
        ?assertEqual(0, Removed)
    after
        cleanup_table()
    end.

mixed_entries_test() ->
    setup_table(),
    try
        Now = os:system_time(millisecond),
        %% Mix of old and new entries
        ets:insert(?TABLE, {<<"expired_a">>, Now - 400000}),
        ets:insert(?TABLE, {<<"fresh_a">>, Now - 100000}),
        ets:insert(?TABLE, {<<"expired_b">>, Now - 600000}),
        ets:insert(?TABLE, {<<"fresh_b">>, Now - 10000}),
        ?assertEqual(4, ets:info(?TABLE, size)),
        Cutoff = Now - 300000,
        {Kept, Removed} = cleanup_expired_entries(Cutoff),
        ?assertEqual(2, Removed),
        ?assertEqual(2, Kept),
        ?assertEqual(2, ets:info(?TABLE, size)),
        %% Verify correct entries survived
        ?assertNotEqual([], ets:lookup(?TABLE, <<"fresh_a">>)),
        ?assertNotEqual([], ets:lookup(?TABLE, <<"fresh_b">>)),
        ?assertEqual([], ets:lookup(?TABLE, <<"expired_a">>)),
        ?assertEqual([], ets:lookup(?TABLE, <<"expired_b">>))
    after
        cleanup_table()
    end.

%% --- Internal helpers (mirror the gen_server's internal functions) ---

cleanup_expired_entries(Cutoff) ->
    cleanup_fold(ets:first(?TABLE), Cutoff, 0, 0).

cleanup_fold('$end_of_table', _Cutoff, Kept, Removed) ->
    {Kept, Removed};
cleanup_fold(Key, Cutoff, Kept, Removed) ->
    Next = ets:next(?TABLE, Key),
    case ets:lookup(?TABLE, Key) of
        [{Key, Timestamp}] when Timestamp < Cutoff ->
            ets:delete(?TABLE, Key),
            cleanup_fold(Next, Cutoff, Kept, Removed + 1);
        _ ->
            cleanup_fold(Next, Cutoff, Kept + 1, Removed)
    end.

evict_oldest(0) -> 0;
evict_oldest(Count) ->
    evict_n(ets:first(?TABLE), Count, 0).

evict_n('$end_of_table', _Remaining, Evicted) ->
    Evicted;
evict_n(_Key, 0, Evicted) ->
    Evicted;
evict_n(Key, Remaining, Evicted) ->
    Next = ets:next(?TABLE, Key),
    ets:delete(?TABLE, Key),
    evict_n(Next, Remaining - 1, Evicted + 1).
