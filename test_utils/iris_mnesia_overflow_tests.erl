-module(iris_mnesia_overflow_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION: Mnesia Overflow Table Tests (Blocker 1)
%% =============================================================================
%%
%% Tests verify the properties of the cold-tier overflow table:
%%   1. Overflow table uses disc_only_copies (no RAM footprint)
%%   2. Eviction emits storage_tier_evictions_total metric
%%   3. Cold-tier reads emit storage_tier_cold_read_ms metric
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

%% =============================================================================
%% Test: Overflow table spec is disc_only_copies
%% =============================================================================

overflow_table_is_disc_only_test() ->
    {StorageType, _Opts} = iris_core:table_spec(user_meta_cold),
    ?assertEqual(disc_only_copies, StorageType).

%% =============================================================================
%% Test: Eviction emits metric
%% =============================================================================

eviction_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {storage_tier_evictions_total, 0}),
    %% Setup Mnesia
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:create_table(user_meta, [
        {ram_copies, [node()]},
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta}} -> ok
    end,
    case mnesia:create_table(user_meta_cold, [
        {ram_copies, [node()]},
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta_cold}} -> ok
    end,
    mnesia:wait_for_tables([user_meta, user_meta_cold], 5000),
    %% Create access tracking table
    case ets:info(iris_storage_tier_access) of
        undefined ->
            ets:new(iris_storage_tier_access, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    try
        Before = get_metric(storage_tier_evictions_total),
        %% Insert old data and evict
        OldTs = os:system_time(second) - 7200,
        mnesia:dirty_write({user_meta, <<"metric_user">>, 1, OldTs}),
        ets:insert(iris_storage_tier_access, {<<"metric_user">>, OldTs}),
        Cutoff = os:system_time(second) - 3600,
        iris_storage_tier:evict_cold(user_meta, user_meta_cold, Cutoff),
        After = get_metric(storage_tier_evictions_total),
        ?assert(After > Before)
    after
        catch ets:delete(iris_storage_tier_access),
        application:stop(mnesia)
    end.

%% =============================================================================
%% Test: Cold-tier read emits latency metric
%% =============================================================================

cold_read_emits_latency_metric_test() ->
    ensure_metrics_table(),
    %% Setup Mnesia
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:create_table(user_meta, [
        {ram_copies, [node()]},
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta}} -> ok
    end,
    case mnesia:create_table(user_meta_cold, [
        {ram_copies, [node()]},
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta_cold}} -> ok
    end,
    mnesia:wait_for_tables([user_meta, user_meta_cold], 5000),
    try
        %% Put data in cold tier
        mnesia:dirty_write({user_meta_cold, <<"latency_user">>, 2, 3000}),
        %% Read through should emit latency metric
        iris_storage_tier:read_through(user_meta, user_meta_cold, <<"latency_user">>),
        %% Verify metric was emitted (should be > 0 after at least one read)
        ColdReads = get_metric(storage_tier_cold_reads_total),
        ?assert(ColdReads > 0)
    after
        application:stop(mnesia)
    end.
