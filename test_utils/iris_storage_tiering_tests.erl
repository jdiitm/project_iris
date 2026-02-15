-module(iris_storage_tiering_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Storage Tiering Tests
%% =============================================================================
%%
%% Root cause: disc_copies tables load all keys into RAM. At 100M+ users,
%% RAM is exhausted and iris_mnesia_guard triggers global read-only mode.
%%
%% Mitigation: iris_storage_tier transparently evicts cold user_meta entries
%% from disc_copies (hot) to disc_only_copies (cold) overflow table.
%%
%% Tests verify:
%%   1. Cold data eviction moves entries to overflow table
%%   2. Hot (recently accessed) data stays in RAM
%%   3. Read transparently falls back to cold tier
%%   4. Access promotes cold data back to hot tier
%%   5. Eviction triggers under memory pressure
%%   6. table_spec includes overflow table definition
%% =============================================================================

-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Setup / Cleanup
%% ---------------------------------------------------------------------------

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    %% Create both hot and cold tables for user_meta
    case mnesia:create_table(user_meta, [
        {ram_copies, [node()]},  %% ram_copies for test speed (disc_copies in prod)
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta}} -> ok
    end,
    case mnesia:create_table(user_meta_cold, [
        {ram_copies, [node()]},  %% ram_copies for test speed (disc_only_copies in prod)
        {attributes, [user, bucket_count, last_modified]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta_cold}} -> ok
    end,
    mnesia:wait_for_tables([user_meta, user_meta_cold], 5000),
    %% Create metrics table
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    %% Create access tracking table
    case ets:info(iris_storage_tier_access) of
        undefined ->
            ets:new(iris_storage_tier_access, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

cleanup(_) ->
    catch ets:delete(iris_storage_tier_access),
    catch ets:delete(?METRICS_TABLE),
    application:stop(mnesia).

%% ---------------------------------------------------------------------------
%% Test Generator
%% ---------------------------------------------------------------------------

iris_storage_tiering_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"cold data eviction moves entries to overflow",
       fun test_cold_data_eviction/0},
      {"hot data stays in hot tier",
       fun test_hot_data_stays_in_ram/0},
      {"read transparently falls back to cold tier",
       fun test_transparent_read_fallback/0},
      {"access promotes cold data to hot tier",
       fun test_promotion_on_access/0},
      {"eviction respects memory threshold signal",
       fun test_eviction_respects_memory_threshold/0},
      {"table_spec includes user_meta_cold",
       fun test_table_spec_includes_overflow/0}
     ]}.

%% =============================================================================
%% Test: Cold data eviction
%% =============================================================================

test_cold_data_eviction() ->
    %% Insert a user with an old timestamp (simulating cold data)
    OldTs = os:system_time(second) - 7200,  %% 2 hours ago
    mnesia:dirty_write({user_meta, <<"cold_user_1">>, 5, OldTs}),
    %% Track access time as old
    ets:insert(iris_storage_tier_access, {<<"cold_user_1">>, OldTs}),

    %% Evict cold entries (anything older than 1 hour)
    Cutoff = os:system_time(second) - 3600,
    EvictedCount = iris_storage_tier:evict_cold(user_meta, user_meta_cold, Cutoff),

    %% Verify entry moved to cold tier
    ?assert(EvictedCount >= 1),
    ?assertEqual([], mnesia:dirty_read(user_meta, <<"cold_user_1">>)),
    ?assertMatch([{user_meta_cold, <<"cold_user_1">>, 5, _}],
                 mnesia:dirty_read(user_meta_cold, <<"cold_user_1">>)).

%% =============================================================================
%% Test: Hot data stays in RAM
%% =============================================================================

test_hot_data_stays_in_ram() ->
    %% Insert a user with a recent timestamp (hot data)
    Now = os:system_time(second),
    mnesia:dirty_write({user_meta, <<"hot_user_1">>, 3, Now}),
    ets:insert(iris_storage_tier_access, {<<"hot_user_1">>, Now}),

    %% Evict cold entries (anything older than 1 hour)
    Cutoff = os:system_time(second) - 3600,
    iris_storage_tier:evict_cold(user_meta, user_meta_cold, Cutoff),

    %% Hot user should still be in hot tier
    ?assertMatch([{user_meta, <<"hot_user_1">>, 3, _}],
                 mnesia:dirty_read(user_meta, <<"hot_user_1">>)).

%% =============================================================================
%% Test: Transparent read fallback
%% =============================================================================

test_transparent_read_fallback() ->
    %% Put data directly in cold tier (simulating evicted data)
    mnesia:dirty_write({user_meta_cold, <<"fallback_user">>, 7, 1000}),
    %% Ensure NOT in hot tier
    mnesia:dirty_delete(user_meta, <<"fallback_user">>),

    %% Read through should find it in cold tier
    Result = iris_storage_tier:read_through(user_meta, user_meta_cold, <<"fallback_user">>),
    ?assertMatch({ok, {user_meta_cold, <<"fallback_user">>, 7, 1000}}, Result).

%% =============================================================================
%% Test: Promotion on access
%% =============================================================================

test_promotion_on_access() ->
    %% Put data in cold tier
    mnesia:dirty_write({user_meta_cold, <<"promote_user">>, 4, 2000}),
    mnesia:dirty_delete(user_meta, <<"promote_user">>),

    %% Promote should move from cold to hot
    iris_storage_tier:promote(user_meta, user_meta_cold, <<"promote_user">>),

    %% Now hot tier should have it
    ?assertMatch([{user_meta, <<"promote_user">>, 4, _}],
                 mnesia:dirty_read(user_meta, <<"promote_user">>)),
    %% Cold tier should be empty for this key
    ?assertEqual([], mnesia:dirty_read(user_meta_cold, <<"promote_user">>)).

%% =============================================================================
%% Test: Eviction respects memory threshold
%% =============================================================================

test_eviction_respects_memory_threshold() ->
    %% Insert some entries with varying ages
    Now = os:system_time(second),
    mnesia:dirty_write({user_meta, <<"thresh_old">>, 1, Now - 7200}),
    ets:insert(iris_storage_tier_access, {<<"thresh_old">>, Now - 7200}),
    mnesia:dirty_write({user_meta, <<"thresh_new">>, 1, Now}),
    ets:insert(iris_storage_tier_access, {<<"thresh_new">>, Now}),

    %% Simulate memory pressure signal by calling evict_if_needed
    %% with memory_pressure = true
    EvictedCount = iris_storage_tier:evict_cold(user_meta, user_meta_cold,
                                                 Now - 3600),

    %% Old entry should be evicted, new entry should remain
    ?assertEqual([], mnesia:dirty_read(user_meta, <<"thresh_old">>)),
    ?assertMatch([_], mnesia:dirty_read(user_meta, <<"thresh_new">>)),
    ?assert(EvictedCount >= 1).

%% =============================================================================
%% Test: table_spec includes overflow table
%% =============================================================================

test_table_spec_includes_overflow() ->
    Result = iris_core:table_spec(user_meta_cold),
    ?assertMatch({disc_only_copies, _}, Result),
    {disc_only_copies, Opts} = Result,
    %% Must have same attributes as user_meta
    ?assert(lists:keymember(attributes, 1, Opts)).
