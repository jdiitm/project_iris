-module(iris_mnesia_scalability_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mnesia Scalability Tests
%% =============================================================================
%%
%% Tests verify:
%% - offline_msg and dedup_log use disc_only_copies (not disc_copies)
%% - TTL cleanup purges expired entries from dedup_log and revoked_tokens
%% - Memory metric emission works for Mnesia tables
%% - table_spec is the single source of truth
%% =============================================================================

setup() ->
    %% Start fresh Mnesia
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    %% Create metrics ETS table
    case ets:info(iris_metrics_table) of
        undefined ->
            ets:new(iris_metrics_table, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

cleanup(_) ->
    application:stop(mnesia),
    try ets:delete(iris_metrics_table) catch _:_ -> ok end.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_mnesia_scalability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"offline_msg uses disc_only_copies",
       fun test_offline_msg_disc_only/0},
      {"dedup_log uses disc_only_copies",
       fun test_dedup_log_disc_only/0},
      {"presence uses ram_copies (no disc)",
       fun test_presence_ram_copies/0},
      {"table_spec is exported and covers all required tables",
       fun test_table_spec_coverage/0},
      {"TTL cleanup function exists and is exported",
       fun test_ttl_cleanup_exists/0},
      {"TTL cleanup deletes expired dedup_log entries",
       fun test_ttl_cleanup_dedup/0},
      {"TTL cleanup deletes expired revoked_tokens",
       fun test_ttl_cleanup_revoked_tokens/0},
      {"memory metric emission function exists",
       fun test_memory_metric_function_exists/0},
      {"cleanup_expired_entries returns {ok, Count}",
       fun test_cleanup_returns_count/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_offline_msg_disc_only() ->
    {StorageType, _Opts} = iris_core:table_spec(offline_msg),
    ?assertEqual(disc_only_copies, StorageType).

test_dedup_log_disc_only() ->
    {StorageType, _Opts} = iris_core:table_spec(dedup_log),
    ?assertEqual(disc_only_copies, StorageType).

test_presence_ram_copies() ->
    {StorageType, _Opts} = iris_core:table_spec(presence),
    ?assertEqual(ram_copies, StorageType).

test_table_spec_coverage() ->
    RequiredTables = [presence, offline_msg, user_meta, user_meta_cold, user_status,
                      revoked_tokens, dedup_log, refresh_tokens,
                      user_blocks, user_reports],
    lists:foreach(fun(Table) ->
        Result = iris_core:table_spec(Table),
        ?assertMatch({_, _}, Result)
    end, RequiredTables).

test_ttl_cleanup_exists() ->
    %% Verify the function is exported
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({cleanup_expired_entries, 0}, Exports)).

test_ttl_cleanup_dedup() ->
    %% Create a dedup_log table
    case mnesia:create_table(dedup_log, [
        {ram_copies, [node()]},  %% ram_copies for test speed
        {attributes, [msg_id, timestamp]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, dedup_log}} -> ok
    end,
    mnesia:wait_for_tables([dedup_log], 5000),

    %% Insert an entry 8 days ago (expired)
    EightDaysAgo = os:system_time(second) - (8 * 86400),
    mnesia:dirty_write({dedup_log, <<"old_msg_1">>, EightDaysAgo}),

    %% Insert a recent entry (not expired)
    Now = os:system_time(second),
    mnesia:dirty_write({dedup_log, <<"new_msg_1">>, Now}),

    %% Verify both exist
    ?assertMatch([_], mnesia:dirty_read(dedup_log, <<"old_msg_1">>)),
    ?assertMatch([_], mnesia:dirty_read(dedup_log, <<"new_msg_1">>)),

    %% Run cleanup
    iris_core:cleanup_expired_entries(),

    %% Old entry should be gone, new entry should remain
    ?assertEqual([], mnesia:dirty_read(dedup_log, <<"old_msg_1">>)),
    ?assertMatch([_], mnesia:dirty_read(dedup_log, <<"new_msg_1">>)).

test_ttl_cleanup_revoked_tokens() ->
    %% Create a revoked_tokens table
    case mnesia:create_table(revoked_tokens, [
        {ram_copies, [node()]},
        {attributes, [jti, timestamp]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, revoked_tokens}} -> ok
    end,
    mnesia:wait_for_tables([revoked_tokens], 5000),

    %% Insert an expired entry (8 days old)
    EightDaysAgo = os:system_time(second) - (8 * 86400),
    mnesia:dirty_write({revoked_tokens, <<"old_jti_1">>, EightDaysAgo}),

    %% Insert a recent entry
    Now = os:system_time(second),
    mnesia:dirty_write({revoked_tokens, <<"new_jti_1">>, Now}),

    %% Run cleanup
    iris_core:cleanup_expired_entries(),

    %% Old entry should be gone, new entry should remain
    ?assertEqual([], mnesia:dirty_read(revoked_tokens, <<"old_jti_1">>)),
    ?assertMatch([_], mnesia:dirty_read(revoked_tokens, <<"new_jti_1">>)).

test_memory_metric_function_exists() ->
    %% Verify the function is exported
    Exports = iris_metrics:module_info(exports),
    ?assert(lists:member({emit_mnesia_table_memory, 0}, Exports)).

test_cleanup_returns_count() ->
    %% Create tables and insert expired + fresh entries
    case mnesia:create_table(dedup_log, [
        {ram_copies, [node()]},
        {attributes, [msg_id, timestamp]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, dedup_log}} ->
            mnesia:clear_table(dedup_log)
    end,
    case mnesia:create_table(revoked_tokens, [
        {ram_copies, [node()]},
        {attributes, [jti, timestamp]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, revoked_tokens}} ->
            mnesia:clear_table(revoked_tokens)
    end,
    case mnesia:create_table(refresh_tokens, [
        {ram_copies, [node()]},
        {attributes, [token_id, user_id, family_id, used, created_at, expires_at]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, refresh_tokens}} ->
            mnesia:clear_table(refresh_tokens)
    end,
    mnesia:wait_for_tables([dedup_log, revoked_tokens, refresh_tokens], 5000),

    %% Insert 3 expired entries in dedup_log
    EightDaysAgo = os:system_time(second) - (8 * 86400),
    mnesia:dirty_write({dedup_log, <<"exp1">>, EightDaysAgo}),
    mnesia:dirty_write({dedup_log, <<"exp2">>, EightDaysAgo}),
    mnesia:dirty_write({dedup_log, <<"exp3">>, EightDaysAgo}),
    %% Insert 1 fresh entry
    Now = os:system_time(second),
    mnesia:dirty_write({dedup_log, <<"fresh">>, Now}),

    %% cleanup_expired_entries should return {ok, Count} where Count > 0
    Result = iris_core:cleanup_expired_entries(),
    ?assertMatch({ok, _}, Result),
    {ok, Count} = Result,
    ?assert(Count >= 3).
