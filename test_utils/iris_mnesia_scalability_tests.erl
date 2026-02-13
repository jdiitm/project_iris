-module(iris_mnesia_scalability_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mnesia Scalability Tests (Audit Mitigation)
%%
%% Validates:
%%   - offline_msg uses disc_only_copies (RAM-bounded)
%%   - dedup_log uses disc_only_copies (RAM-bounded)
%%   - TTL cleanup deletes expired dedup_log entries (>7 days)
%%   - TTL cleanup deletes expired revoked_tokens
%%   - Memory metric is emitted for Mnesia tables
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Setup / Teardown
%% ---------------------------------------------------------------------------

setup() ->
    %% Ensure clean Mnesia state
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    
    %% Create metrics table for memory metric test
    case ets:info(iris_metrics_table) of
        undefined ->
            ets:new(iris_metrics_table, [named_table, public, set, {write_concurrency, true}]);
        _ ->
            ok
    end,
    
    %% Create the tables as iris_core would
    create_test_tables(),
    ok.

cleanup(_) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    try ets:delete(iris_metrics_table) catch error:badarg -> ok end.

create_test_tables() ->
    %% Create offline_msg using iris_core:table_spec
    {OfflineType, OfflineOpts} = iris_core:table_spec(offline_msg),
    create_table(offline_msg, OfflineType, OfflineOpts),
    
    %% Create dedup_log
    {DedupType, DedupOpts} = iris_core:table_spec(dedup_log),
    create_table(dedup_log, DedupType, DedupOpts),
    
    %% Create revoked_tokens
    {RevType, RevOpts} = iris_core:table_spec(revoked_tokens),
    create_table(revoked_tokens, RevType, RevOpts),
    
    %% Create refresh_tokens
    {RefType, RefOpts} = iris_core:table_spec(refresh_tokens),
    create_table(refresh_tokens, RefType, RefOpts),
    
    mnesia:wait_for_tables([offline_msg, dedup_log, revoked_tokens, refresh_tokens], 5000).

create_table(Name, CopyType, Opts) ->
    Attrs = proplists:get_value(attributes, Opts),
    Type = proplists:get_value(type, Opts, set),
    case mnesia:create_table(Name, [
        {CopyType, [node()]},
        {attributes, Attrs},
        {type, Type}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, Name}} -> ok
    end.

%% ---------------------------------------------------------------------------
%% Test Generator
%% ---------------------------------------------------------------------------

mnesia_scalability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"offline_msg uses disc_only_copies", fun check_offline_msg_disc_only/0},
      {"dedup_log uses disc_only_copies", fun check_dedup_log_disc_only/0},
      {"TTL cleanup purges old dedup entries", fun check_ttl_cleanup_dedup/0},
      {"TTL cleanup purges expired revoked tokens", fun check_ttl_cleanup_revoked/0},
      {"memory metric emitted", fun check_memory_metric/0}
     ]}.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

%% offline_msg should use disc_only_copies to avoid unbounded RAM usage.
check_offline_msg_disc_only() ->
    {CopyType, _Opts} = iris_core:table_spec(offline_msg),
    ?assertEqual(disc_only_copies, CopyType).

%% dedup_log should use disc_only_copies to avoid unbounded RAM usage.
check_dedup_log_disc_only() ->
    {CopyType, _Opts} = iris_core:table_spec(dedup_log),
    ?assertEqual(disc_only_copies, CopyType).

%% Insert a dedup_log entry 8 days old. After cleanup, it should be gone.
check_ttl_cleanup_dedup() ->
    EightDaysAgo = os:system_time(second) - (8 * 86400),
    mnesia:transaction(fun() ->
        mnesia:write({dedup_log, <<"old_msg_id">>, EightDaysAgo})
    end),
    %% Insert a recent entry that should survive
    mnesia:transaction(fun() ->
        mnesia:write({dedup_log, <<"new_msg_id">>, os:system_time(second)})
    end),
    
    %% Run cleanup
    iris_core:cleanup_expired_entries(),
    
    %% Old entry gone
    ?assertEqual([], mnesia:dirty_read(dedup_log, <<"old_msg_id">>)),
    %% New entry still present
    ?assertMatch([_], mnesia:dirty_read(dedup_log, <<"new_msg_id">>)).

%% Insert a revoked_token older than TTL. After cleanup, it should be gone.
check_ttl_cleanup_revoked() ->
    EightDaysAgo = os:system_time(second) - (8 * 86400),
    mnesia:transaction(fun() ->
        mnesia:write({revoked_tokens, <<"old_jti">>, EightDaysAgo})
    end),
    mnesia:transaction(fun() ->
        mnesia:write({revoked_tokens, <<"new_jti">>, os:system_time(second)})
    end),
    
    iris_core:cleanup_expired_entries(),
    
    ?assertEqual([], mnesia:dirty_read(revoked_tokens, <<"old_jti">>)),
    ?assertMatch([_], mnesia:dirty_read(revoked_tokens, <<"new_jti">>)).

%% Calling emit_mnesia_table_memory should set a non-zero gauge.
check_memory_metric() ->
    iris_metrics:emit_mnesia_table_memory(),
    Metrics = ets:tab2list(iris_metrics_table),
    %% At least one mnesia_table_memory metric should exist
    MemMetrics = [M || {K, _V} = M <- Metrics,
                       is_atom(K),
                       lists:prefix("mnesia_table_memory_", atom_to_list(K))],
    ?assert(length(MemMetrics) > 0),
    %% All values should be >= 0
    lists:foreach(fun({_K, V}) -> ?assert(V >= 0) end, MemMetrics).
