-module(iris_tls_rotation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% SEC-01: TLS Certificate Rotation
%% =============================================================================
%%
%% No runtime certificate rotation. Expired certs require full node
%% restart to fix, causing downtime.
%%
%% Remediation: Add reload_tls_config/0 that calls ssl:clear_pem_cache/0
%% so the next TLS handshake picks up new cert files from disk.
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
%% Test: reload_tls_config/0 is exported from iris_core
%% =============================================================================

reload_tls_config_exported_test() ->
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({reload_tls_config, 0}, Exports)).

%% =============================================================================
%% Test: reload_tls_config/0 calls ssl:clear_pem_cache (structural)
%% =============================================================================

reload_tls_config_clears_pem_cache_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    ?assertNotEqual(nomatch, binary:match(Src, <<"ssl:clear_pem_cache()">>)).

%% =============================================================================
%% Test: reload_tls_config/0 emits metric
%% =============================================================================

reload_tls_config_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {tls_config_reload_count, 0}),
    %% Ensure ssl app is started so clear_pem_cache works
    application:ensure_all_started(ssl),
    try
        Before = get_metric(tls_config_reload_count),
        ok = iris_core:reload_tls_config(),
        After = get_metric(tls_config_reload_count),
        ?assert(After > Before)
    after
        ok
    end.

%% =============================================================================
%% Test: reload_tls_config/0 returns ok (not crash)
%% =============================================================================

reload_tls_config_returns_ok_test() ->
    application:ensure_all_started(ssl),
    ?assertEqual(ok, iris_core:reload_tls_config()).
