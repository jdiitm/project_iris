-module(iris_cp_mode_alarm_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P0-2: CP Consistency Mode Alarm Tests
%% =============================================================================
%%
%% Tests verify that when consistency_mode=cp in development mode:
%% 1. validate_consistency_mode/0 logs at error level (not warning)
%% 2. A consistency_mode_mismatch metric is set to 1
%% 3. application env consistency_mode_actual is set to hardened_ap
%%
%% The existing test in iris_core_audit_tests.erl already covers:
%% - production mode rejects CP (returns {error, cp_not_implemented})
%% - development mode allows CP with fallback (returns ok)
%% - hardened_ap mode always succeeds
%%
%% These tests verify the OBSERVABILITY additions (error log + metric + env).
%% =============================================================================

setup_metrics() ->
    Table = iris_metrics_table,
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    %% Reset the metric to 0
    ets:insert(Table, {consistency_mode_mismatch, 0}),
    ok.

cleanup_env() ->
    application:unset_env(iris_core, deployment_mode),
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, consistency_mode_actual).

%% =============================================================================
%% Test: CP fallback sets metric
%% =============================================================================

cp_fallback_sets_metric_test() ->
    setup_metrics(),
    application:set_env(iris_core, deployment_mode, development),
    application:set_env(iris_core, consistency_mode, cp),
    try
        ok = iris_core:validate_consistency_mode(),
        [{_, MetricVal}] = ets:lookup(iris_metrics_table, consistency_mode_mismatch),
        ?assertEqual(1, MetricVal)
    after
        cleanup_env()
    end.

%% =============================================================================
%% Test: CP fallback sets consistency_mode_actual env
%% =============================================================================

cp_fallback_sets_actual_mode_env_test() ->
    setup_metrics(),
    application:set_env(iris_core, deployment_mode, development),
    application:set_env(iris_core, consistency_mode, cp),
    try
        ok = iris_core:validate_consistency_mode(),
        ?assertEqual({ok, hardened_ap},
                     application:get_env(iris_core, consistency_mode_actual))
    after
        cleanup_env()
    end.

%% =============================================================================
%% Test: Source code uses logger:error (not logger:warning) for CP fallback
%% =============================================================================

cp_fallback_uses_error_level_log_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    %% The development-mode CP fallback must use logger:error, not logger:warning.
    %% We extract the validate_consistency_mode function body and check that
    %% "Falling back to hardened_ap" is preceded by logger:error (not logger:warning).
    %%
    %% Strategy: find the function, then check no logger:warning appears in
    %% the block that contains "Falling back".
    FallbackMarker = <<"Falling back to hardened_ap">>,
    ?assertNotEqual(nomatch, binary:match(Src, FallbackMarker)),
    %% The fallback log call must NOT use logger:warning.
    %% In the source, the dev branch should have logger:error("... Falling back ...")
    %% not logger:warning("... Falling back ...").
    %% We check: no logger:warning call within 200 bytes before "Falling back"
    {FallbackPos, _} = binary:match(Src, FallbackMarker),
    WindowStart = max(0, FallbackPos - 200),
    WindowLen = FallbackPos - WindowStart,
    Window = binary:part(Src, WindowStart, WindowLen),
    HasWarningNearFallback = binary:match(Window, <<"logger:warning">>) =/= nomatch,
    ?assertNot(HasWarningNearFallback).

%% =============================================================================
%% Test: Non-CP mode does NOT set mismatch metric
%% =============================================================================

hardened_ap_does_not_set_metric_test() ->
    setup_metrics(),
    application:set_env(iris_core, consistency_mode, hardened_ap),
    try
        ok = iris_core:validate_consistency_mode(),
        [{_, MetricVal}] = ets:lookup(iris_metrics_table, consistency_mode_mismatch),
        ?assertEqual(0, MetricVal)
    after
        cleanup_env()
    end.
