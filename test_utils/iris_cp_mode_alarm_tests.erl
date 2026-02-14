-module(iris_cp_mode_alarm_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P0-2 + CRIT-01: CP Consistency Mode Alarm Tests
%% =============================================================================
%%
%% CRIT-01 removed the dev fallback. CP is now fatal in ALL modes.
%% These tests verify:
%% 1. CP in dev returns {error, cp_not_implemented} (no fallback)
%% 2. No metric or env marker is set (fallback path removed)
%% 3. Error-level logging is used for CP rejection
%% 4. hardened_ap does not trigger mismatch metric
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
%% Test: CP in dev returns error (CRIT-01: no fallback, no metric)
%% =============================================================================

cp_dev_returns_error_test() ->
    setup_metrics(),
    application:set_env(iris_core, deployment_mode, development),
    application:set_env(iris_core, consistency_mode, cp),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual({error, cp_not_implemented}, Result),
        %% Metric must NOT be set (fallback path removed)
        [{_, MetricVal}] = ets:lookup(iris_metrics_table, consistency_mode_mismatch),
        ?assertEqual(0, MetricVal)
    after
        cleanup_env()
    end.

%% =============================================================================
%% Test: CP in dev does NOT set consistency_mode_actual (fallback removed)
%% =============================================================================

cp_dev_no_actual_mode_env_test() ->
    setup_metrics(),
    application:unset_env(iris_core, consistency_mode_actual),
    application:set_env(iris_core, deployment_mode, development),
    application:set_env(iris_core, consistency_mode, cp),
    try
        {error, cp_not_implemented} = iris_core:validate_consistency_mode(),
        ?assertEqual(undefined,
                     application:get_env(iris_core, consistency_mode_actual))
    after
        cleanup_env()
    end.

%% =============================================================================
%% Test: CP rejection uses logger:error in source code
%% =============================================================================

cp_rejection_uses_error_level_log_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    %% CRIT-01: The fallback path was removed. Verify that:
    %% 1. "Falling back to hardened_ap" is NO LONGER in the source
    %% 2. The CP rejection uses logger:error (not logger:warning)
    FallbackMarker = <<"Falling back to hardened_ap">>,
    ?assertEqual(nomatch, binary:match(Src, FallbackMarker)),
    %% The CP branch must use logger:error for the "NOT IMPLEMENTED" message
    CpMarker = <<"consistency_mode=cp is NOT IMPLEMENTED">>,
    ?assertNotEqual(nomatch, binary:match(Src, CpMarker)),
    {CpPos, _} = binary:match(Src, CpMarker),
    WindowStart = max(0, CpPos - 200),
    WindowLen = CpPos - WindowStart,
    Window = binary:part(Src, WindowStart, WindowLen),
    HasErrorLog = binary:match(Window, <<"logger:error">>) =/= nomatch,
    ?assert(HasErrorLog).

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
