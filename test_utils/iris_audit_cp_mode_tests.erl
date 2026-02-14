-module(iris_audit_cp_mode_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION — Finding 2.1: CP Consistency Mode Contract Tests
%% =============================================================================
%%
%% The system is formally classified as AP-only (hardened_ap). CP mode is not
%% implemented. These tests formalize the contract:
%%   1. CP in production -> fatal error
%%   2. CP in development -> fallback to hardened_ap with metric + env marker
%%   3. Default mode is hardened_ap
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
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, consistency_mode_actual),
    application:unset_env(iris_core, deployment_mode),
    ok.

%% =============================================================================
%% Test: CP mode returns error in production
%% =============================================================================

cp_mode_crashes_in_production_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, production),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual({error, cp_not_implemented}, Result)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: CP mode emits metric in development and falls back
%% =============================================================================

cp_mode_emits_metric_in_dev_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {consistency_mode_mismatch, 0}),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, development),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual(ok, Result),
        Metric = get_metric(consistency_mode_mismatch),
        ?assertEqual(1, Metric)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: Default mode is hardened_ap (no explicit config)
%% =============================================================================

default_mode_is_hardened_ap_test() ->
    ensure_metrics_table(),
    application:unset_env(iris_core, consistency_mode),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual(ok, Result)
    after
        cleanup()
    end.

%% =============================================================================
%% Test: consistency_mode_actual set to hardened_ap on dev fallback
%% =============================================================================

consistency_mode_actual_set_on_fallback_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, development),
    try
        ok = iris_core:validate_consistency_mode(),
        {ok, Actual} = application:get_env(iris_core, consistency_mode_actual),
        ?assertEqual(hardened_ap, Actual)
    after
        cleanup()
    end.
