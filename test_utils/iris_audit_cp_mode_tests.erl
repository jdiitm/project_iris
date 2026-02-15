-module(iris_audit_cp_mode_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% CP Consistency Mode Contract Tests
%% =============================================================================
%%
%% The system is formally classified as AP-only (hardened_ap). CP mode is not
%% implemented. These tests formalize the contract:
%%   1. CP in production -> fatal error
%%   2. CP in development -> fatal error (no silent fallback)
%%   3. Default mode is hardened_ap
%%   4. No fallback env marker is set (fallback path removed)
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
%% Test: CP mode is fatal in development (no silent fallback)
%% =============================================================================

cp_mode_fatal_in_dev_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, development),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual({error, cp_not_implemented}, Result)
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
%% Test: CP mode does NOT set consistency_mode_actual (fallback removed)
%% =============================================================================

cp_mode_no_fallback_env_set_test() ->
    ensure_metrics_table(),
    application:unset_env(iris_core, consistency_mode_actual),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, development),
    try
        {error, cp_not_implemented} = iris_core:validate_consistency_mode(),
        %% consistency_mode_actual must NOT be set (no fallback path)
        ?assertEqual(undefined, application:get_env(iris_core, consistency_mode_actual))
    after
        cleanup()
    end.
