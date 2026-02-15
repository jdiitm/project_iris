-module(iris_audit_mitigation_v2_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Production Readiness TDD Tests
%% =============================================================================
%%
%% Tests for three production-readiness findings:
%%   CP mode configurable but not implemented — must be fatal everywhere
%%   No backpressure on store_offline_durable primary write path
%%   Cookie enforcement regression (already mitigated, verify it holds)
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

cleanup_consistency() ->
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, consistency_mode_actual),
    application:unset_env(iris_core, deployment_mode),
    ok.

%% =============================================================================
%% CP mode must be fatal in ALL deployment modes
%% =============================================================================

%% CP mode in development must return {error, cp_not_implemented}.
%% Before fix: returns ok (silent fallback to hardened_ap).
cp_mode_fatal_in_all_deployment_modes_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, consistency_mode, cp),
    application:set_env(iris_core, deployment_mode, development),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertEqual({error, cp_not_implemented}, Result)
    after
        cleanup_consistency()
    end.

%% Unknown consistency_mode values must be rejected.
%% Before fix: the catch-all clause returns ok for any non-cp value.
unknown_consistency_mode_rejected_test() ->
    ensure_metrics_table(),
    application:set_env(iris_core, consistency_mode, banana),
    try
        Result = iris_core:validate_consistency_mode(),
        ?assertMatch({error, {unknown_consistency_mode, banana}}, Result)
    after
        cleanup_consistency()
    end.

%% =============================================================================
%% store_offline_durable must reject under memory pressure
%% =============================================================================

%% store_offline_durable/2 must return {error, memory_pressure} when
%% iris_mnesia_guard detects memory exceeds threshold.
%% Before fix: no is_memory_ok() check, attempts Mnesia write regardless.
store_offline_rejects_under_memory_pressure_test() ->
    ensure_metrics_table(),
    mnesia:start(),
    %% Set absurdly low threshold so schema table alone triggers pressure
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    %% Clear cached alarms to force fresh check
    persistent_term:put(iris_mnesia_guard_alarms, []),
    try
        Result = iris_core:store_offline_durable(<<"test_user">>, <<"test_msg">>),
        ?assertEqual({error, memory_pressure}, Result)
    after
        application:unset_env(iris_core, mnesia_memory_alarm_bytes),
        persistent_term:put(iris_mnesia_guard_alarms, []),
        mnesia:stop()
    end.

%% store_offline_durable/2 must increment the backpressure rejection metric.
store_offline_emits_backpressure_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {offline_store_backpressure_rejects, 0}),
    mnesia:start(),
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    persistent_term:put(iris_mnesia_guard_alarms, []),
    try
        Before = get_metric(offline_store_backpressure_rejects),
        _Result = iris_core:store_offline_durable(<<"test_user">>, <<"test_msg">>),
        After = get_metric(offline_store_backpressure_rejects),
        ?assert(After > Before)
    after
        application:unset_env(iris_core, mnesia_memory_alarm_bytes),
        persistent_term:put(iris_mnesia_guard_alarms, []),
        mnesia:stop()
    end.

%% =============================================================================
%% Cookie enforcement regression (already mitigated)
%% =============================================================================

%% Verify that validate_production_cookie/1 rejects iris_secret in production.
%% This test passes immediately — it guards against regressions.
cookie_enforcement_production_regression_test() ->
    application:set_env(iris_core, deployment_mode, production),
    try
        Result = iris_core:validate_production_cookie(iris_secret),
        ?assertEqual({error, default_cookie_in_production}, Result)
    after
        application:unset_env(iris_core, deployment_mode)
    end.
