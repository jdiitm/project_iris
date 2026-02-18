-module(iris_production_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% TDD: Production Config Validation Tests
%%
%% These tests verify that production mode rejects placeholder/unsafe values
%% for critical configuration parameters.
%% =============================================================================

%% --- iris_core: metrics_bearer_token placeholder rejection ---

metrics_placeholder_rejected_test() ->
    application:set_env(iris_core, deployment_mode, production),
    application:set_env(iris_core, metrics_bearer_token, <<"REPLACE_WITH_METRICS_TOKEN">>),
    Result = iris_core:validate_metrics_token(),
    ?assertEqual({error, placeholder_metrics_token}, Result),
    application:unset_env(iris_core, deployment_mode),
    application:unset_env(iris_core, metrics_bearer_token).

metrics_valid_token_accepted_test() ->
    application:set_env(iris_core, deployment_mode, production),
    application:set_env(iris_core, metrics_bearer_token, <<"a_real_32_byte_secret_for_prod!!">>),
    Result = iris_core:validate_metrics_token(),
    ?assertEqual(ok, Result),
    application:unset_env(iris_core, deployment_mode),
    application:unset_env(iris_core, metrics_bearer_token).

metrics_undefined_rejected_in_production_test() ->
    application:set_env(iris_core, deployment_mode, production),
    application:unset_env(iris_core, metrics_bearer_token),
    Result = iris_core:validate_metrics_token(),
    ?assertEqual({error, metrics_token_missing}, Result),
    application:unset_env(iris_core, deployment_mode).

metrics_undefined_ok_in_development_test() ->
    application:set_env(iris_core, deployment_mode, development),
    application:unset_env(iris_core, metrics_bearer_token),
    Result = iris_core:validate_metrics_token(),
    ?assertEqual(ok, Result),
    application:unset_env(iris_core, deployment_mode).

%% --- iris_edge: config validation exports (regression tests) ---

edge_validate_rate_limits_ok_test() ->
    ?assertEqual(ok, iris_edge_app:validate_rate_limits(100, 200)).

edge_validate_rate_limits_inverted_test() ->
    ?assertEqual({error, burst_less_than_rate}, iris_edge_app:validate_rate_limits(200, 100)).

edge_validate_replication_factor_ok_test() ->
    ?assertEqual(ok, iris_edge_app:validate_replication_factor(3)).

edge_validate_replication_factor_zero_test() ->
    ?assertEqual({error, invalid_replication_factor}, iris_edge_app:validate_replication_factor(0)).
