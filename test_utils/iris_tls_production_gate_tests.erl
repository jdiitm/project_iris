-module(iris_tls_production_gate_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mitigation: TLS must be mandatory in production mode.
%% =============================================================================
%% RFC NFR-14: TLS is MANDATORY for all client connections.
%% check_tls_policy(false) must return {error, tls_required} when
%% deployment_mode=production, regardless of allow_insecure setting.
%% =============================================================================

production_mode_rejects_insecure_even_when_allowed_test() ->
    %% Setup: production mode + allow_insecure=true + tls_enabled=false
    application:set_env(iris_edge, deployment_mode, production),
    application:set_env(iris_edge, allow_insecure, true),
    try
        Result = iris_edge_listener:check_tls_policy(false),
        ?assertEqual({error, tls_required_in_production}, Result)
    after
        application:unset_env(iris_edge, deployment_mode),
        application:unset_env(iris_edge, allow_insecure)
    end.

production_mode_rejects_insecure_when_not_allowed_test() ->
    %% Setup: production mode + allow_insecure=false + tls_enabled=false
    application:set_env(iris_edge, deployment_mode, production),
    application:set_env(iris_edge, allow_insecure, false),
    try
        Result = iris_edge_listener:check_tls_policy(false),
        ?assertEqual({error, tls_required_in_production}, Result)
    after
        application:unset_env(iris_edge, deployment_mode),
        application:unset_env(iris_edge, allow_insecure)
    end.

dev_mode_allows_insecure_when_flag_set_test() ->
    %% Setup: development mode + allow_insecure=true
    application:set_env(iris_edge, deployment_mode, development),
    application:set_env(iris_edge, allow_insecure, true),
    try
        Result = iris_edge_listener:check_tls_policy(false),
        ?assertEqual(ok, Result)
    after
        application:unset_env(iris_edge, deployment_mode),
        application:unset_env(iris_edge, allow_insecure)
    end.

dev_mode_rejects_insecure_when_flag_not_set_test() ->
    %% Setup: development mode + allow_insecure=false (default)
    application:set_env(iris_edge, deployment_mode, development),
    application:set_env(iris_edge, allow_insecure, false),
    try
        Result = iris_edge_listener:check_tls_policy(false),
        ?assertEqual({error, tls_required}, Result)
    after
        application:unset_env(iris_edge, deployment_mode),
        application:unset_env(iris_edge, allow_insecure)
    end.

tls_enabled_always_passes_test() ->
    %% TLS enabled = always ok, regardless of mode
    application:set_env(iris_edge, deployment_mode, production),
    try
        ?assertEqual(ok, iris_edge_listener:check_tls_policy(true))
    after
        application:unset_env(iris_edge, deployment_mode)
    end.
