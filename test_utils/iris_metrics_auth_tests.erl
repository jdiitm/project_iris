-module(iris_metrics_auth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Secure /metrics Endpoint Tests
%% =============================================================================
%% Requirement: When metrics_bearer_token is configured, /metrics must
%% require Bearer token authentication. Without valid token, return 401.
%% =============================================================================

setup() ->
    %% Ensure metrics server is running
    case whereis(iris_metrics) of
        undefined -> catch iris_metrics:start_link();
        _ -> ok
    end,
    SavedToken = application:get_env(iris_core, metrics_bearer_token),
    SavedToken.

cleanup(SavedToken) ->
    case SavedToken of
        {ok, Val} -> application:set_env(iris_core, metrics_bearer_token, Val);
        undefined -> application:unset_env(iris_core, metrics_bearer_token)
    end.

%% =============================================================================
%% Test: metrics requires auth when token configured
%% =============================================================================
metrics_requires_auth_when_token_configured_test() ->
    Saved = setup(),
    try
        application:set_env(iris_core, metrics_bearer_token, <<"test_token_123">>),
        %% Request without auth header
        {Status, _CT, _Body} = iris_health_handler:dispatch(<<"/metrics">>, #{}),
        ?assertEqual(401, Status)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: metrics accessible with valid token
%% =============================================================================
metrics_accessible_with_valid_token_test() ->
    Saved = setup(),
    try
        application:set_env(iris_core, metrics_bearer_token, <<"test_token_123">>),
        Headers = #{authorization => <<"Bearer test_token_123">>},
        {Status, _CT, _Body} = iris_health_handler:dispatch(<<"/metrics">>, Headers),
        ?assertEqual(200, Status)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: metrics returns 401 with wrong token
%% =============================================================================
metrics_rejects_wrong_token_test() ->
    Saved = setup(),
    try
        application:set_env(iris_core, metrics_bearer_token, <<"test_token_123">>),
        Headers = #{authorization => <<"Bearer wrong_token">>},
        {Status, _CT, _Body} = iris_health_handler:dispatch(<<"/metrics">>, Headers),
        ?assertEqual(401, Status)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: metrics open when no token configured (backward compatible)
%% =============================================================================
metrics_open_without_token_configured_test() ->
    Saved = setup(),
    try
        application:unset_env(iris_core, metrics_bearer_token),
        {Status, _CT, _Body} = iris_health_handler:dispatch(<<"/metrics">>, #{}),
        ?assertEqual(200, Status)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: production config has metrics_bearer_token set
%% =============================================================================
production_config_has_metrics_token_test() ->
    {ok, [Config]} = file:consult("config/production.config"),
    CoreConfig = proplists:get_value(iris_core, Config),
    Token = proplists:get_value(metrics_bearer_token, CoreConfig, undefined),
    ?assertNotEqual(undefined, Token),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).
