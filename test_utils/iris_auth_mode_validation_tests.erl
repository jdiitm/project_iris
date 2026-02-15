-module(iris_auth_mode_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% H-3: auth_mode Production Validation Tests
%% =============================================================================
%% Requirement: Edge nodes in production MUST use auth_mode=verifier.
%% Only dedicated auth_service nodes may use auth_mode=signer.
%%
%% These tests validate the startup guard that prevents misconfiguration.
%% =============================================================================

setup() ->
    %% Save original env so we can restore it
    SavedEdge = [
        {deployment_mode, application:get_env(iris_edge, deployment_mode)},
        {auth_mode, application:get_env(iris_edge, auth_mode)},
        {node_role, application:get_env(iris_edge, node_role)}
    ],
    SavedEdge.

cleanup(SavedEdge) ->
    lists:foreach(fun({Key, {ok, Val}}) ->
                          application:set_env(iris_edge, Key, Val);
                     ({Key, undefined}) ->
                          application:unset_env(iris_edge, Key)
                  end, SavedEdge).

%% =============================================================================
%% Test: signer mode rejected on edge node in production
%% =============================================================================
signer_rejected_on_edge_in_production_test() ->
    Saved = setup(),
    try
        application:set_env(iris_edge, deployment_mode, production),
        application:set_env(iris_edge, node_role, edge),
        application:set_env(iris_edge, auth_mode, signer),
        Result = iris_edge_app:validate_auth_mode(),
        ?assertEqual({error, signer_on_edge}, Result)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: verifier mode accepted on edge node in production
%% =============================================================================
verifier_accepted_on_edge_test() ->
    Saved = setup(),
    try
        application:set_env(iris_edge, deployment_mode, production),
        application:set_env(iris_edge, node_role, edge),
        application:set_env(iris_edge, auth_mode, verifier),
        Result = iris_edge_app:validate_auth_mode(),
        ?assertEqual(ok, Result)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: signer mode accepted on auth_service node
%% =============================================================================
signer_accepted_on_auth_service_test() ->
    Saved = setup(),
    try
        application:set_env(iris_edge, deployment_mode, production),
        application:set_env(iris_edge, node_role, auth_service),
        application:set_env(iris_edge, auth_mode, signer),
        Result = iris_edge_app:validate_auth_mode(),
        ?assertEqual(ok, Result)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: non-production mode always accepts any auth_mode
%% =============================================================================
any_auth_mode_accepted_in_development_test() ->
    Saved = setup(),
    try
        application:set_env(iris_edge, deployment_mode, development),
        application:set_env(iris_edge, auth_mode, signer),
        Result = iris_edge_app:validate_auth_mode(),
        ?assertEqual(ok, Result)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: production config has auth_mode=verifier for edge
%% =============================================================================
production_config_uses_verifier_test() ->
    %% Parse the production config file and verify auth_mode is verifier
    {ok, [Config]} = file:consult("config/production.config"),
    EdgeConfig = proplists:get_value(iris_edge, Config),
    AuthMode = proplists:get_value(auth_mode, EdgeConfig),
    ?assertEqual(verifier, AuthMode).
