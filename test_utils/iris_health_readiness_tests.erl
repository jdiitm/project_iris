-module(iris_health_readiness_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Application-level health check
%% =============================================================================
%% The /ready endpoint MUST return 503 when Mnesia is not running.
%% Docker healthcheck MUST use /ready (HTTP), not epmd (process check).
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test: /ready returns 503 when Mnesia is not running
%% ---------------------------------------------------------------------------
ready_returns_503_when_mnesia_not_running_test() ->
    %% Mnesia is not started in the test environment
    {Status, ContentType, Body} = iris_health_handler:dispatch(<<"/ready">>, #{}),
    ?assertEqual(503, Status),
    ?assertEqual(<<"application/json">>, ContentType),
    %% Body should indicate Mnesia is not ready
    ?assertNotEqual(nomatch, binary:match(Body, <<"false">>)).

%% ---------------------------------------------------------------------------
%% Test: /health always returns 200 (liveness check)
%% ---------------------------------------------------------------------------
health_returns_200_always_test() ->
    {Status, ContentType, Body} = iris_health_handler:dispatch(<<"/health">>, #{}),
    ?assertEqual(200, Status),
    ?assertEqual(<<"application/json">>, ContentType),
    ?assertNotEqual(nomatch, binary:match(Body, <<"ok">>)).

%% ---------------------------------------------------------------------------
%% Test: Dispatch routes correctly
%% ---------------------------------------------------------------------------
dispatch_routing_test() ->
    %% /health -> 200
    {200, _, _} = iris_health_handler:dispatch(<<"/health">>, #{}),
    %% /ready -> 503 (Mnesia not running)
    {503, _, _} = iris_health_handler:dispatch(<<"/ready">>, #{}),
    %% Unknown -> 404
    {404, _, _} = iris_health_handler:dispatch(<<"/unknown">>, #{}),
    ok.

%% ---------------------------------------------------------------------------
%% Test: /health with trailing slash and query string
%% ---------------------------------------------------------------------------
health_path_normalization_test() ->
    {200, _, _} = iris_health_handler:dispatch(<<"/health/">>, #{}),
    {200, _, _} = iris_health_handler:dispatch(<<"/health?foo=bar">>, #{}),
    ok.
