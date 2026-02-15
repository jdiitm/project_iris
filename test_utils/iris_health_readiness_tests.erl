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
    %% Ensure Mnesia is stopped for this test
    MnesiaWasRunning = case mnesia:system_info(is_running) of
        yes -> mnesia:stop(), true;
        no -> false
    end,
    try
        {Status, ContentType, Body} = iris_health_handler:dispatch(<<"/ready">>, #{}),
        ?assertEqual(503, Status),
        ?assertEqual(<<"application/json">>, ContentType),
        ?assertNotEqual(nomatch, binary:match(Body, <<"false">>))
    after
        case MnesiaWasRunning of
            true -> mnesia:start();
            false -> ok
        end
    end.

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
    %% /ready -> 503 when Mnesia stopped
    MnesiaWasRunning = case mnesia:system_info(is_running) of
        yes -> mnesia:stop(), true;
        no -> false
    end,
    try
        {503, _, _} = iris_health_handler:dispatch(<<"/ready">>, #{})
    after
        case MnesiaWasRunning of
            true -> mnesia:start();
            false -> ok
        end
    end,
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
