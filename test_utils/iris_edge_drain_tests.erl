-module(iris_edge_drain_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-3: Graceful Shutdown Drain Tests
%% =============================================================================
%% Requirement: Application must implement prep_stop/1 to drain connections
%% BEFORE the supervisor tree is terminated. The /ready endpoint must return
%% 503 while draining so the load balancer stops routing new traffic.
%% =============================================================================

setup() ->
    %% Ensure draining flag is cleared
    catch persistent_term:erase(iris_edge_draining),
    ok.

cleanup(_) ->
    catch persistent_term:erase(iris_edge_draining),
    ok.

%% =============================================================================
%% Test: prep_stop sets draining flag
%% =============================================================================
prep_stop_sets_draining_flag_test() ->
    setup(),
    try
        %% Before prep_stop, should not be draining
        ?assertEqual(false, iris_edge_app:is_draining()),
        %% Call prep_stop (with 0ms drain to avoid sleeping in test)
        application:set_env(iris_edge, shutdown_drain_ms, 0),
        _State = iris_edge_app:prep_stop(test_state),
        %% After prep_stop, should be draining
        ?assertEqual(true, iris_edge_app:is_draining())
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: /ready returns 503 when draining
%% =============================================================================
ready_endpoint_returns_503_when_draining_test() ->
    setup(),
    try
        %% Set draining flag directly
        persistent_term:put(iris_edge_draining, true),
        %% Query the /ready endpoint
        {Status, _ContentType, Body} = iris_health_handler:dispatch(<<"/ready">>, #{}),
        ?assertEqual(503, Status),
        ?assertNotEqual(nomatch, binary:match(Body, <<"draining">>))
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: is_draining returns false by default
%% =============================================================================
is_draining_default_false_test() ->
    setup(),
    try
        ?assertEqual(false, iris_edge_app:is_draining())
    after
        cleanup(ok)
    end.
