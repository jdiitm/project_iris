-module(iris_supervision_edge_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 1B TDD: Edge Supervisor must declare all critical gen_servers
%% =============================================================================
%% RED:  These tests FAIL because iris_edge_sup:init/1 does not include
%%       iris_auth, iris_rate_limiter, iris_ingress_guard, iris_discovery.
%% GREEN: Adding child specs to iris_edge_sup:init/1 makes them pass.
%% =============================================================================

%% iris_edge_sup:init/1 requires the iris_edge port env and creates ETS tables.
%% We only set the env and call init/1 directly -- no full app start needed.
get_child_ids() ->
    application:set_env(iris_edge, port, 9999),
    %% Delete ETS tables from prior test invocations so init/1 succeeds
    catch ets:delete(local_presence_v2),
    catch ets:delete(presence_cache),
    {ok, {_SupFlags, Children}} = iris_edge_sup:init([]),
    [maps:get(id, C) || C <- Children].

edge_supervisor_declares_iris_auth_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_auth, Ids)).

edge_supervisor_declares_iris_rate_limiter_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_rate_limiter, Ids)).

edge_supervisor_declares_iris_ingress_guard_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_ingress_guard, Ids)).

edge_supervisor_declares_iris_discovery_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_discovery, Ids)).
