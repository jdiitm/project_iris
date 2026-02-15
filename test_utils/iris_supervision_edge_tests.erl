-module(iris_supervision_edge_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% =============================================================================
%%       iris_auth, iris_rate_limiter, iris_ingress_guard, iris_discovery.
%% =============================================================================

%% iris_edge_sup:init/1 requires the iris_edge port env and creates ETS tables.
%% We only set the env and call init/1 directly -- no full app start needed.
get_child_ids() ->
    application:set_env(iris_edge, port, 9999),
    %% Delete ETS tables from prior test invocations so init/1 succeeds
    try ets:delete(local_presence_v2) catch error:badarg -> ok end,
    try ets:delete(presence_cache) catch error:badarg -> ok end,
    try ets:delete(iris_edge_dedup) catch error:badarg -> ok end,
    try ets:delete(iris_conn_rate) catch error:badarg -> ok end,
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

edge_supervisor_declares_iris_edge_dedup_cleaner_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_edge_dedup_cleaner, Ids)).
