-module(iris_supervision_core_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Core Supervisor must start all critical gen_servers
%% =============================================================================
%% These tests verify that the iris_core supervisor starts every gen_server
%% that the system depends on at runtime. A gen_server with start_link/0
%% that is NOT in the supervision tree is dead code — it will never run.
%%
%%      iris_core:init/1 omits these gen_servers.
%% =============================================================================

%% We test by inspecting the child spec list returned by iris_core:init/1,
%% which does not require starting Mnesia or the full application.
%% This is a structural test: "does the supervisor declare these children?"

get_child_ids() ->
    %% init/1 requires presence_backend to be configured
    application:ensure_started(iris_core),
    application:set_env(iris_core, presence_backend, ets),
    {ok, {_SupFlags, Children}} = iris_core:init([]),
    [maps:get(id, C) || C <- Children].

core_supervisor_declares_iris_metrics_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_metrics, Ids)).

core_supervisor_declares_iris_keys_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_keys, Ids)).

core_supervisor_declares_iris_region_bridge_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_region_bridge, Ids)).

core_supervisor_declares_iris_read_receipts_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_read_receipts, Ids)).

core_supervisor_declares_iris_mailbox_guard_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_mailbox_guard, Ids)).

core_supervisor_declares_iris_mailbox_monitor_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_mailbox_monitor, Ids)).

core_supervisor_declares_iris_efficiency_monitor_test() ->
    Ids = get_child_ids(),
    ?assert(lists:member(iris_efficiency_monitor, Ids)).

%% =============================================================================
%% 5.3: Supervisor Strategy Tests
%% =============================================================================

get_sup_flags() ->
    application:ensure_started(iris_core),
    application:set_env(iris_core, presence_backend, ets),
    {ok, {SupFlags, _Children}} = iris_core:init([]),
    SupFlags.

core_supervisor_uses_rest_for_one_test() ->
    SupFlags = get_sup_flags(),
    ?assertEqual(rest_for_one, maps:get(strategy, SupFlags)).

metrics_starts_before_services_test() ->
    Ids = get_child_ids(),
    MetricsIdx = index_of(iris_metrics, Ids),
    %% iris_dedup, iris_group, iris_region_bridge all depend on metrics
    %% They should come after iris_metrics in the child list
    DedupIdx = index_of(iris_dedup, Ids),
    GroupIdx = index_of(iris_group, Ids),
    BridgeIdx = index_of(iris_region_bridge, Ids),
    ?assert(MetricsIdx < DedupIdx),
    ?assert(MetricsIdx < GroupIdx),
    ?assert(MetricsIdx < BridgeIdx).

index_of(Elem, List) ->
    index_of(Elem, List, 1).
index_of(_Elem, [], _N) ->
    not_found;
index_of(Elem, [Elem|_], N) ->
    N;
index_of(Elem, [_|Rest], N) ->
    index_of(Elem, Rest, N + 1).
