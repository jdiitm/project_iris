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
    {ok, {_SupFlags, TopChildren}} = iris_core:init([]),
    %% With tiered supervisors, we must also inspect sub-supervisor children
    lists:flatmap(fun(Child) ->
        Id = maps:get(id, Child),
        case maps:get(type, Child, worker) of
            supervisor ->
                %% Get children from the sub-supervisor's init/1
                {Mod, _Fun, _Args} = maps:get(start, Child),
                case Mod:init([]) of
                    {ok, {_, SubChildren}} ->
                        [Id | [maps:get(id, SC) || SC <- SubChildren]];
                    _ ->
                        [Id]
                end;
            _ ->
                [Id]
        end
    end, TopChildren).

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
    %% With tiered supervisors, foundation_sup (which contains metrics)
    %% starts before messaging_sup (which contains group, bridge).
    %% So iris_foundation_sup must appear before iris_messaging_sup.
    FoundationIdx = index_of(iris_foundation_sup, Ids),
    MessagingIdx = index_of(iris_messaging_sup, Ids),
    ClusterIdx = index_of(iris_cluster_sup, Ids),
    ?assert(FoundationIdx < MessagingIdx),
    ?assert(MessagingIdx < ClusterIdx),
    %% Within foundation, metrics should still come before dedup
    MetricsIdx = index_of(iris_metrics, Ids),
    DedupIdx = index_of(iris_dedup, Ids),
    ?assert(MetricsIdx < DedupIdx).

index_of(Elem, List) ->
    index_of(Elem, List, 1).
index_of(_Elem, [], _N) ->
    not_found;
index_of(Elem, [Elem|_], N) ->
    N;
index_of(Elem, [_|Rest], N) ->
    index_of(Elem, Rest, N + 1).
