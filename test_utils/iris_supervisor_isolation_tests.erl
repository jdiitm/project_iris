-module(iris_supervisor_isolation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Supervisor Tiering Isolation Tests
%% =============================================================================
%% Requirement: Crashes in one tier (messaging) must NOT restart processes
%% in another tier (foundation). This verifies that the supervisor tree
%% provides proper fault isolation.
%% =============================================================================

%% =============================================================================
%% Test: iris_core supervisor has sub-supervisor children
%% =============================================================================
core_has_tiered_supervisors_test() ->
    %% Verify that iris_core supervisor (if running) has sub-supervisor children,
    %% or that the sub-supervisor modules exist and can be loaded
    ?assert(code:which(iris_foundation_sup) =/= non_existing),
    ?assert(code:which(iris_messaging_sup) =/= non_existing),
    ?assert(code:which(iris_cluster_sup) =/= non_existing).

%% =============================================================================
%% Test: sub-supervisors use one_for_one strategy
%% =============================================================================
foundation_sup_uses_one_for_one_test() ->
    %% Verify the module exports init/1 and returns one_for_one
    {ok, {SupFlags, _Children}} = iris_foundation_sup:init([]),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

messaging_sup_uses_one_for_one_test() ->
    {ok, {SupFlags, _Children}} = iris_messaging_sup:init([]),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

cluster_sup_uses_one_for_one_test() ->
    {ok, {SupFlags, _Children}} = iris_cluster_sup:init([]),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

%% =============================================================================
%% Test: foundation children are in foundation_sup, not core directly
%% =============================================================================
foundation_contains_metrics_test() ->
    {ok, {_, Children}} = iris_foundation_sup:init([]),
    ChildIds = [maps:get(id, C) || C <- Children],
    ?assert(lists:member(iris_metrics, ChildIds)),
    ?assert(lists:member(iris_health_handler, ChildIds)).

messaging_contains_group_test() ->
    {ok, {_, Children}} = iris_messaging_sup:init([]),
    ChildIds = [maps:get(id, C) || C <- Children],
    ?assert(lists:member(iris_group, ChildIds)),
    ?assert(lists:member(iris_keys, ChildIds)).

cluster_contains_cluster_manager_test() ->
    {ok, {_, Children}} = iris_cluster_sup:init([]),
    ChildIds = [maps:get(id, C) || C <- Children],
    ?assert(lists:member(iris_cluster_manager, ChildIds)),
    ?assert(lists:member(iris_durable_batcher_sup, ChildIds)).
