-module(iris_supervision_core_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 1A TDD: Core Supervisor must start all critical gen_servers
%% =============================================================================
%% These tests verify that the iris_core supervisor starts every gen_server
%% that the system depends on at runtime. A gen_server with start_link/0
%% that is NOT in the supervision tree is dead code — it will never run.
%%
%% RED: These tests FAIL before the fix because the Children list in
%%      iris_core:init/1 omits these gen_servers.
%% GREEN: Adding child specs to iris_core:init/1 makes them pass.
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
