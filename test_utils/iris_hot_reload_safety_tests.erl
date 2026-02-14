-module(iris_hot_reload_safety_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION P0-1: Hot Code Loading Safety
%% Every gen_server/gen_statem module MUST export code_change so that
%% hot-code upgrades do not crash processes holding complex state.
%% =============================================================================

%% All gen_server modules that must have code_change/3
gen_server_modules() ->
    [iris_async_router, iris_auth, iris_cluster_join_worker,
     iris_cluster_manager, iris_dedup, iris_discovery,
     iris_durable_batcher, iris_efficiency_monitor,
     iris_flow_controller, iris_group, iris_health_handler,
     iris_hlc, iris_ingress_guard, iris_keys,
     iris_mailbox_guard, iris_mailbox_monitor, iris_metrics,
     iris_partition_guard, iris_presence, iris_rate_limiter,
     iris_read_receipts, iris_region_bridge, iris_registry_ets,
     iris_shard, iris_status_batcher,
     %% These already had code_change -- included for completeness
     iris_circuit_breaker, iris_core_registry, iris_edge_listener,
     iris_router_worker].

%% All gen_statem modules that must have code_change/4
gen_statem_modules() ->
    [iris_edge_conn, iris_ws_lite].

code_change_exists_for_gen_servers_test_() ->
    [{atom_to_list(Mod),
      fun() ->
          code:ensure_loaded(Mod),
          ?assert(erlang:function_exported(Mod, code_change, 3))
      end} || Mod <- gen_server_modules()].

code_change_exists_for_gen_statems_test_() ->
    [{atom_to_list(Mod),
      fun() ->
          code:ensure_loaded(Mod),
          ?assert(erlang:function_exported(Mod, code_change, 4))
      end} || Mod <- gen_statem_modules()].

async_router_state_migration_test() ->
    %% Verify code_change returns {ok, State} for identity migration
    OldState = {state, 1, 0, 0, 0, 0, 0, erlang:system_time(second)},
    {ok, NewState} = iris_async_router:code_change("0.1.0", OldState, []),
    ?assertEqual(OldState, NewState).

appup_includes_async_router_test() ->
    {ok, [AppupTerm]} = file:consult("src/iris_edge.appup.src"),
    {_Vsn, UpInstructions, _DownInstructions} = AppupTerm,
    %% Flatten all upgrade instructions
    AllUpMods = lists:flatmap(
        fun({_FromVsn, Instructions}) ->
            [Mod || Instr <- Instructions,
                    Mod <- case Instr of
                               {update, M, _} -> [M];
                               {load_module, M} -> [M];
                               {add_module, M} -> [M];
                               _ -> []
                           end]
        end, UpInstructions),
    ?assert(lists:member(iris_async_router, AllUpMods)).
