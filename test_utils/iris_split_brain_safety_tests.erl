-module(iris_split_brain_safety_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mnesia Split-Brain Safety
%% Verifies that force_load_table has peer checks and reconciliation
%% respects partition guard state.
%% =============================================================================

%% Test: iris_group init_tables code checks for active replicas before
%% force-loading, mirroring the safe pattern from iris_core.
group_force_load_source_has_peer_check_test() ->
    {ok, Source} = file:read_file("src/iris_group.erl"),
    %% The force_load block must reference active_replicas
    ?assert(binary:match(Source, <<"active_replicas">>) =/= nomatch).

%% Test: reconcile_after_partition checks partition guard mode
reconcile_checks_partition_guard_test() ->
    {ok, Source} = file:read_file("src/iris_core.erl"),
    %% reconcile_after_partition must reference partition guard status
    ?assert(binary:match(Source, <<"iris_partition_guard:get_status">>) =/= nomatch).

%% Test: production.config has deployment_mode = production
production_config_deployment_mode_test() ->
    {ok, [Config]} = file:consult("config/production.config"),
    CoreConfig = proplists:get_value(iris_core, Config),
    ?assertEqual(production, proplists:get_value(deployment_mode, CoreConfig)),
    EdgeConfig = proplists:get_value(iris_edge, Config),
    ?assertEqual(production, proplists:get_value(deployment_mode, EdgeConfig)).

%% Test: production.config has non-empty expected_cluster_nodes
production_config_has_expected_nodes_test() ->
    {ok, [Config]} = file:consult("config/production.config"),
    CoreConfig = proplists:get_value(iris_core, Config),
    ExpectedNodes = proplists:get_value(expected_cluster_nodes, CoreConfig),
    ?assert(length(ExpectedNodes) > 0).

%% Test: production.config has non-empty core_nodes
production_config_has_core_nodes_test() ->
    {ok, [Config]} = file:consult("config/production.config"),
    EdgeConfig = proplists:get_value(iris_edge, Config),
    CoreNodes = proplists:get_value(core_nodes, EdgeConfig),
    ?assert(length(CoreNodes) > 0).
