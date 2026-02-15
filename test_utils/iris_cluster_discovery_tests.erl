-module(iris_cluster_discovery_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% : Brittle Cluster Discovery
%% =============================================================================
%%
%% is_core_node/1 in iris_core and iris_cluster_manager
%% relies on hardcoded string matching ("core", "iris_core") for node names.
%% This breaks in K8s, IP-based naming, or any non-standard naming schema.
%%
%% Remediation: Support config-based role assignment via
%%   application:get_env(iris_core, node_role, undefined)
%% with naming convention as fallback.
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

cleanup() ->
    application:unset_env(iris_core, node_role),
    ok.

%% =============================================================================
%% Tests for iris_core:is_core_node/1
%% =============================================================================

%% Config-based role overrides naming convention
config_role_overrides_naming_convention_test() ->
    application:set_env(iris_core, node_role, core),
    try
        %% A node with a weird name should be recognized as core when config says so
        ?assertEqual(true, iris_core:is_core_node('weird_name@host')),
        ?assertEqual(true, iris_core:is_core_node('edge_1@10.0.0.5')),
        ?assertEqual(true, iris_core:is_core_node('iris-core-0.iris-headless.default.svc.cluster.local'))
    after
        cleanup()
    end.

%% Naming convention still works as fallback when no config
naming_convention_fallback_test() ->
    application:unset_env(iris_core, node_role),
    ?assertEqual(true, iris_core:is_core_node('core_east_1@host')),
    ?assertEqual(true, iris_core:is_core_node('iris_core@10.0.0.1')),
    ?assertEqual(true, iris_core:is_core_node('core@localhost')).

%% Non-core node with no config returns false
non_core_node_returns_false_test() ->
    application:unset_env(iris_core, node_role),
    ?assertEqual(false, iris_core:is_core_node('edge_1@host')),
    ?assertEqual(false, iris_core:is_core_node('worker_3@10.0.0.5')).

%% K8s-style names work with config-based role
k8s_names_work_with_config_test() ->
    application:set_env(iris_core, node_role, core),
    try
        ?assertEqual(true, iris_core:is_core_node('iris-core-0.iris-headless.default.svc.cluster.local')),
        ?assertEqual(true, iris_core:is_core_node('statefulset-0@10.244.0.5'))
    after
        cleanup()
    end.

%% Non-core config role correctly returns false
non_core_role_config_test() ->
    application:set_env(iris_core, node_role, edge),
    try
        %% Even nodes with "core" in name should be false if role is edge
        ?assertEqual(false, iris_core:is_core_node('core_east_1@host'))
    after
        cleanup()
    end.

%% =============================================================================
%% Tests for iris_cluster_manager is_core_node/1 (structural)
%% =============================================================================

%% Verify cluster_manager source uses config-based role check
cluster_manager_uses_config_role_test() ->
    {ok, Src} = file:read_file("src/iris_cluster_manager.erl"),
    %% Must contain node_role config lookup
    ?assertNotEqual(nomatch, binary:match(Src, <<"node_role">>)).

%% =============================================================================
%% Tests for iris_session legacy_core_node/0 (structural)
%% =============================================================================

%% Verify session source has registry-first discovery
session_uses_registry_first_test() ->
    {ok, Src} = file:read_file("src/iris_session.erl"),
    %% get_core_node must call iris_core_registry:get_core() first
    ?assertNotEqual(nomatch, binary:match(Src, <<"iris_core_registry:get_core()">>)),
    %% And must also support config-based node_role
    ?assertNotEqual(nomatch, binary:match(Src, <<"node_role">>)).
