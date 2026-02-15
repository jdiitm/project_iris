-module(iris_mtls_production_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% G1: mTLS Production Enforcement Tests
%%
%% check_mtls_enforcement/0 defaults enforce_mtls to false, allowing
%% production deployments to silently run without mTLS. The RFC mandates
%% "MANDATORY mTLS for all internal traffic" (Section 4.4, NFR-15).
%%
%%      even without enforce_mtls explicitly set. Currently it returns ok.
%% =============================================================================

iris_mtls_production_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"production without explicit enforce_mtls crashes (core)",
       fun test_production_core_crashes_without_config/0},
      {"production without explicit enforce_mtls crashes (edge)",
       fun test_production_edge_crashes_without_config/0},
      {"non-production without enforce_mtls returns ok (core)",
       fun test_dev_core_ok_without_config/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    application:unset_env(iris_core, enforce_mtls),
    application:unset_env(iris_core, env),
    ok.

test_production_core_crashes_without_config() ->
    %% Set environment to production but do NOT set enforce_mtls
    application:set_env(iris_core, env, production),
    application:unset_env(iris_core, enforce_mtls),
    %% In production, should crash because mTLS is mandatory
    ?assertExit(mtls_not_configured, iris_core:check_mtls_enforcement()).

test_production_edge_crashes_without_config() ->
    %% Edge now delegates to iris_core:check_mtls_enforcement/0 (DRY consolidation)
    application:set_env(iris_core, env, production),
    application:unset_env(iris_core, enforce_mtls),
    ?assertExit(mtls_not_configured, iris_core:check_mtls_enforcement()).

test_dev_core_ok_without_config() ->
    %% In development, mTLS is optional (warning only)
    application:set_env(iris_core, env, development),
    application:unset_env(iris_core, enforce_mtls),
    ?assertEqual(ok, iris_core:check_mtls_enforcement()).
