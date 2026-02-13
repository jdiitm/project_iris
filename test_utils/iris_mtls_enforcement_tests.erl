-module(iris_mtls_enforcement_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 7 TDD: mTLS enforcement check must exist and be config-gated
%% =============================================================================
%% RED:  check_mtls_enforcement/0 does not exist in iris_core.
%% GREEN: Add exported check_mtls_enforcement/0 to iris_core and iris_edge_app.
%% =============================================================================

%% When enforce_mtls=true and no ssl_dist_optfile is set,
%% the function must exit with mtls_not_configured.
mtls_enforcement_crashes_without_ssl_dist_test() ->
    application:set_env(iris_core, enforce_mtls, true),
    ?assertExit(mtls_not_configured, iris_core:check_mtls_enforcement()).

%% When enforce_mtls=false, the function must return ok (no crash).
mtls_nonenforced_does_not_crash_test() ->
    application:set_env(iris_core, enforce_mtls, false),
    ?assertEqual(ok, iris_core:check_mtls_enforcement()).

%% Edge app delegates to iris_core:check_mtls_enforcement/0 (DRY consolidation).
%% These tests verify the core function works for both apps.
edge_mtls_enforcement_crashes_without_ssl_dist_test() ->
    application:set_env(iris_core, enforce_mtls, true),
    ?assertExit(mtls_not_configured, iris_core:check_mtls_enforcement()).

edge_mtls_nonenforced_does_not_crash_test() ->
    application:set_env(iris_core, enforce_mtls, false),
    ?assertEqual(ok, iris_core:check_mtls_enforcement()).
