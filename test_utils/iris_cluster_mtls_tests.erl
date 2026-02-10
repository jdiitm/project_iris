-module(iris_cluster_mtls_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Finding 3: mTLS Node-to-Node Enforcement in Cluster Manager (NFR-15)
%%
%% iris_cluster_manager:do_replication/0 connects to remote nodes and triggers
%% cross-region replication without checking if SSL distribution is active.
%% In production with enforce_mtls=true, this can create plaintext links.
%%
%% RED: do_replication must return {error, mtls_required} when env=production
%%      and ssl_dist_optfile is not set. Currently it proceeds regardless.
%% GREEN: Add mTLS pre-check in do_replication/0.
%% =============================================================================

-export([]).

iris_cluster_mtls_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"do_replication rejects when mTLS mandated but not configured",
       fun test_replication_blocked_without_mtls/0},
      {"do_replication allowed when mTLS is not mandated (non-production)",
       fun test_replication_allowed_without_mtls_in_dev/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    application:unset_env(iris_core, env),
    application:unset_env(iris_core, enforce_mtls),
    ok.

test_replication_blocked_without_mtls() ->
    %% In production, mTLS is mandatory. Without ssl_dist_optfile,
    %% do_replication MUST refuse to proceed.
    application:set_env(iris_core, env, production),
    application:unset_env(iris_core, enforce_mtls),
    %% do_replication is internal, but we test via force_replication or
    %% by calling it if exported. We need it exported for testing.
    %% check_replication_mtls/0 is the pre-check function.
    Result = iris_cluster_manager:check_replication_mtls(),
    ?assertEqual({error, mtls_required}, Result).

test_replication_allowed_without_mtls_in_dev() ->
    %% In development, mTLS is not mandated. Replication should be allowed.
    application:set_env(iris_core, env, development),
    application:unset_env(iris_core, enforce_mtls),
    Result = iris_cluster_manager:check_replication_mtls(),
    ?assertEqual(ok, Result).
