-module(iris_wal_tmpfs_enforcement_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F3: WAL tmpfs Enforcement Tests (NFR-1 / RPO=0)
%%
%% validate_wal_storage/2 currently logs a warning when the WAL directory is
%% on tmpfs but does not fail. In production, a RAM-backed WAL defeats the
%% purpose of write-ahead logging (data loss on crash = RPO > 0).
%%
%% RED: validate_wal_storage must return {error, tmpfs_in_production} when
%%      env=production and the path is on tmpfs. Current code returns ok.
%% GREEN: Add production env check; return error on tmpfs in production.
%% =============================================================================

iris_wal_tmpfs_enforcement_test_() ->
    [
     {"validate_wal_storage rejects tmpfs in production",
      fun test_rejects_tmpfs_in_production/0},
     {"validate_wal_storage allows tmpfs in non-production",
      fun test_allows_tmpfs_in_dev/0}
    ].

test_rejects_tmpfs_in_production() ->
    %% Set env to production
    application:set_env(iris_core, env, production),
    %% /dev/shm is guaranteed to be tmpfs on Linux
    Result = iris_durable_batcher:validate_wal_storage("/dev/shm/test_wal", 1),
    %% Must reject in production
    ?assertEqual({error, tmpfs_in_production}, Result),
    %% Cleanup
    application:unset_env(iris_core, env).

test_allows_tmpfs_in_dev() ->
    %% Set env to development (non-production)
    application:set_env(iris_core, env, development),
    Result = iris_durable_batcher:validate_wal_storage("/dev/shm/test_wal", 1),
    %% Should be ok (warning only) in dev
    ?assertEqual(ok, Result),
    application:unset_env(iris_core, env).
