-module(iris_schema_migration_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-6: Mnesia Schema Migration Framework Tests
%% =============================================================================
%% Requirement: The system must track schema versions and apply migrations
%% sequentially. Downgrades are rejected. Schema must be current before
%% the application can proceed.
%% =============================================================================

setup() ->
    %% Ensure Mnesia is running (reuse existing instance)
    case mnesia:system_info(is_running) of
        no ->
            mnesia:create_schema([node()]),
            mnesia:start();
        _ -> ok
    end,
    %% Drop old schema version table if exists
    case lists:member(iris_schema_version, mnesia:system_info(tables)) of
        true -> mnesia:delete_table(iris_schema_version);
        false -> ok
    end,
    %% Clear migration registry
    iris_schema:clear_migrations(),
    ok.

cleanup(_) ->
    case lists:member(iris_schema_version, mnesia:system_info(tables)) of
        true -> catch mnesia:delete_table(iris_schema_version);
        false -> ok
    end,
    iris_schema:clear_migrations().

%% =============================================================================
%% Test: detects schema version mismatch
%% =============================================================================
detects_schema_version_mismatch_test() ->
    setup(),
    try
        iris_schema:init_version_table(),
        iris_schema:set_stored_version(1),
        Result = iris_schema:check(2),
        ?assertEqual({needs_migration, 1, 2}, Result)
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: applies migration forward
%% =============================================================================
applies_migration_forward_test() ->
    setup(),
    try
        iris_schema:init_version_table(),
        iris_schema:set_stored_version(1),
        %% Register a migration from v1 -> v2 that sets a flag
        Self = self(),
        Migration = fun() ->
            Self ! {migration_applied, 1, 2},
            ok
        end,
        iris_schema:register_migration(1, 2, Migration),
        %% Apply migration
        ok = iris_schema:migrate(2),
        %% Verify: migration was executed and version is updated
        receive
            {migration_applied, 1, 2} -> ok
        after 1000 ->
            ?assert(false)
        end,
        ?assertEqual(2, iris_schema:get_stored_version())
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: rejects downgrade
%% =============================================================================
rejects_downgrade_test() ->
    setup(),
    try
        iris_schema:init_version_table(),
        iris_schema:set_stored_version(3),
        Result = iris_schema:check(2),
        ?assertEqual({error, downgrade_not_supported}, Result)
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: already current returns ok
%% =============================================================================
already_current_returns_ok_test() ->
    setup(),
    try
        iris_schema:init_version_table(),
        iris_schema:set_stored_version(5),
        Result = iris_schema:check(5),
        ?assertEqual(ok, Result)
    after
        cleanup(ok)
    end.
