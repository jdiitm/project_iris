-module(iris_schema).

%% =============================================================================
%% B-6: Mnesia Schema Migration Framework
%% =============================================================================
%% Tracks schema version in a dedicated Mnesia table and applies
%% registered migrations sequentially. Downgrades are rejected.
%% =============================================================================

-export([init_version_table/0]).
-export([check/1, migrate/1]).
-export([get_stored_version/0, set_stored_version/1]).
-export([register_migration/3, clear_migrations/0]).

-record(iris_schema_version, {key, version}).

%% Migration registry (process dictionary for simplicity; ETS in production)
-define(MIGRATION_KEY, iris_schema_migrations).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Create the iris_schema_version table if it doesn't exist.
-spec init_version_table() -> ok.
init_version_table() ->
    case lists:member(iris_schema_version, mnesia:system_info(tables)) of
        true -> ok;
        false ->
            {atomic, ok} = mnesia:create_table(iris_schema_version, [
                {attributes, record_info(fields, iris_schema_version)},
                {ram_copies, [node()]},
                {type, set}
            ]),
            mnesia:wait_for_tables([iris_schema_version], 5000),
            %% Initialize to version 0 if brand new
            set_stored_version(0)
    end,
    ok.

%% @doc Check if stored version matches expected code version.
-spec check(pos_integer()) -> ok | {needs_migration, integer(), integer()} | {error, downgrade_not_supported}.
check(ExpectedVersion) ->
    StoredVersion = get_stored_version(),
    if
        StoredVersion =:= ExpectedVersion -> ok;
        StoredVersion < ExpectedVersion -> {needs_migration, StoredVersion, ExpectedVersion};
        StoredVersion > ExpectedVersion -> {error, downgrade_not_supported}
    end.

%% @doc Apply all registered migrations from current stored version to target.
-spec migrate(pos_integer()) -> ok | {error, term()}.
migrate(TargetVersion) ->
    CurrentVersion = get_stored_version(),
    Migrations = get_migrations(),
    apply_migrations(CurrentVersion, TargetVersion, Migrations).

%% @doc Get the stored schema version.
-spec get_stored_version() -> integer().
get_stored_version() ->
    case mnesia:dirty_read(iris_schema_version, schema_version) of
        [#iris_schema_version{version = V}] -> V;
        [] -> 0
    end.

%% @doc Set the stored schema version.
-spec set_stored_version(integer()) -> ok.
set_stored_version(Version) ->
    mnesia:dirty_write(iris_schema_version,
        #iris_schema_version{key = schema_version, version = Version}),
    ok.

%% @doc Register a migration function for FromVersion -> ToVersion.
-spec register_migration(integer(), integer(), fun(() -> term())) -> ok.
register_migration(FromVersion, ToVersion, MigrationFun) ->
    Existing = get_migrations(),
    put(?MIGRATION_KEY, [{FromVersion, ToVersion, MigrationFun} | Existing]),
    ok.

%% @doc Clear all registered migrations.
-spec clear_migrations() -> ok.
clear_migrations() ->
    put(?MIGRATION_KEY, []),
    ok.

%% =============================================================================
%% Internal
%% =============================================================================

get_migrations() ->
    case get(?MIGRATION_KEY) of
        undefined -> [];
        List -> List
    end.

apply_migrations(Current, Target, _Migrations) when Current >= Target ->
    ok;
apply_migrations(Current, Target, Migrations) ->
    %% Find migration for Current -> Current+1
    Next = Current + 1,
    case find_migration(Current, Next, Migrations) of
        {ok, MigrationFun} ->
            logger:info("Applying schema migration v~p -> v~p", [Current, Next]),
            try
                MigrationFun(),
                set_stored_version(Next),
                apply_migrations(Next, Target, Migrations)
            catch
                Class:Reason ->
                    logger:error("Schema migration v~p -> v~p failed: ~p:~p",
                                 [Current, Next, Class, Reason]),
                    {error, {migration_failed, Current, Next, {Class, Reason}}}
            end;
        not_found ->
            %% No explicit migration registered; just bump version
            logger:info("No migration registered for v~p -> v~p, bumping version", [Current, Next]),
            set_stored_version(Next),
            apply_migrations(Next, Target, Migrations)
    end.

find_migration(From, To, Migrations) ->
    case [Fun || {F, T, Fun} <- Migrations, F =:= From, T =:= To] of
        [MigrationFun | _] -> {ok, MigrationFun};
        [] -> not_found
    end.
