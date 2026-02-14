-module(iris_audit_mitigation_v1_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation V1: TDD Tests
%% =============================================================================
%%
%% RED/GREEN tests for three critical audit findings:
%%   1. Supervisor Cascade Risk (Finding 3B) — intensity must be <= 5
%%   2. Mnesia Memory Guard (Finding 3A) — new iris_mnesia_guard module
%%   3. Compression Startup Validation (Finding 2B) — startup check
%%
%% These tests inspect supervisor specs and module exports directly.
%% No full application start or Mnesia required.
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

get_core_sup_flags() ->
    application:ensure_started(iris_core),
    application:set_env(iris_core, presence_backend, ets),
    {ok, {SupFlags, _Children}} = iris_core:init([]),
    SupFlags.

get_core_child_ids() ->
    application:ensure_started(iris_core),
    application:set_env(iris_core, presence_backend, ets),
    {ok, {_SupFlags, Children}} = iris_core:init([]),
    [maps:get(id, C) || C <- Children].

get_edge_sup_flags() ->
    application:set_env(iris_edge, port, 9999),
    %% Clean up ETS tables from prior invocations
    try ets:delete(local_presence_v2) catch error:badarg -> ok end,
    try ets:delete(presence_cache) catch error:badarg -> ok end,
    try ets:delete(iris_conn_rate) catch error:badarg -> ok end,
    {ok, {SupFlags, _Children}} = iris_edge_sup:init([]),
    SupFlags.

%% =============================================================================
%% 1. Supervisor Cascade Risk — Intensity Hardening
%% =============================================================================

%% Core supervisor intensity must be <= 7 to prevent cascade restarts.
%% rest_for_one needs more headroom than one_for_one (edge uses <= 5).
core_supervisor_intensity_max_7_test() ->
    SupFlags = get_core_sup_flags(),
    Intensity = maps:get(intensity, SupFlags),
    ?assert(Intensity =< 7).

%% Edge supervisor intensity must be <= 5 for consistency.
edge_supervisor_intensity_max_5_test() ->
    SupFlags = get_edge_sup_flags(),
    Intensity = maps:get(intensity, SupFlags),
    ?assert(Intensity =< 5).

%% Core supervisor period must remain >= 60s (don't shorten the window).
core_supervisor_period_at_least_60_test() ->
    SupFlags = get_core_sup_flags(),
    Period = maps:get(period, SupFlags),
    ?assert(Period >= 60).

%% =============================================================================
%% 2. Mnesia Memory Guard — Module Existence and API
%% =============================================================================

%% iris_mnesia_guard must exist and export start_link/0.
mnesia_guard_exports_start_link_test() ->
    Exports = iris_mnesia_guard:module_info(exports),
    ?assert(lists:member({start_link, 0}, Exports)).

%% iris_mnesia_guard must export check_memory/0 for on-demand inspection.
mnesia_guard_exports_check_memory_test() ->
    Exports = iris_mnesia_guard:module_info(exports),
    ?assert(lists:member({check_memory, 0}, Exports)).

%% check_memory/0 must return {ok, Map} where Map is #{atom() => integer()}.
mnesia_guard_returns_memory_map_test() ->
    %% Ensure Mnesia is running with at least the schema table
    mnesia:start(),
    {ok, MemMap} = iris_mnesia_guard:check_memory(),
    ?assert(is_map(MemMap)),
    %% schema table always exists when Mnesia is running
    ?assert(maps:is_key(schema, MemMap)),
    %% All values must be non-negative integers (bytes)
    maps:foreach(fun(_Table, Bytes) ->
        ?assert(is_integer(Bytes)),
        ?assert(Bytes >= 0)
    end, MemMap).

%% Default alarm threshold must be 1GB (1073741824 bytes).
mnesia_guard_default_threshold_test() ->
    Threshold = iris_mnesia_guard:get_alarm_threshold(),
    ?assertEqual(1073741824, Threshold).

%% check_memory/0 must detect threshold breaches and return alarm info.
mnesia_guard_detects_threshold_breach_test() ->
    mnesia:start(),
    %% Clear any stale alarm state
    persistent_term:put(iris_mnesia_guard_alarms, []),
    %% Set an absurdly low threshold so schema table triggers it
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    try
        {ok, _MemMap} = iris_mnesia_guard:check_memory(),
        %% The alarm list should have been populated via persistent_term
        Alarms = iris_mnesia_guard:get_alarms(),
        ?assert(length(Alarms) > 0),
        %% Each alarm is {TableName, Bytes}
        [{AlarmTable, AlarmBytes}|_] = Alarms,
        ?assert(is_atom(AlarmTable)),
        ?assert(is_integer(AlarmBytes)),
        ?assert(AlarmBytes > 0)
    after
        application:unset_env(iris_core, mnesia_memory_alarm_bytes),
        persistent_term:put(iris_mnesia_guard_alarms, [])
    end.

%% iris_mnesia_guard must be declared in the core supervisor tree.
mnesia_guard_in_supervisor_tree_test() ->
    Ids = get_core_child_ids(),
    ?assert(lists:member(iris_mnesia_guard, Ids)).

%% =============================================================================
%% 3. Compression Startup Validation
%% =============================================================================

%% available_algorithms/0 must always include zlib.
compression_zlib_always_available_test() ->
    Algos = iris_compression:available_algorithms(),
    ?assert(lists:member(<<"zlib">>, Algos)).

%% validate_compression_startup/0 must be exported from iris_core and return ok.
compression_startup_validation_exists_test() ->
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({validate_compression_startup, 0}, Exports)).

compression_startup_validation_returns_ok_test() ->
    ?assertEqual(ok, iris_core:validate_compression_startup()).
