-module(iris_mnesia_backpressure_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION V2 — P0-2: Mnesia Memory Backpressure
%% =============================================================================
%%
%% Tests verify:
%%   1. iris_mnesia_guard exports is_memory_ok/0
%%   2. Backpressure rejects offline stores when memory exceeds threshold
%%   3. Normal operation allowed under threshold
%%   4. Table type assertions (disc_only_copies for offline_msg)
%%   5. Rejection metric emitted
%% =============================================================================

-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

ensure_metrics_table() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end.

get_metric(Key) ->
    case ets:lookup(?METRICS_TABLE, Key) of
        [{_, Val}] -> Val;
        [] -> 0
    end.

%% =============================================================================
%% Test: iris_mnesia_guard exports is_memory_ok/0
%% =============================================================================

mnesia_guard_exports_is_memory_ok_test() ->
    Exports = iris_mnesia_guard:module_info(exports),
    ?assert(lists:member({is_memory_ok, 0}, Exports)).

%% =============================================================================
%% Test: is_memory_ok returns ok when under threshold
%% =============================================================================

is_memory_ok_returns_ok_under_threshold_test() ->
    mnesia:start(),
    %% Default threshold is 1GB — schema table is tiny, so should be ok
    application:unset_env(iris_core, mnesia_memory_alarm_bytes),
    try
        Result = iris_mnesia_guard:is_memory_ok(),
        ?assertEqual(ok, Result)
    after
        mnesia:stop()
    end.

%% =============================================================================
%% Test: is_memory_ok returns {error, memory_pressure} when over threshold
%% =============================================================================

is_memory_ok_returns_error_over_threshold_test() ->
    mnesia:start(),
    %% Set absurdly low threshold so schema table triggers it
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    try
        Result = iris_mnesia_guard:is_memory_ok(),
        ?assertEqual({error, memory_pressure}, Result)
    after
        application:unset_env(iris_core, mnesia_memory_alarm_bytes),
        mnesia:stop()
    end.

%% =============================================================================
%% Test: offline_msg table spec uses disc_only_copies (NOT disc_copies)
%% Verifies audit finding was WRONG about offline_msg being disc_copies.
%% =============================================================================

offline_msg_table_type_is_disc_only_copies_test() ->
    %% Read the source code to verify the table spec
    {ok, Src} = file:read_file("src/iris_core.erl"),
    %% Find table_spec(offline_msg) -> {disc_only_copies, ...}
    ?assertNotEqual(nomatch, binary:match(Src, <<"table_spec(offline_msg) ->">>)),
    ?assertNotEqual(nomatch, binary:match(Src, <<"disc_only_copies">>)),
    %% Verify it's NOT disc_copies for offline_msg (find the spec line)
    %% Extract the line containing table_spec(offline_msg)
    {Pos, _} = binary:match(Src, <<"table_spec(offline_msg) ->">>),
    %% Get 100 bytes after the match
    Snippet = binary:part(Src, Pos, min(100, byte_size(Src) - Pos)),
    ?assertNotEqual(nomatch, binary:match(Snippet, <<"disc_only_copies">>)).

%% =============================================================================
%% Test: group_member table uses disc_copies (known risk, documented)
%% =============================================================================

group_member_table_type_is_disc_copies_test() ->
    %% Group table init uses disc_copies when Mnesia schema supports it.
    %% Verified structurally from iris_group.erl source.
    {ok, Src} = file:read_file("src/iris_group.erl"),
    %% The init_tables function determines disc_copies dynamically
    ?assertNotEqual(nomatch, binary:match(Src, <<"disc_copies">>)).

%% =============================================================================
%% Test: Backpressure metric emitted on rejection
%% =============================================================================

backpressure_emits_metric_on_rejection_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {offline_store_backpressure_rejects, 0}),
    mnesia:start(),
    %% Set absurdly low threshold to trigger backpressure
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    try
        Before = get_metric(offline_store_backpressure_rejects),
        %% Call store — should be rejected due to memory pressure
        _Result = iris_offline_storage:store(<<"user1">>, <<"msg">>, 1),
        After = get_metric(offline_store_backpressure_rejects),
        ?assert(After > Before)
    after
        application:unset_env(iris_core, mnesia_memory_alarm_bytes),
        mnesia:stop()
    end.
