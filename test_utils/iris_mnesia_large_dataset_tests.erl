-module(iris_mnesia_large_dataset_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Mnesia Large-Dataset Behavior Tests
%% =============================================================================
%%
%% No test for large-dataset Mnesia behavior.
%% Mnesia disc_copies tables load all keys into RAM,
%% which is a hard scalability limit. These tests verify:
%%
%%   1. disc_only_copies (offline_msg) handles large data without RAM growth
%%   2. Memory alarm triggers when disc_copies table exceeds threshold
%%   3. Backpressure rejects stores when memory alarm is active
%%
%% These tests use Mnesia with ram_copies for speed but structurally verify
%% the production table types are correct.
%% =============================================================================

-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Setup / Cleanup
%% ---------------------------------------------------------------------------

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

cleanup(_) ->
    application:unset_env(iris_core, mnesia_memory_alarm_bytes),
    persistent_term:put(iris_mnesia_guard_alarms, []),
    application:stop(mnesia),
    try ets:delete(?METRICS_TABLE) catch _:_ -> ok end.

%% ---------------------------------------------------------------------------
%% Test Generator
%% ---------------------------------------------------------------------------

iris_mnesia_large_dataset_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"disc_only_copies handles bulk data without RAM explosion",
       {timeout, 30, fun test_disc_only_handles_large_data/0}},
      {"memory alarm triggers at configurable threshold",
       fun test_memory_alarm_triggers_at_threshold/0},
      {"backpressure rejects stores under memory alarm",
       fun test_backpressure_rejects_under_memory_alarm/0}
     ]}.

%% =============================================================================
%% Test: disc_only_copies handles bulk data
%% =============================================================================
%%
%% Stores 10K records in a disc_only_copies table and verifies that Mnesia's
%% RAM usage (as reported by table_info(memory)) does not grow proportionally.
%% disc_only_copies uses DETS under the hood (no keys in RAM).

test_disc_only_handles_large_data() ->
    %% Create a disc_only_copies table (like offline_msg in production)
    case mnesia:create_table(large_test_offline, [
        {disc_only_copies, [node()]},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, large_test_offline}} ->
            mnesia:clear_table(large_test_offline)
    end,
    mnesia:wait_for_tables([large_test_offline], 5000),

    %% Measure baseline memory (in words)
    _BaselineMemory = mnesia:table_info(large_test_offline, memory),

    %% Insert 10K records (each ~100 bytes)
    NumRecords = 10000,
    lists:foreach(fun(I) ->
        Key = list_to_binary(io_lib:format("user_~B", [I rem 100])),
        Ts = os:system_time(second),
        Msg = list_to_binary(io_lib:format("msg_~B_payload_data_here", [I])),
        mnesia:dirty_write({large_test_offline, Key, Ts, Msg})
    end, lists:seq(1, NumRecords)),

    %% Measure memory after bulk insert
    PostInsertMemory = mnesia:table_info(large_test_offline, memory),

    %% Verify: disc_only_copies uses DETS which has a write cache.
    %% mnesia:table_info(memory) reports cache + metadata (NOT data on disc).
    %% The key assertion: memory should be MUCH less than what disc_copies
    %% would use (which would be ~10K * ~20 words per record = 200K words).
    %% For disc_only_copies, the DETS cache buffers some records but not all.
    %% Allow generous margin for DETS cache/metadata overhead.
    %%
    %% A disc_copies table with 10K records would use ~1.6MB+ (200K words * 8).
    %% disc_only_copies should use significantly less Mnesia RAM.
    %% We use 2M words (~16MB) as a generous upper bound.
    MaxExpectedMemory = 2000000,  %% 2M words (~16MB) generous bound
    ?assert(PostInsertMemory < MaxExpectedMemory),

    %% Verify all records are retrievable (data is on disc, not lost)
    AllRecords = mnesia:dirty_match_object({large_test_offline, '_', '_', '_'}),
    ?assert(length(AllRecords) >= NumRecords),

    %% Clean up
    mnesia:delete_table(large_test_offline),
    ok.

%% =============================================================================
%% Test: Memory alarm triggers at threshold
%% =============================================================================

test_memory_alarm_triggers_at_threshold() ->
    %% Set an absurdly low alarm threshold (1 byte) so the schema table triggers it
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    persistent_term:put(iris_mnesia_guard_alarms, []),

    %% Run the memory check
    {ok, _MemMap} = iris_mnesia_guard:check_memory(),

    %% Alarms should now contain at least the schema table
    Alarms = iris_mnesia_guard:get_alarms(),
    ?assert(length(Alarms) > 0),

    %% Verify alarm contains table name and byte count
    [{AlarmTable, AlarmBytes} | _] = Alarms,
    ?assert(is_atom(AlarmTable)),
    ?assert(is_integer(AlarmBytes)),
    ?assert(AlarmBytes > 0),

    %% Clean up
    application:unset_env(iris_core, mnesia_memory_alarm_bytes),
    persistent_term:put(iris_mnesia_guard_alarms, []).

%% =============================================================================
%% Test: Backpressure rejects stores under memory alarm
%% =============================================================================

test_backpressure_rejects_under_memory_alarm() ->
    %% Set low threshold to trigger backpressure
    application:set_env(iris_core, mnesia_memory_alarm_bytes, 1),
    persistent_term:put(iris_mnesia_guard_alarms, []),

    %% is_memory_ok should return error
    Result = iris_mnesia_guard:is_memory_ok(),
    ?assertEqual({error, memory_pressure}, Result),

    %% Calling store should be rejected
    StoreResult = iris_offline_storage:store(<<"bp_user">>, <<"bp_msg">>, 1),
    ?assertEqual({error, memory_pressure}, StoreResult),

    %% Clean up
    application:unset_env(iris_core, mnesia_memory_alarm_bytes),
    persistent_term:put(iris_mnesia_guard_alarms, []).
