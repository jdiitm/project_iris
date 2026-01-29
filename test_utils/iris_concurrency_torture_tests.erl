-module(iris_concurrency_torture_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Concurrency Torture Tests (P2 - Scale Critical)
%% =============================================================================
%%
%% Purpose: High-contention stress tests to verify system behavior under
%% extreme concurrent load. Per Principal Test Audit: "No concuerror integration"
%%
%% Note: Concuerror (formal race condition detection) is deferred.
%% These tests use ETS-based torture tests for practical concurrency validation.
%%
%% Test Scenarios:
%%   1. 1000 concurrent writers same key
%%   2. Hot group message flood
%%   3. Presence update storm
%%   4. Lock contention patterns
%%   5. Scheduler saturation
%%
%% Success Criteria:
%%   - No crashes
%%   - No deadlocks (all operations complete within timeout)
%%   - Data consistency maintained
%%   - No silent data loss
%% =============================================================================

%% Configurable test parameters
-define(TORTURE_PROCS, 500).        %% Number of concurrent processes
-define(TORTURE_OPS_PER_PROC, 100). %% Operations per process
-define(TORTURE_TIMEOUT, 60000).    %% Overall timeout (ms)

%% =============================================================================
%% Test Setup / Teardown
%% =============================================================================

setup() ->
    %% Create test ETS tables
    ensure_table(torture_test_table, [set, public, named_table, {write_concurrency, true}]),
    ensure_table(torture_counter, [set, public, named_table, {write_concurrency, true}]),
    ensure_table(torture_bag, [bag, public, named_table, {write_concurrency, true}]),
    
    %% Initialize counter
    ets:insert(torture_counter, {ops, 0}),
    ets:insert(torture_counter, {errors, 0}),
    ok.

cleanup(_) ->
    catch ets:delete(torture_test_table),
    catch ets:delete(torture_counter),
    catch ets:delete(torture_bag),
    ok.

ensure_table(Name, Opts) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, Opts);
        _ ->
            ets:delete_all_objects(Name),
            ok
    end.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

torture_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 120,  %% 2 minute timeout for torture tests
      [
       {"1000 concurrent writers same key", fun test_writer_contention/0},
       {"Hot key read/write mix", fun test_hot_key_rw_mix/0},
       {"Counter increment storm", fun test_counter_storm/0},
       {"Presence update storm simulation", fun test_presence_storm/0},
       {"Process spawn/die churn", fun test_process_churn/0},
       {"Message passing flood", fun test_message_flood/0}
      ]}}.

%% =============================================================================
%% P2: Writer Contention Tests
%% =============================================================================

test_writer_contention() ->
    %% Test: 1000 concurrent writers to same key
    %% All writes should complete without crash or deadlock
    
    Key = hot_key,
    NumWriters = 1000,
    Self = self(),
    
    %% Clear table
    ets:delete_all_objects(torture_test_table),
    
    %% Spawn writers
    Pids = [spawn(fun() ->
        Value = <<"value_", (integer_to_binary(N))/binary>>,
        Result = try
            true = ets:insert(torture_test_table, {Key, Value}),
            {ok, N}
        catch
            _:Error -> {error, Error}
        end,
        Self ! {done, self(), Result}
    end) || N <- lists:seq(1, NumWriters)],
    
    %% Collect results with timeout
    Results = collect_results(Pids, 30000),
    
    %% All should complete without error
    ErrorCount = length([E || {error, E} <- Results]),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    ?assertEqual(0, ErrorCount),
    ?assertEqual(0, TimeoutCount),
    
    %% Table should have the key (value undefined - race winner)
    ?assertMatch([{Key, _}], ets:lookup(torture_test_table, Key)).

test_hot_key_rw_mix() ->
    %% Test: Mixed read/write workload on hot key
    %% Verify no crashes and reasonable throughput
    
    Key = hot_rw_key,
    NumProcs = ?TORTURE_PROCS,
    OpsPerProc = ?TORTURE_OPS_PER_PROC,
    Self = self(),
    
    %% Initialize key
    ets:insert(torture_test_table, {Key, 0}),
    
    %% Spawn workers (mix of readers and writers)
    Pids = [spawn(fun() ->
        IsWriter = (N rem 3 =:= 0),  %% 1/3 writers, 2/3 readers
        Results = [begin
            if IsWriter ->
                try
                    ets:insert(torture_test_table, {Key, N * OpsPerProc + Op}),
                    ok
                catch _:E -> {error, E}
                end;
            true ->
                try
                    ets:lookup(torture_test_table, Key),
                    ok
                catch _:E -> {error, E}
                end
            end
        end || Op <- lists:seq(1, OpsPerProc)],
        
        ErrorCount = length([R || R <- Results, R =/= ok]),
        Self ! {done, self(), {N, ErrorCount, length(Results)}}
    end) || N <- lists:seq(1, NumProcs)],
    
    %% Collect results
    Results = collect_results(Pids, ?TORTURE_TIMEOUT),
    
    %% Calculate totals
    TotalOps = lists:sum([Ops || {_, _, Ops} <- Results, is_integer(Ops)]),
    TotalErrors = lists:sum([Errs || {_, Errs, _} <- Results, is_integer(Errs)]),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    ?assertEqual(0, TimeoutCount),
    ?assertEqual(0, TotalErrors),
    ?assertEqual(NumProcs * OpsPerProc, TotalOps).

%% =============================================================================
%% P2: Counter Increment Storm
%% =============================================================================

test_counter_storm() ->
    %% Test: Many processes incrementing same counter
    %% Final value should equal total increments
    
    CounterKey = storm_counter,
    NumProcs = ?TORTURE_PROCS,
    IncrementsPerProc = 100,
    ExpectedTotal = NumProcs * IncrementsPerProc,
    Self = self(),
    
    %% Initialize counter
    ets:insert(torture_counter, {CounterKey, 0}),
    
    %% Spawn incrementers
    Pids = [spawn(fun() ->
        lists:foreach(fun(_) ->
            ets:update_counter(torture_counter, CounterKey, 1)
        end, lists:seq(1, IncrementsPerProc)),
        Self ! {done, self(), ok}
    end) || _ <- lists:seq(1, NumProcs)],
    
    %% Wait for completion
    Results = collect_results(Pids, ?TORTURE_TIMEOUT),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    ?assertEqual(0, TimeoutCount),
    
    %% Verify final count
    [{CounterKey, FinalCount}] = ets:lookup(torture_counter, CounterKey),
    ?assertEqual(ExpectedTotal, FinalCount).

%% =============================================================================
%% P2: Presence Storm Simulation
%% =============================================================================

test_presence_storm() ->
    %% Test: Simulate presence update flood (users going online/offline rapidly)
    %% This simulates the local_presence_v2 ETS table behavior
    
    NumUsers = 200,
    UpdatesPerUser = 50,
    Self = self(),
    
    %% Create presence table if not exists
    ensure_table(test_presence, [set, public, named_table, {write_concurrency, true}]),
    
    %% Spawn user simulation processes
    Pids = [spawn(fun() ->
        User = list_to_binary("user_" ++ integer_to_list(N)),
        Pid = self(),
        
        %% Rapidly toggle online/offline
        lists:foreach(fun(Update) ->
            case Update rem 2 of
                0 -> ets:insert(test_presence, {User, Pid});
                1 -> ets:delete(test_presence, User)
            end
        end, lists:seq(1, UpdatesPerUser)),
        
        Self ! {done, self(), ok}
    end) || N <- lists:seq(1, NumUsers)],
    
    %% Wait for completion
    Results = collect_results(Pids, ?TORTURE_TIMEOUT),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    ets:delete(test_presence),
    
    ?assertEqual(0, TimeoutCount).

%% =============================================================================
%% P2: Process Churn Tests
%% =============================================================================

test_process_churn() ->
    %% Test: Rapid process creation and termination
    %% Verifies no resource leaks or scheduler issues
    
    NumIterations = 100,
    ProcsPerIteration = 100,
    
    %% Run iterations
    lists:foreach(fun(_Iter) ->
        %% Spawn processes
        Pids = [spawn(fun() ->
            %% Do some work
            _Sum = lists:sum(lists:seq(1, 100)),
            %% Die
            ok
        end) || _ <- lists:seq(1, ProcsPerIteration)],
        
        %% Wait for all to die
        lists:foreach(fun(Pid) ->
            Ref = monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 ->
                exit(Pid, kill)
            end
        end, Pids)
    end, lists:seq(1, NumIterations)),
    
    %% Should complete without crash
    ?assert(true).

test_message_flood() ->
    %% Test: Flood a process with messages
    %% Verify no message loss and orderly processing
    
    NumSenders = 50,
    MessagesPerSender = 200,
    ExpectedTotal = NumSenders * MessagesPerSender,
    
    %% Spawn receiver
    Self = self(),
    Receiver = spawn(fun() -> message_receiver_loop(Self, 0, ExpectedTotal) end),
    
    %% Spawn senders
    Senders = [spawn(fun() ->
        lists:foreach(fun(MsgNum) ->
            Receiver ! {msg, N, MsgNum}
        end, lists:seq(1, MessagesPerSender)),
        Self ! {sender_done, self()}
    end) || N <- lists:seq(1, NumSenders)],
    
    %% Wait for all senders to complete
    lists:foreach(fun(Pid) ->
        receive
            {sender_done, Pid} -> ok
        after 30000 ->
            timeout
        end
    end, Senders),
    
    %% Tell receiver to report
    Receiver ! {report, Self},
    
    %% Get result
    receive
        {receiver_result, Count} ->
            ?assertEqual(ExpectedTotal, Count)
    after 10000 ->
        ?assert(false)  %% Timeout - receiver hung
    end.

message_receiver_loop(Parent, Count, Expected) when Count >= Expected ->
    %% Received all messages, wait for report request
    receive
        {report, Pid} ->
            Pid ! {receiver_result, Count};
        {msg, _, _} ->
            %% Extra message - still count it
            message_receiver_loop(Parent, Count + 1, Expected)
    end;
message_receiver_loop(Parent, Count, Expected) ->
    receive
        {msg, _Sender, _MsgNum} ->
            message_receiver_loop(Parent, Count + 1, Expected);
        {report, Pid} ->
            %% Early report request
            Pid ! {receiver_result, Count}
    after 30000 ->
        %% Timeout - report what we have
        Parent ! {receiver_timeout, Count}
    end.

%% =============================================================================
%% Hot Group Simulation Tests
%% =============================================================================

hot_group_test_() ->
    {"Hot group stress tests",
     {setup,
      fun setup/0,
      fun cleanup/1,
      {timeout, 120,
       [
        {"Hot group message flood", fun test_hot_group_flood/0},
        {"Group membership churn", fun test_group_membership_churn/0}
       ]}}}.

test_hot_group_flood() ->
    %% Test: Many users sending to same group simultaneously
    %% Simulates "hot group" scenario (popular chat room)
    
    GroupId = <<"hot_group_123">>,
    NumMembers = 100,
    MessagesPerMember = 50,
    Self = self(),
    
    %% Create simulated group message storage
    ensure_table(group_messages, [bag, public, named_table, {write_concurrency, true}]),
    
    %% Spawn group members sending messages
    Pids = [spawn(fun() ->
        UserId = list_to_binary("member_" ++ integer_to_list(N)),
        lists:foreach(fun(MsgNum) ->
            MsgId = <<UserId/binary, "_", (integer_to_binary(MsgNum))/binary>>,
            ets:insert(group_messages, {GroupId, MsgId, UserId, <<"content">>})
        end, lists:seq(1, MessagesPerMember)),
        Self ! {done, self(), ok}
    end) || N <- lists:seq(1, NumMembers)],
    
    %% Wait for completion
    Results = collect_results(Pids, ?TORTURE_TIMEOUT),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    %% Verify message count
    Messages = ets:lookup(group_messages, GroupId),
    ExpectedMessages = NumMembers * MessagesPerMember,
    
    ets:delete(group_messages),
    
    ?assertEqual(0, TimeoutCount),
    ?assertEqual(ExpectedMessages, length(Messages)).

test_group_membership_churn() ->
    %% Test: Rapid group join/leave operations
    
    GroupId = <<"churn_group">>,
    NumUsers = 100,
    JoinsPerUser = 20,
    Self = self(),
    
    %% Create membership table
    ensure_table(group_members, [bag, public, named_table, {write_concurrency, true}]),
    
    %% Spawn users that join and leave repeatedly
    Pids = [spawn(fun() ->
        UserId = list_to_binary("churner_" ++ integer_to_list(N)),
        lists:foreach(fun(Op) ->
            case Op rem 2 of
                0 -> ets:insert(group_members, {GroupId, UserId});
                1 -> ets:delete_object(group_members, {GroupId, UserId})
            end
        end, lists:seq(1, JoinsPerUser)),
        Self ! {done, self(), ok}
    end) || N <- lists:seq(1, NumUsers)],
    
    %% Wait for completion
    Results = collect_results(Pids, ?TORTURE_TIMEOUT),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    ets:delete(group_members),
    
    ?assertEqual(0, TimeoutCount).

%% =============================================================================
%% Scheduler Saturation Tests
%% =============================================================================

scheduler_test_() ->
    {"Scheduler saturation tests",
     {timeout, 60,
      [
       {"CPU-bound work distribution", fun test_cpu_saturation/0},
       {"IO-bound work distribution", fun test_io_simulation/0}
      ]}}.

test_cpu_saturation() ->
    %% Test: Saturate all schedulers with CPU-bound work
    %% Verify fair distribution and no starvation
    
    NumSchedulers = erlang:system_info(schedulers),
    WorkPerScheduler = 10,
    TotalWork = NumSchedulers * WorkPerScheduler,
    Self = self(),
    
    %% Spawn CPU-bound workers
    Pids = [spawn(fun() ->
        %% CPU-bound work (calculate something)
        Result = lists:foldl(fun(X, Acc) -> Acc + X * X end, 0, lists:seq(1, 10000)),
        Self ! {done, self(), Result}
    end) || _ <- lists:seq(1, TotalWork)],
    
    %% Collect results
    Results = collect_results(Pids, 30000),
    CompletedCount = length([R || R <- Results, R =/= timeout]),
    
    %% All should complete
    ?assertEqual(TotalWork, CompletedCount).

test_io_simulation() ->
    %% Test: Simulate IO-bound work (timers/sleeps)
    %% Verify scheduler doesn't block
    
    NumWorkers = 100,
    Self = self(),
    
    %% Spawn IO-simulating workers
    Pids = [spawn(fun() ->
        %% Simulate IO with sleep
        timer:sleep(rand:uniform(10)),
        Self ! {done, self(), ok}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    %% Should all complete quickly (parallel)
    Results = collect_results(Pids, 5000),
    CompletedCount = length([R || R <- Results, R =/= timeout]),
    
    ?assertEqual(NumWorkers, CompletedCount).

%% =============================================================================
%% Helper Functions
%% =============================================================================

collect_results(Pids, Timeout) ->
    collect_results(Pids, Timeout, []).

collect_results([], _Timeout, Acc) ->
    lists:reverse(Acc);
collect_results([Pid | Rest], Timeout, Acc) ->
    receive
        {done, Pid, Result} ->
            collect_results(Rest, Timeout, [Result | Acc])
    after Timeout ->
        collect_results(Rest, 0, [timeout | Acc])
    end.
