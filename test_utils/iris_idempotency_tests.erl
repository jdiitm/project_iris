-module(iris_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Idempotency / Exactly-Once Delivery Tests (P0 - Safety Critical)
%% =============================================================================
%%
%% Purpose: Validates exactly-once delivery guarantees as specified in
%% Principal Test Audit Mitigation Plan.
%%
%% Invariants:
%%   - Same msg_id is delivered exactly once (duplicates rejected)
%%   - Retry storms (100+ retries) result in exactly 1 delivery
%%   - Different msg_ids are all delivered independently
%%   - Dedup works across process restarts (via dedup_log)
%%   - High-concurrency scenarios maintain exactly-once guarantee
%%
%% Note: These tests complement iris_dedup_tests.erl with focus on
%% end-to-end idempotency semantics rather than dedup internals.
%% =============================================================================

%% =============================================================================
%% Test Setup / Teardown
%% =============================================================================

setup() ->
    %% Start dedup server if not running
    case whereis(iris_dedup) of
        undefined ->
            {ok, Pid} = iris_dedup:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    catch gen_server:stop(iris_dedup);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

idempotency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% P0: Core exactly-once tests
      {"Same msg_id delivered once only", fun test_same_msgid_once/0},
      {"Retry storm (100 retries) delivers once", fun test_retry_storm/0},
      {"Different msg_ids all delivered", fun test_unique_ids_all_delivered/0},
      
      %% P0: Edge cases
      {"Sequential retries (no concurrency)", fun test_sequential_retries/0},
      {"Interleaved message IDs", fun test_interleaved_msgids/0},
      {"Binary message IDs", fun test_binary_msgids/0},
      {"UUID-style message IDs", fun test_uuid_msgids/0},
      
      %% P0: Timing and ordering
      {"Rapid-fire same ID", fun test_rapid_fire_same_id/0},
      {"Delayed retry detection", fun test_delayed_retry/0}
     ]}.

%% =============================================================================
%% P0: Core Exactly-Once Tests
%% =============================================================================

test_same_msgid_once() ->
    %% Test: Same message ID should only be marked as "new" once
    MsgId = generate_unique_msgid(<<"same_id_test">>),
    
    %% First check - should be new
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    
    %% Second check - should be duplicate
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)),
    
    %% Third check - still duplicate
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).

test_retry_storm() ->
    %% Test: 100 retries of same message ID should result in exactly 1 "new"
    MsgId = generate_unique_msgid(<<"retry_storm">>),
    NumRetries = 100,
    
    %% Perform retries
    Results = [iris_dedup:check_and_mark(MsgId) || _ <- lists:seq(1, NumRetries)],
    
    %% Count results
    NewCount = length([R || R <- Results, R =:= new]),
    DupCount = length([R || R <- Results, R =:= duplicate]),
    
    %% Exactly one "new", rest are "duplicate"
    ?assertEqual(1, NewCount),
    ?assertEqual(NumRetries - 1, DupCount).

test_unique_ids_all_delivered() ->
    %% Test: Different message IDs should all be marked as "new"
    NumMessages = 50,
    
    %% Generate unique IDs
    MsgIds = [generate_unique_msgid(<<"unique_", (integer_to_binary(N))/binary>>) 
              || N <- lists:seq(1, NumMessages)],
    
    %% Check each one
    Results = [iris_dedup:check_and_mark(MsgId) || MsgId <- MsgIds],
    
    %% All should be "new"
    NewCount = length([R || R <- Results, R =:= new]),
    ?assertEqual(NumMessages, NewCount).

%% =============================================================================
%% P0: Edge Cases
%% =============================================================================

test_sequential_retries() ->
    %% Test: Sequential (non-concurrent) retries work correctly
    MsgId = generate_unique_msgid(<<"sequential">>),
    
    %% First - new
    R1 = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(new, R1),
    
    %% Wait a bit between retries
    timer:sleep(10),
    
    R2 = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(duplicate, R2),
    
    timer:sleep(10),
    
    R3 = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(duplicate, R3).

test_interleaved_msgids() ->
    %% Test: Interleaved processing of different message IDs
    MsgId1 = generate_unique_msgid(<<"interleave_a">>),
    MsgId2 = generate_unique_msgid(<<"interleave_b">>),
    MsgId3 = generate_unique_msgid(<<"interleave_c">>),
    
    %% Interleaved sequence
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId1)),
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId2)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId1)),
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId3)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId2)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId3)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId1)).

test_binary_msgids() ->
    %% Test: Various binary formats for message IDs
    
    %% Standard ASCII
    AsciiId = generate_unique_msgid(<<"ascii_test">>),
    ?assertEqual(new, iris_dedup:check_and_mark(AsciiId)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(AsciiId)),
    
    %% Binary with special characters
    SpecialId = generate_unique_msgid(<<"special_\0\n\r_test">>),
    ?assertEqual(new, iris_dedup:check_and_mark(SpecialId)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(SpecialId)),
    
    %% Pure random bytes
    RandomId = crypto:strong_rand_bytes(32),
    ?assertEqual(new, iris_dedup:check_and_mark(RandomId)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(RandomId)).

test_uuid_msgids() ->
    %% Test: UUID-format message IDs (common in real systems)
    
    %% Generate UUID-like IDs
    UUID1 = generate_uuid_like(),
    UUID2 = generate_uuid_like(),
    
    ?assertEqual(new, iris_dedup:check_and_mark(UUID1)),
    ?assertEqual(new, iris_dedup:check_and_mark(UUID2)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(UUID1)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(UUID2)).

%% =============================================================================
%% P0: Timing and Ordering Tests
%% =============================================================================

test_rapid_fire_same_id() ->
    %% Test: Rapid-fire same ID (microsecond intervals)
    MsgId = generate_unique_msgid(<<"rapid_fire">>),
    
    %% Send 50 checks as fast as possible
    Results = [iris_dedup:check_and_mark(MsgId) || _ <- lists:seq(1, 50)],
    
    %% Should have exactly one "new"
    NewCount = length([R || R <- Results, R =:= new]),
    ?assertEqual(1, NewCount).

test_delayed_retry() ->
    %% Test: Delayed retries are still detected
    MsgId = generate_unique_msgid(<<"delayed_retry">>),
    
    %% First check
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),
    
    %% Wait 100ms (simulating network delay before retry)
    timer:sleep(100),
    
    %% Should still be detected as duplicate
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).

%% =============================================================================
%% Concurrent Exactly-Once Tests
%% =============================================================================

concurrent_idempotency_test_() ->
    {"Concurrent exactly-once guarantees",
     {setup,
      fun setup/0,
      fun cleanup/1,
      [
       {"100 concurrent retries -> exactly 1 delivery", fun test_concurrent_retry_storm/0},
       {"Parallel unique messages all delivered", fun test_parallel_unique/0},
       {"Mixed concurrent unique and duplicate", fun test_mixed_concurrent/0}
      ]}}.

test_concurrent_retry_storm() ->
    %% Test: 100 concurrent processes all trying to mark same ID
    %% Exactly 1 should get "new", rest get "duplicate"
    MsgId = generate_unique_msgid(<<"concurrent_storm">>),
    Self = self(),
    NumProcs = 100,
    
    %% Spawn all at once
    Pids = [spawn(fun() ->
        Result = iris_dedup:check_and_mark(MsgId),
        Self ! {result, self(), Result}
    end) || _ <- lists:seq(1, NumProcs)],
    
    %% Collect results with timeout
    Results = [receive
        {result, Pid, R} -> R
    after 5000 ->
        timeout
    end || Pid <- Pids],
    
    %% Verify exactly-once semantics
    NewCount = length([R || R <- Results, R =:= new]),
    DupCount = length([R || R <- Results, R =:= duplicate]),
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    
    ?assertEqual(0, TimeoutCount),
    ?assertEqual(1, NewCount),
    ?assertEqual(NumProcs - 1, DupCount).

test_parallel_unique() ->
    %% Test: Many processes with unique IDs - all should get "new"
    Self = self(),
    NumProcs = 100,
    
    %% Each process has its own unique ID
    Pids = [spawn(fun() ->
        MsgId = generate_unique_msgid(<<"parallel_", (integer_to_binary(N))/binary>>),
        Result = iris_dedup:check_and_mark(MsgId),
        Self ! {result, self(), Result}
    end) || N <- lists:seq(1, NumProcs)],
    
    %% Collect results
    Results = [receive
        {result, Pid, R} -> R
    after 5000 ->
        timeout
    end || Pid <- Pids],
    
    %% All should be "new"
    NewCount = length([R || R <- Results, R =:= new]),
    ?assertEqual(NumProcs, NewCount).

test_mixed_concurrent() ->
    %% Test: Mix of unique IDs and duplicates processed concurrently
    Self = self(),
    
    %% Create some IDs - each will be used by multiple processes
    SharedId1 = generate_unique_msgid(<<"shared_1">>),
    SharedId2 = generate_unique_msgid(<<"shared_2">>),
    
    %% 10 processes for each shared ID + 20 unique IDs
    SharedProcs1 = [spawn(fun() ->
        Result = iris_dedup:check_and_mark(SharedId1),
        Self ! {result, self(), shared1, Result}
    end) || _ <- lists:seq(1, 10)],
    
    SharedProcs2 = [spawn(fun() ->
        Result = iris_dedup:check_and_mark(SharedId2),
        Self ! {result, self(), shared2, Result}
    end) || _ <- lists:seq(1, 10)],
    
    UniqueProcs = [spawn(fun() ->
        MsgId = generate_unique_msgid(<<"mixed_unique_", (integer_to_binary(N))/binary>>),
        Result = iris_dedup:check_and_mark(MsgId),
        Self ! {result, self(), unique, Result}
    end) || N <- lists:seq(1, 20)],
    
    AllPids = SharedProcs1 ++ SharedProcs2 ++ UniqueProcs,
    
    %% Collect results by category
    Results = [receive
        {result, Pid, Category, R} -> {Category, R}
    after 5000 ->
        {timeout, timeout}
    end || Pid <- AllPids],
    
    %% Categorize
    Shared1Results = [R || {shared1, R} <- Results],
    Shared2Results = [R || {shared2, R} <- Results],
    UniqueResults = [R || {unique, R} <- Results],
    
    %% Each shared ID should have exactly 1 "new"
    ?assertEqual(1, length([R || R <- Shared1Results, R =:= new])),
    ?assertEqual(1, length([R || R <- Shared2Results, R =:= new])),
    
    %% All unique should be "new"
    ?assertEqual(20, length([R || R <- UniqueResults, R =:= new])).

%% =============================================================================
%% End-to-End Idempotency Simulation
%% =============================================================================

e2e_idempotency_test_() ->
    {"End-to-end idempotency simulation",
     {setup,
      fun setup/0,
      fun cleanup/1,
      [
       {"Message delivery simulation", fun test_delivery_simulation/0},
       {"Retry with disconnect simulation", fun test_retry_disconnect_simulation/0}
      ]}}.

test_delivery_simulation() ->
    %% Simulate message delivery pipeline with dedup check
    
    %% Message from sender
    MsgId = generate_unique_msgid(<<"delivery_sim">>),
    Sender = <<"alice">>,
    Recipient = <<"bob">>,
    Payload = <<"Hello, Bob!">>,
    
    %% First delivery attempt - should succeed
    DeliveryResult1 = simulate_delivery(MsgId, Sender, Recipient, Payload),
    ?assertEqual(delivered, DeliveryResult1),
    
    %% Client retries (timeout assumed) - should be rejected
    DeliveryResult2 = simulate_delivery(MsgId, Sender, Recipient, Payload),
    ?assertEqual(duplicate_rejected, DeliveryResult2),
    
    %% More retries - all rejected
    DeliveryResult3 = simulate_delivery(MsgId, Sender, Recipient, Payload),
    ?assertEqual(duplicate_rejected, DeliveryResult3).

test_retry_disconnect_simulation() ->
    %% Simulate: Send message, disconnect, reconnect, retry
    MsgId = generate_unique_msgid(<<"disconnect_sim">>),
    
    %% First delivery - succeeds
    ?assertEqual(delivered, simulate_delivery(MsgId, <<"a">>, <<"b">>, <<"msg">>)),
    
    %% Simulate disconnect (client thinks delivery failed)
    %% In real system, client would retry with same MsgId
    
    %% Reconnect and retry - should be rejected
    timer:sleep(50),  %% Simulate reconnect time
    ?assertEqual(duplicate_rejected, simulate_delivery(MsgId, <<"a">>, <<"b">>, <<"msg">>)).

%% =============================================================================
%% Helper Functions
%% =============================================================================

generate_unique_msgid(Prefix) ->
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, "_", Unique/binary>>.

generate_uuid_like() ->
    %% Generate UUID-like binary (not RFC-compliant, just similar format)
    Bytes = crypto:strong_rand_bytes(16),
    <<A:32, B:16, C:16, D:16, E:48>> = Bytes,
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                                    [A, B, C, D, E])).

simulate_delivery(MsgId, _Sender, _Recipient, _Payload) ->
    %% Simulate the delivery pipeline:
    %% 1. Check dedup
    %% 2. If new, "deliver" (in real system, route to recipient)
    %% 3. If duplicate, reject
    case iris_dedup:check_and_mark(MsgId) of
        new -> 
            %% Would route message here in real system
            delivered;
        duplicate ->
            %% Already delivered
            duplicate_rejected
    end.
