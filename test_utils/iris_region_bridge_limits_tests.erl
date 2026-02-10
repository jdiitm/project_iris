-module(iris_region_bridge_limits_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P0-2 (FM-1): Outbox Queue Limit Tests
%%
%% RFC-001 v4.0 Section 7.2 Outbox Queue Operational Parameters:
%% - Max size: 10,000 messages per destination region
%% - Overflow policy: NACK to sender with retry hint
%% - Persistence: fsync before ACK
%%
%% Tests verify:
%% 1. Queueing below max_queue_size succeeds
%% 2. Queueing at/above max_queue_size returns overflow error
%% 3. Overflow error includes retry_after hint
%% 4. Limit is per destination region
%% 5. Queue accepts after drain
%%
%% Pattern: follows iris_dedup_tests.erl for Mnesia + gen_server setup.
%% =============================================================================

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    %% pg is required by iris_region_bridge for group membership
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,

    %% Create the outbound table
    case mnesia:create_table(cross_region_outbound, [
        {attributes, [id, target_region, user_id, msg, status, attempts,
                      created_at, next_retry_at, last_error]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, cross_region_outbound}} -> ok
    end,

    %% Create dead letter table (required by bridge)
    case mnesia:create_table(cross_region_dead_letter, [
        {attributes, [id, target_region, user_id, msg, status, attempts,
                      created_at, next_retry_at, last_error]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, cross_region_dead_letter}} -> ok
    end,

    mnesia:wait_for_tables([cross_region_outbound, cross_region_dead_letter], 5000),

    case whereis(iris_region_bridge) of
        undefined ->
            {ok, Pid} = iris_region_bridge:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_region_bridge),
    catch mnesia:delete_table(cross_region_outbound),
    catch mnesia:delete_table(cross_region_dead_letter),
    application:stop(mnesia);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_region_bridge_limits_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Queue accepts under limit", fun test_queue_accepts_under_limit/0},
      {"Queue rejects at overflow", fun test_queue_rejects_at_overflow/0},
      {"Overflow error includes retry hint", fun test_queue_nack_includes_retry_hint/0},
      {"Queue limit is per region", fun test_queue_depth_per_region/0},
      {"Queue resumes after drain", fun test_queue_resumes_after_drain/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

%% Helper: bulk-insert records into Mnesia AND set the ETS depth counter.
%% The depth counter is maintained by iris_region_bridge for O(1) overflow checks.
bulk_insert_and_set_counter(Region, Count) ->
    lists:foreach(fun(I) ->
        MsgId = list_to_binary(io_lib:format("~s_msg_~p", [Region, I])),
        Now = erlang:system_time(millisecond),
        mnesia:dirty_write({cross_region_outbound,
            MsgId, Region, <<"user1">>, <<"data">>,
            pending, 0, Now, 0, undefined})
    end, lists:seq(1, Count)),
    %% G-3: Set the ETS depth counter to match the Mnesia state
    try ets:insert(iris_region_bridge_depth, {{queue_depth, Region}, Count})
    catch error:badarg -> ok end.

test_queue_accepts_under_limit() ->
    %% A single message should be accepted
    Result = iris_region_bridge:send_cross_region(
        <<"region_b">>, <<"user1">>, <<"hello">>),
    ?assertEqual(ok, Result).

test_queue_rejects_at_overflow() ->
    %% Fill queue to max_queue_size for a region, then verify next is rejected
    Region = <<"overflow_region">>,
    MaxSize = iris_region_bridge:get_max_queue_size(),
    bulk_insert_and_set_counter(Region, MaxSize),
    %% Now the next message should be rejected
    Result = iris_region_bridge:send_cross_region(Region, <<"user2">>, <<"rejected">>),
    ?assertMatch({error, {queue_overflow, _}}, Result).

test_queue_nack_includes_retry_hint() ->
    %% Overflow error should include retry_after hint
    Region = <<"nack_region">>,
    MaxSize = iris_region_bridge:get_max_queue_size(),
    bulk_insert_and_set_counter(Region, MaxSize),
    Result = iris_region_bridge:send_cross_region(Region, <<"user2">>, <<"test">>),
    ?assertMatch({error, {queue_overflow, #{retry_after := _}}}, Result).

test_queue_depth_per_region() ->
    %% Filling region_a should not block region_c
    RegionA = <<"full_region_a">>,
    RegionC = <<"empty_region_c">>,
    MaxSize = iris_region_bridge:get_max_queue_size(),
    bulk_insert_and_set_counter(RegionA, MaxSize),
    %% Region A full
    ResultA = iris_region_bridge:send_cross_region(RegionA, <<"u">>, <<"msg">>),
    ?assertMatch({error, {queue_overflow, _}}, ResultA),
    %% Region C still accepts
    ResultC = iris_region_bridge:send_cross_region(RegionC, <<"u">>, <<"msg">>),
    ?assertEqual(ok, ResultC).

test_queue_resumes_after_drain() ->
    %% After clearing a full queue, new messages should be accepted again
    Region = <<"drain_region">>,
    MaxSize = iris_region_bridge:get_max_queue_size(),
    bulk_insert_and_set_counter(Region, MaxSize),
    %% Verify full
    ?assertMatch({error, {queue_overflow, _}},
                 iris_region_bridge:send_cross_region(Region, <<"u">>, <<"x">>)),
    %% Clear all entries for this region AND reset counter
    lists:foreach(fun(I) ->
        MsgId = list_to_binary(io_lib:format("~s_msg_~p", [Region, I])),
        mnesia:dirty_delete(cross_region_outbound, MsgId)
    end, lists:seq(1, MaxSize)),
    try ets:insert(iris_region_bridge_depth, {{queue_depth, Region}, 0})
    catch error:badarg -> ok end,
    %% Now should accept
    Result = iris_region_bridge:send_cross_region(Region, <<"u">>, <<"accepted">>),
    ?assertEqual(ok, Result).
