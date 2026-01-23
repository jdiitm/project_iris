-module(iris_router_pool_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_router_pool.erl
%% RFC Reference: NFR-2 (Throughput), NFR-3 (Latency)
%% =============================================================================

%% =============================================================================
%% Test Fixtures
%% =============================================================================

router_pool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Start pool with default config", fun test_start_default/0},
         {"Get pool stats", fun test_get_stats/0},
         {"Async routing", fun test_async_route/0},
         {"Pool resize", fun test_resize_pool/0},
         {"Health check", fun test_health_check/0},
         {"Batch routing", fun test_route_batch/0}
     ]}.

setup() ->
    %% Stop any existing pool
    case whereis(iris_router_pool) of
        undefined -> ok;
        Pid -> 
            exit(Pid, shutdown),
            timer:sleep(100)
    end,
    ok.

cleanup(_) ->
    case whereis(iris_router_pool) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    ok.

%% =============================================================================
%% Test Cases
%% =============================================================================

test_start_default() ->
    %% Start pool with default settings
    {ok, Pid} = iris_router_pool:start_link([{pool_size, 4}]),
    
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    
    %% Get stats to verify pool is running
    Stats = iris_router_pool:get_stats(),
    ?assertEqual(4, maps:get(pool_size, Stats)),
    ?assertEqual(4, maps:get(active_workers, Stats)),
    
    ok.

test_get_stats() ->
    %% Verify stats structure
    Stats = iris_router_pool:get_stats(),
    
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(pool_size, Stats)),
    ?assert(maps:is_key(active_workers, Stats)),
    ?assert(maps:is_key(overflow_workers, Stats)),
    ?assert(maps:is_key(total_routed, Stats)),
    ?assert(maps:is_key(total_errors, Stats)),
    ?assert(maps:is_key(strategy, Stats)),
    
    %% Default strategy should be round_robin
    ?assertEqual(round_robin, maps:get(strategy, Stats)),
    
    ok.

test_async_route() ->
    %% Async routing should return immediately
    Start = erlang:monotonic_time(millisecond),
    
    %% Send async message
    ok = iris_router_pool:async_route(<<"sender">>, <<"receiver">>, <<"test msg">>),
    
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    
    %% Should be very fast (non-blocking)
    ?assert(Elapsed < 100),
    
    %% Give time for message to be processed
    timer:sleep(100),
    
    %% Check stats show routing attempt
    Stats = iris_router_pool:get_stats(),
    Routed = maps:get(total_routed, Stats),
    Errors = maps:get(total_errors, Stats),
    
    %% Either routed or errored (user doesn't exist in test)
    ?assert((Routed + Errors) >= 1),
    
    ok.

test_resize_pool() ->
    %% Get initial size
    Stats1 = iris_router_pool:get_stats(),
    InitialSize = maps:get(active_workers, Stats1),
    
    %% Increase pool size
    NewSize = InitialSize + 2,
    ok = iris_router_pool:resize_pool(NewSize),
    
    %% Verify new size
    timer:sleep(100),  %% Give time for workers to start
    Stats2 = iris_router_pool:get_stats(),
    ?assertEqual(NewSize, maps:get(pool_size, Stats2)),
    ?assertEqual(NewSize, maps:get(active_workers, Stats2)),
    
    %% Decrease pool size
    ok = iris_router_pool:resize_pool(InitialSize),
    timer:sleep(100),
    
    Stats3 = iris_router_pool:get_stats(),
    ?assertEqual(InitialSize, maps:get(pool_size, Stats3)),
    
    ok.

test_health_check() ->
    %% Health should be healthy when all workers are alive
    Health = iris_router_pool:health_check(),
    ?assertEqual(healthy, Health),
    
    ok.

test_route_batch() ->
    %% Create batch of messages
    Messages = [
        {<<"user1">>, <<"msg1">>},
        {<<"user2">>, <<"msg2">>},
        {<<"user3">>, <<"msg3">>}
    ],
    
    %% Route batch
    {ok, BatchStats} = iris_router_pool:route_batch(<<"sender">>, Messages),
    
    ?assert(is_map(BatchStats)),
    ?assertEqual(3, maps:get(total, BatchStats)),
    
    %% Successes + failures should equal total
    Successes = maps:get(successes, BatchStats),
    Failures = maps:get(failures, BatchStats),
    ?assertEqual(3, Successes + Failures),
    
    ok.
