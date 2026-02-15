-module(iris_router_overflow_nack_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% G-1: Silent Message Loss Prevention Tests (RFC Section 7.2 / NFR-8)
%%
%% The current iris_async_router:route/3 always returns `ok` (fire-and-forget
%% via gen_server:cast). If the outbox queue is full, the message is dropped
%% but the caller believes it was accepted. This violates NFR-8 (Zero data
%% loss) and the At-Least-Once guarantee.
%%
%%      {error, queue_overflow} when the outbox is saturated.
%% =============================================================================

-define(DEPTH_ETS, iris_region_bridge_depth).

iris_router_overflow_nack_test_() ->
    [
     {"route/3 returns {error, queue_overflow} when outbox is saturated",
      fun test_route_nacks_on_overflow/0}
    ].

test_route_nacks_on_overflow() ->
    %% Setup: Create the depth counter ETS table and saturate a region
    catch ets:delete(?DEPTH_ETS),
    ets:new(?DEPTH_ETS, [set, named_table, public, {write_concurrency, true}]),
    
    %% Set the depth counter to exceed the max (10000)
    MaxQueue = iris_region_bridge:get_max_queue_size(),
    ets:insert(?DEPTH_ETS, {{queue_depth, <<"default">>}, MaxQueue + 1}),
    
    %% Register a mock iris_region_bridge process so the pre-flight check
    %% detects we're in multi-region mode
    Self = self(),
    MockPid = spawn(fun() -> 
        register(iris_region_bridge, self()),
        Self ! registered,
        receive stop -> ok end 
    end),
    receive registered -> ok after 1000 -> error(timeout) end,
    
    %% The route call should return {error, queue_overflow} instead of ok
    Result = iris_async_router:route(<<"user1">>, <<"hello">>, #{}),
    ?assertEqual({error, queue_overflow}, Result),
    
    %% Cleanup
    MockPid ! stop,
    ets:delete(?DEPTH_ETS).
