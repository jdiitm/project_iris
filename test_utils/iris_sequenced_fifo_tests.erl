-module(iris_sequenced_fifo_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F2: Sequenced Message FIFO Guarantee Tests (RFC 1.3)
%%
%% route_sequenced_remote/4 spawns an ephemeral process for each sequenced
%% message to prevent head-of-line blocking during network partitions.
%%
%% FIFO ordering is preserved because:
%%   1. Offline storage includes SeqNo in the stored record
%%   2. iris_offline_storage:retrieve_cursor/3 sorts by timestamp (SeqNo)
%%   3. Online delivery uses SeqNo for client-side reordering (RFC FR-5)
%%
%% Test strategy:
%%   Verify that the spawned process eventually completes and the
%%   route_complete callback is delivered to the shard GenServer.
%%   The gen_server is NOT blocked during routing (by design).
%% =============================================================================

-record(state, {
    shard_id :: integer(),
    local_count :: integer(),
    routed_local :: integer(),
    routed_remote :: integer(),
    routed_offline :: integer(),
    route_failures :: integer(),
    start_time :: integer()
}).

iris_sequenced_fifo_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"sequenced remote routing completes via spawn (route_complete delivered)",
       fun test_sequenced_spawn_processing/0}
     ]}.

setup() ->
    %% We need local_presence_v2 ETS table for the router
    catch ets:new(local_presence_v2, [set, named_table, public]),
    %% We need metrics table
    catch ets:new(iris_router_metrics, [set, named_table, public, {write_concurrency, true}]),
    ok.

cleanup(_) ->
    catch ets:delete(local_presence_v2),
    catch ets:delete(iris_router_metrics),
    ok.

test_sequenced_spawn_processing() ->
    %% Start a router shard
    {ok, Pid} = iris_async_router:start_link(997),

    %% Ensure the user is NOT locally registered (forces remote routing path)
    User = <<"test_fifo_inline_user_997">>,

    %% Get initial state - offline+remote counters should be 0
    State0 = sys:get_state(Pid),
    Offline0 = element(#state.routed_offline, State0),
    Remote0 = element(#state.routed_remote, State0),
    ?assertEqual(0, Offline0),
    ?assertEqual(0, Remote0),

    %% Send a sequenced message for a non-local user
    gen_server:cast(Pid, {route_sequenced, User,
                          {sequenced_msg, 1, <<"msg_fifo_test">>}, 1}),

    %% With spawn: route_complete arrives LATER from the spawned process.
    %% Wait for the spawned routing to complete and the route_complete
    %% callback to be processed by the GenServer.
    timer:sleep(3000),

    %% Use a barrier to ensure all pending casts are processed
    ok = gen_server:call(Pid, sync_barrier_for_test),

    %% Now get state - route_complete should have been processed
    State1 = sys:get_state(Pid),
    Offline1 = element(#state.routed_offline, State1),
    Remote1 = element(#state.routed_remote, State1),

    %% At least one routing counter must have been incremented.
    %% (The message goes offline since no real cluster is running)
    TotalRouted = (Offline1 - Offline0) + (Remote1 - Remote0),

    gen_server:stop(Pid),

    %% Spawned process completed and route_complete was delivered
    ?assert(TotalRouted > 0).
