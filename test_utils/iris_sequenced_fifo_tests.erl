-module(iris_sequenced_fifo_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F2: Sequenced Message FIFO Guarantee Tests (RFC 1.3)
%%
%% route_sequenced_remote/4 spawns an ephemeral process for each sequenced
%% message. Two sequenced messages dispatched from the same shard GenServer
%% can race, violating FIFO ordering.
%%
%% Test strategy:
%%   With INLINE processing, gen_server:cast(self(), {route_complete, ...})
%%   is sent during handle_cast, so it's in the mailbox BEFORE any subsequent
%%   messages. After a gen_server:call barrier (which serializes in the
%%   mailbox after route_complete), the state reflects the route_complete update.
%%
%%   With SPAWN, the route_complete cast arrives LATER (from the spawned
%%   process), so a gen_server:call barrier returns BEFORE route_complete
%%   is processed, and the state is stale (counter = 0).
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
      {"sequenced remote routing completes synchronously (state updated inline)",
       fun test_sequenced_inline_processing/0}
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

test_sequenced_inline_processing() ->
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

    %% Synchronize using gen_server:call which sends a {'$gen_call', ...}
    %% message that is processed IN ORDER (not priority like system messages).
    %%
    %% With INLINE:
    %%   Mailbox after route_sequenced cast:
    %%     [{route_complete, ...}, {'$gen_call', barrier}]
    %%   Processing order: route_complete (counter++), then barrier (reply ok)
    %%   After barrier returns: counter has been incremented.
    %%
    %% With SPAWN:
    %%   Mailbox after route_sequenced cast:
    %%     [{'$gen_call', barrier}]  (route_complete hasn't been sent yet)
    %%   Processing order: barrier (reply ok)
    %%   After barrier returns: counter is still 0.
    ok = gen_server:call(Pid, sync_barrier_for_test),

    %% Now get state - route_complete has been processed (inline) or not (spawn)
    State1 = sys:get_state(Pid),
    Offline1 = element(#state.routed_offline, State1),
    Remote1 = element(#state.routed_remote, State1),

    %% At least one routing counter must have been incremented.
    %% (The message goes offline since no real cluster is running)
    TotalRouted = (Offline1 - Offline0) + (Remote1 - Remote0),

    gen_server:stop(Pid),

    %% With INLINE processing: TotalRouted >= 1 (PASS)
    %% With SPAWN processing: TotalRouted == 0 (FAIL)
    ?assert(TotalRouted > 0).
