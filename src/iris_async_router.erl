-module(iris_async_router).
-behaviour(gen_server).

%% =============================================================================
%% Planetary Scale Async Router (Partitioned)
%% =============================================================================
%% Key Design Principles:
%% 1. Partitioned Worker Pool (auto-tuned) to saturate Multi-Core CPUs.
%% 2. Consistent Hashing via phash2 for sticky user routing.
%% 3. Stats Aggregation across the pool.
%%
%% Silent Loss Prevention
%% - All routing failures MUST fall back to offline storage
%% - Every message is tracked (route_attempt → route_success | route_offline)
%% - Zero silent message drops
%%
%% ORDERING CONTRACT:
%%
%% This module provides TWO routing paths with different ordering guarantees:
%%
%% 1. UNSEQUENCED (handle_cast {route, User, Msg, MsgId}):
%%    - route_to_remote/4 spawns a separate process per message
%%    - Delivery is guaranteed but ORDER IS NOT
%%    - Use for: presence updates, typing indicators, fire-and-forget
%%
%% 2. SEQUENCED (handle_cast {route_sequenced, User, Msg, SeqNo}):
%%    - route_sequenced_remote/4 processes INLINE (no spawn)
%%    - FIFO ordering IS guaranteed (RFC FR-5)
%%    - Use for: chat messages, state mutations, anything order-sensitive
%%
%% INVARIANT: Never route order-sensitive messages through path (1).
%% =============================================================================

-export([start_link/1, route/2, route/3, route_async/2, route_sequenced/3]).
-export([register_local/2, unregister_local/1]).
-export([get_local_count/0, get_stats/0, get_pool_size/0]).
-export([route_via_outbox_or_offline/3]).  %% RFC Section 7.2: Outbox-aware fallback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOCAL_PRESENCE, local_presence_v2).

%% Pool size configuration
%% Auto-tune based on scheduler count instead of hardcoded value
-define(MIN_POOL_SIZE, 4).
-define(MAX_POOL_SIZE, 128).
-define(DEFAULT_POOL_SIZE, 8).  %% Fallback if auto-tune fails

%% Metrics ETS table for delivery tracking
-define(METRICS_ETS, iris_router_metrics).

-record(state, {
    shard_id :: integer(),
    local_count = 0 :: integer(),
    routed_local = 0 :: integer(),
    routed_remote = 0 :: integer(),
    routed_offline = 0 :: integer(),  %% Track offline fallback
    route_failures = 0 :: integer(),   %% Track failures
    start_time :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

%% Start a specific shard (Called by Supervisor)
start_link(ShardId) ->
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    gen_server:start_link({local, Name}, ?MODULE, [ShardId], []).

%% @doc Get the current pool size (auto-tuned or configured)
%% This is called by supervisors to determine how many workers to start
-spec get_pool_size() -> pos_integer().
get_pool_size() ->
    case application:get_env(iris_edge, router_pool_size) of
        {ok, Size} when is_integer(Size), Size > 0 ->
            %% Explicitly configured - use it
            Size;
        _ ->
            %% Auto-tune: Use 75% of schedulers, bounded by min/max
            Schedulers = erlang:system_info(schedulers_online),
            TargetSize = max(?MIN_POOL_SIZE, (Schedulers * 3) div 4),
            min(TargetSize, ?MAX_POOL_SIZE)
    end.

%% Route a message - Sharded by User ID
-spec route(binary(), binary()) -> ok | {ok, offline}.
route(User, Msg) ->
    route(User, Msg, #{}).

%% @doc Route with options (including msg_id for tracking)
%% Returns {error, queue_overflow} if outbox is saturated (NACK).
%% This prevents silent message loss per RFC 7.2 / NFR-8.
-spec route(binary(), binary(), map()) -> ok | {ok, offline} | {error, queue_overflow}.
route(User, Msg, Opts) ->
    %% Synchronous pre-flight overflow check before async cast.
    %% Uses the O(1) ETS counter -- cheap on the hot path.
    case preflight_overflow_check(User) of
        ok ->
            %% Record attempt
            incr_metric(route_attempt),
            
            PoolSize = get_pool_size(),
            ShardId = (erlang:phash2(User, PoolSize) + 1),
            Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
            
            %% Include msg_id if provided for tracking
            MsgId = maps:get(msg_id, Opts, undefined),
            gen_server:cast(Name, {route, User, Msg, MsgId}),
            ok;
        {error, queue_overflow} = Err ->
            incr_metric(route_failure),
            Err
    end.

-spec route_async(binary(), binary()) -> ok.
route_async(User, Msg) ->
    route(User, Msg).

%% =============================================================================
%% RFC FR-5: Sequenced routing for FIFO ordering
%% =============================================================================
%% Route with client-provided sequence number for guaranteed ordering.
%% The sequence number is used as the storage timestamp for offline messages.
-spec route_sequenced(binary(), term(), non_neg_integer()) -> ok.
route_sequenced(User, Msg, SeqNo) ->
    incr_metric(route_attempt),
    
    PoolSize = get_pool_size(),
    ShardId = (erlang:phash2(User, PoolSize) + 1),
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    
    gen_server:cast(Name, {route_sequenced, User, Msg, SeqNo}),
    ok.

%% Register is global (ETS is public), but we can track count locally if needed.
%% For now, we don't track per-shard local count strictly, as ETS is the source of truth.
register_local(User, Pid) ->
    true = ets:insert(?LOCAL_PRESENCE, {User, Pid}),
    ok.

unregister_local(User) ->
    true = ets:delete(?LOCAL_PRESENCE, User),
    ok.

get_local_count() ->
    ets:info(?LOCAL_PRESENCE, size).

%% Aggregate Stats from all shards
get_stats() ->
    PoolSize = get_pool_size(),
    Shards = lists:seq(1, PoolSize),
    StatsList = [call_shard_stats(I) || I <- Shards],
    BaseStats = aggregate_stats(StatsList),
    
    %% Include global metrics
    GlobalMetrics = get_global_metrics(),
    maps:merge(BaseStats, GlobalMetrics).

call_shard_stats(ShardId) ->
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    try gen_server:call(Name, get_stats_local, 100)
    catch Class:Reason ->
        logger:warning("iris_async_router:call_shard_stats catch-all: ~p:~p", [Class, Reason]),
        #{routed_local => 0, routed_remote => 0, routed_offline => 0, route_failures => 0}
    end.

aggregate_stats(StatsList) ->
    lists:foldl(fun(S, Acc) ->
        #{
            routed_local => maps:get(routed_local, S, 0) + maps:get(routed_local, Acc, 0),
            routed_remote => maps:get(routed_remote, S, 0) + maps:get(routed_remote, Acc, 0),
            routed_offline => maps:get(routed_offline, S, 0) + maps:get(routed_offline, Acc, 0),
            route_failures => maps:get(route_failures, S, 0) + maps:get(route_failures, Acc, 0),
            local_users => get_local_count(), %% Global ETS count
            uptime_seconds => maps:get(uptime_seconds, S, 0) %% Just take one, roughly same
        }
    end, #{routed_local => 0, routed_remote => 0, routed_offline => 0, route_failures => 0}, StatsList).


%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([ShardId]) ->
    %% FIXED: Removed duplicate local_presence_v2 ETS table creation
    %% The supervisor (iris_edge_sup) creates and owns local_presence_v2
    %% This ensures the table survives worker crashes
    
    %% Only Shard 1 creates the metrics table
    if ShardId =:= 1 ->
        case ets:info(?METRICS_ETS) of
            undefined ->
                ets:new(?METRICS_ETS, [set, named_table, public, {write_concurrency, true}]),
                ets:insert(?METRICS_ETS, {route_attempt, 0}),
                ets:insert(?METRICS_ETS, {route_success, 0}),
                ets:insert(?METRICS_ETS, {route_offline, 0}),
                ets:insert(?METRICS_ETS, {route_failure, 0});
            _ -> ok
        end;
       true -> ok
    end,
    {ok, #state{shard_id = ShardId, start_time = erlang:system_time(second)}}.

handle_call(get_stats_local, _From, State) ->
    Reply = #{
        shard_id => State#state.shard_id,
        routed_local => State#state.routed_local,
        routed_remote => State#state.routed_remote,
        routed_offline => State#state.routed_offline,
        route_failures => State#state.route_failures,
        uptime_seconds => erlang:system_time(second) - State#state.start_time
    },
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Handle route with MsgId for tracking
handle_cast({route, User, Msg, MsgId}, State) ->
    %% CRITICAL PATH - Sharded
    case ets:lookup(?LOCAL_PRESENCE, User) of
        [{User, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    %% RFC 1.2: Dedup + unwrap {idempotent_msg, IdKey, Payload}
                    %% before local delivery. Dedup MUST only run on the confirmed
                    %% delivery path — marking the key before we know the path would
                    %% cause the offline storage fallback to drop the message as
                    %% "duplicate" even though it was never actually delivered.
                    case dedup_and_deliver_local(User, Msg, Pid) of
                        delivered ->
                            incr_metric(route_success),
                            {noreply, State#state{routed_local = State#state.routed_local + 1}};
                        duplicate ->
                            iris_metrics:dedup_hit(),
                            {noreply, State}
                    end;
                false ->
                    %% Stale entry — pass original Msg (keeps wrapper for offline dedup)
                    ets:delete(?LOCAL_PRESENCE, User),
                    route_to_remote(User, Msg, MsgId, State)
            end;
        [] ->
            route_to_remote(User, Msg, MsgId, State)
    end;

%% Backwards compatibility for old route format
handle_cast({route, User, Msg}, State) ->
    handle_cast({route, User, Msg, undefined}, State);

%% Handle completion callback from spawned routing tasks
handle_cast({route_complete, {success, remote}}, State) ->
    incr_metric(route_success),
    {noreply, State#state{routed_remote = State#state.routed_remote + 1}};

handle_cast({route_complete, {success, offline}}, State) ->
    incr_metric(route_offline),
    {noreply, State#state{routed_offline = State#state.routed_offline + 1}};

handle_cast({route_complete, {failure, _Reason}}, State) ->
    incr_metric(route_offline),
    {noreply, State#state{routed_offline = State#state.routed_offline + 1,
                          route_failures = State#state.route_failures + 1}};

%% =============================================================================
%% RFC FR-5: Sequenced routing for FIFO ordering
%% =============================================================================
handle_cast({route_sequenced, User, {sequenced_msg, SeqNo, Msg}, _SeqNo}, State) ->
    case ets:lookup(?LOCAL_PRESENCE, User) of
        [{User, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid ! {deliver_msg, Msg},
                    incr_metric(route_success),
                    {noreply, State#state{routed_local = State#state.routed_local + 1}};
                false ->
                    %% Stale entry - try remote routing first
                    ets:delete(?LOCAL_PRESENCE, User),
                    route_sequenced_remote(User, Msg, SeqNo, State)
            end;
        [] ->
            %% User not on THIS edge - try remote routing (cross-edge delivery)
            %% FIX: Was storing offline directly; now routes to remote edges/cores
            route_sequenced_remote(User, Msg, SeqNo, State)
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal
%% =============================================================================

route_to_remote(User, Msg, MsgId, State) ->
    %% Spawn ephemeral task to avoid HOL blocking.
    %% The blocking rpc:call was causing shard GenServers to stall for up to
    %% 2 seconds per slow cross-region lookup, creating head-of-line blocking.
    %% Now: GenServer returns immediately, spawned task handles remote routing.
    Self = self(),
    spawn(fun() ->
        Result = do_remote_route(User, Msg, MsgId),
        gen_server:cast(Self, {route_complete, Result})
    end),
    {noreply, State}.

%% FIX: Sequenced remote routing - routes across edges/cores with sequence number
%% This was missing, causing cross-edge messages to be stored offline directly
%% Process synchronously (NO spawn) to preserve FIFO ordering (RFC 1.3).
%% The shard GenServer already serializes casts, so inline processing guarantees
%% that seq N completes before seq N+1 starts. The RPC timeout cost (~5s max)
%% is acceptable for sequenced messages where correctness > throughput.
route_sequenced_remote(User, Msg, SeqNo, State) ->
    Result = do_sequenced_remote_route(User, Msg, SeqNo),
    gen_server:cast(self(), {route_complete, Result}),
    {noreply, State}.

do_sequenced_remote_route(User, Msg, SeqNo) ->
    %% Re-wrap message with SeqNo for offline storage path
    %% The handle_cast unwraps {sequenced_msg, SeqNo, Msg} for local delivery,
    %% but we need the wrapper for store_offline_via_node to extract SeqNo
    WrappedMsg = {sequenced_msg, SeqNo, Msg},
    case get_shard_nodes(User) of
        [] ->
            %% No shard nodes - query all cores for user
            do_sequenced_route_fallback(User, WrappedMsg, SeqNo);
        [Primary | Fallbacks] ->
            case route_to_node(Primary, User, WrappedMsg, Fallbacks) of
                ok ->
                    {success, remote};
                {ok, offline} ->
                    {success, offline};
                {error, _Reason} ->
                    %% Fallback - store offline with sequence number
                    store_offline_sequenced_sync(User, Msg, SeqNo),
                    {success, offline}
            end
    end.

do_sequenced_route_fallback(User, Msg, SeqNo) ->
    %% Query all connected cores to find user
    AllCores = get_discovery_nodes(),
    case find_user_across_cores(AllCores, User) of
        {ok, UserPid} when is_pid(UserPid) ->
            %% RFC FR-5: Unwrap for online delivery (wrappers are internal)
            DeliverMsg = case Msg of
                {sequenced_msg, _S, RealMsg} -> RealMsg;
                {idempotent_msg, _IdKey, RealMsg} -> RealMsg;
                _ -> Msg
            end,
            UserPid ! {deliver_msg, DeliverMsg},
            {success, remote};
        not_found ->
            %% User not online - store offline
            %% Msg may be wrapped as {sequenced_msg, SeqNo, RealMsg} - extract RealMsg for storage
            RealMsg = case Msg of
                {sequenced_msg, _S, RM} -> RM;
                _ -> Msg
            end,
            store_offline_sequenced_sync(User, RealMsg, SeqNo),
            {success, offline}
    end.

store_offline_sequenced_sync(User, Msg, SeqNo) ->
    CoreNode = get_any_core_node(),
    case CoreNode of
        undefined ->
            logger:error("No core node available for offline storage of ~p", [User]),
            {error, no_core_available};
        Node ->
            %% Store with SeqNo as timestamp for ordering
            Result = rpc:call(Node, iris_core, store_offline_durable, [User, {SeqNo, Msg}], 5000),
            case Result of
                ok ->
                    logger:debug("Stored offline message for ~p on ~p (seq=~p)", [User, Node, SeqNo]),
                    ok;
                {error, Reason} ->
                    logger:error("Failed to store offline message for ~p: ~p", [User, Reason]),
                    {error, Reason};
                {badrpc, Reason} ->
                    logger:error("RPC failed storing offline message for ~p on ~p: ~p", [User, Node, Reason]),
                    %% Try failover to another core
                    store_offline_sequenced_failover(User, Msg, SeqNo, Node);
                Other ->
                    logger:warning("Unexpected result storing offline for ~p: ~p", [User, Other]),
                    ok  %% Assume success for non-error responses
            end
    end.

%% Failover: try other core nodes if primary fails
store_offline_sequenced_failover(User, Msg, SeqNo, FailedNode) ->
    OtherNodes = [N || N <- get_discovery_nodes(), N =/= FailedNode],
    case OtherNodes of
        [] ->
            logger:error("All core nodes failed for offline storage of ~p", [User]),
            {error, all_cores_failed};
        [Node | _] ->
            logger:info("Trying failover core ~p for offline storage of ~p", [Node, User]),
            case rpc:call(Node, iris_core, store_offline_durable, [User, {SeqNo, Msg}], 5000) of
                ok -> ok;
                {error, R} -> {error, R};
                {badrpc, R} -> 
                    logger:error("Failover core ~p also failed: ~p", [Node, R]),
                    {error, {badrpc, R}};
                _ -> ok
            end
    end.

get_any_core_node() ->
    %% FIX: Filter out unreachable nodes to avoid RPC timeouts during partitions
    %% This prevents message loss when some cores are partitioned
    case get_discovery_nodes() of
        [] -> undefined;
        Nodes -> 
            %% Find first reachable node (quick ping check)
            find_first_reachable(Nodes)
    end.

%% Find first node that responds to ping (filters out partitioned nodes)
find_first_reachable([]) -> undefined;
find_first_reachable([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong -> Node;
        pang -> find_first_reachable(Rest)
    end.

%% Extracted blocking logic into separate function
%% This runs in a spawned process, not blocking the shard GenServer
do_remote_route(User, Msg, MsgId) ->
    %% Check destination rate limit before routing
    %% This protects hot recipients (celebrities) from being overwhelmed
    case check_destination_rate(User) of
        allow ->
            do_remote_route_inner(User, Msg, MsgId);
        {deny, RetryAfter} ->
            %% Destination is rate limited - store offline for later delivery
            logger:warning("Destination ~p rate limited (msg_id=~p), storing offline. Retry in ~pms",
                          [User, MsgId, RetryAfter]),
            route_via_outbox_or_offline(User, Msg, MsgId),
            {success, offline}  %% Not a failure - graceful degradation
    end.

%% Check destination rate limit
check_destination_rate(User) ->
    case whereis(iris_rate_limiter) of
        undefined -> allow;  %% Rate limiter not running
        _ -> 
            try iris_rate_limiter:check_destination(User)
            catch Class:Reason ->
                logger:error("iris_async_router:check_destination_rate FAIL-OPEN for ~p: ~p:~p", [User, Class, Reason]),
                allow
            end
    end.

do_remote_route_inner(User, Msg, MsgId) ->
    case get_shard_nodes(User) of
        [] ->
            %% No shard nodes - try legacy routing (fire-and-forget)
            do_legacy_route(User, Msg, MsgId);
        [Primary | Fallbacks] ->
            %% Use circuit breaker with fallback nodes
            case route_to_node(Primary, User, Msg, Fallbacks) of
                ok ->
                    {success, remote};
                {ok, offline} ->
                    {success, offline};
                {error, Reason} ->
                    %% Guaranteed fallback — use outbox queue if cross-region
                    logger:warning("Route failed for user ~p (msg_id=~p): ~p, storing offline",
                                   [User, MsgId, Reason]),
                    route_via_outbox_or_offline(User, Msg, MsgId),
                    {failure, Reason}
            end
    end.

%% Legacy routing extracted for spawned task
do_legacy_route(User, Msg, MsgId) ->
    Members = pg:get_members(iris_shards),
    case Members of
        [] ->
            %% No cluster members — use outbox queue if cross-region
            route_via_outbox_or_offline(User, Msg, MsgId),
            {success, offline};
        [TargetPid | _] ->
            TargetPid ! {route_remote, User, Msg},
            {success, remote}
    end.

%% Get nodes for user's shard
get_shard_nodes(User) ->
    case whereis(iris_shard) of
        undefined ->
            %% Shard module not running - use discovery
            get_discovery_nodes();
        _ ->
            ShardId = iris_shard:get_shard(User),
            case iris_shard:get_shard_nodes(ShardId) of
                [] -> get_discovery_nodes();
                Nodes -> Nodes
            end
    end.

%% Fallback to discovery service
%% Include ALL core nodes across ALL regions for cross-region routing
get_discovery_nodes() ->
    LocalNodes = case whereis(iris_discovery) of
        undefined ->
            discover_via_pg_or_connected();
        _ ->
            case iris_discovery:get_nodes(iris_core) of
                [] ->
                    %% Discovery running but no cores registered yet; fall back
                    discover_via_pg_or_connected();
                Nodes ->
                    Nodes
            end
    end,
    
    %% Also include cross-region cores for global user lookup
    CrossRegionNodes = get_all_region_cores(),
    lists:usort(LocalNodes ++ CrossRegionNodes).

%% Discover core nodes via pg groups or Erlang connected nodes
discover_via_pg_or_connected() ->
    case pg:get_members(iris_shards) of
        [] ->
            %% Include hidden nodes (edge nodes run with -hidden flag)
            AllNodes = nodes(connected),
            [node() | AllNodes];
        Pids -> [node(P) || P <- Pids]
    end.

%% Get all core nodes across all configured regions
get_all_region_cores() ->
    case whereis(iris_region_router) of
        undefined -> [];
        _ ->
            try
                Regions = iris_region_router:get_all_regions(),
                lists:flatmap(fun(Region) ->
                    case iris_region_router:get_region_endpoint(Region) of
                        {ok, Nodes} -> Nodes;
                        _ -> []
                    end
                end, Regions)
            catch Class:Reason ->
                logger:warning("iris_async_router:get_region_nodes catch-all: ~p:~p", [Class, Reason]),
                []
            end
    end.

%% Route using circuit breaker with fallback
%% FIXED: Query ALL cores to find user (Mnesia not replicated across regions)
route_to_node(Node, User, Msg, Fallbacks) ->
    AllCores = [Node | Fallbacks],
    case find_user_across_cores(AllCores, User) of
        {ok, UserPid} when is_pid(UserPid) ->
            %% User found ONLINE - deliver directly
            %% FIX: Unwrap sequenced/idempotent messages for delivery (wrappers are internal)
            DeliverMsg = case Msg of
                {sequenced_msg, _SeqNo, RealMsg} -> RealMsg;
                {idempotent_msg, _IdKey, RealMsg} -> RealMsg;
                _ -> Msg
            end,
            UserPid ! {deliver_msg, DeliverMsg},
            ok;
        not_found ->
            %% User not online on any core - store offline
            case store_offline_via_node(Node, User, Msg, Fallbacks) of
                ok -> {ok, offline};
                {error, Reason} -> {error, Reason}
            end
    end.

%% Query all cores to find user (needed when Mnesia not replicated)
%% FIX: Use net_adm:ping to filter out partitioned nodes before RPC
find_user_across_cores([], _User) ->
    not_found;
find_user_across_cores([Core | Rest], User) ->
    %% Quick connectivity check - skip unreachable nodes
    case net_adm:ping(Core) of
        pang ->
            %% Node unreachable (partitioned) - skip
            find_user_across_cores(Rest, User);
        pong ->
            case rpc:call(Core, iris_core, lookup_user, [User], 2000) of
                {ok, _Node, UserPid} when is_pid(UserPid) ->
                    {ok, UserPid};
                {error, not_found} ->
                    find_user_across_cores(Rest, User);
                {badrpc, _} ->
                    find_user_across_cores(Rest, User)
            end
    end.

store_offline_via_node(Node, User, Msg, Fallbacks) ->
    %% Use store_offline_durable for RPO=0 guarantee
    %% Unwrap sequenced messages to pass SeqNo for FIFO ordering
    StorableMsg = case Msg of
        {sequenced_msg, SeqNo, RealMsg} when is_integer(SeqNo) ->
            %% Pass as {SeqNo, RealMsg} tuple so iris_core uses SeqNo as timestamp
            {SeqNo, RealMsg};
        _ ->
            Msg
    end,
    case whereis(iris_circuit_breaker) of
        undefined ->
            case rpc:call(Node, iris_core, store_offline_durable, [User, StorableMsg], 5000) of
                {badrpc, _} -> try_route_fallbacks(Fallbacks, User, Msg);
                ok -> ok;
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            case iris_circuit_breaker:call_with_fallback(
                    Node, iris_core, store_offline_durable, [User, StorableMsg], Fallbacks) of
                {error, circuit_open} -> try_route_fallbacks(Fallbacks, User, Msg);
                {badrpc, _} -> try_route_fallbacks(Fallbacks, User, Msg);
                ok -> ok;
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

try_route_fallbacks([], _User, _Msg) ->
    {error, no_available_nodes};
try_route_fallbacks([Node | Rest], User, Msg) ->
    %% Try to lookup and deliver, or store offline
    case rpc:call(Node, iris_core, lookup_user, [User], 5000) of
        {ok, _UserNode, UserPid} when is_pid(UserPid) ->
            %% RFC FR-5: Unwrap sequenced/idempotent messages for delivery
            DeliverMsg = case Msg of
                {sequenced_msg, _SeqNo, RealMsg} -> RealMsg;
                {idempotent_msg, _IdKey, RealMsg} -> RealMsg;
                _ -> Msg
            end,
            UserPid ! {deliver_msg, DeliverMsg},
            ok;
        {error, not_found} ->
            %% Use store_offline_durable for RPO=0 guarantee
            %% Unwrap sequenced messages to pass SeqNo for FIFO ordering
            StorableMsg = case Msg of
                {sequenced_msg, SeqNo, RealMsg} when is_integer(SeqNo) ->
                    {SeqNo, RealMsg};
                _ ->
                    Msg
            end,
            case rpc:call(Node, iris_core, store_offline_durable, [User, StorableMsg], 5000) of
                {badrpc, _} -> try_route_fallbacks(Rest, User, Msg);
                ok -> ok;
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {badrpc, _} -> 
            try_route_fallbacks(Rest, User, Msg)
    end.

%% =============================================================================
%% RFC Section 7.2: Outbox-aware fallback routing
%% =============================================================================
%% When routing fails, this function decides WHERE to store the message:
%% - If iris_region_bridge is running (multi-region mode), delegate to the
%%   region bridge which enforces 10k/7d outbox queue controls per RFC 7.2.
%% - Otherwise (single-region mode), fall back to store_offline_guaranteed
%%   for best-effort local storage.
%%
%% This prevents cross-region partition traffic from filling up the generic
%% offline message store, which has no per-region overflow or TTL controls.
%% =============================================================================
-spec route_via_outbox_or_offline(binary(), binary(), binary() | undefined) -> ok | {error, term()}.
route_via_outbox_or_offline(User, Msg, MsgId) ->
    case whereis(iris_region_bridge) of
        undefined ->
            %% No region bridge running — single-region mode.
            %% Use generic offline storage (existing behavior).
            store_offline_guaranteed(User, Msg, MsgId);
        _Pid ->
            %% Region bridge is running — multi-region mode.
            %% Determine target region and delegate to the bridge,
            %% which enforces 10k per-region limit and 7-day TTL.
            TargetRegion = get_target_region(User),
            case iris_region_bridge:send_cross_region(TargetRegion, User, Msg) of
                ok ->
                    ok;
                {error, {queue_overflow, _Details}} ->
                    %% RFC 7.2: NACK on overflow — do NOT fall through to
                    %% unbounded offline storage. Propagate backpressure.
                    logger:warning("Outbox overflow for user ~p in region ~s (msg_id=~p)",
                                   [User, TargetRegion, MsgId]),
                    {error, queue_overflow};
                {error, Reason} ->
                    %% Bridge failed for other reasons — fall back to offline
                    logger:warning("Region bridge failed for ~p: ~p, falling back to offline",
                                   [User, Reason]),
                    store_offline_guaranteed(User, Msg, MsgId)
            end
    end.

%% Synchronous pre-flight check for outbox overflow.
%% If iris_region_bridge is running (multi-region mode) and the target
%% region's queue is at capacity, return {error, queue_overflow} immediately.
%% The caller can then NACK the client instead of false-ACKing.
preflight_overflow_check(User) ->
    case whereis(iris_region_bridge) of
        undefined ->
            %% Single-region mode -- no outbox queue to overflow
            ok;
        _Pid ->
            TargetRegion = get_target_region(User),
            MaxQueue = iris_region_bridge:get_max_queue_size(),
            Depth = iris_region_bridge:get_queue_depth_fast(TargetRegion),
            case Depth >= MaxQueue of
                true ->
                    logger:warning("Pre-flight NACK for ~p: queue depth ~p >= max ~p",
                                   [User, Depth, MaxQueue]),
                    {error, queue_overflow};
                false ->
                    ok
            end
    end.

%% Determine target region for a user via iris_region_router if available,
%% otherwise use a default region identifier.
get_target_region(User) ->
    case whereis(iris_region_router) of
        undefined ->
            <<"default">>;
        _Pid ->
            try iris_region_router:get_home_region(User)
            catch Class:Reason ->
                logger:warning("iris_async_router:get_user_region catch-all: ~p:~p", [Class, Reason]),
                <<"default">>
            end
    end.

%% Guaranteed offline storage - NEVER returns error
%% If all nodes fail, store locally and queue for later delivery
store_offline_guaranteed(User, Msg, MsgId) ->
    %% Try remote storage first
    Nodes = get_discovery_nodes(),
    case store_offline_any_node(Nodes, User, Msg) of
        ok ->
            ok;
        {error, _Reason} ->
            %% Last resort - store locally in Mnesia
            %% This ensures the message is NEVER silently dropped
            logger:warning("All remote offline storage failed for ~p, storing locally", [User]),
            store_offline_local(User, Msg, MsgId)
    end.

store_offline_any_node([], _User, _Msg) ->
    {error, all_nodes_failed};
store_offline_any_node([Node | Rest], User, Msg) ->
    case rpc:call(Node, iris_core, store_offline_durable, [User, Msg], 5000) of
        ok -> ok;
        {ok, _} -> ok;
        _ -> store_offline_any_node(Rest, User, Msg)
    end.

%% =============================================================================
%% Local fallback storage
store_offline_local(User, Msg, MsgId) ->
    %% Store in local Mnesia (will be synced when cluster is healthy)
    try
        case whereis(iris_store) of
            undefined ->
                %% Fallback: use iris_core directly
                iris_core:store_offline(User, Msg);
            _ ->
                Key = {User, erlang:system_time(microsecond), MsgId},
                iris_store:put(offline_msg_local, Key, Msg, #{durability => guaranteed})
        end,
        ok
    catch
        Class:Reason ->
            %% CRITICAL: This should never happen - log for investigation
            logger:error("CRITICAL: Failed to store offline locally: ~p:~p for user ~p",
                        [Class, Reason, User]),
            incr_metric(route_failure),
            {error, local_storage_failed}
    end.

%% =============================================================================
%% RFC 1.2: Idempotent message dedup + unwrap for online delivery
%% =============================================================================
%% For {idempotent_msg, IdKey, Payload}, check dedup before delivery.
%% The idempotent wrapper is an internal routing concern — recipients receive
%% only the inner Payload. Without this, online users bypass dedup entirely
%% (dedup previously only ran in the offline storage path in iris_core).
%%
%% IMPORTANT: This MUST only be called on the confirmed-local-delivery path.
%% Marking the key before confirming the delivery path would cause the offline
%% fallback to drop the message as "duplicate" even though it was never delivered.

-spec dedup_and_deliver_local(binary(), term(), pid()) -> delivered | duplicate.
dedup_and_deliver_local(User, {idempotent_msg, IdKey, Payload}, Pid) ->
    DedupKey = <<User/binary, ":", IdKey/binary>>,
    case edge_dedup_check(DedupKey) of
        duplicate ->
            logger:debug("Dedup: idempotency_key duplicate for online user ~p", [User]),
            duplicate;
        new ->
            Pid ! {deliver_msg, Payload},
            delivered
    end;
dedup_and_deliver_local(_User, Msg, Pid) ->
    Pid ! {deliver_msg, Msg},
    delivered.

%% @doc Atomic check-and-mark for idempotency dedup.
%% Tries iris_dedup (full 3-tier: ETS + bloom + Mnesia) on core nodes.
%% Falls back to iris_edge_dedup (ETS-only hot tier) on edge nodes where
%% iris_dedup is not running.  ets:insert_new/2 is atomic — no TOCTOU race.
-spec edge_dedup_check(binary()) -> new | duplicate.
edge_dedup_check(DedupKey) ->
    case ets:info(iris_dedup_seen, name) of
        iris_dedup_seen ->
            %% Core node: use full iris_dedup (3-tier dedup with Mnesia persistence)
            iris_dedup:check_and_mark(DedupKey);
        undefined ->
            %% Edge node: use supervisor-owned iris_edge_dedup (ETS-only)
            Now = os:system_time(millisecond),
            case ets:insert_new(iris_edge_dedup, {DedupKey, Now}) of
                false -> duplicate;
                true  -> new
            end
    end.

%% =============================================================================
%% Metrics
%% =============================================================================

incr_metric(Key) ->
    try
        ets:update_counter(?METRICS_ETS, Key, 1, {Key, 0})
    catch
        error:badarg -> ok  %% Table not created yet
    end.

get_global_metrics() ->
    try
        #{
            total_route_attempts => ets:lookup_element(?METRICS_ETS, route_attempt, 2),
            total_route_success => ets:lookup_element(?METRICS_ETS, route_success, 2),
            total_route_offline => ets:lookup_element(?METRICS_ETS, route_offline, 2),
            total_route_failures => ets:lookup_element(?METRICS_ETS, route_failure, 2)
        }
    catch
        error:badarg -> #{}  %% Table not created yet
    end.
