-module(iris_region_bridge).
-behaviour(gen_server).

%% =============================================================================
%% Cross-Region Message Bridge
%% =============================================================================
%% 
%% Purpose: Reliable async message delivery across regional Mnesia clusters.
%% 
%% DESIGN:
%% 1. Messages are durably queued in local Mnesia before ACK
%% 2. Background workers drain queue and deliver to remote regions
%% 3. Failed deliveries are retried with exponential backoff
%% 4. After max_attempts, message goes to dead-letter queue for manual review
%% 
%% GUARANTEES:
%% - At-least-once delivery (client dedup handles duplicates)
%% - No silent message loss (all failures tracked)
%% - Survives sender region crash (messages durable before ACK)
%% 
%% =============================================================================

-export([start_link/0]).
-export([send_cross_region/3, send_cross_region/4]).
-export([get_queue_depth/0, get_queue_depth/1, get_queue_depth_fast/1]).
-export([get_max_queue_size/0]).  %% FM-1: Expose limit for tests
-export([get_stats/0, drain_region/1]).
-export([init_tables/0]).
%% GEO-001 FIX: Mesh health and auto-recovery
-export([get_mesh_health/0, get_disconnected_nodes/0, force_reconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(OUTBOUND_TABLE, cross_region_outbound).
-define(DEAD_LETTER_TABLE, cross_region_dead_letter).
-define(DRAIN_INTERVAL_MS, 100).
-define(MAX_ATTEMPTS, 5).
-define(BASE_BACKOFF_MS, 1000).
-define(MAX_BACKOFF_MS, 60000).
-define(BATCH_SIZE, 100).
-define(MAX_QUEUE_SIZE, 10000).  %% FM-1: Max messages per destination region
-define(DEPTH_ETS, iris_region_bridge_depth).  %% G-3: O(1) queue depth counter
-define(OUTBOX_TTL_MS, 604800000).  %% RFC Section 7.2: 7 days in milliseconds
%% GEO-001 FIX: Auto-reconnection constants
-define(RECONNECT_INTERVAL_MS, 5000).   %% 5 seconds between reconnect attempts
-define(HEALTH_CHECK_INTERVAL_MS, 10000). %% 10 seconds health check
-define(NODE_TIMEOUT_MS, 30000).         %% Mark node dead after 30s unreachable

%% Outbound message record - name MUST match table name for Mnesia writes
-record(cross_region_outbound, {
    id,              %% Unique message ID
    target_region,   %% Destination region
    user_id,         %% Target user
    msg,             %% Message payload
    status,          %% pending | in_flight | delivered | failed
    attempts,        %% Delivery attempt count
    created_at,      %% Timestamp when queued
    next_retry_at,   %% Timestamp for next retry (0 = immediate)
    last_error       %% Last error reason (if any)
}).

%% Dead letter record - name MUST match table name for Mnesia writes
-record(cross_region_dead_letter, {
    id,              %% Unique message ID
    target_region,   %% Destination region
    user_id,         %% Target user
    msg,             %% Message payload
    status,          %% pending | in_flight | delivered | failed
    attempts,        %% Delivery attempt count
    created_at,      %% Timestamp when queued
    next_retry_at,   %% Timestamp for next retry (0 = immediate)
    last_error       %% Last error reason (if any)
}).

-record(state, {
    drain_timer,        %% Timer ref for periodic drain
    health_timer,       %% GEO-001: Timer for health checks
    stats,              %% Delivery statistics
    disconnected = [],  %% GEO-001: List of disconnected nodes
    reconnect_timers = #{} %% GEO-001: Node -> Timer map for reconnection
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Queue a message for cross-region delivery.
%% Returns immediately after durable write - delivery is async.
-spec send_cross_region(binary(), binary(), binary()) -> ok | {error, term()}.
send_cross_region(TargetRegion, UserId, Msg) ->
    send_cross_region(TargetRegion, UserId, Msg, #{}).

%% @doc Get max queue size per destination region (FM-1: RFC-001 v4.0 Section 7.2)
-spec get_max_queue_size() -> non_neg_integer().
get_max_queue_size() ->
    ?MAX_QUEUE_SIZE.

-spec send_cross_region(binary(), binary(), binary(), map()) -> ok | {error, term()}.
send_cross_region(TargetRegion, UserId, Msg, Opts) ->
    %% FM-1: Check queue depth before accepting message (NACK on overflow)
    case check_queue_overflow(TargetRegion) of
        ok ->
            do_send_cross_region(TargetRegion, UserId, Msg, Opts);
        {error, _} = Err ->
            Err
    end.

check_queue_overflow(TargetRegion) ->
    try
        Depth = get_queue_depth(TargetRegion),
        %% RFC Section 7.2: Alert at 50% of max queue depth (GAP-2 fix)
        case Depth >= (?MAX_QUEUE_SIZE div 2) of
            true -> iris_metrics:inc(iris_outbox_queue_warning);
            false -> ok
        end,
        case Depth >= ?MAX_QUEUE_SIZE of
            true ->
                logger:warning("Outbox queue overflow for region ~s: ~p/~p",
                               [TargetRegion, Depth, ?MAX_QUEUE_SIZE]),
                {error, {queue_overflow, #{retry_after => 5, depth => Depth, max => ?MAX_QUEUE_SIZE}}};
            false ->
                ok
        end
    catch
        Class:Reason ->
            %% AUDIT MITIGATION P0-3: Fail-closed -- reject message if depth check crashes.
            %% Under memory pressure (the condition causing overflow), fail-open is useless.
            logger:error("Region bridge queue depth check failed for ~s: ~p:~p",
                         [TargetRegion, Class, Reason]),
            {error, {queue_check_failed, TargetRegion}}
    end.

do_send_cross_region(TargetRegion, UserId, Msg, Opts) ->
    MsgId = maps:get(msg_id, Opts, generate_msg_id()),
    Now = erlang:system_time(millisecond),
    
    Record = #cross_region_outbound{
        id = MsgId,
        target_region = TargetRegion,
        user_id = UserId,
        msg = Msg,
        status = pending,
        attempts = 0,
        created_at = Now,
        next_retry_at = 0,  %% Immediate
        last_error = undefined
    },
    
    %% Durable write before returning OK
    case mnesia:activity(sync_transaction, fun() ->
        mnesia:write(?OUTBOUND_TABLE, Record, write)
    end) of
        ok -> 
            %% G-3 FIX: Atomically increment depth counter for this region
            incr_depth(TargetRegion),
            %% Notify bridge to drain
            gen_server:cast(?SERVER, drain_now),
            ok;
        {error, Reason} ->
            logger:error("Failed to queue cross-region message: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Get number of messages pending delivery
-spec get_queue_depth() -> non_neg_integer().
get_queue_depth() ->
    mnesia:table_info(?OUTBOUND_TABLE, size).

-spec get_queue_depth(binary()) -> non_neg_integer().
get_queue_depth(Region) ->
    %% G-3 FIX: Prefer O(1) ETS counter; fall back to O(N) scan if counter unavailable
    get_queue_depth_fast(Region).

%% @doc O(1) queue depth via ETS atomic counter (G-3 Fix: RFC 7.2 / FM-1).
%% Called on every send_cross_region -- must be fast.
-spec get_queue_depth_fast(binary()) -> non_neg_integer().
get_queue_depth_fast(Region) ->
    try
        case ets:lookup(?DEPTH_ETS, {queue_depth, Region}) of
            [{_, V}] when V >= 0 -> V;
            [{_, _}] -> 0;  %% Negative due to race; treat as 0
            [] -> 0
        end
    catch
        error:badarg ->
            %% ETS table not created yet (startup race) -- fall back to scan
            get_queue_depth_scan(Region)
    end.

%% @doc O(N) fallback -- only used if ETS counter table is unavailable.
-spec get_queue_depth_scan(binary()) -> non_neg_integer().
get_queue_depth_scan(Region) ->
    try
        mnesia:activity(transaction, fun() ->
            length(mnesia:match_object(?OUTBOUND_TABLE,
                #cross_region_outbound{target_region = Region, status = pending, _ = '_'}, read))
        end)
    catch
        _:_ -> 0
    end.

%% @doc Get delivery statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Force drain all pending messages for a region
-spec drain_region(binary()) -> ok.
drain_region(Region) ->
    gen_server:cast(?SERVER, {drain_region, Region}).

%% =============================================================================
%% GEO-001 FIX: Mesh Health API
%% =============================================================================

%% @doc Get current mesh health status
-spec get_mesh_health() -> map().
get_mesh_health() ->
    gen_server:call(?SERVER, get_mesh_health).

%% @doc Get list of currently disconnected nodes
-spec get_disconnected_nodes() -> [node()].
get_disconnected_nodes() ->
    gen_server:call(?SERVER, get_disconnected_nodes).

%% @doc Force immediate reconnection attempt to a node
-spec force_reconnect(node()) -> ok | {error, term()}.
force_reconnect(Node) ->
    gen_server:call(?SERVER, {force_reconnect, Node}).

%% @doc Initialize Mnesia tables for cross-region messaging
-spec init_tables() -> ok.
init_tables() ->
    %% Get all core nodes for multi-node replication (survives single node failure)
    CoreNodes = get_core_nodes(),
    logger:info("Initializing cross-region bridge tables with disc_copies on: ~p", [CoreNodes]),
    
    %% Outbound queue table - replicated to all core nodes for durability
    case mnesia:create_table(?OUTBOUND_TABLE, [
        {attributes, record_info(fields, cross_region_outbound)},
        {disc_copies, CoreNodes},
        {type, set},
        {index, [target_region, status, next_retry_at]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?OUTBOUND_TABLE}} -> 
            %% Table exists, ensure schema is replicated to all core nodes
            ensure_disc_copies(?OUTBOUND_TABLE, CoreNodes),
            ok;
        {aborted, Reason1} ->
            logger:warning("Failed to create outbound table: ~p", [Reason1])
    end,
    
    %% Dead letter table for failed messages - also replicated for durability
    case mnesia:create_table(?DEAD_LETTER_TABLE, [
        {attributes, record_info(fields, cross_region_dead_letter)},
        {disc_copies, CoreNodes},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?DEAD_LETTER_TABLE}} -> 
            ensure_disc_copies(?DEAD_LETTER_TABLE, CoreNodes),
            ok;
        {aborted, Reason2} ->
            logger:warning("Failed to create dead letter table: ~p", [Reason2])
    end,
    
    %% Wait for tables
    mnesia:wait_for_tables([?OUTBOUND_TABLE, ?DEAD_LETTER_TABLE], 10000),
    ok.

%% @doc Get all core nodes for Mnesia replication
%% Returns at least the current node, plus any connected core nodes
-spec get_core_nodes() -> [node()].
get_core_nodes() ->
    AllNodes = [node() | nodes()],
    CoreNodes = [N || N <- AllNodes, is_core_node(N)],
    case CoreNodes of
        [] -> 
            %% Fallback to current node if no core nodes detected
            %% (e.g., single-node dev setup or test environment)
            [node()];
        _ -> 
            CoreNodes
    end.

%% @doc Check if a node is a core node based on naming convention
-spec is_core_node(node()) -> boolean().
is_core_node(Node) ->
    NodeStr = atom_to_list(Node),
    lists:prefix("core", NodeStr) orelse 
    lists:prefix("iris_core", NodeStr) orelse
    lists:prefix("iris@core", NodeStr).

%% @doc Ensure disc_copies exist on all specified nodes
%% Used when table already exists but may need replication added
-spec ensure_disc_copies(atom(), [node()]) -> ok.
ensure_disc_copies(Table, Nodes) ->
    CurrentCopies = mnesia:table_info(Table, disc_copies),
    MissingNodes = Nodes -- CurrentCopies,
    lists:foreach(fun(Node) ->
        case mnesia:add_table_copy(Table, Node, disc_copies) of
            {atomic, ok} ->
                logger:info("Added disc_copy of ~p to ~p", [Table, Node]);
            {aborted, {already_exists, _, _}} ->
                ok;
            {aborted, Reason} ->
                logger:warning("Failed to add disc_copy of ~p to ~p: ~p", 
                             [Table, Node, Reason])
        end
    end, MissingNodes),
    ok.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% G-3 FIX: Create O(1) depth counter ETS table
    case ets:info(?DEPTH_ETS) of
        undefined ->
            ets:new(?DEPTH_ETS, [set, named_table, public, {write_concurrency, true}]);
        _ -> ok
    end,
    
    %% Ensure tables exist
    init_tables(),
    
    %% Start periodic drain timer
    DrainTimer = erlang:send_after(?DRAIN_INTERVAL_MS, self(), drain),
    
    %% GEO-001 FIX: Start health check timer
    HealthTimer = erlang:send_after(?HEALTH_CHECK_INTERVAL_MS, self(), health_check),
    
    %% GEO-001 FIX: Monitor all connected nodes for disconnection
    lists:foreach(fun(Node) ->
        erlang:monitor_node(Node, true)
    end, nodes()),
    
    %% Join the region bridge pg group for discovery
    pg:join(iris_region_bridges, self()),
    
    State = #state{
        drain_timer = DrainTimer,
        health_timer = HealthTimer,
        stats = #{
            sent => 0,
            delivered => 0,
            failed => 0,
            retried => 0,
            reconnected => 0  %% GEO-001: Track successful reconnections
        },
        disconnected = [],
        reconnect_timers = #{}
    },
    
    logger:info("Cross-region bridge started for region ~s (GEO-001: auto-reconnect enabled)", 
                [iris_region_router:get_current_region()]),
    
    {ok, State}.

handle_call(get_stats, _From, State = #state{stats = Stats}) ->
    QueueDepth = get_queue_depth(),
    {reply, Stats#{queue_depth => QueueDepth}, State};

%% GEO-001 FIX: Mesh health API handlers
handle_call(get_mesh_health, _From, State = #state{disconnected = Disconnected, stats = Stats}) ->
    ConnectedNodes = nodes(),
    Health = #{
        connected_nodes => ConnectedNodes,
        disconnected_nodes => Disconnected,
        total_connected => length(ConnectedNodes),
        total_disconnected => length(Disconnected),
        reconnections => maps:get(reconnected, Stats, 0),
        healthy => length(Disconnected) == 0
    },
    {reply, Health, State};

handle_call(get_disconnected_nodes, _From, State = #state{disconnected = Disconnected}) ->
    {reply, Disconnected, State};

handle_call({force_reconnect, Node}, _From, State = #state{reconnect_timers = Timers}) ->
    %% Cancel any existing timer
    NewTimers = case maps:get(Node, Timers, undefined) of
        undefined -> Timers;
        Timer -> 
            erlang:cancel_timer(Timer),
            maps:remove(Node, Timers)
    end,
    
    %% Try immediate reconnect
    Result = case net_kernel:connect_node(Node) of
        true -> ok;
        false -> {error, connect_failed}
    end,
    {reply, Result, State#state{reconnect_timers = NewTimers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(drain_now, State) ->
    %% Cancel existing timer and drain immediately
    erlang:cancel_timer(State#state.drain_timer),
    NewState = do_drain(State),
    Timer = erlang:send_after(?DRAIN_INTERVAL_MS, self(), drain),
    {noreply, NewState#state{drain_timer = Timer}};

handle_cast({drain_region, Region}, State) ->
    NewState = do_drain_region(Region, State),
    {noreply, NewState};

handle_cast({route, TargetRegion, UserId, Msg}, State) ->
    %% Handle direct message from iris_region_router
    case send_cross_region(TargetRegion, UserId, Msg) of
        ok -> 
            {noreply, increment_stat(sent, State)};
        {error, _} ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(drain, State) ->
    %% RFC Section 7.2: Purge messages older than 7-day TTL (GAP-1 fix)
    cleanup_expired_outbox(),
    NewState = do_drain(State),
    Timer = erlang:send_after(?DRAIN_INTERVAL_MS, self(), drain),
    {noreply, NewState#state{drain_timer = Timer}};

%% =============================================================================
%% GEO-001 FIX: Node Monitoring and Auto-Reconnection
%% =============================================================================

handle_info({nodedown, Node}, State = #state{disconnected = Disconnected, 
                                              reconnect_timers = Timers}) ->
    %% Node went down - schedule reconnection attempt
    logger:warning("GEO-001: Node ~p went down, scheduling reconnect in ~pms", 
                  [Node, ?RECONNECT_INTERVAL_MS]),
    
    %% Cancel any existing timer for this node
    NewTimers = case maps:get(Node, Timers, undefined) of
        undefined -> Timers;
        OldTimer -> 
            erlang:cancel_timer(OldTimer),
            maps:remove(Node, Timers)
    end,
    
    %% Schedule reconnection
    ReconnectTimer = erlang:send_after(?RECONNECT_INTERVAL_MS, self(), {reconnect, Node}),
    
    %% Add to disconnected list if not already there
    NewDisconnected = case lists:member(Node, Disconnected) of
        true -> Disconnected;
        false -> [Node | Disconnected]
    end,
    
    {noreply, State#state{
        disconnected = NewDisconnected,
        reconnect_timers = NewTimers#{Node => ReconnectTimer}
    }};

handle_info({nodeup, Node}, State = #state{disconnected = Disconnected,
                                            reconnect_timers = Timers,
                                            stats = Stats}) ->
    %% Node came back up (via external reconnect or our attempt)
    logger:info("GEO-001: Node ~p reconnected", [Node]),
    
    %% Cancel any pending reconnect timer
    NewTimers = case maps:get(Node, Timers, undefined) of
        undefined -> Timers;
        Timer -> 
            erlang:cancel_timer(Timer),
            maps:remove(Node, Timers)
    end,
    
    %% Remove from disconnected list
    NewDisconnected = lists:delete(Node, Disconnected),
    
    %% Re-enable node monitoring
    erlang:monitor_node(Node, true),
    
    %% Update stats
    NewStats = maps:update_with(reconnected, fun(V) -> V + 1 end, 1, Stats),
    
    {noreply, State#state{
        disconnected = NewDisconnected,
        reconnect_timers = NewTimers,
        stats = NewStats
    }};

handle_info({reconnect, Node}, State = #state{disconnected = _Disconnected,
                                               reconnect_timers = Timers}) ->
    %% Attempt to reconnect to node
    NewTimers = maps:remove(Node, Timers),
    
    case net_kernel:connect_node(Node) of
        true ->
            logger:info("GEO-001: Successfully reconnected to ~p", [Node]),
            %% nodeup message will handle the rest
            {noreply, State#state{reconnect_timers = NewTimers}};
        false ->
            %% Reconnection failed - schedule another attempt
            logger:warning("GEO-001: Reconnection to ~p failed, retrying in ~pms", 
                          [Node, ?RECONNECT_INTERVAL_MS]),
            ReconnectTimer = erlang:send_after(?RECONNECT_INTERVAL_MS, self(), {reconnect, Node}),
            {noreply, State#state{reconnect_timers = NewTimers#{Node => ReconnectTimer}}}
    end;

handle_info(health_check, State = #state{disconnected = Disconnected}) ->
    %% Periodic health check of all known nodes
    NewState = do_health_check(State),
    
    %% Log status if there are disconnected nodes
    case Disconnected of
        [] -> ok;
        _ -> logger:warning("GEO-001: ~p node(s) currently disconnected: ~p", 
                           [length(Disconnected), Disconnected])
    end,
    
    %% Reschedule health check
    HealthTimer = erlang:send_after(?HEALTH_CHECK_INTERVAL_MS, self(), health_check),
    {noreply, NewState#state{health_timer = HealthTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.drain_timer),
    %% GEO-001: Cancel health timer
    case State#state.health_timer of
        undefined -> ok;
        HTimer -> erlang:cancel_timer(HTimer)
    end,
    %% GEO-001: Cancel all reconnect timers
    maps:foreach(fun(_Node, Timer) ->
        erlang:cancel_timer(Timer)
    end, State#state.reconnect_timers),
    pg:leave(iris_region_bridges, self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal: Drain Logic
%% =============================================================================

%% @doc Purge outbox messages older than 7-day TTL (RFC Section 7.2 GAP-1 fix)
cleanup_expired_outbox() ->
    try
        CutoffMs = erlang:system_time(millisecond) - ?OUTBOX_TTL_MS,
        Expired = mnesia:activity(transaction, fun() ->
            All = mnesia:match_object(?OUTBOUND_TABLE,
                #cross_region_outbound{_ = '_'}, read),
            [M || M <- All, M#cross_region_outbound.created_at < CutoffMs]
        end),
        case Expired of
            [] -> ok;
            _ ->
                mnesia:activity(transaction, fun() ->
                    lists:foreach(fun(M) ->
                        mnesia:delete(?OUTBOUND_TABLE, M#cross_region_outbound.id, write)
                    end, Expired)
                end),
                %% G-3 FIX: Decrement depth counters for expired messages
                RegionCounts = lists:foldl(fun(M, Acc) ->
                    R = M#cross_region_outbound.target_region,
                    maps:update_with(R, fun(V) -> V + 1 end, 1, Acc)
                end, #{}, Expired),
                maps:foreach(fun(R, Count) ->
                    try ets:update_counter(?DEPTH_ETS, {queue_depth, R},
                            {2, -Count, 0, 0}, {{queue_depth, R}, 0})
                    catch error:badarg -> ok end
                end, RegionCounts),
                logger:info("Outbox TTL: purged ~p expired messages (older than 7 days)",
                           [length(Expired)])
        end
    catch
        _:_ -> ok  %% If cleanup fails, don't crash the drain cycle
    end.

do_drain(State) ->
    Now = erlang:system_time(millisecond),
    
    %% Find pending messages ready for delivery
    Messages = mnesia:activity(transaction, fun() ->
        %% Get pending messages where next_retry_at <= Now
        All = mnesia:match_object(?OUTBOUND_TABLE, 
            #cross_region_outbound{status = pending, _ = '_'}, read),
        [M || M <- All, M#cross_region_outbound.next_retry_at =< Now]
    end),
    
    %% Process in batches
    BatchedMsgs = lists:sublist(Messages, ?BATCH_SIZE),
    lists:foldl(fun(Msg, AccState) ->
        deliver_message(Msg, AccState)
    end, State, BatchedMsgs).

do_drain_region(Region, State) ->
    Messages = mnesia:activity(transaction, fun() ->
        mnesia:match_object(?OUTBOUND_TABLE,
            #cross_region_outbound{target_region = Region, status = pending, _ = '_'}, read)
    end),
    
    lists:foldl(fun(Msg, AccState) ->
        deliver_message(Msg, AccState)
    end, State, Messages).

deliver_message(Msg = #cross_region_outbound{id = MsgId, target_region = Region, 
                                      user_id = UserId, msg = Payload,
                                      attempts = Attempts}, State) ->
    %% Mark as in-flight
    mnesia:activity(transaction, fun() ->
        mnesia:write(?OUTBOUND_TABLE, 
            Msg#cross_region_outbound{status = in_flight}, write)
    end),
    
    %% Attempt delivery
    Result = try_deliver(Region, UserId, Payload),
    
    case Result of
        ok ->
            %% Success - delete from queue
            mnesia:activity(transaction, fun() ->
                mnesia:delete(?OUTBOUND_TABLE, MsgId, write)
            end),
            %% G-3 FIX: Decrement depth counter
            decr_depth(Region),
            logger:debug("Delivered cross-region message ~p to ~s", [MsgId, Region]),
            increment_stat(delivered, State);
            
        {error, Reason} ->
            NewAttempts = Attempts + 1,
            case NewAttempts >= ?MAX_ATTEMPTS of
                true ->
                    %% Max retries exceeded - move to dead letter
                    logger:error("Cross-region message ~p failed after ~p attempts: ~p",
                                [MsgId, NewAttempts, Reason]),
                    move_to_dead_letter(Msg#cross_region_outbound{
                        attempts = NewAttempts,
                        last_error = Reason
                    }),
                    %% G-3 FIX: Decrement depth counter (moved out of pending)
                    decr_depth(Region),
                    increment_stat(failed, State);
                false ->
                    %% Schedule retry with exponential backoff
                    BackoffMs = calculate_backoff(NewAttempts),
                    NextRetry = erlang:system_time(millisecond) + BackoffMs,
                    mnesia:activity(transaction, fun() ->
                        mnesia:write(?OUTBOUND_TABLE, Msg#cross_region_outbound{
                            status = pending,
                            attempts = NewAttempts,
                            next_retry_at = NextRetry,
                            last_error = Reason
                        }, write)
                    end),
                    logger:warning("Cross-region message ~p retry ~p in ~pms: ~p",
                                  [MsgId, NewAttempts, BackoffMs, Reason]),
                    increment_stat(retried, State)
            end
    end.

try_deliver(Region, UserId, Payload) ->
    %% Get region endpoints
    case iris_region_router:get_region_endpoint(Region) of
        {ok, [Node | _]} ->
            %% Try RPC to remote region
            case rpc:call(Node, iris_async_router, route, [UserId, Payload], 5000) of
                ok -> ok;
                {badrpc, Reason} -> {error, {rpc_failed, Reason}};
                {error, Reason} -> {error, Reason}
            end;
        {ok, []} ->
            {error, no_endpoints};
        {error, Reason} ->
            %% Try pg discovery as fallback
            GroupName = binary_to_atom(<<"iris_region_", Region/binary>>, utf8),
            case pg:get_members(GroupName) of
                [] -> {error, {no_nodes, Reason}};
                [Pid | _] ->
                    Node = node(Pid),
                    case rpc:call(Node, iris_async_router, route, [UserId, Payload], 5000) of
                        ok -> ok;
                        {badrpc, R} -> {error, {rpc_failed, R}};
                        {error, R} -> {error, R}
                    end
            end
    end.

move_to_dead_letter(Msg) ->
    %% Convert outbound record to dead_letter record (same fields, different table)
    DeadLetterMsg = #cross_region_dead_letter{
        id = Msg#cross_region_outbound.id,
        target_region = Msg#cross_region_outbound.target_region,
        user_id = Msg#cross_region_outbound.user_id,
        msg = Msg#cross_region_outbound.msg,
        status = failed,
        attempts = Msg#cross_region_outbound.attempts,
        created_at = Msg#cross_region_outbound.created_at,
        next_retry_at = Msg#cross_region_outbound.next_retry_at,
        last_error = Msg#cross_region_outbound.last_error
    },
    mnesia:activity(transaction, fun() ->
        mnesia:delete(?OUTBOUND_TABLE, Msg#cross_region_outbound.id, write),
        mnesia:write(?DEAD_LETTER_TABLE, DeadLetterMsg, write)
    end).

calculate_backoff(Attempt) ->
    %% Exponential backoff: base * 2^attempt, capped at max
    Backoff = ?BASE_BACKOFF_MS * (1 bsl (Attempt - 1)),
    min(Backoff, ?MAX_BACKOFF_MS).

%% =============================================================================
%% Internal: Helpers
%% =============================================================================

generate_msg_id() ->
    %% Time-sortable ID: timestamp + random
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    <<Timestamp:64, Random:16>>.

increment_stat(Key, State = #state{stats = Stats}) ->
    NewStats = maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats),
    State#state{stats = NewStats}.

%% G-3 FIX: Atomic O(1) depth counter operations
incr_depth(Region) ->
    try ets:update_counter(?DEPTH_ETS, {queue_depth, Region}, 1, {{queue_depth, Region}, 0})
    catch error:badarg -> ok end.

decr_depth(Region) ->
    try ets:update_counter(?DEPTH_ETS, {queue_depth, Region}, {2, -1, 0, 0}, {{queue_depth, Region}, 0})
    catch error:badarg -> ok end.

%% =============================================================================
%% GEO-001 FIX: Health Check Implementation
%% =============================================================================

do_health_check(State = #state{disconnected = Disconnected, reconnect_timers = Timers}) ->
    %% Check if any disconnected nodes have come back
    %% (might have reconnected via other means)
    {StillDisconnected, NowConnected} = lists:partition(
        fun(Node) -> not lists:member(Node, nodes()) end,
        Disconnected
    ),
    
    %% For nodes that reconnected externally, clean up state
    NewTimers = lists:foldl(fun(Node, Acc) ->
        logger:info("GEO-001: Node ~p reconnected (detected via health check)", [Node]),
        case maps:get(Node, Acc, undefined) of
            undefined -> Acc;
            Timer -> 
                erlang:cancel_timer(Timer),
                maps:remove(Node, Acc)
        end
    end, Timers, NowConnected),
    
    %% Ensure all connected nodes are being monitored
    lists:foreach(fun(Node) ->
        erlang:monitor_node(Node, true)
    end, nodes()),
    
    %% Ping all known region endpoints to discover new nodes
    discover_and_monitor_regions(),
    
    State#state{
        disconnected = StillDisconnected,
        reconnect_timers = NewTimers
    }.

%% Discover region endpoints and set up monitoring
discover_and_monitor_regions() ->
    case whereis(iris_region_router) of
        undefined -> ok;
        _ ->
            try
                Regions = iris_region_router:get_all_regions(),
                lists:foreach(fun(Region) ->
                    case iris_region_router:get_region_endpoint(Region) of
                        {ok, Nodes} ->
                            lists:foreach(fun(Node) ->
                                case lists:member(Node, nodes()) of
                                    true -> ok;
                                    false ->
                                        %% Try to connect to unknown node
                                        case net_kernel:connect_node(Node) of
                                            true ->
                                                logger:info("GEO-001: Discovered and connected to new node ~p in region ~s",
                                                           [Node, Region]),
                                                erlang:monitor_node(Node, true);
                                            false -> ok
                                        end
                                end
                            end, Nodes);
                        _ -> ok
                    end
                end, Regions)
            catch Class:Reason ->
                logger:warning("iris_region_bridge:broadcast_invalidation catch-all: ~p:~p", [Class, Reason]),
                ok
            end
    end.
