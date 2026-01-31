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
-export([get_queue_depth/0, get_queue_depth/1]).
-export([get_stats/0, drain_region/1]).
-export([init_tables/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(OUTBOUND_TABLE, cross_region_outbound).
-define(DEAD_LETTER_TABLE, cross_region_dead_letter).
-define(DRAIN_INTERVAL_MS, 100).
-define(MAX_ATTEMPTS, 5).
-define(BASE_BACKOFF_MS, 1000).
-define(MAX_BACKOFF_MS, 60000).
-define(BATCH_SIZE, 100).

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
    drain_timer,     %% Timer ref for periodic drain
    stats           %% Delivery statistics
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

-spec send_cross_region(binary(), binary(), binary(), map()) -> ok | {error, term()}.
send_cross_region(TargetRegion, UserId, Msg, Opts) ->
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
    mnesia:activity(transaction, fun() ->
        length(mnesia:match_object(?OUTBOUND_TABLE, 
            #cross_region_outbound{target_region = Region, status = pending, _ = '_'}, read))
    end).

%% @doc Get delivery statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Force drain all pending messages for a region
-spec drain_region(binary()) -> ok.
drain_region(Region) ->
    gen_server:cast(?SERVER, {drain_region, Region}).

%% @doc Initialize Mnesia tables for cross-region messaging
-spec init_tables() -> ok.
init_tables() ->
    %% Outbound queue table
    case mnesia:create_table(?OUTBOUND_TABLE, [
        {attributes, record_info(fields, cross_region_outbound)},
        {disc_copies, [node()]},
        {type, set},
        {index, [target_region, status, next_retry_at]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?OUTBOUND_TABLE}} -> ok;
        {aborted, Reason1} ->
            logger:warning("Failed to create outbound table: ~p", [Reason1])
    end,
    
    %% Dead letter table for failed messages
    case mnesia:create_table(?DEAD_LETTER_TABLE, [
        {attributes, record_info(fields, cross_region_dead_letter)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?DEAD_LETTER_TABLE}} -> ok;
        {aborted, Reason2} ->
            logger:warning("Failed to create dead letter table: ~p", [Reason2])
    end,
    
    %% Wait for tables
    mnesia:wait_for_tables([?OUTBOUND_TABLE, ?DEAD_LETTER_TABLE], 10000),
    ok.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Ensure tables exist
    init_tables(),
    
    %% Start periodic drain timer
    Timer = erlang:send_after(?DRAIN_INTERVAL_MS, self(), drain),
    
    %% Join the region bridge pg group for discovery
    pg:join(iris_region_bridges, self()),
    
    State = #state{
        drain_timer = Timer,
        stats = #{
            sent => 0,
            delivered => 0,
            failed => 0,
            retried => 0
        }
    },
    
    logger:info("Cross-region bridge started for region ~s", 
                [iris_region_router:get_current_region()]),
    
    {ok, State}.

handle_call(get_stats, _From, State = #state{stats = Stats}) ->
    QueueDepth = get_queue_depth(),
    {reply, Stats#{queue_depth => QueueDepth}, State};

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
    NewState = do_drain(State),
    Timer = erlang:send_after(?DRAIN_INTERVAL_MS, self(), drain),
    {noreply, NewState#state{drain_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.drain_timer),
    pg:leave(iris_region_bridges, self()),
    ok.

%% =============================================================================
%% Internal: Drain Logic
%% =============================================================================

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
