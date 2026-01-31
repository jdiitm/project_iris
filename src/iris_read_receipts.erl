-module(iris_read_receipts).

%% =============================================================================
%% Read Receipts Module (RFC FR-4: Optional Feature)
%% =============================================================================
%% 
%% Provides read receipt tracking for messages. Per RFC:
%% - Read receipts are OPTIONAL
%% - Best-effort delivery (like typing indicators)
%% - No durability requirement - if original sender is offline, discard
%% 
%% Implementation Notes:
%% - Receipts are stored temporarily in ETS (configurable TTL)
%% - Only recent receipts are kept to bound memory usage
%% - Batch receipts can be sent to reduce network overhead
%% =============================================================================

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([record_read/3, get_read_status/2]).
-export([relay_read_receipt/3]).
-export([is_enabled/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(RECEIPTS_TABLE, iris_read_receipts_ets).
-define(DEFAULT_TTL_MS, 86400000).  %% 24 hours default
-define(CLEANUP_INTERVAL_MS, 60000). %% Cleanup every minute

-record(state, {
    ttl_ms :: integer(),
    cleanup_timer :: reference()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if read receipts are enabled
-spec is_enabled() -> boolean().
is_enabled() ->
    case whereis(?SERVER) of
        undefined -> false;
        _ -> application:get_env(iris_core, read_receipts_enabled, false)
    end.

%% @doc Record that a user has read a message
%% MsgId: The message ID that was read
%% Reader: The user who read the message  
%% OriginalSender: The user who sent the original message
-spec record_read(binary(), binary(), binary()) -> ok | {error, disabled}.
record_read(MsgId, Reader, OriginalSender) ->
    case is_enabled() of
        true ->
            gen_server:cast(?SERVER, {record_read, MsgId, Reader, OriginalSender}),
            ok;
        false ->
            {error, disabled}
    end.

%% @doc Get the read status of a message
%% Returns list of {Reader, Timestamp} tuples
-spec get_read_status(binary(), binary()) -> [{binary(), integer()}] | {error, term()}.
get_read_status(MsgId, RequestingUser) ->
    case is_enabled() of
        true ->
            gen_server:call(?SERVER, {get_read_status, MsgId, RequestingUser});
        false ->
            {error, disabled}
    end.

%% @doc Relay a read receipt to the original sender
%% Called by iris_session when processing a read_receipt packet
-spec relay_read_receipt(binary(), binary(), binary()) -> ok.
relay_read_receipt(MsgId, Reader, OriginalSender) ->
    case is_enabled() of
        true ->
            %% Record locally
            gen_server:cast(?SERVER, {record_read, MsgId, Reader, OriginalSender}),
            %% Try to deliver to original sender if online
            deliver_receipt_to_sender(MsgId, Reader, OriginalSender);
        false ->
            ok
    end.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Create ETS table for receipt storage
    ets:new(?RECEIPTS_TABLE, [named_table, public, bag, {read_concurrency, true}]),
    
    TtlMs = application:get_env(iris_core, read_receipts_ttl_ms, ?DEFAULT_TTL_MS),
    
    %% Start periodic cleanup
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_old_receipts),
    
    {ok, #state{ttl_ms = TtlMs, cleanup_timer = Timer}}.

handle_call({get_read_status, MsgId, _RequestingUser}, _From, State) ->
    %% Return all readers of this message
    Receipts = ets:lookup(?RECEIPTS_TABLE, MsgId),
    Readers = [{Reader, Timestamp} || {_, Reader, _, Timestamp} <- Receipts],
    {reply, Readers, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_read, MsgId, Reader, OriginalSender}, State) ->
    Timestamp = os:system_time(millisecond),
    %% Store: {MsgId, Reader, OriginalSender, Timestamp}
    ets:insert(?RECEIPTS_TABLE, {MsgId, Reader, OriginalSender, Timestamp}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_old_receipts, State = #state{ttl_ms = TtlMs}) ->
    %% Remove receipts older than TTL
    Now = os:system_time(millisecond),
    Cutoff = Now - TtlMs,
    
    %% Use match_delete for efficiency
    ets:select_delete(?RECEIPTS_TABLE, [
        {{'_', '_', '_', '$1'}, [{'<', '$1', Cutoff}], [true]}
    ]),
    
    %% Schedule next cleanup
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_old_receipts),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cleanup_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

%% =============================================================================
%% Internal
%% =============================================================================

%% @doc Try to deliver read receipt to original sender
deliver_receipt_to_sender(MsgId, Reader, OriginalSender) ->
    Timestamp = os:system_time(millisecond),
    Packet = iris_proto:encode_read_receipt_relay(MsgId, Reader, Timestamp),
    
    %% Look up sender in local presence first (fast path)
    case ets:lookup(local_presence_v2, OriginalSender) of
        [{OriginalSender, Pid}] when is_pid(Pid) ->
            %% Sender is on this node - send directly
            Pid ! {deliver_read_receipt, Packet},
            ok;
        [] ->
            %% Sender not on this node - try remote delivery (best-effort)
            spawn(fun() ->
                try_remote_delivery(OriginalSender, Packet)
            end),
            ok
    end.

try_remote_delivery(User, Packet) ->
    case get_core_node() of
        undefined -> ok;
        CoreNode ->
            case rpc:call(CoreNode, iris_core, lookup_user, [User], 1000) of
                {online, TargetNode, TargetPid} when is_pid(TargetPid) ->
                    catch rpc:cast(TargetNode, erlang, send, [TargetPid, {deliver_read_receipt, Packet}]);
                _ ->
                    %% Sender offline - discard receipt (expected behavior)
                    ok
            end
    end.

get_core_node() ->
    case iris_core_registry:get_core() of
        {ok, Node} -> Node;
        {error, _} -> undefined
    end.
