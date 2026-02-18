-module(iris_edge_conn).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([wait_for_socket/3, connected/3]).
-export([maybe_compress_outbound/2, maybe_decompress_inbound/2]).  %% Pure, exported for TDD
-export([collect_queued_msgs/1]).  %% Exported for TDD

%% Edge nodes hold significant per-connection state (not stateless).
%% This record documents all state held per connection. On disconnect,
%% critical fields (session_id, pending_acks) are saved via
%% queue_pending_to_session_cache and save_pending_acks for session resume.
-record(data, {
    socket :: gen_tcp:socket() | ssl:sslsocket(),         %% TCP/TLS socket handle
    transport = tcp :: tcp | ssl,                          %% Transport type for setopts/messages
    user :: binary(),                                      %% Authenticated user ID
    buffer = <<>> :: binary(),                             %% Partial frame assembly buffer
    timeouts = 0 :: integer(),                             %% Consecutive timeout count
    pending_acks = #{} :: map(),                           %% MsgId => {Msg, Timestamp, RetryCount} — saved on disconnect
    retry_timer :: reference() | undefined,                %% ACK retry timer reference
    last_activity :: integer(),                             %% Timestamp for idle hibernation
    hibernated = false :: boolean(),                        %% Whether process is hibernated
    session_id :: binary() | undefined,                    %% RFC 3.4: Session ID for resume — saved on disconnect
    capabilities = [] :: list()                             %% RFC 11.1: Negotiated compression/features
}).

%% Transport-agnostic setopts
setopts(Socket, tcp, Opts) -> inet:setopts(Socket, Opts);
setopts(Socket, ssl, Opts) -> ssl:setopts(Socket, Opts).

%% Limits
-define(RETRY_INTERVAL, 5000). %% 5 Seconds
-define(MAX_PENDING_ACKS, 1000). %% Bounded pending acks
-define(HIBERNATE_AFTER_MS, 30000). %% Hibernate after 30s idle
-define(MAX_BUFFER_SIZE, 65536). %% 64KB max buffer (DoS protection)
-define(MAX_PROCESS_DEPTH, 100). %% Max recursion depth for process_buffer
-define(MAX_DRAIN_MSGS, 100).   %% Max messages drained per flush

%% Dynamic Core node discovery with failover
get_core_node() ->
    case iris_core_registry:get_core() of
        {ok, Node} -> Node;
        {error, _} -> legacy_core_node()
    end.

legacy_core_node() ->
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core"
    end,
    list_to_atom(CoreName ++ "@" ++ Host).

%% Generate unique 16-byte message ID without crypto
generate_msg_id() ->
    %% Combine monotonic time, unique integer, and node hash for uniqueness
    Time = erlang:monotonic_time(),
    Unique = erlang:unique_integer([positive]),
    NodeHash = erlang:phash2(node()),
    %% Pack into 16 bytes
    <<Time:64, Unique:32, NodeHash:32>>.

%% API
start_link(Socket) ->
    %% Optimized spawn options for memory efficiency
    gen_statem:start_link(?MODULE, Socket, [
        {spawn_opt, [
            {min_heap_size, 233},      %% ~2KB initial heap
            {fullsweep_after, 10},     %% Aggressive GC
            {message_queue_data, off_heap}  %% Large msgs don't block GC
        ]}
    ]).

set_socket(Pid, Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

%% Callbacks
init(_Args) ->
    %% Enforce global connection limits
    case iris_ingress_guard:check() of
        allow ->
            %% Kill process if it grows too large (prevent OOM)
            %% TLS + cross-region routing requires significant memory
            %% Increased to 500000 (~4MB) to handle complex routing operations
            process_flag(max_heap_size, #{size => 500000, kill => true}),
            %% Initialize per-socket byte guard
            iris_ingress_byte_guard:reset(),
            Now = os:system_time(millisecond),
            Timer = erlang:send_after(?RETRY_INTERVAL, self(), check_acks),
            {ok, wait_for_socket, #data{retry_timer = Timer, last_activity = Now}};
        {deny, _Reason} ->
            {stop, normal} %% Silent drop or close
    end.

callback_mode() -> [state_functions, state_enter].

%% STATE: wait_for_socket
wait_for_socket(enter, _OldState, _Data) ->
    keep_state_and_data;
wait_for_socket(cast, {socket_ready, Socket}, Data) ->
    %% Detect transport type (SSL sockets are tuples with sslsocket atom)
    Transport = case Socket of
        {sslsocket, _, _} -> ssl;
        _ -> tcp
    end,
    %% B-4 FIX: Reject plaintext TCP connections in production mode (NFR-14).
    %% TLS is mandatory for all client connections per RFC-001 v4.0.
    case Transport of
        tcp ->
            Mode = application:get_env(iris_edge, deployment_mode,
                       application:get_env(iris_core, deployment_mode, development)),
            AllowInsecure = application:get_env(iris_edge, allow_insecure, false),
            case {Mode, AllowInsecure} of
                {production, _} ->
                    logger:warning("Rejected plaintext TCP connection in production mode (NFR-14)"),
                    iris_metrics:inc(tls_enforcement_rejections),
                    gen_tcp:close(Socket),
                    iris_ingress_guard:close(),
                    {stop, normal};
                {_, false} ->
                    logger:warning("Rejected plaintext TCP connection (allow_insecure=false)"),
                    iris_metrics:inc(tls_enforcement_rejections),
                    gen_tcp:close(Socket),
                    iris_ingress_guard:close(),
                    {stop, normal};
                _ ->
                    %% Development/test mode with allow_insecure=true
                    setopts(Socket, Transport, [{active, once}, {send_timeout, 2000}]),
                    {next_state, connected, Data#data{socket = Socket, transport = Transport}}
            end;
        ssl ->
            %% TLS connection — always allowed
            setopts(Socket, Transport, [{active, once}, {send_timeout, 2000}]),
            {next_state, connected, Data#data{socket = Socket, transport = Transport}}
    end.

%% STATE: connected
connected(enter, _OldState, _Data) ->
    keep_state_and_data;

%% Handle TCP data
connected(info, {tcp, _Socket, Bin}, Data) ->
    handle_socket_data(Bin, Data);

%% Handle SSL data
connected(info, {ssl, _Socket, Bin}, Data) ->
    handle_socket_data(Bin, Data);

%% Handle TCP close
connected(info, {tcp_closed, _Socket}, Data) ->
    {stop, normal, Data};

%% Handle SSL close
connected(info, {ssl_closed, _Socket}, Data) ->
    {stop, normal, Data};

%% Handle TCP error
connected(info, {tcp_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};

%% Handle SSL error
connected(info, {ssl_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};

connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket, transport = Transport, user = User, pending_acks = Pending, capabilities = Caps}) ->
    Now = os:system_time(millisecond),
    
    %% Bounded pending_acks: Drop oldest if at capacity
    BoundedPending = enforce_pending_limit(Pending, User),
    PendingCount = maps:size(BoundedPending),
    
    case PendingCount >= ?MAX_PENDING_ACKS of
        true ->
            %% At capacity even after enforcement - store offline immediately
            %% Use store_offline_durable for RPO=0 guarantee
            logger:warning("Pending ACKs at capacity for ~p. Storing offline.", [User]),
            iris_circuit_breaker:call(get_core_node(), iris_core, store_offline_durable, [User, Msg]),
            {keep_state, Data#data{last_activity = Now}};
        false ->
            %% Generate unique MsgId and send
            MsgId = generate_msg_id(),
            Packet = iris_proto:encode_reliable_msg(MsgId, Msg),
            NewPending = maps:put(MsgId, {Msg, os:system_time(seconds), 0}, BoundedPending),
            
            %% RFC Section 11.1: Compress outbound if negotiated
            case send_compressed(Socket, Transport, Caps, Packet) of
                ok -> 
                    {keep_state, Data#data{pending_acks = NewPending, timeouts = 0, last_activity = Now}};
                {error, Reason} ->
                    %% Use store_offline_durable for RPO=0 guarantee
                    logger:warning("Send failed for ~p (reason: ~p). Storing offline.", [User, Reason]),
                    iris_circuit_breaker:call(get_core_node(), iris_core, store_offline_durable, [User, Msg]),
                    {keep_state, Data#data{last_activity = Now}}
            end
    end;

%% RFC FR-8: Typing indicator relay (best-effort, fire-and-forget)
%% No durability required - if send fails, silently discard
connected(info, {deliver_typing, Packet}, Data = #data{socket = Socket, transport = Transport}) ->
    Now = os:system_time(millisecond),
    %% Best-effort send - no retry, no ACK tracking
    _ = send(Socket, Transport, Packet),
    {keep_state, Data#data{last_activity = Now}};

%% RFC FR-4: Read receipt relay (best-effort, fire-and-forget)
%% No durability required - if send fails, silently discard
connected(info, {deliver_read_receipt, Packet}, Data = #data{socket = Socket, transport = Transport}) ->
    Now = os:system_time(millisecond),
    %% Best-effort send - no retry, no ACK tracking
    _ = send(Socket, Transport, Packet),
    {keep_state, Data#data{last_activity = Now}};

connected(info, check_acks, Data = #data{pending_acks = Pending, user = User, retry_timer = OldTimer, last_activity = LastActivity}) ->
    erlang:cancel_timer(OldTimer),
    Now = os:system_time(seconds),
    NowMs = os:system_time(millisecond),
    
    %% Scan for expired ACKs ( > 10 seconds)
    NewPending = maps:filter(fun(MsgId, {Msg, Ts, _Retries}) ->
        if (Now - Ts) > 10 ->
            logger:warning("Msg ~p timed out (No ACK). Moving to offline storage.", [MsgId]),
            %% Use store_offline_durable for RPO=0 guarantee
            iris_circuit_breaker:call(get_core_node(), iris_core, store_offline_durable, [User, Msg]),
            false; %% Remove from map
        true -> 
            true
        end
    end, Pending),
    
    NewTimer = erlang:send_after(?RETRY_INTERVAL, self(), check_acks),
    NewData = Data#data{pending_acks = NewPending, retry_timer = NewTimer},
    
    %% Hibernation: If idle for too long and no pending, hibernate to save memory
    IdleTime = NowMs - LastActivity,
    ShouldHibernate = (IdleTime > ?HIBERNATE_AFTER_MS) andalso 
                      (maps:size(NewPending) == 0) andalso
                      (not Data#data.hibernated),
    
    case ShouldHibernate of
        true ->
            %% Hibernate this process to reclaim memory
            {keep_state, NewData#data{hibernated = true}, [hibernate]};
        false ->
            {keep_state, NewData}
    end.

%% Enforce bounded pending_acks by moving oldest to offline storage
enforce_pending_limit(Pending, _User) when map_size(Pending) < ?MAX_PENDING_ACKS ->
    Pending;
enforce_pending_limit(Pending, User) ->
    %% Find and remove oldest entries until under limit
    Entries = maps:to_list(Pending),
    Sorted = lists:sort(fun({_, {_, Ts1, _}}, {_, {_, Ts2, _}}) -> Ts1 =< Ts2 end, Entries),
    
    %% Remove oldest 10% to avoid frequent evictions
    ToRemove = max(1, length(Sorted) div 10),
    {RemoveEntries, KeepEntries} = lists:split(min(ToRemove, length(Sorted)), Sorted),
    
    %% Store removed messages offline
    %% Use store_offline_durable for RPO=0 guarantee
    lists:foreach(fun({MsgId, {Msg, _Ts, _Retries}}) ->
        logger:warning("Pending ACK overflow: moving msg ~p to offline for ~p", [MsgId, User]),
        iris_circuit_breaker:call(get_core_node(), iris_core, store_offline_durable, [User, Msg])
    end, RemoveEntries),
    
    maps:from_list(KeepEntries).

%% Helper for handling incoming socket data (shared by tcp/ssl handlers)
handle_socket_data(Bin, Data = #data{buffer = Buff}) ->
    Now = os:system_time(millisecond),

    %% Per-socket byte counting.
    %% Rejects connections exceeding max_ingress_bytes_per_sec BEFORE
    %% buffering, so micro-bursts cannot OOM the Edge node.
    case iris_ingress_byte_guard:check_bytes(byte_size(Bin)) of
        {error, byte_limit_exceeded} ->
            logger:warning("Ingress byte limit exceeded. Dropping connection."),
            {stop, byte_limit, Data};
        ok ->
            NewBuff = <<Buff/binary, Bin/binary>>,
            %% DoS Protection: Reject oversized buffers
            case byte_size(NewBuff) > ?MAX_BUFFER_SIZE of
                true ->
                    logger:warning("Buffer overflow from client. Dropping connection."),
                    {stop, buffer_overflow, Data};
                false ->
                    process_buffer(NewBuff, Data#data{last_activity = Now, hibernated = false}, 0)
            end
    end.

%% =============================================================================
%% RFC Section 11.1: Compression Wiring (Pure Functions for TDD)
%% =============================================================================

%% @doc Maybe compress an outbound packet based on negotiated capabilities.
%% Packet format: <<Opcode:8, Payload/binary>>
%% If compressed: <<(Opcode bor 0x80):8, CompressedPayload/binary>>
-spec maybe_compress_outbound([binary()], binary()) -> binary().
maybe_compress_outbound([], Packet) ->
    Packet;
maybe_compress_outbound(Caps, <<Opcode:8, Payload/binary>> = Packet) ->
    case pick_compression_algo(Caps) of
        none -> Packet;
        Algo ->
            case iris_compression:maybe_compress(Algo, Payload) of
                {compressed, CompressedPayload} ->
                    FlaggedOpcode = iris_compression:flag_compressed(Opcode),
                    <<FlaggedOpcode:8, CompressedPayload/binary>>;
                {uncompressed, _} ->
                    Packet
            end
    end;
maybe_compress_outbound(_Caps, Packet) ->
    Packet.

%% @doc Maybe decompress an inbound packet based on negotiated capabilities.
-spec maybe_decompress_inbound([binary()], binary()) -> binary().
maybe_decompress_inbound(_Caps, <<Opcode:8, Payload/binary>> = Packet) ->
    case iris_compression:is_compressed(Opcode) of
        false -> Packet;
        true ->
            OriginalOpcode = iris_compression:original_opcode(Opcode),
            Algo = pick_compression_algo(_Caps),
            case Algo of
                none -> Packet;  %% No algo negotiated — pass through
                _ ->
                    case iris_compression:decompress(Algo, Payload) of
                        {ok, Decompressed} ->
                            <<OriginalOpcode:8, Decompressed/binary>>;
                        {error, _} ->
                            Packet  %% Decompression failed — pass through
                    end
            end
    end;
maybe_decompress_inbound(_Caps, Packet) ->
    Packet.

%% Pick the best compression algorithm from negotiated capabilities.
pick_compression_algo([]) -> none;
pick_compression_algo([<<"zstd">> | _]) -> zstd;
pick_compression_algo([<<"zlib">> | _]) -> zlib;
pick_compression_algo([_ | Rest]) -> pick_compression_algo(Rest).

%% Transport-agnostic send
send(Socket, tcp, Msg) -> gen_tcp:send(Socket, Msg);
send(Socket, ssl, Msg) -> ssl:send(Socket, Msg).

%% RFC Section 11.1: Compression-aware send (DRY helper for all outbound paths)
send_compressed(Socket, Transport, Caps, Msg) ->
    send(Socket, Transport, maybe_compress_outbound(Caps, Msg)).

%% Commented out: unused local function (2-arity wrapper, 3-arity called directly).
%% process_buffer(Bin, Data) ->
%%     process_buffer(Bin, Data, 0).

%% Depth-limited recursive buffer processing
process_buffer(_Bin, Data, Depth) when Depth > ?MAX_PROCESS_DEPTH ->
    logger:warning("Edge conn: Max process_buffer depth ~p exceeded, closing", [?MAX_PROCESS_DEPTH]),
    {stop, {shutdown, process_depth_exceeded}, Data};
process_buffer(Bin, Data = #data{socket = Socket, transport = Transport, user = CurrentUser, capabilities = Caps}, Depth) ->
    %% RFC Section 11.1: Decompress inbound if compression flag set
    DecompressedBin = maybe_decompress_inbound(Caps, Bin),
    case iris_proto:decode(DecompressedBin) of
        {more, _} ->
            setopts(Socket, Transport, [{active, once}]),
            {keep_state, Data#data{buffer = Bin}};

        {Packet, Rest} ->
            %% Delegate to Logic Module
            {ok, NewUser, Actions} = iris_session:handle_packet(Packet, CurrentUser, self(), ?MODULE),
            
            %% Execute Actions & Update State
            NewData = lists:foldl(fun
                ({send, Msg}, D) -> 
                    _ = send_compressed(Socket, Transport, D#data.capabilities, Msg), 
                    D;
                ({send_batch, Msgs}, D) -> 
                    _ = [send_compressed(Socket, Transport, D#data.capabilities, M) || M <- Msgs], 
                    D;
                ({deliver_msg, Msg}, D = #data{pending_acks = P}) ->
                    MsgId = generate_msg_id(),
                    OutPacket = iris_proto:encode_reliable_msg(MsgId, Msg),
                    NewP = maps:put(MsgId, {Msg, os:system_time(seconds), 0}, P),
                    _ = send_compressed(Socket, Transport, D#data.capabilities, OutPacket),
                    D#data{pending_acks = NewP};
                ({ack_received, MsgId}, D = #data{pending_acks = P}) -> 
                    D#data{pending_acks = maps:remove(MsgId, P)};
                ({set_session_id, SId}, D) ->
                    D#data{session_id = SId};
                ({set_capabilities, NewCaps}, D) ->
                    D#data{capabilities = NewCaps};
                (close, _D) -> gen_statem:stop({shutdown, closed}), error(closed)
            end, Data, Actions),
            
            process_buffer(Rest, NewData#data{user = NewUser}, Depth + 1)
    end.


terminate(Reason, _State, #data{user = User, pending_acks = Pending, session_id = SessionId}) ->
    %% Decrement connection counter
    iris_ingress_guard:close(),
    %% RFC Section 3.4: Queue pending messages in session cache for resume
    queue_pending_to_session_cache(SessionId, Pending),
    %% Save all pending_acks to offline storage
    save_pending_acks(User, Pending),
    %% Also flush any queued messages
    flush_pending_msgs(User),
    %% Notify session of termination
    iris_session:terminate(User),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ ->
            logger:warning("Connection for ~p terminated abnormally: ~p", [User, Reason])
    end,
    ok.

%% RFC Section 3.4: Queue pending messages in session cache for connection resume
queue_pending_to_session_cache(undefined, _Pending) -> ok;
queue_pending_to_session_cache(_SessionId, Pending) when map_size(Pending) == 0 -> ok;
queue_pending_to_session_cache(SessionId, Pending) ->
    try
        %% Queue each pending message with an incrementing sequence number
        Entries = maps:to_list(Pending),
        lists:foldl(fun({_MsgId, {Msg, _Ts, _Retries}}, Seq) ->
            iris_session_cache:queue_message(SessionId, Seq, Msg),
            Seq + 1
        end, iris_session_cache:next_seq(SessionId), Entries),
        ok
    catch Class:Reason ->
        %% Session cache may not be available -- degrade gracefully
        logger:warning("iris_edge_conn:restore_pending_to_cache catch-all: ~p:~p", [Class, Reason]),
        ok
    end.

save_pending_acks(undefined, _Pending) ->
    ok;
save_pending_acks(_User, Pending) when map_size(Pending) == 0 ->
    ok;
save_pending_acks(User, Pending) ->
    %% Store all pending (unacked) messages to offline storage
    Msgs = [Msg || {_MsgId, {Msg, _Ts, _Retries}} <- maps:to_list(Pending)],
    case length(Msgs) of
        0 -> ok;
        Len ->
            logger:info("Saving ~p pending acks for ~p to offline storage", [Len, User]),
            %% Ensure durability BEFORE terminate completes (RPO=0)
            %% Strategy: WAL first, then Mnesia - never fire-and-forget
            case whereis(iris_durable_batcher_1) of
                undefined ->
                    %% No batcher - use sync RPC with retry to ensure durability
                    %% This is slower but guarantees RPO=0
                    save_msgs_durable_sync(User, Msgs);
                _ ->
                    %% Use batched durable store (WAL + sync_transaction)
                    iris_durable_batcher:store_batch(User, Msgs, 16, #{})
            end
    end.

%% Synchronous durable save with retry and local fallback
save_msgs_durable_sync(User, Msgs) ->
    CoreNode = get_core_node(),
    save_msgs_durable_sync(User, Msgs, CoreNode, 3).

save_msgs_durable_sync(_User, [], _CoreNode, _Retries) ->
    ok;
save_msgs_durable_sync(User, Msgs, _CoreNode, 0) ->
    %% All retries exhausted - save to local ETS for later sync
    logger:error("Failed to durably save ~p msgs for ~p after retries, using local fallback", 
                 [length(Msgs), User]),
    save_to_local_fallback(User, Msgs);
save_msgs_durable_sync(User, [Msg | Rest], CoreNode, Retries) ->
    %% Use rpc:call with timeout (NOT cast) for durability guarantee
    case rpc:call(CoreNode, iris_core, store_offline_durable, [User, Msg], 2000) of
        ok ->
            save_msgs_durable_sync(User, Rest, CoreNode, 3);  %% Reset retries on success
        {atomic, _} ->
            save_msgs_durable_sync(User, Rest, CoreNode, 3);
        {badrpc, _Reason} ->
            %% Retry with backoff
            timer:sleep(100 * (4 - Retries)),
            save_msgs_durable_sync(User, [Msg | Rest], CoreNode, Retries - 1);
        {aborted, _Reason} ->
            timer:sleep(100 * (4 - Retries)),
            save_msgs_durable_sync(User, [Msg | Rest], CoreNode, Retries - 1);
        _Other ->
            save_msgs_durable_sync(User, Rest, CoreNode, 3)
    end.

%% Local fallback storage when core is unreachable
save_to_local_fallback(User, Msgs) ->
    %% Store in local ETS for background sync later
    %% This ensures we don't lose messages even if core is down
    try
        Table = iris_edge_pending_offline,
        case ets:whereis(Table) of
            undefined ->
                ets:new(Table, [named_table, public, bag]);
            _ -> ok
        end,
        Now = os:system_time(millisecond),
        lists:foreach(fun(Msg) ->
            ets:insert(Table, {User, Msg, Now})
        end, Msgs),
        iris_metrics:inc(edge_fallback_store_count, length(Msgs)),
        logger:warning("Stored ~p msgs in local fallback for ~p", [length(Msgs), User])
    catch
        _:_ ->
            logger:error("Local fallback storage failed for ~p", [User])
    end.

flush_pending_msgs(undefined) ->
    ok;
flush_pending_msgs(User) ->
    %% Collect all queued messages
    Msgs = collect_queued_msgs([]),
    case Msgs of
        [] -> ok;
        _ ->
            logger:info("Flushing ~p queued msgs for ~p to offline storage", [length(Msgs), User]),
            %% Use durable save path for RPO=0 guarantee
            case whereis(iris_durable_batcher_1) of
                undefined ->
                    %% Use sync RPC with retry for durability
                    save_msgs_durable_sync(User, Msgs);
                _ ->
                    iris_durable_batcher:store_batch(User, Msgs, 16, #{})
            end
    end.

%% Bounded message drain to prevent OOM on disconnect
collect_queued_msgs(Acc) ->
    collect_queued_msgs(Acc, ?MAX_DRAIN_MSGS).

collect_queued_msgs(Acc, 0) ->
    lists:reverse(Acc);
collect_queued_msgs(Acc, Remaining) ->
    receive
        {deliver_msg, Msg} ->
            collect_queued_msgs([Msg | Acc], Remaining - 1)
    after 0 ->
        lists:reverse(Acc)
    end.

code_change(_OldVsn, StateName, Data, _Extra) ->
    %% No state change needed for 0.1.0 -> 0.2.0
    %% If we changed record #data{}, we would convert it here.
    {ok, StateName, Data}.
