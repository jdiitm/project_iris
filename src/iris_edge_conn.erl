-module(iris_edge_conn).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([wait_for_socket/3, connected/3]).

-record(data, {
    socket :: gen_tcp:socket(),
    user :: binary(),
    buffer = <<>> :: binary(),
    timeouts = 0 :: integer(),
    pending_acks = #{} :: map(), %% MsgId => {Msg, Timestamp, RetryCount}
    retry_timer :: reference() | undefined,
    last_activity :: integer(),   %% For hibernation
    hibernated = false :: boolean()
}).

%% Limits
-define(RETRY_INTERVAL, 5000). %% 5 Seconds
-define(MAX_PENDING_ACKS, 1000). %% Bounded pending acks
-define(HIBERNATE_AFTER_MS, 30000). %% Hibernate after 30s idle
-define(MAX_BUFFER_SIZE, 65536). %% 64KB max buffer (DoS protection)

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
    Now = os:system_time(millisecond),
    Timer = erlang:send_after(?RETRY_INTERVAL, self(), check_acks),
    {ok, wait_for_socket, #data{retry_timer = Timer, last_activity = Now}}.

callback_mode() -> [state_functions, state_enter].

%% STATE: wait_for_socket
wait_for_socket(enter, _OldState, _Data) ->
    keep_state_and_data;
wait_for_socket(cast, {socket_ready, Socket}, Data) ->
    %% Now we own the socket, set active once + send_timeout (2s)
    inet:setopts(Socket, [{active, once}, {send_timeout, 2000}]),
    {next_state, connected, Data#data{socket = Socket}}.

%% STATE: connected
connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(info, {tcp, _Socket, Bin}, Data = #data{buffer = Buff, socket = Socket}) ->
    Now = os:system_time(millisecond),
    NewBuff = <<Buff/binary, Bin/binary>>,
    
    %% DoS Protection: Reject oversized buffers
    case byte_size(NewBuff) > ?MAX_BUFFER_SIZE of
        true ->
            logger:warning("Buffer overflow from client. Dropping connection."),
            {stop, buffer_overflow, Data};
        false ->
            process_buffer(NewBuff, Data#data{last_activity = Now, hibernated = false})
    end;

connected(info, {tcp_closed, _Socket}, Data) ->
    {stop, normal, Data};

connected(info, {tcp_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};

connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket, user = User, pending_acks = Pending}) ->
    Now = os:system_time(millisecond),
    
    %% Bounded pending_acks: Drop oldest if at capacity
    BoundedPending = enforce_pending_limit(Pending, User),
    PendingCount = maps:size(BoundedPending),
    
    case PendingCount >= ?MAX_PENDING_ACKS of
        true ->
            %% At capacity even after enforcement - store offline immediately
            logger:warning("Pending ACKs at capacity for ~p. Storing offline.", [User]),
            iris_circuit_breaker:call(get_core_node(), iris_core, store_offline, [User, Msg]),
            {keep_state, Data#data{last_activity = Now}};
        false ->
            %% Generate unique MsgId and send
            MsgId = generate_msg_id(),
            Packet = iris_proto:encode_reliable_msg(MsgId, Msg),
            NewPending = maps:put(MsgId, {Msg, os:system_time(seconds), 0}, BoundedPending),
            
            case gen_tcp:send(Socket, Packet) of
                ok -> 
                    {keep_state, Data#data{pending_acks = NewPending, timeouts = 0, last_activity = Now}};
                {error, _} ->
                    logger:warning("Send failed for ~p. Storing offline.", [User]),
                    iris_circuit_breaker:call(get_core_node(), iris_core, store_offline, [User, Msg]),
                    {keep_state, Data#data{last_activity = Now}}
            end
    end;

connected(info, check_acks, Data = #data{pending_acks = Pending, user = User, retry_timer = OldTimer, last_activity = LastActivity}) ->
    erlang:cancel_timer(OldTimer),
    Now = os:system_time(seconds),
    NowMs = os:system_time(millisecond),
    
    %% Scan for expired ACKs ( > 10 seconds)
    NewPending = maps:filter(fun(MsgId, {Msg, Ts, _Retries}) ->
        if (Now - Ts) > 10 ->
            logger:warning("Msg ~p timed out (No ACK). Moving to offline storage.", [MsgId]),
            iris_circuit_breaker:call(get_core_node(), iris_core, store_offline, [User, Msg]),
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
enforce_pending_limit(Pending, User) when map_size(Pending) < ?MAX_PENDING_ACKS ->
    Pending;
enforce_pending_limit(Pending, User) ->
    %% Find and remove oldest entries until under limit
    Entries = maps:to_list(Pending),
    Sorted = lists:sort(fun({_, {_, Ts1, _}}, {_, {_, Ts2, _}}) -> Ts1 =< Ts2 end, Entries),
    
    %% Remove oldest 10% to avoid frequent evictions
    ToRemove = max(1, length(Sorted) div 10),
    {RemoveEntries, KeepEntries} = lists:split(min(ToRemove, length(Sorted)), Sorted),
    
    %% Store removed messages offline
    lists:foreach(fun({MsgId, {Msg, _Ts, _Retries}}) ->
        logger:warning("Pending ACK overflow: moving msg ~p to offline for ~p", [MsgId, User]),
        iris_circuit_breaker:call(get_core_node(), iris_core, store_offline, [User, Msg])
    end, RemoveEntries),
    
    maps:from_list(KeepEntries).

process_buffer(Bin, Data = #data{socket = Socket, user = CurrentUser}) ->
    case iris_proto:decode(Bin) of
        {more, _} ->
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = Bin}};

        {Packet, Rest} ->
            %% Delegate to Logic Module
            {ok, NewUser, Actions} = iris_session:handle_packet(Packet, CurrentUser, self(), ?MODULE),
            
            %% Execute Actions
            %% Execute Actions & Update State
            NewData = lists:foldl(fun
                ({send, Msg}, D) -> 
                    _ = gen_tcp:send(Socket, Msg), 
                    D;
                ({send_batch, Msgs}, D) -> 
                    _ = [gen_tcp:send(Socket, M) || M <- Msgs], 
                    D;
                ({deliver_msg, Msg}, D = #data{pending_acks = P}) ->
                    %% Wrap in reliable message format
                    MsgId = generate_msg_id(),
                    OutPacket = iris_proto:encode_reliable_msg(MsgId, Msg),
                    NewP = maps:put(MsgId, {Msg, os:system_time(seconds), 0}, P),
                    _ = gen_tcp:send(Socket, OutPacket),
                    D#data{pending_acks = NewP};
                ({ack_received, MsgId}, D = #data{pending_acks = P, user = AckUser}) -> 
                    %% Remove from pending
                    %% io:format("[ACK] Received for ~p from ~s~n", [MsgId, AckUser]),
                    D#data{pending_acks = maps:remove(MsgId, P)};
                (close, _D) -> gen_statem:stop({shutdown, closed}), error(closed)
            end, Data, Actions),
            
            process_buffer(Rest, NewData#data{user = NewUser})
    end.


terminate(Reason, _State, #data{user = User, pending_acks = Pending}) ->
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
            %% Use durable batcher if available, else direct Mnesia
            case whereis(iris_durable_batcher_1) of
                undefined ->
                    %% Fallback to direct store
                    lists:foreach(fun(Msg) ->
                        rpc:call(get_core_node(), iris_core, store_offline, [User, Msg])
                    end, Msgs);
                _ ->
                    %% Use batched durable store
                    iris_durable_batcher:store_batch(User, Msgs, 16, #{})
            end
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
            case whereis(iris_durable_batcher_1) of
                undefined ->
                    lists:foreach(fun(Msg) ->
                        rpc:call(get_core_node(), iris_core, store_offline, [User, Msg])
                    end, Msgs);
                _ ->
                    iris_durable_batcher:store_batch(User, Msgs, 16, #{})
            end
    end.

collect_queued_msgs(Acc) ->
    receive
        {deliver_msg, Msg} ->
            collect_queued_msgs([Msg | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

code_change(_OldVsn, StateName, Data, _Extra) ->
    %% No state change needed for 0.1.0 -> 0.2.0
    %% If we changed record #data{}, we would convert it here.
    {ok, StateName, Data}.
