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
    retry_timer :: reference() | undefined
}).

-define(RETRY_INTERVAL, 5000). %% 5 Seconds

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
    gen_statem:start_link(?MODULE, Socket, []).

set_socket(Pid, Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

%% Callbacks
init(_Args) ->
    Timer = erlang:send_after(?RETRY_INTERVAL, self(), check_acks),
    {ok, wait_for_socket, #data{retry_timer = Timer}}.

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

connected(info, {tcp, _Socket, Bin}, Data = #data{buffer = Buff}) ->
    NewBuff = <<Buff/binary, Bin/binary>>,
    process_buffer(NewBuff, Data);

connected(info, {tcp_closed, _Socket}, Data) ->
    %% io:format("Client disconnected~n"),
    {stop, normal, Data};
connected(info, {tcp_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};
connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket, user = User, pending_acks = Pending}) ->
    %% Wrap in Reliable Packet
    %% Generate unique MsgId without crypto module
    MsgId = generate_msg_id(),
    Packet = iris_proto:encode_reliable_msg(MsgId, Msg),
    
    %% Store in Pending Acks
    NewPending = maps:put(MsgId, {Msg, os:system_time(seconds), 0}, Pending),
    
    case gen_tcp:send(Socket, Packet) of
        ok -> 
            {keep_state, Data#data{pending_acks = NewPending, timeouts = 0}};
        {error, _} ->
            %% If send fails immediately, fallback to offline (let retry timer or simple fallback handle it?)
            %% Actually, if socket error, we usually die. 
            %% But if timeout, we might want to just queue it?
            %% Let's stick to simple "Store Offline if Send Fails" for now.
            logger:warning("Send failed for ~p. Storing offline.", [User]),
            iris_circuit_breaker:call(get_core_node(), iris_core, store_offline, [User, Msg]),
            {keep_state, Data}
    end;

connected(info, check_acks, Data = #data{pending_acks = Pending, user = User, retry_timer = OldTimer}) ->
    erlang:cancel_timer(OldTimer),
    Now = os:system_time(seconds),
    
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
    {keep_state, Data#data{pending_acks = NewPending, retry_timer = NewTimer}}.

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


terminate(_Reason, _State, #data{user = User}) ->
    flush_pending_msgs(User),
    iris_session:terminate(User),
    ok.

flush_pending_msgs(User) ->
    receive
        {deliver_msg, Msg} ->
            %% io:format("Terminating: Saving pending msg for ~p~n", [User]),
            rpc:call(get_core_node(), iris_core, store_offline, [User, Msg]),
            flush_pending_msgs(User)
    after 0 -> ok
    end.

code_change(_OldVsn, StateName, Data, _Extra) ->
    %% No state change needed for 0.1.0 -> 0.2.0
    %% If we changed record #data{}, we would convert it here.
    {ok, StateName, Data}.
