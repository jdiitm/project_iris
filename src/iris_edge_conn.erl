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

-define(CORE_NODE, core_node()).

core_node() ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom("iris_core@" ++ Host).

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
    io:format("Client disconnected~n"),
    {stop, normal, Data};
connected(info, {tcp_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};
connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket, user = User, pending_acks = Pending}) ->
    %% Wrap in Reliable Packet
    MsgId = crypto:strong_rand_bytes(16),
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
            iris_circuit_breaker:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
            {keep_state, Data}
    end;

connected(info, check_acks, Data = #data{pending_acks = Pending, user = User, retry_timer = OldTimer}) ->
    erlang:cancel_timer(OldTimer),
    Now = os:system_time(seconds),
    
    %% Scan for expired ACKs ( > 10 seconds)
    NewPending = maps:filter(fun(MsgId, {Msg, Ts, _Retries}) ->
        if (Now - Ts) > 10 ->
            logger:warning("Msg ~p timed out (No ACK). Moving to offline storage.", [MsgId]),
            iris_circuit_breaker:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
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
                ({send, Msg}, D) -> gen_tcp:send(Socket, Msg), D;
                ({send_batch, Msgs}, D) -> [gen_tcp:send(Socket, M) || M <- Msgs], D;
                ({ack_received, MsgId}, D = #data{pending_acks = P}) -> 
                    %% Remove from pending
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
            io:format("Terminating: Saving pending msg for ~p~n", [User]),
            rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
            flush_pending_msgs(User)
    after 0 -> ok
    end.

code_change(_OldVsn, StateName, Data, _Extra) ->
    %% No state change needed for 0.1.0 -> 0.2.0
    %% If we changed record #data{}, we would convert it here.
    {ok, StateName, Data}.
