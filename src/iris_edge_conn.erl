-module(iris_edge_conn).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([wait_for_socket/3, connected/3]).

-record(data, {
    socket :: gen_tcp:socket(),
    user :: binary(),
    buffer = <<>> :: binary(),
    timeouts = 0 :: integer()
}).

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
    {ok, wait_for_socket, #data{}}.

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
connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket, user = User, timeouts = T}) ->
    %% Message delivered from Router
    %% io:format("Delivering message to client: ~p (size ~p)~n", [Msg, byte_size(Msg)]),
    case gen_tcp:send(Socket, Msg) of
        ok -> 
            {keep_state, Data#data{timeouts = 0}}; %% Reset timeouts on success
        {error, timeout} ->
            if T < 5 ->
                io:format("WARNING: Client slow. Fallback to Offline Storage. (Streak: ~p)~n", [T+1]),
                %% 1. Store Offline via Core (handles bucketing)
                rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
                %% 2. Keep Connection
                {keep_state, Data#data{timeouts = T + 1}};
            true ->
                io:format("CRITICAL: Client zombie (5 timeouts). Terminating.~n"),
                {stop, normal, Data}
            end;
        {error, enobufs} ->
             %% If buffer immediately full, treat as timeout/slow
             if T < 5 ->
                io:format("WARNING: Client buffer full. Fallback to Offline. (Streak: ~p)~n", [T+1]),
                rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
                {keep_state, Data#data{timeouts = T + 1}};
             true -> 
                io:format("CRITICAL: Client buffer zombie. Terminating.~n"),
                {stop, normal, Data}
             end;
        {error, Reason} ->
            io:format("Client send error: ~p. Storing offline.~n", [Reason]),
            rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]),
            {stop, normal, Data}
    end.

process_buffer(Bin, Data = #data{socket = Socket, user = CurrentUser}) ->
    case iris_proto:decode(Bin) of
        {more, _} ->
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = Bin}};

        {Packet, Rest} ->
            %% Delegate to Logic Module
            {ok, NewUser, Actions} = iris_session:handle_packet(Packet, CurrentUser, self(), ?MODULE),
            
            %% Execute Actions
            lists:foreach(fun
                ({send, Msg}) -> gen_tcp:send(Socket, Msg);
                ({send_batch, Msgs}) -> [gen_tcp:send(Socket, M) || M <- Msgs];
                (close) -> gen_statem:stop({shutdown, closed})
            end, Actions),
            
            process_buffer(Rest, Data#data{user = NewUser})
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
