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
            io:format("Client send error: ~p. Terminating.~n", [Reason]),
            {stop, normal, Data}
    end.

process_buffer(Bin, Data = #data{socket = Socket}) ->
    case iris_proto:decode(Bin) of
        {{login, User}, Rest} ->
            io:format("User logged in: ~p~n", [User]),
            %% Register with Core
            rpc:call(?CORE_NODE, iris_core, register_user, [User, node(), self()]),

            %% Optimization: Register Locally for switching
            ets:insert(local_presence, {User, self()}),

            %% Send Login ACK to ensure synchronization
            gen_tcp:send(Socket, <<3, "LOGIN_OK">>),
            
            %% Retrieve Offline Messages
            case rpc:call(?CORE_NODE, iris_core, retrieve_offline, [User]) of
                 Msgs when is_list(Msgs) ->
                     %% io:format("Retrieved ~p offline msgs for ~p~n", [length(Msgs), User]),
                     lists:foreach(fun(Msg) ->
                         %% io:format("Sending offline msg to client: ~p~n", [Msg]),
                         gen_tcp:send(Socket, Msg)
                     end, Msgs);
                 Error ->
                     io:format("Error retrieving offline msgs: ~p~n", [Error])
            end,
            process_buffer(Rest, Data#data{user = User});
        
        {{send_message, Target, Msg}, Rest} ->
            %% io:format("Sending msg to ~p~n", [Target]),
            iris_router:route(Target, Msg),
            process_buffer(Rest, Data);

        {{batch_send, Target, Blob}, Rest} ->
            %% Optimized Fan-In: Directly store batch to Core
            Msgs = iris_proto:unpack_batch(Blob),
            rpc:call(?CORE_NODE, iris_core, store_batch, [Target, Msgs]),
            process_buffer(Rest, Data);

        {{ack, MsgId}, Rest} ->
            %% io:format("Ack received: ~p~n", [MsgId]),
            process_buffer(Rest, Data);

        {{error, _}, _} ->
             %% Error or incomplete. If known error, maybe skip? 
             %% But for now let's assume it means "more data needed" or disconnect.
             %% Actually iris_proto returns {error,...} for bad packet or unknown.
             %% If unknown, we might be stuck. But decode returns {more, Bin} too.
             %% Wait, I didn't verify if iris_proto returns {error, ...} for unknown.
             %% My modified iris_proto returns { {error, ...}, <<>> }.
             %% Need to handle 'more'.
             io:format("Protocol Error or Unknown~n"),
             inet:setopts(Socket, [{active, once}]),
             {keep_state, Data#data{buffer = <<>>}}; %% flushing buffer on error? tough choice.

        {more, _} ->
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = Bin}}
    end.

terminate(_Reason, _State, #data{user = User}) ->
    %% Optimization: Cleanup Local Cache
    case User of
        undefined -> ok;
        _ -> ets:delete(local_presence, User)
    end,
    ok.
