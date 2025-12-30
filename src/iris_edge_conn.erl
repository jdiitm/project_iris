-module(iris_edge_conn).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([wait_for_socket/3, connected/3]).

-record(data, {
    socket :: gen_tcp:socket(),
    user :: binary()
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
    %% Now we own the socket, set active once
    inet:setopts(Socket, [{active, once}]),
    {next_state, connected, Data#data{socket = Socket}}.

%% STATE: connected
connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(info, {tcp, Socket, Bin}, Data) ->
    %% Handle Incoming Data
    case iris_proto:decode(Bin) of
        {login, User} ->
            io:format("User logged in: ~p~n", [User]),
            %% Register with Core
            rpc:call(?CORE_NODE, iris_core, register_user, [User, node(), self()]),
            %% Retrieve Offline Messages
            case rpc:call(?CORE_NODE, iris_core, retrieve_offline, [User]) of
                 Msgs when is_list(Msgs) ->
                     io:format("Retrieved ~p offline msgs for ~p~n", [length(Msgs), User]),
                     lists:foreach(fun(Msg) ->
                         io:format("Sending offline msg to client: ~p~n", [Msg]),
                         gen_tcp:send(Socket, Msg)
                     end, Msgs);
                 Error ->
                     io:format("Error retrieving offline msgs: ~p~n", [Error])
            end,
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{user = User}};
        
        {send_message, Target, Msg} ->
            io:format("Sending msg to ~p~n", [Target]),
            iris_router:route(Target, Msg),
            inet:setopts(Socket, [{active, once}]),
            keep_state_and_data;

        {ack, MsgId} ->
            io:format("Ack received: ~p~n", [MsgId]),
            inet:setopts(Socket, [{active, once}]),
            keep_state_and_data;

        Error ->
            io:format("Protocol Error: ~p~n", [Error]),
            inet:setopts(Socket, [{active, once}]),
            keep_state_and_data
    end;

connected(info, {tcp_closed, _Socket}, Data) ->
    io:format("Client disconnected~n"),
    {stop, normal, Data};

connected(info, {tcp_error, _Socket, _Reason}, Data) ->
    {stop, normal, Data};

connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket}) ->
    %% Message delivered from Router
    io:format("Delivering message to client: ~p (size ~p)~n", [Msg, byte_size(Msg)]),
    Res = gen_tcp:send(Socket, Msg),
    io:format("gen_tcp:send result: ~p~n", [Res]),
    {keep_state, Data}.

terminate(_Reason, _State, _Data) ->
    ok.
