-module(iris_edge_listener).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    lsock :: gen_tcp:socket(),
    handler :: atom()
}).

start_link(Port) ->
    start_link(Port, iris_edge_conn).

start_link(Port, HandlerMod) ->
    %% Name must be unique if running multiple listeners
    Name = list_to_atom("iris_edge_listener_" ++ integer_to_list(Port)),
    gen_server:start_link({local, Name}, ?MODULE, [Port, HandlerMod], []).

init([Port, HandlerMod]) ->
    %% Tuning: High backlog for burst connections
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, 4096}]),
    io:format("Listener started on port ~p (Handler: ~p)~n", [Port, HandlerMod]),
    %% Tuning: Spawn 100 parallel acceptors
    [spawn_acceptor(LSock, HandlerMod) || _ <- lists:seq(1, 100)],
    {ok, #state{lsock = LSock, handler = HandlerMod}}.

spawn_acceptor(LSock, HandlerMod) ->
    spawn_link(fun() -> acceptor(LSock, HandlerMod) end).

acceptor(LSock, HandlerMod) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            %% Handover to stats machine
            {ok, Pid} = HandlerMod:start_link(Sock),
            gen_tcp:controlling_process(Sock, Pid),
            %% Tell conn it's ready
            HandlerMod:set_socket(Pid, Sock),
            %% Loop for next connection
            acceptor(LSock, HandlerMod);
        Error ->
            io:format("Accept Error: ~p~n", [Error])
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
