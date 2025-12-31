-module(iris_edge_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    lsock :: gen_tcp:socket()
}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    %% Tuning: High backlog for burst connections
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, 4096}]),
    io:format("Edge Listener started on port ~p (Pool Size: 100)~n", [Port]),
    %% Tuning: Spawn 100 parallel acceptors
    [spawn_acceptor(LSock) || _ <- lists:seq(1, 100)],
    {ok, #state{lsock = LSock}}.

spawn_acceptor(LSock) ->
    spawn_link(fun() -> acceptor(LSock) end).

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            %% Handover to stats machine
            {ok, Pid} = iris_edge_conn:start_link(Sock),
            gen_tcp:controlling_process(Sock, Pid),
            %% Tell conn it's ready
            iris_edge_conn:set_socket(Pid, Sock),
            %% Loop for next connection
            acceptor(LSock);
        Error ->
            io:format("Accept Error: ~p~n", [Error])
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
