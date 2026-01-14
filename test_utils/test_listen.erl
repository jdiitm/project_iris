-module(test_listen).
-export([start/0]).

start() ->
    Port = 8000,
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    io:format("Attempting listen on ~p with options ~p~n", [Port, Options]),
    case gen_tcp:listen(Port, Options) of
        {ok, Sock} -> 
            io:format("Success: ~p~n", [Sock]),
            gen_tcp:close(Sock);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.
