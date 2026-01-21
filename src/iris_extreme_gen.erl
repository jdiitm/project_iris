-module(iris_extreme_gen).
-export([start/2, stop/0]).

%% Stub module for stress testing
%% Generates extreme load patterns for churn testing

start(NumClients, MsgRate) ->
    io:format("[extreme_gen] Starting with ~p clients, ~p msg/s~n", [NumClients, MsgRate]),
    {ok, spawn(fun() -> gen_loop(NumClients, MsgRate) end)}.

stop() ->
    io:format("[extreme_gen] Stopped~n"),
    ok.

gen_loop(_, _) ->
    receive
        stop -> ok
    after 1000 ->
        gen_loop(0, 0)
    end.
