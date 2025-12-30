-module(chaos_dist).
-export([start/1]).

start(Interval) ->
    spawn(fun() -> loop(Interval) end).

loop(Interval) ->
    Wait = rand:uniform(Interval),
    timer:sleep(Wait),
    Nodes = nodes(),
    case Nodes of
        [] -> ok;
        _ ->
            Target = lists:nth(rand:uniform(length(Nodes)), Nodes),
            io:format("CHAOS: Disconnecting node ~p~n", [Target]),
            erlang:disconnect_node(Target)
    end,
    loop(Interval).
