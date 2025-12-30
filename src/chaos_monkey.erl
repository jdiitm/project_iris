-module(chaos_monkey).
-export([start/0, start/2, stop/0, loop/2]).

%% Start with default: Kill 1 connection every 500ms
start() ->
    start(500, 1.0).

start(IntervalMs, KillCount) ->
    Pid = spawn(?MODULE, loop, [IntervalMs, KillCount]),
    register(?MODULE, Pid),
    io:format("Chaos Monkey started with interval ~p ms. Beware!~n", [IntervalMs]).

stop() ->
    ?MODULE ! stop,
    unregister(?MODULE).

loop(Interval, Count) ->
    receive
        stop -> 
            io:format("Chaos Monkey stopped.~n"),
            ok
    after Interval ->
        %% Find potential victims
        Victims = [P || P <- processes(), 
                        process_info(P, initial_call) == {iris_edge_conn, init, 1}],
        
        %% Kill random victims
        kill_random(Victims, Count),
        loop(Interval, Count)
    end.

kill_random([], _) -> ok;
kill_random(_, 0) -> ok;
kill_random(Victims, N) ->
    Len = length(Victims),
    Idx = rand:uniform(Len),
    Victim = lists:nth(Idx, Victims),
    io:format("[CHAOS] Killing process ~p~n", [Victim]),
    exit(Victim, kill),
    kill_random(lists:delete(Victim, Victims), N - 1).
