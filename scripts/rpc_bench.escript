#!/usr/bin/env escript
%%! -name rpc_bench@127.0.0.1 -setcookie iris_secret

main(_) ->
    Node = 'iris_core1@100.95.21.52',
    io:format("Benchmarking RPC to ~p...~n", [Node]),
    
    %% Warmup
    case net_adm:ping(Node) of
        pong -> ok;
        pang -> 
             io:format("Node unreachable!~n"),
             halt(1)
    end,

    %% Spawn 100 workers, each doing 100 calls
    Start = erlang:system_time(millisecond),
    Parent = self(),
    Pids = [spawn_link(fun() -> worker(Node, 100, Parent) end) || _ <- lists:seq(1, 100)],
    
    wait_workers(Pids),
    End = erlang:system_time(millisecond),
    
    TotalCalls = 100 * 100,
    Duration = (End - Start) / 1000,
    TPS = TotalCalls / Duration,
    
    io:format("Total: ~p calls in ~p sec~n", [TotalCalls, Duration]),
    io:format("Throughput: ~p TPS~n", [TPS]).

worker(_, 0, Parent) -> 
    Parent ! {done, self()};
worker(Node, N, Parent) ->
    %% Call ACTUAL DB write
    User = <<"bench_user_", (integer_to_binary(N))/binary>>,
    Msg = <<"bench_msg">>,
    rpc:call(Node, iris_core, store_offline, [User, Msg]),
    worker(Node, N - 1, Parent).

wait_workers([]) -> ok;
wait_workers(Pids) ->
    receive
        {done, Pid} -> wait_workers(Pids -- [Pid])
    end.
