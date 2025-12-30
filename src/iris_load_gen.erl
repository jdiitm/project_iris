-module(iris_load_gen).
-export([start/2, worker_init/3]).

%% Config
-define(TARGET_HOST, {127,0,0,1}).
-define(TARGET_PORT, 8085).
-define(TOTAL_USERS, 1000000).

%% Start Load Gen
%% Workers: Number of concurrent connections to maintain.
%% RatePerWorker: Approx msgs/sec per worker (0 = max speed).
start(Workers, DurationSec) ->
    io:format("Starting Load Gen: ~p workers, ~p seconds...~n", [Workers, DurationSec]),
    
    %% Start Stats Collector
    StatsPid = spawn(fun() -> stats_loop(0, 0) end),
    register(load_stats, StatsPid),
    
    %% Spawn Workers
    lists:foreach(fun(I) ->
        spawn(?MODULE, worker_init, [I, StatsPid, DurationSec])
    end, lists:seq(1, Workers)),
    
    %% Wait and Report
    timer:sleep(DurationSec * 1000),
    StatsPid ! report.

worker_init(Id, StatsPid, Duration) ->
    %% Connect
    case gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT, [binary, {packet, 0}, {active, false}]) of
        {ok, Sock} ->
            %% Login
            User = list_to_binary("user_" ++ integer_to_list(Id)),
            gen_tcp:send(Sock, <<1, User/binary>>),
            %% Host sends Login ACK
            case gen_tcp:recv(Sock, 0) of
                {ok, <<3, "LOGIN_OK">>} -> ok;
                _ -> exit(login_failed)
            end,
            
            EndTime = os:system_time(second) + Duration,
            worker_loop(Sock, Id, StatsPid, EndTime);
        {error, Reason} ->
            io:format("Worker ~p connect failed: ~p~n", [Id, Reason])
    end.

worker_loop(Sock, Id, StatsPid, EndTime) ->
    Now = os:system_time(second),
    if Now >= EndTime ->
        ok; %% Done
    true ->
        %% Send Message
        TargetId = rand:uniform(?TOTAL_USERS),
        Target = list_to_binary("user_" ++ integer_to_list(TargetId)),
        Msg = <<"ChaosPayload">>,
        
        %% Framing: 0x02 | TLen(16) | Target | MLen(16) | Msg
        TLen = byte_size(Target),
        MLen = byte_size(Msg),
        Packet = <<2, TLen:16, Target/binary, MLen:16, Msg/binary>>,
        
        case gen_tcp:send(Sock, Packet) of
            ok ->
                StatsPid ! {add, 1},
                worker_loop(Sock, Id, StatsPid, EndTime);
            {error, _} ->
                %% Connection died (Chaos Monkey?), reconnect?
                %% For now just die, simulating user dropping.
                exit(connection_closed)
        end
    end.

stats_loop(Count, StartTime) ->
    case StartTime of
        0 -> 
             %% First update sets start time
             receive 
                {add, N} -> stats_loop(Count + N, os:system_time(millisecond)); 
                report -> io:format("No activity.~n")
             end;
        _ ->
            receive
                {add, N} -> 
                    stats_loop(Count + N, StartTime);
                report ->
                    End = os:system_time(millisecond),
                    DiffSec = (End - StartTime) / 1000.0,
                    Rate = Count / DiffSec,
                    io:format("--- LOAD TEST RESULTS ---~n"),
                    io:format("Total Messages: ~p~n", [Count]),
                    io:format("Duration: ~p s~n", [DiffSec]),
                    io:format("Throughput: ~p msgs/sec~n", [Rate])
            end
    end.
