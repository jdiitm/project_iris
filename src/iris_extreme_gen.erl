-module(iris_extreme_gen).
-export([start/3, worker_init/5]).

-define(TARGET_HOST, {127,0,0,1}).
-define(TARGET_PORT, 8085).
-define(TOTAL_USERS, 1000000).

%% Mode: normal | slow_consumer | offline_flood
start(Count, Duration, Mode) ->
    io:format("Starting EXTREME Load Gen (~p): ~p connections...~n", [Mode, Count]),
    StatsPid = spawn(fun() -> stats_loop(0, 0) end),
    register(extreme_stats, StatsPid),
    
    spawn(fun() -> batch_spawn(Count, StatsPid, Duration, Mode) end).

batch_spawn(0, _, _, _) -> io:format("All workers spawned.~n");
batch_spawn(N, StatsPid, Duration, Mode) ->
    spawn(?MODULE, worker_init, [N, StatsPid, Duration, (N rem 20) + 1, Mode]),
    if N rem 1000 == 0 -> timer:sleep(100); true -> ok end, 
    batch_spawn(N-1, StatsPid, Duration, Mode).

worker_init(Id, StatsPid, Duration, IpOffset, Mode) ->
    SrcIp = {127,0,0, IpOffset},
    Opts = [binary, {packet, 0}, {active, false}, {ip, SrcIp}, {reuseaddr, true}],
    
    case gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT, Opts) of
        {ok, Sock} ->
            %% Login
            User = list_to_binary("user_" ++ integer_to_list(Id)),
            gen_tcp:send(Sock, <<1, User/binary>>),
            case gen_tcp:recv(Sock, 0) of
                {ok, <<3, "LOGIN_OK">>} -> 
                    EndTime = os:system_time(second) + Duration,
                    case Mode of
                        slow_consumer -> 
                            %% In slow consumer mode, we DO NOT read from socket.
                            %% causing server-side TCP buffers to fill.
                            flood_loop(Sock, Id, StatsPid, EndTime, 1000000); %% Fast flood
                        normal ->
                            worker_loop(Sock, Id, StatsPid, EndTime);
                        offline_flood ->
                            %% Flood messages to users likely offline (high IDs)
                            flood_loop(Sock, Id, StatsPid, EndTime, 1000000)
                    end;
                _ -> exit(login_failed)
            end;
        {error, _} -> 
            exit(connect_failed)
    end.

worker_loop(Sock, Id, StatsPid, EndTime) ->
    Now = os:system_time(second),
    if Now >= EndTime -> ok;
    true ->
        %% Read any incoming (ACKs/Messages) to keep buffer clear
        gen_tcp:recv(Sock, 0, 0), 
        
        TargetId = rand:uniform(?TOTAL_USERS),
        send_msg(Sock, TargetId, StatsPid),
        worker_loop(Sock, Id, StatsPid, EndTime)
    end.

flood_loop(Sock, Id, StatsPid, EndTime, RangeStart) ->
    Now = os:system_time(second),
    if Now >= EndTime -> ok;
    true ->
        %% DO NOT READ in slow_consumer mode
        %% Target arbitrary user
        TargetId = rand:uniform(?TOTAL_USERS), 
        send_msg(Sock, TargetId, StatsPid),
        %% Sleep small to allow CPU for others, but fast enough to flood
        timer:sleep(10), 
        flood_loop(Sock, Id, StatsPid, EndTime, RangeStart)
    end.

send_msg(Sock, TargetId, StatsPid) ->
    Target = list_to_binary("user_" ++ integer_to_list(TargetId)),
    Msg = <<"X">>,
    Packet = <<2, (byte_size(Target)):16, Target/binary, (byte_size(Msg)):16, Msg/binary>>,
    gen_tcp:send(Sock, Packet),
    StatsPid ! {add, 1}.

stats_loop(Count, StartTime) ->
    receive 
        {add, N} -> 
            NewStart = if StartTime == 0 -> os:system_time(millisecond); true -> StartTime end,
            stats_loop(Count + N, NewStart);
        report ->
            End = os:system_time(millisecond),
            Diff = (End - StartTime) / 1000.0,
            io:format("Total: ~p, Secs: ~p, Rate: ~p/sec~n", [Count, Diff, Count/Diff])
    end.
