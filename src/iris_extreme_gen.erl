-module(iris_extreme_gen).
-export([start/2, worker_init/4]).

-define(TARGET_HOST, {127,0,0,1}).
-define(TARGET_PORT, 8085).
-define(TOTAL_USERS, 1000000).

%% Start 800k connections
%% Count: Total connections (e.g., 800000)
%% Duration: Test duration in seconds
start(Count, Duration) ->
    io:format("Starting EXTREME Load Gen: ~p connections...~n", [Count]),
    StatsPid = spawn(fun() -> stats_loop(0, 0) end),
    register(extreme_stats, StatsPid),
    
    %% batch spawn to avoid clogging scheduler immediately
    spawn(fun() -> batch_spawn(Count, StatsPid, Duration) end).

batch_spawn(0, _, _) -> io:format("All workers spawned.~n");
batch_spawn(N, StatsPid, Duration) ->
    spawn(?MODULE, worker_init, [N, StatsPid, Duration, (N rem 20) + 1]),
    if N rem 1000 == 0 -> timer:sleep(100); true -> ok end, %% 10k/sec ramp up
    batch_spawn(N-1, StatsPid, Duration).

worker_init(Id, StatsPid, Duration, IpOffset) ->
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
                    worker_loop(Sock, Id, StatsPid, EndTime);
                _ -> exit(login_failed)
            end;
        {error, _} -> 
            %% Retry or die? For exact count we should retry, but for chaos, dying is fine.
            exit(connect_failed)
    end.

worker_loop(Sock, Id, StatsPid, EndTime) ->
    Now = os:system_time(second),
    if Now >= EndTime -> ok;
    true ->
        TargetId = rand:uniform(?TOTAL_USERS),
        Target = list_to_binary("user_" ++ integer_to_list(TargetId)),
        Msg = <<"X">>,
        Packet = <<2, (byte_size(Target)):16, Target/binary, (byte_size(Msg)):16, Msg/binary>>,
        
        case gen_tcp:send(Sock, Packet) of
            ok ->
                StatsPid ! {add, 1},
                timer:sleep(1000), %% 1 msg/sec per user = 800k msgs/sec total at peak
                worker_loop(Sock, Id, StatsPid, EndTime);
            {error, _} -> exit(conn_closed)
        end
    end.

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
