-module(iris_verification_gen).
-export([start/3, worker_init/5]).

-define(TARGET_HOST, {127,0,0,1}).
-define(TARGET_PORT, 8085).

%% Mode: verify_cleanup
start(Count, Duration, _Mode) ->
    io:format("Starting VERIFICATION Gen: ~p users...~n", [Count]),
    StatsPid = spawn(fun() -> stats_loop(0, 0, 0) end),
    register(verif_stats, StatsPid),
    spawn(fun() -> batch_spawn(Count, StatsPid, Duration) end).

batch_spawn(0, _, _) -> io:format("All verifiers spawned.~n");
batch_spawn(N, StatsPid, Duration) ->
    spawn(?MODULE, worker_init, [N, StatsPid, Duration, (N rem 50) + 1, verify]),
    if N rem 2000 == 0 -> timer:sleep(50); true -> ok end, %% Throttle slightly
    batch_spawn(N-1, StatsPid, Duration).

worker_init(Id, StatsPid, _Duration, IpOffset, verify) ->
    SrcIp = {127,0,0, IpOffset},
    Opts = [binary, {packet, 0}, {active, false}, {ip, SrcIp}, {reuseaddr, true}],
    User = list_to_binary("user_" ++ integer_to_list(Id)),

    %% STEP 1: Connect & Consume
    case gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT, Opts) of
        {ok, Sock1} ->
            gen_tcp:send(Sock1, <<1, User/binary>>), %% Login
            case gen_tcp:recv(Sock1, 0) of
                {ok, <<3, "LOGIN_OK">>} ->
                    consume_all(Sock1),
                    gen_tcp:close(Sock1),
                    
                    %% STEP 2: Re-Connect & Verify
                    timer:sleep(100), %% Short pause
                    case gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT, Opts) of
                        {ok, Sock2} ->
                            gen_tcp:send(Sock2, <<1, User/binary>>), %% Login
                            case gen_tcp:recv(Sock2, 0) of
                                {ok, <<3, "LOGIN_OK">>} ->
                                    verify_empty(Sock2, StatsPid),
                                    gen_tcp:close(Sock2);
                                _ -> StatsPid ! {fail, login2}
                            end;
                        _ -> StatsPid ! {fail, connect2}
                    end;
                _ -> StatsPid ! {fail, login1}
            end;
        {error, _} -> 
            StatsPid ! {fail, connect1}
    end.

consume_all(Sock) ->
    %% Read with timeout until empty
    case gen_tcp:recv(Sock, 0, 500) of
        {ok, _} -> consume_all(Sock);
        {error, timeout} -> ok;
        {error, _} -> ok
    end.

verify_empty(Sock, StatsPid) ->
    %% Expect NO data
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> 
            io:format("FAILURE: User received data on verification: ~p~n", [Data]),
            StatsPid ! {fail, not_empty};
        {error, timeout} -> 
            StatsPid ! success;
        {error, closed} ->
            StatsPid ! {fail, closed_prematurely}
    end.

stats_loop(Success, Fail, Total) ->
    receive 
        success -> 
            NewSuc = Success + 1,
            if (NewSuc rem 1000) == 0 -> io:format("Verified: ~p, Fail: ~p~n", [NewSuc, Fail]); true -> ok end,
            stats_loop(NewSuc, Fail, Total + 1);
        {fail, Reason} ->
            io:format("Verif FAIL: ~p~n", [Reason]),
            stats_loop(Success, Fail + 1, Total + 1);
        report ->
            io:format("--- VERIFICATION RESULTS ---~nSuccess: ~p~nFail: ~p~nTotal: ~p~n", [Success, Fail, Total])
    end.
