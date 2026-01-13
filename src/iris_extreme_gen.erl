-module(iris_extreme_gen).
-export([start/3, start/5, worker_init/8, get_stats/0, get_latency_stats/0]).

-define(TARGET_HOST, {127,0,0,1}).
-define(TARGET_PORT, 8080).
-define(PENDING_MSGS, pending_msgs_ets).
-define(LATENCY_SAMPLES, latency_samples_ets).

%% =============================================================================
%% API - Stats Collection
%% =============================================================================

%% Get basic send/recv counts
get_stats() ->
    Ref = make_ref(),
    extreme_stats ! {get, self(), Ref},
    receive
        {stats, Ref, Sent, Recv} -> {ok, Sent, Recv}
    after 1000 ->
        {error, timeout}
    end.

%% Get latency percentiles (P50, P95, P99, max, avg)
get_latency_stats() ->
    Ref = make_ref(),
    extreme_stats ! {get_latency, self(), Ref},
    receive
        {latency_stats, Ref, Stats} -> {ok, Stats}
    after 1000 ->
        {error, timeout}
    end.

%% =============================================================================
%% Start Functions
%% =============================================================================

start(Count, Duration, Mode) ->
    start(Count, Duration, Mode, ?TARGET_HOST, ?TARGET_PORT).

start(Count, Duration, Mode, Host, Port) ->
    io:format("Starting EXTREME Load Gen (~p): ~p connections to ~p:~p...~n", [Mode, Count, Host, Port]),
    
    %% Create ETS tables for latency tracking
    catch ets:delete(?PENDING_MSGS),
    catch ets:delete(?LATENCY_SAMPLES),
    ets:new(?PENDING_MSGS, [named_table, public, set, {write_concurrency, true}]),
    ets:new(?LATENCY_SAMPLES, [named_table, public, ordered_set]),
    
    StatsPid = spawn(fun() -> stats_loop(0, 0, []) end),
    catch unregister(extreme_stats),
    register(extreme_stats, StatsPid),
    
    %% Optimization: Spawn in batches to reach 100k faster
    spawn(fun() ->
        spawn_workers(Count, Count, StatsPid, Duration, Mode, Host, Port)
    end).

spawn_workers(0, _MaxID, _StatsPid, _Duration, _Mode, _Host, _Port) -> ok;
spawn_workers(N, MaxID, StatsPid, Duration, Mode, Host, Port) ->
    BatchSize = 100,
    CurrentBatch = min(N, BatchSize),
    
    lists:foreach(fun(I) ->
        Id = MaxID - N + I,
        spawn(?MODULE, worker_init, [Id, StatsPid, Duration, (Id rem 20) + 1, Mode, Host, Port, MaxID])
    end, lists:seq(1, CurrentBatch)),
    
    timer:sleep(10), %% 10ms delay between batches of 100 = 10k/sec startup rate
    spawn_workers(N - CurrentBatch, MaxID, StatsPid, Duration, Mode, Host, Port).


%% =============================================================================
%% Worker
%% =============================================================================

worker_init(Id, StatsPid, Duration, _IpOffset, Mode, Host, Port, MaxID) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    
    case gen_tcp:connect(Host, Port, Opts, 5000) of
        {ok, Sock} ->
            User = list_to_binary("user_" ++ integer_to_list(Id)),
            gen_tcp:send(Sock, <<1, User/binary>>),
            case gen_tcp:recv(Sock, 0, 60000) of
                {ok, Data} when byte_size(Data) >= 8 -> 
                    case binary:match(Data, <<"LOGIN_OK">>) of
                        nomatch ->
                            io:format("Worker ~p: Unexpected login response: ~p~n", [Id, Data]),
                            exit(login_failed);
                        _ ->
                            EndTime = os:system_time(second) + Duration,
                            case Mode of
                                normal ->
                                    worker_loop(Sock, Id, StatsPid, EndTime);
                                extreme_load ->
                                    erlang:send_after(rand:uniform(1000), self(), trigger_burst),
                                    inet:setopts(Sock, [{active, once}]),
                                    duplex_loop(Sock, Id, StatsPid, EndTime, MaxID);
                                fan_in ->
                                    erlang:send_after(rand:uniform(1000), self(), trigger_burst),
                                    inet:setopts(Sock, [{active, once}]),
                                    fan_in_loop(Sock, Id, StatsPid, EndTime)
                            end
                    end;
                {ok, Data} ->
                    io:format("Worker ~p: Short login response: ~p~n", [Id, Data]),
                    exit(login_failed);
                {error, Reason} ->  
                    io:format("Worker ~p: Login recv error: ~p~n", [Id, Reason]),
                    exit(login_failed)
            end;
        {error, Reason} -> 
            % io:format("Worker ~p: Connect failed: ~p~n", [Id, Reason]),
            exit(connect_failed)
    end.

worker_loop(Sock, Id, StatsPid, EndTime) ->
    receive
        {tcp, Sock, _Data} ->
            %% Just keep connection open and count headers if needed
            %% For now, just drain buffer
            inet:setopts(Sock, [{active, once}]),
            worker_loop(Sock, Id, StatsPid, EndTime);
        {tcp_closed, Sock} ->
            ok;
        _ ->
            worker_loop(Sock, Id, StatsPid, EndTime)
    after 1000 ->
        %% Check time
        Now = os:system_time(second),
        if Now >= EndTime -> ok;
        true -> worker_loop(Sock, Id, StatsPid, EndTime)
        end
    end.

%% =============================================================================
%% Duplex Loop with Latency Tracking
%% =============================================================================

duplex_loop(Sock, Id, StatsPid, EndTime, MaxID) ->
    receive
        {tcp, Sock, Data} ->
            handle_incoming(Sock, Data, StatsPid),
            inet:setopts(Sock, [{active, once}]),
            duplex_loop(Sock, Id, StatsPid, EndTime, MaxID);
            
        {tcp_closed, Sock} ->
            exit(normal);
            
        trigger_burst ->
             Now = os:system_time(second),
             if Now >= EndTime -> ok;
             true ->
                 %% Send Burst of 5 messages with timestamps
                 %% FIX: Only target users that have likely started (1..MyId)
                 %% This avoids "Cold Start Misses" where User 1 messages User 2500 (who is offline)
                 [send_msg_with_timestamp(Sock, rand:uniform(Id), StatsPid) || _ <- lists:seq(1, 5)],
                 erlang:send_after(10000 + rand:uniform(50000), self(), trigger_burst),
                 duplex_loop(Sock, Id, StatsPid, EndTime, MaxID)
             end
    end.

%% =============================================================================
%% Fan-In Loop (Messi Problem)
%% =============================================================================

fan_in_loop(Sock, Id, StatsPid, EndTime) ->
    receive
        {tcp, Sock, Data} ->
            handle_incoming(Sock, Data, StatsPid),
            inet:setopts(Sock, [{active, once}]),
            fan_in_loop(Sock, Id, StatsPid, EndTime);
            
        {tcp_closed, Sock} ->
            exit(normal);
            
        trigger_burst ->
             Now = os:system_time(second),
             if Now >= EndTime -> ok;
             true ->
                 %% Send 1 message to vip_messi
                 Target = <<"vip_messi">>,
                 Msg = <<"GOAL">>,
                 Packet = <<2, (byte_size(Target)):16, Target/binary, (byte_size(Msg)):16, Msg/binary>>,
                 gen_tcp:send(Sock, Packet),
                 StatsPid ! {add, 1, 0},
                 
                 %% Reduced frequency: 1 msg every 30-60s per user
                 %% 100k users = ~2000 msgs/sec
                 erlang:send_after(30000 + rand:uniform(30000), self(), trigger_burst),
                 fan_in_loop(Sock, Id, StatsPid, EndTime)
             end
    end.

%% Parse incoming and calculate latency
%% NOTE: We can't track individual message latency because the MsgId changes
%% when routed through the system. Instead, we track approximate latency
%% based on when we receive ANY message relative to our recent sends.
handle_incoming(Sock, <<16, IdLen:16, MsgId:IdLen/binary, _MsgLen:32, _Msg/binary>>, StatsPid) ->
    %% Reliable message received - send ACK
    gen_tcp:send(Sock, <<3, MsgId/binary>>),
    RecvTime = erlang:monotonic_time(microsecond),
    
    %% Report receive time to stats for latency calculation
    StatsPid ! {message_received, RecvTime},
    StatsPid ! {add, 0, 1};

handle_incoming(_Sock, _Data, StatsPid) ->
    StatsPid ! {add, 0, 1}.

%% Send message and store timestamp for latency measurement
send_msg_with_timestamp(Sock, TargetId, StatsPid) ->
    Target = list_to_binary("user_" ++ integer_to_list(TargetId)),
    Msg = <<"X">>,
    
    %% Generate a unique message ID for tracking
    MsgId = <<(erlang:monotonic_time()):64, (erlang:unique_integer()):32>>,
    SendTime = erlang:monotonic_time(microsecond),
    
    %% Store send timestamp
    ets:insert(?PENDING_MSGS, {MsgId, SendTime}),
    
    Packet = <<2, (byte_size(Target)):16, Target/binary, (byte_size(Msg)):16, Msg/binary>>,
    gen_tcp:send(Sock, Packet),
    StatsPid ! {add, 1, 0}.

%% =============================================================================
%% Stats Loop with Latency Collection
%% =============================================================================

stats_loop(Sent, Recv, Latencies) ->
    receive 
        {add, S, R} -> 
            stats_loop(Sent + S, Recv + R, Latencies);
        
        {latency, L} ->
            %% Keep last 10000 samples for percentile calculation
            NewLatencies = if length(Latencies) >= 10000 ->
                lists:sublist([L | Latencies], 10000);
            true ->
                [L | Latencies]
            end,
            stats_loop(Sent, Recv, NewLatencies);
        
        {message_received, RecvTime} ->
            %% Calculate latency from ETS send timestamps
            %% Look for any pending message and use its send time
            case ets:first(pending_msgs_ets) of
                '$end_of_table' -> 
                    stats_loop(Sent, Recv, Latencies);
                Key ->
                    case ets:lookup(pending_msgs_ets, Key) of
                        [{Key, SendTime}] ->
                            Latency = RecvTime - SendTime,
                            ets:delete(pending_msgs_ets, Key),
                            NewLatencies = if length(Latencies) >= 10000 ->
                                lists:sublist([Latency | Latencies], 10000);
                            true ->
                                [Latency | Latencies]
                            end,
                            stats_loop(Sent, Recv, NewLatencies);
                        [] ->
                            stats_loop(Sent, Recv, Latencies)
                    end
            end;
        
        {get, Pid, Ref} ->
            Pid ! {stats, Ref, Sent, Recv},
            stats_loop(Sent, Recv, Latencies);
        
        {get_latency, Pid, Ref} ->
            Stats = calculate_latency_stats(Latencies),
            Pid ! {latency_stats, Ref, Stats},
            stats_loop(Sent, Recv, Latencies);
        
        report ->
            io:format("STATS: Sent=~p, Recv=~p, Delta=~p~n", [Sent, Recv, Sent - Recv]),
            stats_loop(Sent, Recv, Latencies)
    end.

%% Calculate P50, P95, P99, max, avg from latency samples
calculate_latency_stats([]) ->
    #{p50 => 0, p95 => 0, p99 => 0, max => 0, avg => 0, samples => 0};
calculate_latency_stats(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50Idx = max(1, round(Len * 0.50)),
    P95Idx = max(1, round(Len * 0.95)),
    P99Idx = max(1, round(Len * 0.99)),
    
    #{
        p50 => lists:nth(P50Idx, Sorted) div 1000,  % Convert to ms
        p95 => lists:nth(P95Idx, Sorted) div 1000,
        p99 => lists:nth(P99Idx, Sorted) div 1000,
        max => lists:last(Sorted) div 1000,
        avg => (lists:sum(Latencies) div Len) div 1000,
        samples => Len
    }.
