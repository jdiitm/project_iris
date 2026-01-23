-module(iris_extreme_gen).
-export([start/3, start/2, stop/0, get_stats/0]).

%% =============================================================================
%% Extreme Load Generator for Stress Testing
%% =============================================================================
%% Creates real TCP connections to the edge node and performs logins.
%% Designed for churn testing (connect/disconnect storms) and limits testing.
%%
%% Usage:
%%   iris_extreme_gen:start(10000, 3600, normal).  %% 10k clients for 1 hour
%%   iris_extreme_gen:start(50000, 300, burst).    %% 50k burst connections
%%   iris_extreme_gen:stop().
%% =============================================================================

-define(EDGE_HOST, {127,0,0,1}).
-define(EDGE_PORT, 8085).
-define(CONNECT_TIMEOUT, 5000).
-define(BATCH_SIZE, 100).        %% Connections per batch
-define(BATCH_DELAY_MS, 50).     %% Delay between batches (rate limiting)
-define(STATS_INTERVAL, 5000).   %% Stats reporting interval

-record(state, {
    target :: integer(),
    duration :: integer(),
    mode :: normal | burst,
    clients = [] :: [pid()],
    connected = 0 :: integer(),
    failed = 0 :: integer(),
    start_time :: integer(),
    coordinator :: pid()
}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% Backwards compatible 2-arg version
start(NumClients, MsgRate) ->
    start(NumClients, MsgRate, normal).

%% Main entry point: start/3
start(NumClients, DurationSec, Mode) when is_integer(NumClients), 
                                           is_integer(DurationSec),
                                           (Mode =:= normal orelse Mode =:= burst) ->
    io:format("[extreme_gen] Starting ~p clients, duration=~ps, mode=~p~n", 
              [NumClients, DurationSec, Mode]),
    
    %% Register coordinator for stats access
    case whereis(extreme_gen_coordinator) of
        undefined -> ok;
        OldPid -> 
            exit(OldPid, shutdown),
            timer:sleep(100)
    end,
    
    Coordinator = spawn_link(fun() -> 
        coordinator_init(NumClients, DurationSec, Mode) 
    end),
    register(extreme_gen_coordinator, Coordinator),
    {ok, Coordinator}.

stop() ->
    case whereis(extreme_gen_coordinator) of
        undefined -> 
            io:format("[extreme_gen] Not running~n"),
            ok;
        Pid ->
            Pid ! stop,
            io:format("[extreme_gen] Stop signal sent~n"),
            ok
    end.

get_stats() ->
    case whereis(extreme_gen_coordinator) of
        undefined -> {error, not_running};
        Pid ->
            Pid ! {get_stats, self()},
            receive
                {stats, Stats} -> {ok, Stats}
            after 5000 ->
                {error, timeout}
            end
    end.

%% -----------------------------------------------------------------------------
%% Coordinator Process
%% -----------------------------------------------------------------------------

coordinator_init(NumClients, DurationSec, Mode) ->
    process_flag(trap_exit, true),
    StartTime = erlang:system_time(second),
    State = #state{
        target = NumClients,
        duration = DurationSec,
        mode = Mode,
        start_time = StartTime,
        coordinator = self()
    },
    
    %% Start stats reporter
    erlang:send_after(?STATS_INTERVAL, self(), report_stats),
    
    %% Start spawning clients in batches
    self() ! spawn_batch,
    
    %% Set duration timer
    erlang:send_after(DurationSec * 1000, self(), duration_expired),
    
    coordinator_loop(State).

coordinator_loop(State = #state{target = Target, connected = Connected, 
                                 failed = Failed, clients = Clients,
                                 duration = Duration, start_time = StartTime,
                                 mode = Mode}) ->
    receive
        spawn_batch when Connected + Failed < Target ->
            %% Spawn a batch of clients
            BatchSize = min(?BATCH_SIZE, Target - Connected - Failed),
            NewClients = spawn_client_batch(BatchSize, Connected, Mode),
            erlang:send_after(?BATCH_DELAY_MS, self(), spawn_batch),
            coordinator_loop(State#state{clients = NewClients ++ Clients});
        
        spawn_batch ->
            %% Target reached, no more spawning needed
            coordinator_loop(State);
        
        {client_connected, _Pid} ->
            coordinator_loop(State#state{connected = Connected + 1});
        
        {client_failed, _Pid, _Reason} ->
            coordinator_loop(State#state{failed = Failed + 1});
        
        {'EXIT', Pid, _Reason} ->
            %% Client died - remove from list, decrement connected
            NewClients = lists:delete(Pid, Clients),
            NewConnected = max(0, Connected - 1),
            coordinator_loop(State#state{clients = NewClients, connected = NewConnected});
        
        report_stats ->
            Elapsed = erlang:system_time(second) - StartTime,
            io:format("[extreme_gen] Stats: connected=~p, failed=~p, target=~p, elapsed=~ps/~ps~n",
                      [Connected, Failed, Target, Elapsed, Duration]),
            erlang:send_after(?STATS_INTERVAL, self(), report_stats),
            coordinator_loop(State);
        
        {get_stats, From} ->
            Elapsed = erlang:system_time(second) - StartTime,
            Stats = #{
                connected => Connected,
                failed => Failed,
                target => Target,
                elapsed => Elapsed,
                duration => Duration,
                mode => Mode
            },
            From ! {stats, Stats},
            coordinator_loop(State);
        
        duration_expired ->
            io:format("[extreme_gen] Duration expired, shutting down ~p clients~n", [length(Clients)]),
            shutdown_clients(Clients),
            io:format("[extreme_gen] Shutdown complete~n");
        
        stop ->
            io:format("[extreme_gen] Stop requested, shutting down ~p clients~n", [length(Clients)]),
            shutdown_clients(Clients),
            io:format("[extreme_gen] Shutdown complete~n")
    end.

shutdown_clients(Clients) ->
    %% Send stop to all clients
    [Pid ! stop || Pid <- Clients, is_process_alive(Pid)],
    %% Wait briefly for graceful shutdown
    timer:sleep(500),
    %% Force kill any remaining
    [exit(Pid, kill) || Pid <- Clients, is_process_alive(Pid)],
    ok.

%% -----------------------------------------------------------------------------
%% Client Spawning
%% -----------------------------------------------------------------------------

spawn_client_batch(Count, Offset, Mode) ->
    Coordinator = self(),
    [spawn_link(fun() -> client_start(N + Offset, Coordinator, Mode) end) 
     || N <- lists:seq(1, Count)].

client_start(Id, Coordinator, Mode) ->
    Username = list_to_binary(io_lib:format("extreme_~p_~p", [Id, erlang:system_time(microsecond) rem 1000000])),
    case connect_and_login(Username) of
        {ok, Socket} ->
            Coordinator ! {client_connected, self()},
            client_loop(Socket, Mode);
        {error, Reason} ->
            Coordinator ! {client_failed, self(), Reason}
    end.

connect_and_login(Username) ->
    case gen_tcp:connect(?EDGE_HOST, ?EDGE_PORT, 
                         [binary, {active, false}, {packet, raw}], 
                         ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            %% Send login: opcode 0x01 + username
            LoginPacket = <<16#01, Username/binary>>,
            case gen_tcp:send(Socket, LoginPacket) of
                ok ->
                    %% Wait for response (with short timeout - don't block)
                    %% For stress testing, any connection is success
                    case gen_tcp:recv(Socket, 0, 2000) of
                        {ok, _Data} ->
                            %% Got response, connection is live
                            {ok, Socket};
                        {error, timeout} ->
                            %% No response but connection established - still ok for stress test
                            {ok, Socket};
                        {error, Reason} ->
                            gen_tcp:close(Socket),
                            {error, {recv_failed, Reason}}
                    end;
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, {send_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {connect_failed, Reason}}
    end.

%% -----------------------------------------------------------------------------
%% Client Loop (maintains connection)
%% -----------------------------------------------------------------------------

client_loop(Socket, normal) ->
    %% Normal mode: keep connection alive, respond to pings
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, _Data} ->
            %% Received data, just continue
            client_loop(Socket, normal);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _Reason} ->
            ok;
        stop ->
            gen_tcp:close(Socket),
            ok
    after 30000 ->
        %% Keepalive: send a ping every 30s
        case gen_tcp:send(Socket, <<16#FF>>) of
            ok -> client_loop(Socket, normal);
            {error, _} -> ok
        end
    end;

client_loop(Socket, burst) ->
    %% Burst mode: disconnect immediately after login
    gen_tcp:close(Socket),
    ok.
