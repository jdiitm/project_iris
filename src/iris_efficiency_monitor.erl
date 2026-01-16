-module(iris_efficiency_monitor).
-behaviour(gen_server).

%% =============================================================================
%% Hardware Efficiency Monitor
%% =============================================================================
%% Purpose: Monitor and report on system efficiency metrics.
%% Design:
%% 1. Track CPU, memory, network, and scheduler utilization
%% 2. Calculate efficiency scores and recommendations
%% 3. Detect resource waste and optimization opportunities
%% =============================================================================

-export([start_link/0]).
-export([get_metrics/0, get_efficiency_score/0]).
-export([get_scheduler_stats/0, get_memory_stats/0]).
-export([get_process_limit_usage/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(SAMPLE_INTERVAL_MS, 5000).

-record(state, {
    samples = [] :: list(),
    max_samples = 60 :: integer(),  %% 5 min of history at 5s interval
    
    %% Cumulative stats
    total_scheduler_wall = 0 :: integer(),
    total_scheduler_active = 0 :: integer(),
    
    %% Connection tracking
    peak_connections = 0 :: integer(),
    current_connections = 0 :: integer()
}).

-record(sample, {
    timestamp :: integer(),
    memory :: map(),
    scheduler_util :: list(),
    process_count :: integer(),
    port_count :: integer(),
    reductions :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get current efficiency metrics
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%% @doc Get overall efficiency score (0-100)
-spec get_efficiency_score() -> integer().
get_efficiency_score() ->
    gen_server:call(?SERVER, get_efficiency_score).

%% @doc Get scheduler utilization stats
get_scheduler_stats() ->
    gen_server:call(?SERVER, get_scheduler_stats).

%% @doc Get memory utilization stats
get_memory_stats() ->
    gen_server:call(?SERVER, get_memory_stats).

%% @doc Get process/port limit usage
get_process_limit_usage() ->
    gen_server:call(?SERVER, get_process_limit_usage).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Enable scheduler wall time for utilization tracking
    erlang:system_flag(scheduler_wall_time, true),
    
    %% Start sampling timer
    erlang:send_after(?SAMPLE_INTERVAL_MS, self(), sample),
    
    {ok, #state{}}.

handle_call(get_metrics, _From, State) ->
    Metrics = build_metrics(State),
    {reply, Metrics, State};

handle_call(get_efficiency_score, _From, State) ->
    Score = calculate_efficiency_score(State),
    {reply, Score, State};

handle_call(get_scheduler_stats, _From, State) ->
    Stats = get_scheduler_utilization(),
    {reply, Stats, State};

handle_call(get_memory_stats, _From, State) ->
    Stats = build_memory_stats(),
    {reply, Stats, State};

handle_call(get_process_limit_usage, _From, State) ->
    Stats = build_process_stats(),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sample, State) ->
    NewState = take_sample(State),
    erlang:send_after(?SAMPLE_INTERVAL_MS, self(), sample),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    erlang:system_flag(scheduler_wall_time, false),
    ok.

%% =============================================================================
%% Internal: Sampling
%% =============================================================================

take_sample(State = #state{samples = Samples, max_samples = MaxSamples}) ->
    Sample = #sample{
        timestamp = os:system_time(millisecond),
        memory = erlang:memory(),
        scheduler_util = scheduler_wall_time_snapshot(),
        process_count = erlang:system_info(process_count),
        port_count = erlang:system_info(port_count),
        reductions = total_reductions()
    },
    
    %% Update connection tracking
    ProcCount = Sample#sample.process_count,
    NewPeak = max(State#state.peak_connections, ProcCount),
    
    %% Keep only recent samples
    NewSamples = lists:sublist([Sample | Samples], MaxSamples),
    
    State#state{
        samples = NewSamples,
        peak_connections = NewPeak,
        current_connections = ProcCount
    }.

scheduler_wall_time_snapshot() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined -> [];
        WT -> WT
    end.

total_reductions() ->
    lists:foldl(fun({reductions, R}, Acc) -> R + Acc end, 0,
                [process_info(P, reductions) || P <- erlang:processes(),
                                                 process_info(P, reductions) =/= undefined]).

%% =============================================================================
%% Internal: Metrics Building
%% =============================================================================

build_metrics(State) ->
    #{
        memory => build_memory_stats(),
        scheduler => get_scheduler_utilization(),
        processes => build_process_stats(),
        connections => #{
            current => State#state.current_connections,
            peak => State#state.peak_connections
        },
        efficiency_score => calculate_efficiency_score(State),
        recommendations => generate_recommendations(State)
    }.

build_memory_stats() ->
    Mem = erlang:memory(),
    Total = proplists:get_value(total, Mem, 0),
    Processes = proplists:get_value(processes, Mem, 0),
    Binary = proplists:get_value(binary, Mem, 0),
    ETS = proplists:get_value(ets, Mem, 0),
    System = proplists:get_value(system, Mem, 0),
    
    #{
        total_bytes => Total,
        total_mb => Total div (1024 * 1024),
        processes_bytes => Processes,
        processes_pct => safe_pct(Processes, Total),
        binary_bytes => Binary,
        binary_pct => safe_pct(Binary, Total),
        ets_bytes => ETS,
        ets_pct => safe_pct(ETS, Total),
        system_bytes => System,
        system_pct => safe_pct(System, Total)
    }.

build_process_stats() ->
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    PortCount = erlang:system_info(port_count),
    PortLimit = erlang:system_info(port_limit),
    
    #{
        process_count => ProcCount,
        process_limit => ProcLimit,
        process_usage_pct => safe_pct(ProcCount, ProcLimit),
        port_count => PortCount,
        port_limit => PortLimit,
        port_usage_pct => safe_pct(PortCount, PortLimit)
    }.

get_scheduler_utilization() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            #{enabled => false, utilization => []};
        WT ->
            %% Calculate utilization per scheduler
            Utilization = lists:map(fun({I, Active, Total}) ->
                Pct = case Total of
                    0 -> 0.0;
                    _ -> (Active / Total) * 100
                end,
                {I, round(Pct * 10) / 10}
            end, WT),
            
            %% Calculate average
            TotalActive = lists:sum([A || {_, A, _} <- WT]),
            TotalWall = lists:sum([T || {_, _, T} <- WT]),
            AvgUtil = case TotalWall of
                0 -> 0.0;
                _ -> (TotalActive / TotalWall) * 100
            end,
            
            #{
                enabled => true,
                average_pct => round(AvgUtil * 10) / 10,
                per_scheduler => Utilization,
                scheduler_count => length(WT)
            }
    end.

%% =============================================================================
%% Internal: Efficiency Score
%% =============================================================================

calculate_efficiency_score(State) ->
    %% Score components (each 0-100)
    MemScore = memory_efficiency_score(),
    SchedScore = scheduler_efficiency_score(),
    ProcScore = process_efficiency_score(),
    
    %% Weighted average
    round((MemScore * 0.4) + (SchedScore * 0.4) + (ProcScore * 0.2)).

memory_efficiency_score() ->
    Mem = erlang:memory(),
    Total = proplists:get_value(total, Mem, 1),
    Processes = proplists:get_value(processes, Mem, 0),
    
    %% Good if process memory is high % of total (less waste)
    ProcessPct = (Processes / max(1, Total)) * 100,
    
    %% Score based on process memory ratio
    if
        ProcessPct > 60 -> 90;  %% Excellent - mostly process memory
        ProcessPct > 40 -> 80;
        ProcessPct > 20 -> 60;
        true -> 40  %% Too much system overhead
    end.

scheduler_efficiency_score() ->
    case get_scheduler_utilization() of
        #{enabled := false} -> 50;
        #{average_pct := AvgUtil} ->
            %% Ideal utilization is 60-80% (headroom for spikes)
            if
                AvgUtil >= 95 -> 50;   %% Overloaded
                AvgUtil >= 80 -> 70;   %% High but OK
                AvgUtil >= 60 -> 100;  %% Optimal
                AvgUtil >= 40 -> 90;   %% Good
                AvgUtil >= 20 -> 70;   %% Underutilized
                true -> 50             %% Very underutilized
            end
    end.

process_efficiency_score() ->
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    UsagePct = (ProcCount / max(1, ProcLimit)) * 100,
    
    %% Score based on limit usage (should have headroom)
    if
        UsagePct > 90 -> 30;   %% Danger zone
        UsagePct > 70 -> 50;   %% Getting tight
        UsagePct > 50 -> 70;   %% OK
        UsagePct > 20 -> 90;   %% Good headroom
        true -> 100            %% Plenty of room
    end.

%% =============================================================================
%% Internal: Recommendations
%% =============================================================================

generate_recommendations(State) ->
    Recommendations = [],
    
    %% Memory recommendations
    Mem = erlang:memory(),
    Binary = proplists:get_value(binary, Mem, 0),
    Total = proplists:get_value(total, Mem, 1),
    BinaryPct = (Binary / Total) * 100,
    
    R1 = if
        BinaryPct > 30 ->
            [<<"High binary memory usage - check for binary leaks">> | Recommendations];
        true ->
            Recommendations
    end,
    
    %% Scheduler recommendations
    R2 = case get_scheduler_utilization() of
        #{average_pct := Util} when Util > 90 ->
            [<<"Scheduler overload - consider adding nodes">> | R1];
        #{average_pct := Util} when Util < 10 ->
            [<<"Very low scheduler utilization - system is idle">> | R1];
        _ -> R1
    end,
    
    %% Process limit recommendations
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    ProcPct = (ProcCount / ProcLimit) * 100,
    
    R3 = if
        ProcPct > 80 ->
            [<<"Process limit nearly exhausted - increase +P flag">> | R2];
        true ->
            R2
    end,
    
    R3.

safe_pct(Part, Total) when Total > 0 ->
    round((Part / Total) * 1000) / 10;  %% One decimal place
safe_pct(_, _) ->
    0.0.
