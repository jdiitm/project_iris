-module(iris_flow_controller).
-behaviour(gen_server).

%% =============================================================================
%% Global-Scale Flow Controller with Cascade Failure Protection
%% =============================================================================
%% Design Principles:
%% 1. Multi-level adaptive backpressure (NORMAL → SLOW → SHED → CRITICAL)
%% 2. Cascade failure detection via downstream health tracking
%% 3. Per-destination queue tracking for fan-out throttling
%% 4. Automatic recovery when pressure subsides
%%
%% AUDIT FIX: All admission checks are now lockfree via sharded ETS.
%% Previously, non-NORMAL levels serialized through gen_server:call.
%% Now all levels use ETS for admission decisions.
%% =============================================================================

-export([start_link/0]).
-export([check_admission/1, check_admission/2]).
-export([record_success/1, record_failure/1]).
-export([get_level/0, get_stats/0]).
-export([track_destination/2, get_destination_priority/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%% Pressure levels
-define(LEVEL_NORMAL, 1).
-define(LEVEL_SLOW, 2).
-define(LEVEL_SHED, 3).
-define(LEVEL_CRITICAL, 4).

%% Thresholds (configurable via application env)
-define(MEMORY_WARNING, 0.70).
-define(MEMORY_SHED, 0.85).
-define(MEMORY_CRITICAL, 0.95).
-define(CASCADE_FAILURE_THRESHOLD, 0.50).  %% 50% of cores failing

%% Intervals
-define(CHECK_INTERVAL_MS, 1000).
-define(DECAY_INTERVAL_MS, 5000).

%% AUDIT FIX: Sharded ETS for lockfree admission checks at ALL levels
-define(FLOW_ETS, iris_flow_controller_ets).
-define(SHARD_COUNT, 16).  %% Number of shards for rate counters
-define(DEST_ETS, iris_flow_controller_dest_ets).  %% Per-destination tracking

-record(state, {
    level = ?LEVEL_NORMAL :: integer(),
    
    %% Resource tracking
    memory_percent = 0.0 :: float(),
    cpu_percent = 0.0 :: float(),
    
    %% Downstream health tracking
    core_health = #{} :: map(),  %% Node -> {successes, failures, last_check_time}
    cascade_detected = false :: boolean(),
    
    %% Rate limiting state
    window_start :: integer(),
    
    %% Stats (updated periodically from shards)
    admitted = 0 :: integer(),
    rejected = 0 :: integer(),
    shed_by_fanout = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Check if a request should be admitted
%% Returns: {allow, Level} | {deny, Reason, RetryAfterMs}
%% 
%% AUDIT FIX: This is now FULLY lockfree - no gen_server:call at any level.
%% All state is read from ETS tables.
-spec check_admission(binary()) -> {allow, integer()} | {deny, atom(), integer()}.
check_admission(User) ->
    check_admission(User, #{}).

-spec check_admission(binary(), map()) -> {allow, integer()} | {deny, atom(), integer()}.
check_admission(User, _Opts) ->
    %% Determine shard for this user (consistent hashing)
    Shard = erlang:phash2(User, ?SHARD_COUNT),
    
    %% Read level from ETS (lockfree)
    Level = case ets:lookup(?FLOW_ETS, level) of
        [{level, L}] -> L;
        [] -> ?LEVEL_NORMAL
    end,
    
    %% All admission decisions are now lockfree
    case Level of
        ?LEVEL_NORMAL ->
            %% Fast path: always allow, increment shard counter
            ets:update_counter(?FLOW_ETS, {admitted, Shard}, 1, {{admitted, Shard}, 0}),
            {allow, Level};
        
        ?LEVEL_SLOW ->
            %% Rate limit check via sharded counter
            case check_rate_limit_lockfree(Shard) of
                allow ->
                    ets:update_counter(?FLOW_ETS, {admitted, Shard}, 1, {{admitted, Shard}, 0}),
                    {allow, Level};
                deny ->
                    ets:update_counter(?FLOW_ETS, {rejected, Shard}, 1, {{rejected, Shard}, 0}),
                    {deny, rate_limited, 100}
            end;
        
        ?LEVEL_SHED ->
            %% Check destination priority via ETS
            case check_fanout_priority_lockfree(User) of
                allow ->
                    ets:update_counter(?FLOW_ETS, {admitted, Shard}, 1, {{admitted, Shard}, 0}),
                    {allow, Level};
                deny ->
                    ets:update_counter(?FLOW_ETS, {rejected, Shard}, 1, {{rejected, Shard}, 0}),
                    ets:update_counter(?FLOW_ETS, {shed_fanout, Shard}, 1, {{shed_fanout, Shard}, 0}),
                    {deny, load_shed, 1000}
            end;
        
        ?LEVEL_CRITICAL ->
            %% Reject everything
            ets:update_counter(?FLOW_ETS, {rejected, Shard}, 1, {{rejected, Shard}, 0}),
            {deny, system_overload, 5000}
    end.

%% Record downstream call result (for cascade detection)
record_success(Node) ->
    gen_server:cast(?SERVER, {downstream_success, Node}).

record_failure(Node) ->
    gen_server:cast(?SERVER, {downstream_failure, Node}).

%% Track messages queued for a destination (for fan-out control)
track_destination(User, QueueDepth) ->
    %% Lockfree: write directly to ETS
    Now = erlang:system_time(millisecond),
    
    %% Read current state (if any)
    Priority = case ets:lookup(?DEST_ETS, User) of
        [{User, OldDepth, _OldDrainRate, _OldPriority, LastUpdate}] ->
            %% Calculate drain rate
            Elapsed = max(1, Now - LastUpdate),
            DrainRate = (OldDepth - QueueDepth) / Elapsed * 1000,
            calculate_priority(QueueDepth, DrainRate);
        [] ->
            normal
    end,
    
    ets:insert(?DEST_ETS, {User, QueueDepth, 0.0, Priority, Now}),
    ok.

%% Get priority for a destination (lockfree)
get_destination_priority(User) ->
    case ets:lookup(?DEST_ETS, User) of
        [{User, _Depth, _Rate, Priority, _LastUpdate}] -> Priority;
        [] -> normal
    end.

%% Get current pressure level (lockfree)
get_level() ->
    case ets:lookup(?FLOW_ETS, level) of
        [{level, L}] -> L;
        [] -> ?LEVEL_NORMAL
    end.

%% Get detailed stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    %% Create ETS tables for lockfree operation
    %% Flow control table (level, counters)
    ets:new(?FLOW_ETS, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(?FLOW_ETS, {level, ?LEVEL_NORMAL}),
    
    %% Initialize shard counters
    lists:foreach(fun(Shard) ->
        ets:insert(?FLOW_ETS, {{admitted, Shard}, 0}),
        ets:insert(?FLOW_ETS, {{rejected, Shard}, 0}),
        ets:insert(?FLOW_ETS, {{shed_fanout, Shard}, 0}),
        ets:insert(?FLOW_ETS, {{requests, Shard}, 0})
    end, lists:seq(0, ?SHARD_COUNT - 1)),
    
    %% Per-destination tracking table
    ets:new(?DEST_ETS, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]),
    
    %% Initialize rate limiting state
    ets:insert(?FLOW_ETS, {window_start, erlang:system_time(second)}),
    ets:insert(?FLOW_ETS, {max_rate, application:get_env(iris_core, max_request_rate, 100000)}),
    
    %% Start periodic check
    erlang:send_after(?CHECK_INTERVAL_MS, self(), check_resources),
    erlang:send_after(?DECAY_INTERVAL_MS, self(), decay_counters),
    
    {ok, #state{window_start = erlang:system_time(second)}}.

handle_call(get_stats, _From, State) ->
    %% Aggregate stats from all shards
    {TotalAdmitted, TotalRejected, TotalShedFanout} = aggregate_shard_stats(),
    
    Stats = #{
        level => level_name(State#state.level),
        memory_percent => State#state.memory_percent,
        cascade_detected => State#state.cascade_detected,
        admitted => TotalAdmitted,
        rejected => TotalRejected,
        shed_by_fanout => TotalShedFanout,
        core_health => format_core_health(State#state.core_health),
        high_fanout_dests => count_high_fanout_ets(),
        shard_count => ?SHARD_COUNT
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({downstream_success, Node}, State) ->
    NewHealth = update_core_health(Node, success, State#state.core_health),
    NewCascade = detect_cascade(NewHealth),
    {noreply, State#state{core_health = NewHealth, cascade_detected = NewCascade}};

handle_cast({downstream_failure, Node}, State) ->
    NewHealth = update_core_health(Node, failure, State#state.core_health),
    NewCascade = detect_cascade(NewHealth),
    {noreply, State#state{core_health = NewHealth, cascade_detected = NewCascade}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_resources, State) ->
    NewState = update_resource_metrics(State),
    NewLevel = calculate_level(NewState),
    
    %% Update ETS for lockfree reads
    ets:insert(?FLOW_ETS, {level, NewLevel}),
    
    %% Log level changes
    case NewLevel =/= State#state.level of
        true ->
            logger:warning("Flow controller level change: ~s -> ~s", 
                          [level_name(State#state.level), level_name(NewLevel)]);
        false -> ok
    end,
    
    erlang:send_after(?CHECK_INTERVAL_MS, self(), check_resources),
    {noreply, NewState#state{level = NewLevel}};

handle_info(decay_counters, State) ->
    %% Decay old core health data
    Now = erlang:system_time(second),
    NewHealth = maps:filter(fun(_Node, {_S, _F, LastCheck}) ->
        Now - LastCheck < 60  %% Keep data for 60 seconds
    end, State#state.core_health),
    
    %% Clean up stale destination tracking
    NowMs = erlang:system_time(millisecond),
    ets:foldl(fun({User, _Depth, _Rate, _Priority, LastUpdate}, Acc) ->
        if NowMs - LastUpdate > 30000 ->  %% 30 second TTL
            ets:delete(?DEST_ETS, User);
           true -> ok
        end,
        Acc
    end, ok, ?DEST_ETS),
    
    %% Reset rate limit window
    ets:insert(?FLOW_ETS, {window_start, erlang:system_time(second)}),
    lists:foreach(fun(Shard) ->
        ets:insert(?FLOW_ETS, {{requests, Shard}, 0})
    end, lists:seq(0, ?SHARD_COUNT - 1)),
    
    erlang:send_after(?DECAY_INTERVAL_MS, self(), decay_counters),
    {noreply, State#state{core_health = NewHealth}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal: Lockfree Admission Checks
%% =============================================================================

%% Rate limit check - reads from sharded counters
check_rate_limit_lockfree(Shard) ->
    %% Increment request counter for this shard
    ets:update_counter(?FLOW_ETS, {requests, Shard}, 1, {{requests, Shard}, 0}),
    
    %% Check total rate across all shards
    TotalRequests = lists:sum([
        case ets:lookup(?FLOW_ETS, {requests, S}) of
            [{{requests, S}, Count}] -> Count;
            [] -> 0
        end
    || S <- lists:seq(0, ?SHARD_COUNT - 1)]),
    
    [{window_start, WindowStart}] = ets:lookup(?FLOW_ETS, window_start),
    [{max_rate, MaxRate}] = ets:lookup(?FLOW_ETS, max_rate),
    
    WindowDuration = max(1, erlang:system_time(second) - WindowStart),
    RatePerSecond = TotalRequests / WindowDuration,
    
    case RatePerSecond > MaxRate of
        true -> deny;
        false -> allow
    end.

%% Fan-out priority check - reads from destination ETS
check_fanout_priority_lockfree(User) ->
    case ets:lookup(?DEST_ETS, User) of
        [{User, _Depth, _Rate, blocked, _LastUpdate}] -> deny;
        [{User, _Depth, _Rate, throttled, _LastUpdate}] ->
            %% 50% chance of allowing throttled destinations
            case rand:uniform(2) of
                1 -> allow;
                2 -> deny
            end;
        _ -> allow
    end.

%% =============================================================================
%% Internal: Level Calculation
%% =============================================================================

update_resource_metrics(State) ->
    %% Memory check
    MemInfo = erlang:memory(),
    Total = proplists:get_value(total, MemInfo, 0),
    MaxMem = get_max_memory(),
    MemPercent = case MaxMem of
        0 -> 0.0;
        _ -> Total / MaxMem
    end,
    
    State#state{memory_percent = MemPercent}.

get_max_memory() ->
    %% Get system memory limit or use 8GB default
    case application:get_env(iris_core, max_memory_bytes) of
        {ok, Max} -> Max;
        undefined ->
            %% Try to detect system memory (memsup may not be started)
            try
                case memsup:get_system_memory_data() of
                    [{total_memory, Total} | _] -> Total;
                    _ -> 8 * 1024 * 1024 * 1024  %% 8GB default
                end
            catch
                _:_ -> 8 * 1024 * 1024 * 1024  %% 8GB default if memsup not available
            end
    end.

calculate_level(#state{memory_percent = Mem, cascade_detected = Cascade}) ->
    if
        Mem > ?MEMORY_CRITICAL -> ?LEVEL_CRITICAL;
        Cascade -> ?LEVEL_SHED;  %% Cascade failure detected
        Mem > ?MEMORY_SHED -> ?LEVEL_SHED;
        Mem > ?MEMORY_WARNING -> ?LEVEL_SLOW;
        true -> ?LEVEL_NORMAL
    end.

%% =============================================================================
%% Internal: Cascade Detection
%% =============================================================================

update_core_health(Node, Result, Health) ->
    Now = erlang:system_time(second),
    {Successes, Failures, _} = maps:get(Node, Health, {0, 0, Now}),
    NewEntry = case Result of
        success -> {Successes + 1, max(0, Failures - 1), Now};
        failure -> {Successes, Failures + 1, Now}
    end,
    maps:put(Node, NewEntry, Health).

detect_cascade(Health) when map_size(Health) == 0 ->
    false;
detect_cascade(Health) ->
    %% Calculate failure ratio across all known cores
    {TotalSuccess, TotalFailure} = maps:fold(fun(_Node, {S, F, _}, {AccS, AccF}) ->
        {AccS + S, AccF + F}
    end, {0, 0}, Health),
    
    Total = TotalSuccess + TotalFailure,
    case Total of
        0 -> false;
        _ ->
            FailureRatio = TotalFailure / Total,
            FailureRatio > ?CASCADE_FAILURE_THRESHOLD
    end.

%% =============================================================================
%% Internal: Fan-out Priority
%% =============================================================================

calculate_priority(QueueDepth, DrainRate) ->
    %% Read current level from ETS
    Level = case ets:lookup(?FLOW_ETS, level) of
        [{level, L}] -> L;
        [] -> ?LEVEL_NORMAL
    end,
    
    %% High queue depth with slow drain = problem
    if
        QueueDepth > 10000 -> blocked;
        QueueDepth > 5000, DrainRate < 100 -> blocked;
        QueueDepth > 1000, Level >= ?LEVEL_SHED -> throttled;
        QueueDepth > 500, DrainRate < 50, Level >= ?LEVEL_SLOW -> throttled;
        true -> normal
    end.

%% =============================================================================
%% Internal: Stats Aggregation
%% =============================================================================

aggregate_shard_stats() ->
    lists:foldl(fun(Shard, {AccA, AccR, AccS}) ->
        Admitted = case ets:lookup(?FLOW_ETS, {admitted, Shard}) of
            [{{admitted, Shard}, A}] -> A;
            [] -> 0
        end,
        Rejected = case ets:lookup(?FLOW_ETS, {rejected, Shard}) of
            [{{rejected, Shard}, R}] -> R;
            [] -> 0
        end,
        ShedFanout = case ets:lookup(?FLOW_ETS, {shed_fanout, Shard}) of
            [{{shed_fanout, Shard}, S}] -> S;
            [] -> 0
        end,
        {AccA + Admitted, AccR + Rejected, AccS + ShedFanout}
    end, {0, 0, 0}, lists:seq(0, ?SHARD_COUNT - 1)).

count_high_fanout_ets() ->
    ets:foldl(fun({_User, _Depth, _Rate, Priority, _LastUpdate}, Acc) ->
        case Priority of
            normal -> Acc;
            _ -> Acc + 1
        end
    end, 0, ?DEST_ETS).

format_core_health(Health) ->
    maps:fold(fun(Node, {S, F, _}, Acc) ->
        maps:put(Node, #{successes => S, failures => F}, Acc)
    end, #{}, Health).

level_name(?LEVEL_NORMAL) -> normal;
level_name(?LEVEL_SLOW) -> slow;
level_name(?LEVEL_SHED) -> shed_load;
level_name(?LEVEL_CRITICAL) -> critical.
