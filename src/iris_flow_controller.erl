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

%% AUDIT2 FIX: ETS table for lockfree admission checks
-define(FLOW_ETS, iris_flow_controller_ets).

-record(state, {
    level = ?LEVEL_NORMAL :: integer(),
    
    %% Resource tracking
    memory_percent = 0.0 :: float(),
    cpu_percent = 0.0 :: float(),
    
    %% Downstream health tracking
    core_health = #{} :: map(),  %% Node -> {successes, failures, last_check_time}
    cascade_detected = false :: boolean(),
    
    %% Per-destination tracking (for fan-out control)
    dest_queues = #{} :: map(),  %% User -> {depth, drain_rate, priority}
    
    %% Rate limiting state
    requests_this_window = 0 :: integer(),
    window_start :: integer(),
    
    %% Stats
    admitted = 0 :: integer(),
    rejected = 0 :: integer(),
    shed_by_fanout = 0 :: integer()
}).

-record(dest_stats, {
    depth = 0 :: integer(),
    drain_rate = 0.0 :: float(),
    priority = normal :: normal | throttled | blocked,
    last_update :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Check if a request should be admitted
%% Returns: {allow, Level} | {deny, Reason, RetryAfterMs}
-spec check_admission(binary()) -> {allow, integer()} | {deny, atom(), integer()}.
check_admission(User) ->
    check_admission(User, #{}).

-spec check_admission(binary(), map()) -> {allow, integer()} | {deny, atom(), integer()}.
check_admission(User, Opts) ->
    %% AUDIT2 FIX: Lockfree fast path for NORMAL level
    %% Read level from ETS to avoid gen_server:call bottleneck
    case ets:lookup(?FLOW_ETS, level) of
        [{level, ?LEVEL_NORMAL}] ->
            %% Fast path: just allow (update stats async)
            gen_server:cast(?SERVER, {admit, 1}),
            {allow, ?LEVEL_NORMAL};
        _ ->
            %% Complex path: delegate to gen_server
            gen_server:call(?SERVER, {check_admission, User, Opts})
    end.

%% Record downstream call result (for cascade detection)
record_success(Node) ->
    gen_server:cast(?SERVER, {downstream_success, Node}).

record_failure(Node) ->
    gen_server:cast(?SERVER, {downstream_failure, Node}).

%% Track messages queued for a destination (for fan-out control)
track_destination(User, QueueDepth) ->
    gen_server:cast(?SERVER, {track_dest, User, QueueDepth}).

%% Get priority for a destination
get_destination_priority(User) ->
    gen_server:call(?SERVER, {get_dest_priority, User}).

%% Get current pressure level
get_level() ->
    gen_server:call(?SERVER, get_level).

%% Get detailed stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    %% AUDIT2 FIX: Create ETS table for lockfree reads
    %% This removes the gen_server:call bottleneck for admission checks
    ets:new(?FLOW_ETS, [named_table, public, {read_concurrency, true}]),
    ets:insert(?FLOW_ETS, {level, ?LEVEL_NORMAL}),
    
    %% Start periodic check
    erlang:send_after(?CHECK_INTERVAL_MS, self(), check_resources),
    erlang:send_after(?DECAY_INTERVAL_MS, self(), decay_counters),
    
    {ok, #state{window_start = os:system_time(second)}}.

handle_call({check_admission, User, Opts}, _From, State) ->
    {Result, NewState} = do_check_admission(User, Opts, State),
    {reply, Result, NewState};

handle_call({get_dest_priority, User}, _From, State) ->
    Priority = case maps:get(User, State#state.dest_queues, undefined) of
        undefined -> normal;
        #dest_stats{priority = P} -> P
    end,
    {reply, Priority, State};

handle_call(get_level, _From, State) ->
    {reply, State#state.level, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        level => level_name(State#state.level),
        memory_percent => State#state.memory_percent,
        cascade_detected => State#state.cascade_detected,
        admitted => State#state.admitted,
        rejected => State#state.rejected,
        shed_by_fanout => State#state.shed_by_fanout,
        core_health => format_core_health(State#state.core_health),
        high_fanout_dests => count_high_fanout(State#state.dest_queues)
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

handle_cast({track_dest, User, QueueDepth}, State) ->
    Now = os:system_time(millisecond),
    NewStats = case maps:get(User, State#state.dest_queues, undefined) of
        undefined ->
            #dest_stats{depth = QueueDepth, last_update = Now};
        Old = #dest_stats{depth = OldDepth, last_update = LastUpdate} ->
            %% Calculate drain rate
            Elapsed = max(1, Now - LastUpdate),
            DrainRate = (OldDepth - QueueDepth) / Elapsed * 1000,  %% msgs/sec
            Priority = calculate_priority(QueueDepth, DrainRate, State#state.level),
            Old#dest_stats{
                depth = QueueDepth,
                drain_rate = DrainRate,
                priority = Priority,
                last_update = Now
            }
    end,
    NewQueues = maps:put(User, NewStats, State#state.dest_queues),
    {noreply, State#state{dest_queues = NewQueues}};

%% AUDIT2 FIX: Handle async admit counter increment
handle_cast({admit, Count}, State) ->
    {noreply, State#state{
        admitted = State#state.admitted + Count,
        requests_this_window = State#state.requests_this_window + Count
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_resources, State) ->
    NewState = update_resource_metrics(State),
    NewLevel = calculate_level(NewState),
    
    %% AUDIT2 FIX: Update ETS for lockfree reads
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
    Now = os:system_time(second),
    NewHealth = maps:filter(fun(_Node, {_S, _F, LastCheck}) ->
        Now - LastCheck < 60  %% Keep data for 60 seconds
    end, State#state.core_health),
    
    %% Clean up stale destination tracking
    CleanQueues = maps:filter(fun(_User, #dest_stats{last_update = LU}) ->
        os:system_time(millisecond) - LU < 30000  %% 30 second TTL
    end, State#state.dest_queues),
    
    erlang:send_after(?DECAY_INTERVAL_MS, self(), decay_counters),
    {noreply, State#state{core_health = NewHealth, dest_queues = CleanQueues}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal: Admission Control
%% =============================================================================

do_check_admission(User, Opts, State = #state{level = Level}) ->
    %% Fast path for normal operation
    case Level of
        ?LEVEL_NORMAL ->
            NewState = State#state{
                admitted = State#state.admitted + 1,
                requests_this_window = State#state.requests_this_window + 1
            },
            {{allow, Level}, NewState};
        
        ?LEVEL_SLOW ->
            %% Rate limit but generally allow
            case should_rate_limit(State) of
                false ->
                    NewState = State#state{admitted = State#state.admitted + 1},
                    {{allow, Level}, NewState};
                true ->
                    NewState = State#state{rejected = State#state.rejected + 1},
                    {{deny, rate_limited, 100}, NewState}
            end;
        
        ?LEVEL_SHED ->
            %% Check destination priority - shed high fanout first
            case check_fanout_priority(User, Opts, State) of
                allow ->
                    NewState = State#state{admitted = State#state.admitted + 1},
                    {{allow, Level}, NewState};
                deny ->
                    NewState = State#state{
                        rejected = State#state.rejected + 1,
                        shed_by_fanout = State#state.shed_by_fanout + 1
                    },
                    {{deny, load_shed, 1000}, NewState}
            end;
        
        ?LEVEL_CRITICAL ->
            %% Reject everything
            NewState = State#state{rejected = State#state.rejected + 1},
            {{deny, system_overload, 5000}, NewState}
    end.

should_rate_limit(#state{requests_this_window = Requests, window_start = Start}) ->
    Now = os:system_time(second),
    WindowDuration = Now - Start,
    if WindowDuration < 1 -> false;
       true ->
           RatePerSecond = Requests / max(1, WindowDuration),
           MaxRate = application:get_env(iris_core, max_request_rate, 100000),
           RatePerSecond > MaxRate
    end.

check_fanout_priority(User, _Opts, #state{dest_queues = Queues}) ->
    case maps:get(User, Queues, undefined) of
        undefined -> allow;
        #dest_stats{priority = blocked} -> deny;
        #dest_stats{priority = throttled} ->
            %% 50% chance of allowing throttled destinations
            case rand:uniform(2) of
                1 -> allow;
                2 -> deny
            end;
        #dest_stats{priority = normal} -> allow
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
    Now = os:system_time(second),
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

calculate_priority(QueueDepth, DrainRate, Level) ->
    %% High queue depth with slow drain = problem
    if
        QueueDepth > 10000 -> blocked;
        QueueDepth > 5000, DrainRate < 100 -> blocked;
        QueueDepth > 1000, Level >= ?LEVEL_SHED -> throttled;
        QueueDepth > 500, DrainRate < 50, Level >= ?LEVEL_SLOW -> throttled;
        true -> normal
    end.

count_high_fanout(Queues) ->
    maps:fold(fun(_User, #dest_stats{priority = P}, Acc) ->
        case P of
            normal -> Acc;
            _ -> Acc + 1
        end
    end, 0, Queues).

format_core_health(Health) ->
    maps:fold(fun(Node, {S, F, _}, Acc) ->
        maps:put(Node, #{successes => S, failures => F}, Acc)
    end, #{}, Health).

level_name(?LEVEL_NORMAL) -> normal;
level_name(?LEVEL_SLOW) -> slow;
level_name(?LEVEL_SHED) -> shed_load;
level_name(?LEVEL_CRITICAL) -> critical.
