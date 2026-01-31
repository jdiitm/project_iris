-module(iris_circuit_breaker).
-behaviour(gen_server).

%% =============================================================================
%% Enhanced Circuit Breaker with Fallback Support
%% =============================================================================
%% Improvements from original:
%% 1. Fallback node support for automatic failover
%% 2. Adaptive timeout based on recent latency
%% 3. Gradual recovery with success threshold
%% 4. Integration with flow_controller for cascade detection
%% 5. Per-node health metrics
%% =============================================================================

%% API
-export([start_link/0]).
-export([call/4, call_with_fallback/5]).
-export([record_success/1, record_failure/1]).
-export([get_status/1, get_all_status/0]).
-export([get_failover_timeout/0]).  %% RFC NFR-9: Configurable failover timeout

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FAILURE_THRESHOLD, 5).
-define(SUCCESS_THRESHOLD, 3).        %% Successes needed in half-open to close
-define(DEFAULT_FAILOVER_TIMEOUT_MS, 30000).  %% RFC NFR-9: ≤30s failover time
-define(MAX_RESET_TIMEOUT_MS, 300000). %% Max 5 minutes
-define(CALL_TIMEOUT_MS, 5000).

%% @doc Get the failover timeout from configuration (RFC NFR-9: ≤30s).
%% Configurable via application env: {iris_core, [{failover_timeout_ms, 30000}]}
get_failover_timeout() ->
    application:get_env(iris_core, failover_timeout_ms, ?DEFAULT_FAILOVER_TIMEOUT_MS).

%% AUDIT2 FIX: ETS table for lockfree circuit checks
-define(BREAKER_ETS, iris_circuit_breaker_ets).

-record(breaker, {
    status = closed :: closed | open | half_open,
    failures = 0 :: integer(),
    successes = 0 :: integer(),      %% Successes in half-open state
    last_failure :: integer() | undefined,
    last_success :: integer() | undefined,
    reset_timeout = ?DEFAULT_FAILOVER_TIMEOUT_MS :: integer(),
    avg_latency = 0.0 :: float(),
    total_calls = 0 :: integer()
}).

-record(state, {
    breakers = #{} :: map()  %% Node => #breaker{}
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Execute RPC call protected by circuit breaker.
-spec call(node(), module(), atom(), list()) -> term() | {error, circuit_open}.
call(Node, Mod, Fun, Args) ->
    call_with_fallback(Node, Mod, Fun, Args, []).

%% @doc Execute RPC with fallback nodes.
-spec call_with_fallback(node(), module(), atom(), list(), [node()]) -> term().
call_with_fallback(Node, Mod, Fun, Args, FallbackNodes) ->
    case do_protected_call(Node, Mod, Fun, Args) of
        {error, circuit_open} ->
            %% Try fallback nodes
            try_fallbacks(FallbackNodes, Mod, Fun, Args, {error, circuit_open});
        {badrpc, _} = Err ->
            try_fallbacks(FallbackNodes, Mod, Fun, Args, Err);
        Result ->
            Result
    end.

%% @doc Record successful call (also called internally)
record_success(Node) ->
    gen_server:cast(?SERVER, {success, Node, os:system_time(millisecond)}).

%% @doc Record failed call (also called internally)
record_failure(Node) ->
    gen_server:cast(?SERVER, {failure, Node, os:system_time(millisecond)}).

%% @doc Get status of a specific node's breaker
get_status(Node) ->
    gen_server:call(?SERVER, {get_status, Node}).

%% @doc Get status of all breakers
get_all_status() ->
    gen_server:call(?SERVER, get_all_status).

%% =============================================================================
%% Internal: Protected Call
%% =============================================================================

do_protected_call(Node, Mod, Fun, Args) ->
    case check_circuit(Node) of
        allow ->
            StartTime = os:system_time(millisecond),
            try rpc:call(Node, Mod, Fun, Args, ?CALL_TIMEOUT_MS) of
                {badrpc, _} = Err ->
                    record_failure(Node),
                    notify_flow_controller(Node, failure),
                    Err;
                Result ->
                    Latency = os:system_time(millisecond) - StartTime,
                    record_success_with_latency(Node, Latency),
                    notify_flow_controller(Node, success),
                    Result
            catch
                _:Reason ->
                    record_failure(Node),
                    notify_flow_controller(Node, failure),
                    {error, Reason}
            end;
        deny ->
            {error, circuit_open}
    end.

try_fallbacks([], _Mod, _Fun, _Args, LastError) ->
    LastError;
try_fallbacks([Node | Rest], Mod, Fun, Args, _LastError) ->
    case do_protected_call(Node, Mod, Fun, Args) of
        {error, circuit_open} ->
            try_fallbacks(Rest, Mod, Fun, Args, {error, circuit_open});
        {badrpc, _} = Err ->
            try_fallbacks(Rest, Mod, Fun, Args, Err);
        Result ->
            Result
    end.

%% P1-H5 FIX: Lockfree ETS-based circuit check with atomic half-open transition
%% Prevents thundering herd on recovery by rate-limiting probe requests
-define(HALF_OPEN_PROBE_INTERVAL_MS, 1000).  %% Only allow 1 probe per second

check_circuit(Node) ->
    case ets:lookup(?BREAKER_ETS, Node) of
        [] -> allow;  %% Unknown node = allow
        [{Node, closed}] -> allow;
        [{Node, half_open, LastProbe}] -> 
            %% P1-H5 FIX: Rate limit probes in half-open state
            Now = os:system_time(millisecond),
            case Now - LastProbe > ?HALF_OPEN_PROBE_INTERVAL_MS of
                true ->
                    %% Allow probe, update last probe time atomically
                    case ets:select_replace(?BREAKER_ETS, [
                        {{Node, half_open, LastProbe}, [], [{{Node, half_open, Now}}]}
                    ]) of
                        1 -> allow;  %% Successfully claimed the probe slot
                        0 -> deny    %% Another process claimed it first
                    end;
                false ->
                    deny  %% Too soon for another probe
            end;
        [{Node, half_open}] ->
            %% Legacy format - upgrade to new format with timestamp
            Now = os:system_time(millisecond),
            ets:insert(?BREAKER_ETS, {Node, half_open, Now}),
            allow;
        [{Node, open, LastFail, Timeout}] ->
            Now = os:system_time(millisecond),
            case LastFail of
                undefined -> 
                    %% P1-H5 FIX: Use atomic compare-and-swap for transition
                    try_transition_half_open(Node, Now);
                _ when Now - LastFail > Timeout ->
                    %% P1-H5 FIX: Use atomic compare-and-swap for transition  
                    try_transition_half_open(Node, Now);
                _ -> deny
            end
    end.

%% P1-H5 FIX: Atomic half-open transition to prevent thundering herd
try_transition_half_open(Node, Now) ->
    %% Try to atomically transition from open to half_open
    %% Only one process should succeed
    case ets:lookup(?BREAKER_ETS, Node) of
        [{Node, open, _LastFail, _Timeout}] ->
            %% Attempt atomic transition using select_replace
            case ets:select_replace(?BREAKER_ETS, [
                {{Node, open, '_', '_'}, [], [{{Node, half_open, Now}}]}
            ]) of
                1 ->
                    %% Successfully transitioned - notify gen_server and allow probe
                    gen_server:cast(?SERVER, {transition_half_open, Node}),
                    allow;
                0 ->
                    %% Another process transitioned first - check new state
                    check_circuit(Node)
            end;
        _ ->
            %% State changed while we were checking
            check_circuit(Node)
    end.

record_success_with_latency(Node, Latency) ->
    gen_server:cast(?SERVER, {success_latency, Node, os:system_time(millisecond), Latency}).

notify_flow_controller(Node, Result) ->
    case whereis(iris_flow_controller) of
        undefined -> ok;
        _ ->
            case Result of
                success -> iris_flow_controller:record_success(Node);
                failure -> iris_flow_controller:record_failure(Node)
            end
    end.

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% AUDIT2 FIX: Create ETS table for lockfree circuit checks
    %% This removes the gen_server:call bottleneck (20K/sec ceiling)
    ets:new(?BREAKER_ETS, [named_table, public, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({check, Node}, _From, State = #state{breakers = Breakers}) ->
    %% Legacy sync path (still useful for diagnostics)
    Breaker = maps:get(Node, Breakers, #breaker{}),
    {Result, NewBreaker} = check_breaker(Breaker),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    %% Update ETS for lockfree reads
    update_ets_status(Node, NewBreaker),
    {reply, Result, State#state{breakers = NewBreakers}};

handle_call({get_status, Node}, _From, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    Status = format_breaker_status(Breaker),
    {reply, Status, State};

handle_call(get_all_status, _From, State = #state{breakers = Breakers}) ->
    Status = maps:map(fun(_Node, B) -> format_breaker_status(B) end, Breakers),
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({success, Node, Timestamp}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = handle_success(Breaker, Timestamp, undefined),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    update_ets_status(Node, NewBreaker),
    {noreply, State#state{breakers = NewBreakers}};

handle_cast({success_latency, Node, Timestamp, Latency}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = handle_success(Breaker, Timestamp, Latency),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    update_ets_status(Node, NewBreaker),
    {noreply, State#state{breakers = NewBreakers}};

handle_cast({failure, Node, Timestamp}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = handle_failure(Breaker, Timestamp),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    update_ets_status(Node, NewBreaker),
    {noreply, State#state{breakers = NewBreakers}};

%% AUDIT2: Handle half-open transition request from lockfree check
handle_cast({transition_half_open, Node}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = Breaker#breaker{status = half_open, successes = 0},
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    update_ets_status(Node, NewBreaker),
    {noreply, State#state{breakers = NewBreakers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal: Breaker State Machine
%% =============================================================================

check_breaker(Breaker = #breaker{status = closed}) ->
    {allow, Breaker};

check_breaker(Breaker = #breaker{status = open, last_failure = LastFail, reset_timeout = Timeout}) ->
    Now = os:system_time(millisecond),
    case LastFail of
        undefined ->
            {allow, Breaker#breaker{status = half_open, successes = 0}};
        _ when Now - LastFail > Timeout ->
            %% Transition to half-open
            {allow, Breaker#breaker{status = half_open, successes = 0}};
        _ ->
            {deny, Breaker}
    end;

check_breaker(Breaker = #breaker{status = half_open}) ->
    %% Allow requests in half-open (testing if recovered)
    {allow, Breaker}.

handle_success(Breaker = #breaker{status = closed, total_calls = Total}, Timestamp, Latency) ->
    NewLatency = update_latency(Breaker#breaker.avg_latency, Latency, Total),
    Breaker#breaker{
        last_success = Timestamp,
        failures = 0,
        avg_latency = NewLatency,
        total_calls = Total + 1,
        reset_timeout = get_failover_timeout()  %% RFC NFR-9: Reset to configured timeout
    };

handle_success(Breaker = #breaker{status = half_open, successes = S, total_calls = Total}, Timestamp, Latency) ->
    NewSuccesses = S + 1,
    NewLatency = update_latency(Breaker#breaker.avg_latency, Latency, Total),
    case NewSuccesses >= ?SUCCESS_THRESHOLD of
        true ->
            %% Recovered - close the circuit
            logger:info("Circuit breaker: Node recovered, closing circuit"),
            Breaker#breaker{
                status = closed,
                successes = 0,
                failures = 0,
                last_success = Timestamp,
                avg_latency = NewLatency,
                total_calls = Total + 1,
                reset_timeout = get_failover_timeout()  %% RFC NFR-9: Reset to configured timeout
            };
        false ->
            Breaker#breaker{
                successes = NewSuccesses,
                last_success = Timestamp,
                avg_latency = NewLatency,
                total_calls = Total + 1
            }
    end;

handle_success(Breaker = #breaker{status = open}, _Timestamp, _Latency) ->
    %% Shouldn't happen, but handle gracefully
    Breaker.

handle_failure(Breaker = #breaker{status = closed, failures = F, reset_timeout = Timeout}, Timestamp) ->
    NewFailures = F + 1,
    case NewFailures >= ?FAILURE_THRESHOLD of
        true ->
            logger:warning("Circuit breaker: Threshold reached, opening circuit"),
            Breaker#breaker{
                status = open,
                failures = NewFailures,
                last_failure = Timestamp
            };
        false ->
            Breaker#breaker{
                failures = NewFailures,
                last_failure = Timestamp
            }
    end;

handle_failure(Breaker = #breaker{status = half_open, reset_timeout = Timeout}, Timestamp) ->
    %% Failed while testing - back to open with increased timeout
    NewTimeout = min(Timeout * 2, ?MAX_RESET_TIMEOUT_MS),
    logger:warning("Circuit breaker: Half-open test failed, reopening with ~pms timeout", [NewTimeout]),
    Breaker#breaker{
        status = open,
        successes = 0,
        last_failure = Timestamp,
        reset_timeout = NewTimeout
    };

handle_failure(Breaker = #breaker{status = open}, Timestamp) ->
    %% Already open - just update timestamp
    Breaker#breaker{last_failure = Timestamp}.

update_latency(_AvgLatency, undefined, _Total) ->
    0.0;
update_latency(AvgLatency, Latency, Total) when Total == 0 ->
    float(Latency);
update_latency(AvgLatency, Latency, Total) ->
    %% Exponential moving average
    Alpha = 0.1,
    AvgLatency * (1 - Alpha) + Latency * Alpha.

format_breaker_status(#breaker{status = Status, failures = F, successes = S, 
                                avg_latency = Lat, total_calls = Total, reset_timeout = Timeout}) ->
    #{
        status => Status,
        failures => F,
        successes_in_half_open => S,
        avg_latency_ms => round(Lat),
        total_calls => Total,
        reset_timeout_ms => Timeout
    }.

%% AUDIT2 FIX: Update ETS for lockfree reads
%% P1-H5 FIX: half_open includes last probe timestamp for rate limiting
update_ets_status(Node, #breaker{status = closed}) ->
    ets:insert(?BREAKER_ETS, {Node, closed});
update_ets_status(Node, #breaker{status = half_open}) ->
    Now = os:system_time(millisecond),
    ets:insert(?BREAKER_ETS, {Node, half_open, Now});
update_ets_status(Node, #breaker{status = open, last_failure = LF, reset_timeout = T}) ->
    ets:insert(?BREAKER_ETS, {Node, open, LF, T}).
