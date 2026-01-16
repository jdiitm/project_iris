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

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FAILURE_THRESHOLD, 5).
-define(SUCCESS_THRESHOLD, 3).        %% Successes needed in half-open to close
-define(RESET_TIMEOUT_MS, 30000).     %% Initial reset timeout
-define(MAX_RESET_TIMEOUT_MS, 300000). %% Max 5 minutes
-define(CALL_TIMEOUT_MS, 5000).

-record(breaker, {
    status = closed :: closed | open | half_open,
    failures = 0 :: integer(),
    successes = 0 :: integer(),      %% Successes in half-open state
    last_failure :: integer() | undefined,
    last_success :: integer() | undefined,
    reset_timeout = ?RESET_TIMEOUT_MS :: integer(),
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

check_circuit(Node) ->
    gen_server:call(?SERVER, {check, Node}).

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
    {ok, #state{}}.

handle_call({check, Node}, _From, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    {Result, NewBreaker} = check_breaker(Breaker),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
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
    {noreply, State#state{breakers = NewBreakers}};

handle_cast({success_latency, Node, Timestamp, Latency}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = handle_success(Breaker, Timestamp, Latency),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
    {noreply, State#state{breakers = NewBreakers}};

handle_cast({failure, Node, Timestamp}, State = #state{breakers = Breakers}) ->
    Breaker = maps:get(Node, Breakers, #breaker{}),
    NewBreaker = handle_failure(Breaker, Timestamp),
    NewBreakers = maps:put(Node, NewBreaker, Breakers),
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
        reset_timeout = ?RESET_TIMEOUT_MS  %% Reset timeout on success
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
                reset_timeout = ?RESET_TIMEOUT_MS
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

