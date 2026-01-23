-module(iris_router_pool).
-behaviour(gen_server).

%% =============================================================================
%% Worker Pool for Message Routing
%% =============================================================================
%% Purpose: Efficient message routing using a worker pool.
%% RFC Reference: NFR-2 (Throughput), NFR-3 (Latency)
%%
%% Design:
%% 1. Fixed pool of worker processes
%% 2. Round-robin or least-loaded distribution
%% 3. Overflow handling for spikes
%% 4. Health monitoring and auto-recovery
%%
%% Configuration:
%% - pool_size: Number of workers (default: schedulers * 2)
%% - max_overflow: Maximum temporary workers (default: pool_size / 2)
%% - strategy: round_robin | least_loaded | hash_ring
%% =============================================================================

-export([start_link/0, start_link/1]).

%% API
-export([
    route_msg/3,              %% (From, To, Msg) -> ok | {error, Reason}
    route_batch/2,            %% (From, [{To, Msg}]) -> {ok, Stats}
    async_route/3,            %% (From, To, Msg) -> ok (non-blocking)
    get_stats/0,              %% () -> PoolStats
    resize_pool/1,            %% (NewSize) -> ok
    health_check/0            %% () -> healthy | degraded | unhealthy
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, erlang:system_info(schedulers) * 2).
-define(DEFAULT_MAX_OVERFLOW, ?DEFAULT_POOL_SIZE div 2).
-define(WORKER_TIMEOUT, 5000).
-define(HEALTH_CHECK_INTERVAL, 10000).

-record(state, {
    workers       :: [pid()],          %% Active worker pids
    overflow      :: [pid()],          %% Overflow worker pids
    pool_size     :: integer(),        %% Target pool size
    max_overflow  :: integer(),        %% Max overflow workers
    strategy      :: round_robin | least_loaded | hash_ring,
    next_worker   :: integer(),        %% Round-robin counter
    
    %% Stats
    routed        :: integer(),        %% Total messages routed
    errors        :: integer(),        %% Total errors
    queue_depth   :: integer()         %% Current queue depth
}).

-record(worker_state, {
    id           :: integer(),
    handled      :: integer(),         %% Messages handled
    errors       :: integer(),         %% Errors encountered
    last_msg     :: integer()          %% Last message timestamp
}).

%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Route a message from one user to another.
-spec route_msg(binary(), binary(), term()) -> ok | {error, term()}.
route_msg(From, To, Msg) ->
    gen_server:call(?SERVER, {route, From, To, Msg}, ?WORKER_TIMEOUT).

%% @doc Route a batch of messages from one sender.
-spec route_batch(binary(), [{binary(), term()}]) -> {ok, map()}.
route_batch(From, Messages) ->
    gen_server:call(?SERVER, {route_batch, From, Messages}, ?WORKER_TIMEOUT * 2).

%% @doc Asynchronously route a message (fire-and-forget).
-spec async_route(binary(), binary(), term()) -> ok.
async_route(From, To, Msg) ->
    gen_server:cast(?SERVER, {route, From, To, Msg}),
    ok.

%% @doc Get pool statistics.
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Resize the worker pool.
-spec resize_pool(integer()) -> ok.
resize_pool(NewSize) when NewSize > 0 ->
    gen_server:call(?SERVER, {resize, NewSize}).

%% @doc Check pool health.
-spec health_check() -> healthy | degraded | unhealthy.
health_check() ->
    gen_server:call(?SERVER, health_check).

%% =============================================================================
%% gen_server Callbacks
%% =============================================================================

init(Opts) ->
    PoolSize = proplists:get_value(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    MaxOverflow = proplists:get_value(max_overflow, Opts, ?DEFAULT_MAX_OVERFLOW),
    Strategy = proplists:get_value(strategy, Opts, round_robin),
    
    %% Start worker processes
    Workers = start_workers(PoolSize),
    
    %% Schedule health checks
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_tick),
    
    {ok, #state{
        workers = Workers,
        overflow = [],
        pool_size = PoolSize,
        max_overflow = MaxOverflow,
        strategy = Strategy,
        next_worker = 0,
        routed = 0,
        errors = 0,
        queue_depth = 0
    }}.

handle_call({route, From, To, Msg}, _From, State) ->
    {Result, NewState} = do_route(From, To, Msg, State),
    {reply, Result, NewState};

handle_call({route_batch, From, Messages}, _From, State) ->
    {Result, NewState} = do_route_batch(From, Messages, State),
    {reply, Result, NewState};

handle_call(get_stats, _From, State) ->
    Stats = #{
        pool_size => State#state.pool_size,
        active_workers => length(State#state.workers),
        overflow_workers => length(State#state.overflow),
        max_overflow => State#state.max_overflow,
        strategy => State#state.strategy,
        total_routed => State#state.routed,
        total_errors => State#state.errors,
        queue_depth => State#state.queue_depth
    },
    {reply, Stats, State};

handle_call({resize, NewSize}, _From, State) ->
    NewState = do_resize(NewSize, State),
    {reply, ok, NewState};

handle_call(health_check, _From, State) ->
    Health = calculate_health(State),
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({route, From, To, Msg}, State) ->
    {_Result, NewState} = do_route(From, To, Msg, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Worker died, restart it
    NewState = handle_worker_death(Pid, State),
    {noreply, NewState};

handle_info(health_tick, State) ->
    %% Periodic health check and cleanup
    NewState = perform_health_check(State),
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_tick),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop all workers
    [exit(Pid, shutdown) || Pid <- State#state.workers],
    [exit(Pid, shutdown) || Pid <- State#state.overflow],
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

start_workers(Count) ->
    [start_worker(N) || N <- lists:seq(1, Count)].

start_worker(Id) ->
    Pid = spawn_link(fun() -> worker_loop(#worker_state{
        id = Id,
        handled = 0,
        errors = 0,
        last_msg = erlang:monotonic_time(millisecond)
    }) end),
    erlang:monitor(process, Pid),
    Pid.

worker_loop(State) ->
    receive
        {route, From, To, Msg, ReplyTo} ->
            Result = do_deliver(From, To, Msg),
            ReplyTo ! {route_result, self(), Result},
            NewState = case Result of
                ok -> State#worker_state{
                    handled = State#worker_state.handled + 1,
                    last_msg = erlang:monotonic_time(millisecond)
                };
                {error, _} -> State#worker_state{
                    errors = State#worker_state.errors + 1
                }
            end,
            worker_loop(NewState);
        
        {get_stats, ReplyTo} ->
            ReplyTo ! {worker_stats, self(), State},
            worker_loop(State);
        
        stop ->
            ok
    end.

do_route(From, To, Msg, State) ->
    %% Select worker based on strategy
    {Worker, NewState} = select_worker(State),
    
    %% Send to worker and wait for result
    Worker ! {route, From, To, Msg, self()},
    
    receive
        {route_result, Worker, ok} ->
            {ok, NewState#state{routed = NewState#state.routed + 1}};
        {route_result, Worker, {error, Reason}} ->
            {{error, Reason}, NewState#state{errors = NewState#state.errors + 1}}
    after ?WORKER_TIMEOUT ->
        {{error, timeout}, NewState#state{errors = NewState#state.errors + 1}}
    end.

do_route_batch(From, Messages, State) ->
    Results = lists:foldl(
        fun({To, Msg}, {Acc, S}) ->
            {Result, NewS} = do_route(From, To, Msg, S),
            {[Result | Acc], NewS}
        end,
        {[], State},
        Messages
    ),
    
    {ResultList, FinalState} = Results,
    
    Successes = length([R || R <- ResultList, R == ok]),
    Failures = length(ResultList) - Successes,
    
    Stats = #{
        total => length(Messages),
        successes => Successes,
        failures => Failures
    },
    
    {{ok, Stats}, FinalState}.

select_worker(State = #state{strategy = round_robin, workers = Workers, next_worker = N}) ->
    Len = length(Workers),
    Index = (N rem Len) + 1,
    Worker = lists:nth(Index, Workers),
    {Worker, State#state{next_worker = N + 1}};

select_worker(State = #state{strategy = least_loaded, workers = Workers}) ->
    %% For simplicity, just use round-robin for now
    %% In production, would track message counts per worker
    select_worker(State#state{strategy = round_robin});

select_worker(State = #state{strategy = hash_ring}) ->
    %% For simplicity, just use round-robin
    select_worker(State#state{strategy = round_robin}).

do_deliver(From, To, Msg) ->
    %% Attempt delivery via various methods
    case catch iris_core:lookup_user(To) of
        {ok, Pid} ->
            %% User is online
            try
                Pid ! {msg, From, Msg},
                ok
            catch
                _:_ -> {error, send_failed}
            end;
        _ ->
            %% User is offline - store message
            case catch iris_offline_storage:store_durable(To, #{
                from => From,
                msg => Msg,
                timestamp => erlang:system_time(second)
            }, 1) of
                ok -> ok;
                _ -> {error, storage_failed}
            end
    end.

do_resize(NewSize, State) ->
    CurrentSize = length(State#state.workers),
    
    if
        NewSize > CurrentSize ->
            %% Add workers
            NewWorkers = start_workers(NewSize - CurrentSize),
            State#state{
                workers = State#state.workers ++ NewWorkers,
                pool_size = NewSize
            };
        NewSize < CurrentSize ->
            %% Remove workers
            {Keep, Remove} = lists:split(NewSize, State#state.workers),
            [Pid ! stop || Pid <- Remove],
            State#state{
                workers = Keep,
                pool_size = NewSize
            };
        true ->
            State
    end.

handle_worker_death(Pid, State) ->
    %% Remove dead worker from lists
    Workers = lists:delete(Pid, State#state.workers),
    Overflow = lists:delete(Pid, State#state.overflow),
    
    %% Start replacement worker if below pool size
    NewWorkers = if
        length(Workers) < State#state.pool_size ->
            Workers ++ [start_worker(length(Workers) + 1)];
        true ->
            Workers
    end,
    
    State#state{workers = NewWorkers, overflow = Overflow}.

perform_health_check(State) ->
    %% Check worker health, restart unhealthy ones
    %% For now, just verify all workers are alive
    AliveWorkers = [Pid || Pid <- State#state.workers, is_process_alive(Pid)],
    
    %% Start replacements for dead workers
    MissingCount = State#state.pool_size - length(AliveWorkers),
    NewWorkers = if
        MissingCount > 0 ->
            AliveWorkers ++ start_workers(MissingCount);
        true ->
            AliveWorkers
    end,
    
    %% Clean up overflow workers that are no longer needed
    AliveOverflow = [Pid || Pid <- State#state.overflow, is_process_alive(Pid)],
    
    State#state{workers = NewWorkers, overflow = AliveOverflow}.

calculate_health(State) ->
    AliveWorkers = length([Pid || Pid <- State#state.workers, is_process_alive(Pid)]),
    PoolSize = State#state.pool_size,
    
    Ratio = AliveWorkers / max(1, PoolSize),
    
    if
        Ratio >= 0.9 -> healthy;
        Ratio >= 0.5 -> degraded;
        true -> unhealthy
    end.
