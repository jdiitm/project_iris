-module(iris_metrics).

%% =============================================================================
%% Prometheus-Compatible Metrics Exporter
%% =============================================================================
%% P1-4: Provides observability for production operations.
%% Exposes metrics in Prometheus text format at /metrics endpoint.
%% =============================================================================

-behaviour(gen_server).

-export([start_link/0]).
-export([inc/1, inc/2, observe/2, set/2]).
-export([get_metrics/0, export_prometheus/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(METRICS_TABLE, iris_metrics_table).

-record(state, {
    start_time :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Increment a counter by 1
-spec inc(atom()) -> ok.
inc(Metric) ->
    inc(Metric, 1).

%% @doc Increment a counter by N
-spec inc(atom(), integer()) -> ok.
inc(Metric, N) ->
    try
        ets:update_counter(?METRICS_TABLE, Metric, N, {Metric, 0})
    catch
        error:badarg ->
            %% Table doesn't exist yet, ignore
            ok
    end,
    ok.

%% @doc Record a value in a histogram/gauge
-spec observe(atom(), number()) -> ok.
observe(Metric, Value) ->
    gen_server:cast(?SERVER, {observe, Metric, Value}).

%% @doc Set a gauge to a specific value
-spec set(atom(), number()) -> ok.
set(Metric, Value) ->
    try
        ets:insert(?METRICS_TABLE, {Metric, Value})
    catch
        error:badarg -> ok
    end,
    ok.

%% @doc Get all metrics as a map
-spec get_metrics() -> map().
get_metrics() ->
    try
        Maps = ets:tab2list(?METRICS_TABLE),
        maps:from_list(Maps)
    catch
        error:badarg -> #{}
    end.

%% @doc Export metrics in Prometheus text format
-spec export_prometheus() -> binary().
export_prometheus() ->
    Metrics = get_metrics(),
    Lines = maps:fold(fun(K, V, Acc) ->
        Name = atom_to_binary(K, utf8),
        Value = format_value(V),
        [<<Name/binary, " ", Value/binary, "\n">> | Acc]
    end, [], Metrics),
    iolist_to_binary(Lines).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Create metrics table
    ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]),
    
    %% Initialize default metrics
    init_default_metrics(),
    
    %% Start periodic metric collection
    erlang:send_after(5000, self(), collect_system_metrics),
    
    StartTime = os:system_time(second),
    logger:info("Iris metrics exporter started"),
    
    {ok, #state{start_time = StartTime}}.

handle_call(get_all, _From, State) ->
    {reply, get_metrics(), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({observe, Metric, Value}, State) ->
    %% For histograms, we track sum and count
    SumKey = list_to_atom(atom_to_list(Metric) ++ "_sum"),
    CountKey = list_to_atom(atom_to_list(Metric) ++ "_count"),
    
    try
        ets:update_counter(?METRICS_TABLE, SumKey, {2, Value}, {SumKey, 0}),
        ets:update_counter(?METRICS_TABLE, CountKey, 1, {CountKey, 0})
    catch
        error:badarg -> ok
    end,
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_system_metrics, State = #state{start_time = StartTime}) ->
    %% Collect system metrics
    Now = os:system_time(second),
    Uptime = Now - StartTime,
    set(iris_uptime_seconds, Uptime),
    
    %% Process count
    set(iris_process_count, erlang:system_info(process_count)),
    
    %% Memory
    [{total, TotalMem}] = erlang:memory([total]),
    set(iris_memory_bytes, TotalMem),
    
    %% ETS memory
    EtsMem = lists:sum([ets:info(T, memory) * erlang:system_info(wordsize) 
                        || T <- ets:all(), is_atom(T)]),
    set(iris_ets_memory_bytes, EtsMem),
    
    %% Scheduler utilization (if available)
    try
        SchedUtil = scheduler:utilization(1),
        AvgUtil = lists:sum([U || {_, U, _} <- SchedUtil]) / length(SchedUtil),
        set(iris_scheduler_utilization, round(AvgUtil * 100))
    catch
        _:_ -> ok
    end,
    
    %% Node count
    set(iris_connected_nodes, length(nodes()) + 1),
    
    %% Reschedule
    erlang:send_after(5000, self(), collect_system_metrics),
    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal
%% =============================================================================

init_default_metrics() ->
    %% Counters
    ets:insert(?METRICS_TABLE, {iris_messages_sent_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_messages_delivered_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_messages_offline_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_connections_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_auth_success_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_auth_failure_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_errors_total, 0}),
    
    %% Gauges
    ets:insert(?METRICS_TABLE, {iris_active_connections, 0}),
    ets:insert(?METRICS_TABLE, {iris_uptime_seconds, 0}),
    ets:insert(?METRICS_TABLE, {iris_process_count, 0}),
    ets:insert(?METRICS_TABLE, {iris_memory_bytes, 0}).

format_value(V) when is_integer(V) ->
    integer_to_binary(V);
format_value(V) when is_float(V) ->
    list_to_binary(io_lib:format("~.2f", [V]));
format_value(V) ->
    iolist_to_binary(io_lib:format("~p", [V])).
