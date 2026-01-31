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
%% PRINCIPAL_AUDIT_REPORT: Route-specific latency tracking
-export([observe_latency/3, observe_latency/4]).
-export([get_latency_percentile/2, get_latency_stats/1]).

%% RFC-001 v3.0 NFR-32/33: Standard counters and latency tracking
-export([msg_in/0, msg_out/0, ack_sent/0, dedup_hit/0]).
-export([observe_e2e_latency/1, observe_db_write_latency/1]).
-export([observe_inbox_append_latency/1, observe_route_latency/2]).
-export([get_nfr_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(METRICS_TABLE, iris_metrics_table).
-define(LATENCY_TABLE, iris_latency_samples).
-define(MAX_SAMPLES, 1000).  %% Keep last 1000 samples per metric

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
    %% Legacy Counters (backward compatibility)
    ets:insert(?METRICS_TABLE, {iris_messages_sent_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_messages_delivered_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_messages_offline_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_connections_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_auth_success_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_auth_failure_total, 0}),
    ets:insert(?METRICS_TABLE, {iris_errors_total, 0}),
    
    %% ==========================================================================
    %% RFC-001 v3.0 NFR-32: Standard Counters (MUST emit)
    %% ==========================================================================
    ets:insert(?METRICS_TABLE, {iris_msg_in, 0}),          %% Messages received
    ets:insert(?METRICS_TABLE, {iris_msg_out, 0}),         %% Messages delivered
    ets:insert(?METRICS_TABLE, {iris_ack_sent, 0}),        %% ACKs sent to clients
    ets:insert(?METRICS_TABLE, {iris_dedup_hit, 0}),       %% Duplicate message rejections
    ets:insert(?METRICS_TABLE, {iris_inbox_append, 0}),    %% Inbox log appends
    ets:insert(?METRICS_TABLE, {iris_inbox_scan, 0}),      %% Inbox log scans
    ets:insert(?METRICS_TABLE, {iris_group_fanout, 0}),    %% Group message fan-outs
    ets:insert(?METRICS_TABLE, {iris_rate_limited, 0}),    %% Rate limit rejections
    
    %% ==========================================================================
    %% RFC-001 v3.0 NFR-33: Latency Histograms (MUST emit P50/P90/P99)
    %% Histogram sum/count pairs for calculating percentiles
    %% ==========================================================================
    %% End-to-end message latency
    ets:insert(?METRICS_TABLE, {iris_e2e_latency_sum, 0}),
    ets:insert(?METRICS_TABLE, {iris_e2e_latency_count, 0}),
    %% Database write latency
    ets:insert(?METRICS_TABLE, {iris_db_write_latency_sum, 0}),
    ets:insert(?METRICS_TABLE, {iris_db_write_latency_count, 0}),
    %% Inbox append latency
    ets:insert(?METRICS_TABLE, {iris_inbox_append_latency_sum, 0}),
    ets:insert(?METRICS_TABLE, {iris_inbox_append_latency_count, 0}),
    %% Route latency (local vs cross-region)
    ets:insert(?METRICS_TABLE, {iris_route_local_latency_sum, 0}),
    ets:insert(?METRICS_TABLE, {iris_route_local_latency_count, 0}),
    ets:insert(?METRICS_TABLE, {iris_route_remote_latency_sum, 0}),
    ets:insert(?METRICS_TABLE, {iris_route_remote_latency_count, 0}),
    
    %% Gauges
    ets:insert(?METRICS_TABLE, {iris_active_connections, 0}),
    ets:insert(?METRICS_TABLE, {iris_uptime_seconds, 0}),
    ets:insert(?METRICS_TABLE, {iris_process_count, 0}),
    ets:insert(?METRICS_TABLE, {iris_memory_bytes, 0}),
    
    %% Create the latency samples table for percentile calculations
    try
        ets:new(?LATENCY_TABLE, [named_table, public, set, {write_concurrency, true}])
    catch
        error:badarg -> ok  %% Already exists
    end.

format_value(V) when is_integer(V) ->
    integer_to_binary(V);
format_value(V) when is_float(V) ->
    list_to_binary(io_lib:format("~.2f", [V]));
format_value(V) ->
    iolist_to_binary(io_lib:format("~p", [V])).

%% =============================================================================
%% Route-Specific Latency Tracking (Per PRINCIPAL_AUDIT_REPORT Section 5)
%% =============================================================================
%% Metrics tracked:
%% - iris.latency.intra_region.p50/p99
%% - iris.latency.cross_region.{source}.{dest}.p50/p99
%% - iris.presence_lookup.duration_ms
%% - iris.offline_store.duration_ms
%% - iris.rpc.cross_region.duration_ms
%% =============================================================================

%% @doc Observe a latency measurement for a route
%% Route types: intra_region, cross_region, presence_lookup, offline_store, rpc
-spec observe_latency(atom(), atom(), number()) -> ok.
observe_latency(Route, Phase, DurationMs) ->
    observe_latency(Route, Phase, undefined, DurationMs).

%% @doc Observe a latency measurement with source/dest (for cross-region)
-spec observe_latency(atom(), atom(), atom() | undefined, number()) -> ok.
observe_latency(Route, Phase, Dest, DurationMs) ->
    %% Build the metric key
    Key = case Dest of
        undefined -> {latency, Route, Phase};
        _ -> {latency, Route, Phase, Dest}
    end,
    
    %% Store sample in ETS (ring buffer style)
    Timestamp = erlang:system_time(millisecond),
    
    try
        %% Get or create the sample list
        case ets:lookup(?LATENCY_TABLE, Key) of
            [] ->
                ets:insert(?LATENCY_TABLE, {Key, [{Timestamp, DurationMs}]});
            [{Key, Samples}] ->
                %% Keep only last MAX_SAMPLES
                NewSamples = lists:sublist([{Timestamp, DurationMs} | Samples], ?MAX_SAMPLES),
                ets:insert(?LATENCY_TABLE, {Key, NewSamples})
        end
    catch
        error:badarg ->
            %% Table doesn't exist, create it
            try
                ets:new(?LATENCY_TABLE, [named_table, public, set, {write_concurrency, true}]),
                ets:insert(?LATENCY_TABLE, {Key, [{Timestamp, DurationMs}]})
            catch
                error:badarg -> ok  %% Race condition, another process created it
            end
    end,
    
    %% Also update the standard histogram
    MetricName = make_latency_metric_name(Route, Phase, Dest),
    observe(MetricName, DurationMs),
    
    ok.

%% @doc Get a specific percentile for a latency metric
-spec get_latency_percentile(tuple(), float()) -> number() | undefined.
get_latency_percentile(Key, Percentile) when Percentile >= 0, Percentile =< 1 ->
    case ets:lookup(?LATENCY_TABLE, Key) of
        [] -> undefined;
        [{Key, Samples}] ->
            Values = [V || {_, V} <- Samples],
            calculate_percentile(Values, Percentile)
    end.

%% @doc Get latency stats for a metric (p50, p99, count, mean)
-spec get_latency_stats(tuple()) -> map() | undefined.
get_latency_stats(Key) ->
    case ets:lookup(?LATENCY_TABLE, Key) of
        [] -> undefined;
        [{Key, Samples}] ->
            Values = [V || {_, V} <- Samples],
            Count = length(Values),
            if
                Count > 0 ->
                    #{
                        count => Count,
                        p50 => calculate_percentile(Values, 0.5),
                        p99 => calculate_percentile(Values, 0.99),
                        mean => lists:sum(Values) / Count,
                        min => lists:min(Values),
                        max => lists:max(Values)
                    };
                true ->
                    undefined
            end
    end.

%% Internal helpers for latency tracking

make_latency_metric_name(Route, Phase, undefined) ->
    list_to_atom("iris_latency_" ++ atom_to_list(Route) ++ "_" ++ atom_to_list(Phase));
make_latency_metric_name(Route, Phase, Dest) ->
    list_to_atom("iris_latency_" ++ atom_to_list(Route) ++ "_" ++ 
                 atom_to_list(Phase) ++ "_" ++ atom_to_list(Dest)).

calculate_percentile([], _) -> 0;
calculate_percentile(Values, Percentile) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Index = round(Percentile * Len),
    Idx = max(1, min(Index, Len)),
    lists:nth(Idx, Sorted).

%% =============================================================================
%% RFC-001 v3.0 NFR-32: Standard Counter Helpers
%% =============================================================================

%% @doc Increment message received counter
-spec msg_in() -> ok.
msg_in() -> inc(iris_msg_in).

%% @doc Increment message delivered counter
-spec msg_out() -> ok.
msg_out() -> inc(iris_msg_out).

%% @doc Increment ACK sent counter
-spec ack_sent() -> ok.
ack_sent() -> inc(iris_ack_sent).

%% @doc Increment deduplication hit counter
-spec dedup_hit() -> ok.
dedup_hit() -> inc(iris_dedup_hit).

%% =============================================================================
%% RFC-001 v3.0 NFR-33: Latency Histogram Helpers
%% =============================================================================

%% @doc Observe end-to-end message latency
-spec observe_e2e_latency(number()) -> ok.
observe_e2e_latency(LatencyMs) ->
    observe(iris_e2e_latency, LatencyMs),
    observe_latency(message, e2e, LatencyMs).

%% @doc Observe database write latency
-spec observe_db_write_latency(number()) -> ok.
observe_db_write_latency(LatencyMs) ->
    observe(iris_db_write_latency, LatencyMs),
    observe_latency(database, write, LatencyMs).

%% @doc Observe inbox append latency
-spec observe_inbox_append_latency(number()) -> ok.
observe_inbox_append_latency(LatencyMs) ->
    observe(iris_inbox_append_latency, LatencyMs),
    observe_latency(inbox, append, LatencyMs).

%% @doc Observe route latency (local or remote)
-spec observe_route_latency(local | remote, number()) -> ok.
observe_route_latency(local, LatencyMs) ->
    observe(iris_route_local_latency, LatencyMs),
    observe_latency(route, local, LatencyMs);
observe_route_latency(remote, LatencyMs) ->
    observe(iris_route_remote_latency, LatencyMs),
    observe_latency(route, remote, LatencyMs).

%% @doc Get all NFR-required metrics in a structured format
-spec get_nfr_metrics() -> map().
get_nfr_metrics() ->
    Metrics = get_metrics(),
    #{
        %% NFR-32: Standard Counters
        counters => #{
            msg_in => maps:get(iris_msg_in, Metrics, 0),
            msg_out => maps:get(iris_msg_out, Metrics, 0),
            ack_sent => maps:get(iris_ack_sent, Metrics, 0),
            dedup_hit => maps:get(iris_dedup_hit, Metrics, 0)
        },
        %% NFR-33: Latency Histograms
        latencies => #{
            e2e => get_latency_stats({latency, message, e2e}),
            db_write => get_latency_stats({latency, database, write}),
            inbox_append => get_latency_stats({latency, inbox, append}),
            route_local => get_latency_stats({latency, route, local}),
            route_remote => get_latency_stats({latency, route, remote})
        }
    }.
