%%%-------------------------------------------------------------------
%%% @doc Distributed Tracing Module (RFC-001 v3.0 NFR-30, NFR-31)
%%%
%%% Provides distributed tracing capabilities:
%%% - trace_id: Unique identifier for a request across all services
%%% - span_id: Unique identifier for a specific operation within a trace
%%% - parent_span_id: Links spans in a parent-child relationship
%%%
%%% Every RPC MUST propagate trace_id as per NFR-30.
%%% Every operation MUST emit span_id with duration as per NFR-31.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_trace).

%% API - Trace Context
-export([new_trace/0, new_trace/1]).
-export([new_span/1, new_span/2]).
-export([end_span/1, end_span/2]).
-export([get_trace_id/0, get_span_id/0, get_context/0]).
-export([set_context/1, clear_context/0]).

%% API - Context Propagation
-export([inject/1, extract/1]).
-export([with_context/2]).

%% API - Span Recording
-export([record_span/3, record_span/4]).
-export([add_event/2, set_attribute/2]).

%% Internal exports for testing
-export([generate_id/0]).

-define(TRACE_KEY, iris_trace_context).

%% Trace context record
-record(trace_ctx, {
    trace_id     :: binary(),           %% 128-bit trace identifier
    span_id      :: binary(),           %% 64-bit span identifier  
    parent_span  :: binary() | undefined,
    start_time   :: integer(),          %% Monotonic time in microseconds
    attributes   :: map(),
    events       :: list()
}).

-type trace_ctx() :: #trace_ctx{}.
-type span_result() :: #{
    trace_id := binary(),
    span_id := binary(),
    parent_span_id := binary() | undefined,
    name := binary(),
    start_time := integer(),
    end_time := integer(),
    duration_us := integer(),
    attributes := map(),
    events := list()
}.

-export_type([trace_ctx/0, span_result/0]).

%%====================================================================
%% API - Trace Context Management
%%====================================================================

%% @doc Create a new trace with auto-generated trace_id.
-spec new_trace() -> trace_ctx().
new_trace() ->
    new_trace(generate_trace_id()).

%% @doc Create a new trace with specified trace_id (for propagated traces).
-spec new_trace(binary()) -> trace_ctx().
new_trace(TraceId) ->
    Ctx = #trace_ctx{
        trace_id = TraceId,
        span_id = generate_span_id(),
        parent_span = undefined,
        start_time = erlang:monotonic_time(microsecond),
        attributes = #{},
        events = []
    },
    set_context(Ctx),
    Ctx.

%% @doc Create a new span under the current trace.
-spec new_span(binary()) -> trace_ctx().
new_span(SpanName) ->
    new_span(SpanName, #{}).

%% @doc Create a new span with attributes.
-spec new_span(binary(), map()) -> trace_ctx().
new_span(SpanName, Attributes) ->
    case get_context() of
        undefined ->
            %% No active trace, create new one
            Ctx = new_trace(),
            Ctx#trace_ctx{
                attributes = Attributes#{name => SpanName}
            };
        ParentCtx = #trace_ctx{trace_id = TraceId, span_id = ParentSpan} ->
            %% Create child span
            NewCtx = #trace_ctx{
                trace_id = TraceId,
                span_id = generate_span_id(),
                parent_span = ParentSpan,
                start_time = erlang:monotonic_time(microsecond),
                attributes = Attributes#{name => SpanName},
                events = []
            },
            set_context(NewCtx),
            %% Store parent for restoration
            put(iris_trace_parent, ParentCtx),
            NewCtx
    end.

%% @doc End the current span and record metrics.
-spec end_span(binary()) -> span_result().
end_span(SpanName) ->
    end_span(SpanName, ok).

%% @doc End span with status.
-spec end_span(binary(), ok | {error, term()}) -> span_result().
end_span(SpanName, Status) ->
    EndTime = erlang:monotonic_time(microsecond),
    Ctx = get_context(),
    
    Result = case Ctx of
        undefined ->
            #{
                trace_id => <<"unknown">>,
                span_id => <<"unknown">>,
                parent_span_id => undefined,
                name => SpanName,
                start_time => EndTime,
                end_time => EndTime,
                duration_us => 0,
                attributes => #{status => Status},
                events => []
            };
        #trace_ctx{trace_id = TraceId, span_id = SpanId, parent_span = ParentSpan,
                   start_time = StartTime, attributes = Attrs, events = Events} ->
            Duration = EndTime - StartTime,
            
            %% Record to metrics
            record_span_metrics(SpanName, Duration, Status),
            
            #{
                trace_id => TraceId,
                span_id => SpanId,
                parent_span_id => ParentSpan,
                name => SpanName,
                start_time => StartTime,
                end_time => EndTime,
                duration_us => Duration,
                attributes => Attrs#{status => Status},
                events => lists:reverse(Events)
            }
    end,
    
    %% Restore parent context if exists
    case get(iris_trace_parent) of
        undefined -> ok;
        ParentCtx ->
            set_context(ParentCtx),
            erase(iris_trace_parent)
    end,
    
    Result.

%% @doc Get current trace_id.
-spec get_trace_id() -> binary() | undefined.
get_trace_id() ->
    case get_context() of
        undefined -> undefined;
        #trace_ctx{trace_id = TraceId} -> TraceId
    end.

%% @doc Get current span_id.
-spec get_span_id() -> binary() | undefined.
get_span_id() ->
    case get_context() of
        undefined -> undefined;
        #trace_ctx{span_id = SpanId} -> SpanId
    end.

%% @doc Get the current trace context.
-spec get_context() -> trace_ctx() | undefined.
get_context() ->
    get(?TRACE_KEY).

%% @doc Set the current trace context.
-spec set_context(trace_ctx()) -> ok.
set_context(Ctx) ->
    put(?TRACE_KEY, Ctx),
    ok.

%% @doc Clear the current trace context.
-spec clear_context() -> ok.
clear_context() ->
    erase(?TRACE_KEY),
    erase(iris_trace_parent),
    ok.

%%====================================================================
%% API - Context Propagation
%%====================================================================

%% @doc Inject trace context into a carrier map (for RPC propagation).
%% Returns a map suitable for inclusion in RPC calls.
-spec inject(map()) -> map().
inject(Carrier) ->
    case get_context() of
        undefined ->
            Carrier;
        #trace_ctx{trace_id = TraceId, span_id = SpanId} ->
            Carrier#{
                <<"trace_id">> => TraceId,
                <<"span_id">> => SpanId
            }
    end.

%% @doc Extract trace context from a carrier map (received RPC).
%% Creates a child span under the extracted trace.
-spec extract(map()) -> trace_ctx() | undefined.
extract(Carrier) ->
    case {maps:get(<<"trace_id">>, Carrier, undefined),
          maps:get(<<"span_id">>, Carrier, undefined)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {TraceId, ParentSpan} ->
            Ctx = #trace_ctx{
                trace_id = TraceId,
                span_id = generate_span_id(),
                parent_span = ParentSpan,
                start_time = erlang:monotonic_time(microsecond),
                attributes = #{},
                events = []
            },
            set_context(Ctx),
            Ctx
    end.

%% @doc Execute a function with trace context, ensuring cleanup.
-spec with_context(trace_ctx(), fun(() -> T)) -> T.
with_context(Ctx, Fun) ->
    OldCtx = get_context(),
    try
        set_context(Ctx),
        Fun()
    after
        case OldCtx of
            undefined -> clear_context();
            _ -> set_context(OldCtx)
        end
    end.

%%====================================================================
%% API - Span Recording
%%====================================================================

%% @doc Record a complete span (for simple operations).
-spec record_span(binary(), integer(), ok | {error, term()}) -> ok.
record_span(SpanName, DurationUs, Status) ->
    record_span(SpanName, DurationUs, Status, #{}).

%% @doc Record a complete span with attributes.
-spec record_span(binary(), integer(), ok | {error, term()}, map()) -> ok.
record_span(SpanName, DurationUs, Status, Attributes) ->
    TraceId = case get_trace_id() of
        undefined -> <<"untraced">>;
        T -> T
    end,
    SpanId = generate_span_id(),
    
    %% Record metrics
    record_span_metrics(SpanName, DurationUs, Status),
    
    %% Log for structured logging / export
    logger:debug("span: ~s trace=~s span=~s duration_us=~p status=~p attrs=~p",
                [SpanName, TraceId, SpanId, DurationUs, Status, Attributes]),
    ok.

%% @doc Add an event to the current span.
-spec add_event(binary(), map()) -> ok.
add_event(EventName, Attributes) ->
    case get_context() of
        undefined -> ok;
        Ctx = #trace_ctx{events = Events} ->
            Event = #{
                name => EventName,
                timestamp => erlang:monotonic_time(microsecond),
                attributes => Attributes
            },
            set_context(Ctx#trace_ctx{events = [Event | Events]})
    end,
    ok.

%% @doc Set an attribute on the current span.
-spec set_attribute(binary(), term()) -> ok.
set_attribute(Key, Value) ->
    case get_context() of
        undefined -> ok;
        Ctx = #trace_ctx{attributes = Attrs} ->
            set_context(Ctx#trace_ctx{attributes = Attrs#{Key => Value}})
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate a 128-bit trace ID (32 hex chars).
generate_trace_id() ->
    generate_id(16).

%% @doc Generate a 64-bit span ID (16 hex chars).
generate_span_id() ->
    generate_id(8).

%% @doc Generate a random hex ID of given byte length.
-spec generate_id() -> binary().
generate_id() ->
    generate_id(8).

generate_id(Bytes) ->
    Rand = crypto:strong_rand_bytes(Bytes),
    list_to_binary([integer_to_list(B, 16) || <<B>> <= Rand]).

%% @doc Record span metrics to iris_metrics.
record_span_metrics(SpanName, DurationUs, Status) ->
    %% Convert to milliseconds for metrics
    DurationMs = DurationUs / 1000,
    
    %% Standard latency histogram
    MetricName = binary_to_atom(<<"span_", SpanName/binary, "_duration_ms">>, utf8),
    catch iris_metrics:observe(MetricName, DurationMs),
    
    %% Status counter
    StatusMetric = case Status of
        ok -> 
            binary_to_atom(<<"span_", SpanName/binary, "_success">>, utf8);
        {error, _} -> 
            binary_to_atom(<<"span_", SpanName/binary, "_error">>, utf8)
    end,
    catch iris_metrics:inc(StatusMetric).
