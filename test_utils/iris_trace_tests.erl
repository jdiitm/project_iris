%%%-------------------------------------------------------------------
%%% @doc Tests for Distributed Tracing (iris_trace) module.
%%% 
%%% RFC-001 v3.0 NFR-30, NFR-31 Requirements:
%%% - Every RPC MUST propagate trace_id
%%% - Every operation MUST emit span_id with duration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_trace_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    iris_trace:clear_context(),
    ok.

cleanup(_) ->
    iris_trace:clear_context(),
    ok.

trace_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"New trace generation", fun test_new_trace/0},
        {"Trace with specified ID", fun test_trace_with_id/0},
        {"Span creation", fun test_span_creation/0},
        {"Nested spans", fun test_nested_spans/0},
        {"Span ending", fun test_span_ending/0},
        {"Context management", fun test_context_management/0},
        {"Context propagation - inject", fun test_inject/0},
        {"Context propagation - extract", fun test_extract/0},
        {"With context helper", fun test_with_context/0},
        {"Record span directly", fun test_record_span/0},
        {"Span events", fun test_span_events/0},
        {"Span attributes", fun test_span_attributes/0},
        {"Concurrent traces", fun test_concurrent_traces/0}
     ]
    }.

%% ============================================================================
%% Basic Trace Tests
%% ============================================================================

test_new_trace() ->
    %% Initially no context
    ?assertEqual(undefined, iris_trace:get_context()),
    ?assertEqual(undefined, iris_trace:get_trace_id()),
    
    %% Create new trace
    Ctx = iris_trace:new_trace(),
    ?assertMatch({trace_ctx, _, _, _, _, _, _}, Ctx),
    
    %% Context should be set
    ?assertNotEqual(undefined, iris_trace:get_context()),
    
    %% Trace ID should be a binary
    TraceId = iris_trace:get_trace_id(),
    ?assert(is_binary(TraceId)),
    ?assert(byte_size(TraceId) >= 16),  %% At least 16 hex chars
    
    %% Span ID should be set
    SpanId = iris_trace:get_span_id(),
    ?assert(is_binary(SpanId)),
    ?assert(byte_size(SpanId) >= 8).

test_trace_with_id() ->
    %% Create trace with specific ID
    MyTraceId = <<"my-custom-trace-id-123">>,
    Ctx = iris_trace:new_trace(MyTraceId),
    
    ?assertEqual(MyTraceId, iris_trace:get_trace_id()),
    ?assertMatch({trace_ctx, MyTraceId, _, _, _, _, _}, Ctx).

%% ============================================================================
%% Span Tests
%% ============================================================================

test_span_creation() ->
    %% Create a trace first
    _Trace = iris_trace:new_trace(),
    OriginalSpan = iris_trace:get_span_id(),
    
    %% Create a new span
    Span = iris_trace:new_span(<<"test_operation">>),
    NewSpanId = iris_trace:get_span_id(),
    
    %% Span ID should change
    ?assertNotEqual(OriginalSpan, NewSpanId),
    
    %% Trace ID should stay the same
    ?assertMatch({trace_ctx, _, NewSpanId, OriginalSpan, _, _, _}, Span).

test_nested_spans() ->
    %% Create trace
    iris_trace:new_trace(),
    TraceId = iris_trace:get_trace_id(),
    RootSpan = iris_trace:get_span_id(),
    
    %% Create child span
    iris_trace:new_span(<<"operation_1">>),
    Span1 = iris_trace:get_span_id(),
    ?assertNotEqual(RootSpan, Span1),
    
    %% Create grandchild span
    iris_trace:new_span(<<"operation_2">>),
    Span2 = iris_trace:get_span_id(),
    ?assertNotEqual(Span1, Span2),
    
    %% All spans should share trace ID
    ?assertEqual(TraceId, iris_trace:get_trace_id()).

test_span_ending() ->
    %% Create trace and span
    iris_trace:new_trace(),
    OriginalSpan = iris_trace:get_span_id(),
    
    %% Create child span
    iris_trace:new_span(<<"child_operation">>),
    ChildSpan = iris_trace:get_span_id(),
    ?assertNotEqual(OriginalSpan, ChildSpan),
    
    %% End the child span
    Result = iris_trace:end_span(<<"child_operation">>),
    
    %% Should return span result
    ?assertMatch(#{
        trace_id := _,
        span_id := ChildSpan,
        parent_span_id := OriginalSpan,
        name := <<"child_operation">>,
        duration_us := _
    }, Result),
    
    %% Duration should be positive
    #{duration_us := Duration} = Result,
    ?assert(Duration >= 0),
    
    %% Context should be restored to parent
    ?assertEqual(OriginalSpan, iris_trace:get_span_id()).

%% ============================================================================
%% Context Management Tests
%% ============================================================================

test_context_management() ->
    %% Start with no context
    ?assertEqual(undefined, iris_trace:get_context()),
    
    %% Set context
    Ctx = iris_trace:new_trace(),
    ?assertEqual(Ctx, iris_trace:get_context()),
    
    %% Clear context
    iris_trace:clear_context(),
    ?assertEqual(undefined, iris_trace:get_context()).

test_inject() ->
    %% Create trace
    iris_trace:new_trace(),
    TraceId = iris_trace:get_trace_id(),
    SpanId = iris_trace:get_span_id(),
    
    %% Inject into carrier
    Carrier = #{<<"some_key">> => <<"some_value">>},
    Injected = iris_trace:inject(Carrier),
    
    %% Should contain trace context
    ?assertEqual(TraceId, maps:get(<<"trace_id">>, Injected)),
    ?assertEqual(SpanId, maps:get(<<"span_id">>, Injected)),
    
    %% Original key should be preserved
    ?assertEqual(<<"some_value">>, maps:get(<<"some_key">>, Injected)).

test_extract() ->
    %% Clear any existing context
    iris_trace:clear_context(),
    
    %% Create carrier with trace context
    Carrier = #{
        <<"trace_id">> => <<"extracted-trace-123">>,
        <<"span_id">> => <<"extracted-span-456">>
    },
    
    %% Extract should create new context
    Ctx = iris_trace:extract(Carrier),
    ?assertNotEqual(undefined, Ctx),
    
    %% Trace ID should match
    ?assertEqual(<<"extracted-trace-123">>, iris_trace:get_trace_id()),
    
    %% Span ID should be NEW (child of extracted span)
    ?assertNotEqual(<<"extracted-span-456">>, iris_trace:get_span_id()),
    
    %% Parent should be the extracted span
    {trace_ctx, _, _, ParentSpan, _, _, _} = iris_trace:get_context(),
    ?assertEqual(<<"extracted-span-456">>, ParentSpan).

test_with_context() ->
    %% Start fresh
    iris_trace:clear_context(),
    
    %% Create outer context and save its ID
    OuterCtx = iris_trace:new_trace(<<"outer-trace">>),
    ?assertEqual(<<"outer-trace">>, iris_trace:get_trace_id()),
    
    %% Create a separate inner context (this will overwrite current context)
    InnerCtx = iris_trace:new_trace(<<"inner-trace">>),
    ?assertEqual(<<"inner-trace">>, iris_trace:get_trace_id()),
    
    %% Now restore the outer context
    iris_trace:set_context(OuterCtx),
    ?assertEqual(<<"outer-trace">>, iris_trace:get_trace_id()),
    
    %% Use with_context to temporarily switch to inner
    Result = iris_trace:with_context(InnerCtx, fun() ->
        ?assertEqual(<<"inner-trace">>, iris_trace:get_trace_id()),
        inner_result
    end),
    
    ?assertEqual(inner_result, Result),
    
    %% Should be restored to outer context
    ?assertEqual(<<"outer-trace">>, iris_trace:get_trace_id()).

%% ============================================================================
%% Span Recording Tests
%% ============================================================================

test_record_span() ->
    %% Create trace for context
    iris_trace:new_trace(),
    
    %% Record a simple span
    ok = iris_trace:record_span(<<"simple_operation">>, 1000, ok),
    
    %% Record a span with error
    ok = iris_trace:record_span(<<"failed_operation">>, 500, {error, timeout}),
    
    %% Record with attributes
    ok = iris_trace:record_span(<<"detailed_operation">>, 2000, ok, #{
        user_id => <<"alice">>,
        message_count => 5
    }).

test_span_events() ->
    %% Create trace and span
    iris_trace:new_trace(),
    iris_trace:new_span(<<"operation_with_events">>),
    
    %% Add events
    ok = iris_trace:add_event(<<"started_processing">>, #{item_count => 100}),
    ok = iris_trace:add_event(<<"checkpoint">>, #{processed => 50}),
    ok = iris_trace:add_event(<<"completed">>, #{total => 100}),
    
    %% End span and check events
    Result = iris_trace:end_span(<<"operation_with_events">>),
    #{events := Events} = Result,
    
    ?assertEqual(3, length(Events)),
    
    %% Events should be in chronological order
    [E1, E2, E3] = Events,
    ?assertEqual(<<"started_processing">>, maps:get(name, E1)),
    ?assertEqual(<<"checkpoint">>, maps:get(name, E2)),
    ?assertEqual(<<"completed">>, maps:get(name, E3)).

test_span_attributes() ->
    %% Create trace and span
    iris_trace:new_trace(),
    iris_trace:new_span(<<"operation_with_attrs">>),
    
    %% Add attributes
    ok = iris_trace:set_attribute(<<"user_id">>, <<"bob">>),
    ok = iris_trace:set_attribute(<<"request_size">>, 1024),
    ok = iris_trace:set_attribute(<<"is_retry">>, true),
    
    %% End span and check attributes
    Result = iris_trace:end_span(<<"operation_with_attrs">>),
    #{attributes := Attrs} = Result,
    
    ?assertEqual(<<"bob">>, maps:get(<<"user_id">>, Attrs)),
    ?assertEqual(1024, maps:get(<<"request_size">>, Attrs)),
    ?assertEqual(true, maps:get(<<"is_retry">>, Attrs)).

%% ============================================================================
%% Concurrent Trace Tests
%% ============================================================================

test_concurrent_traces() ->
    %% Each process should have its own trace context
    Self = self(),
    
    Pids = [spawn(fun() ->
        %% Create unique trace
        iris_trace:new_trace(),
        TraceId = iris_trace:get_trace_id(),
        
        %% Do some "work"
        timer:sleep(10),
        
        %% Verify trace ID hasn't changed
        ?assertEqual(TraceId, iris_trace:get_trace_id()),
        
        Self ! {done, self(), TraceId}
    end) || _ <- lists:seq(1, 10)],
    
    %% Collect results
    TraceIds = [receive {done, Pid, TId} -> TId after 1000 -> error(timeout) end || Pid <- Pids],
    
    %% All trace IDs should be unique
    UniqueIds = lists:usort(TraceIds),
    ?assertEqual(10, length(UniqueIds)).

%% ============================================================================
%% ID Generation Tests
%% ============================================================================

id_generation_test_() ->
    [
        {"Generate ID produces valid hex", fun() ->
            Id = iris_trace:generate_id(),
            ?assert(is_binary(Id)),
            %% Should be valid hex (all chars 0-9 or A-F)
            ?assert(is_valid_hex(Id))
        end},
        
        {"Generated IDs are unique", fun() ->
            Ids = [iris_trace:generate_id() || _ <- lists:seq(1, 1000)],
            UniqueIds = lists:usort(Ids),
            ?assertEqual(1000, length(UniqueIds))
        end}
    ].

is_valid_hex(<<>>) -> true;
is_valid_hex(<<C, Rest/binary>>) when C >= $0, C =< $9 -> is_valid_hex(Rest);
is_valid_hex(<<C, Rest/binary>>) when C >= $A, C =< $F -> is_valid_hex(Rest);
is_valid_hex(<<C, Rest/binary>>) when C >= $a, C =< $f -> is_valid_hex(Rest);
is_valid_hex(_) -> false.

%% ============================================================================
%% Edge Cases
%% ============================================================================

edge_cases_test_() ->
    [
        {"End span without active context", fun() ->
            iris_trace:clear_context(),
            Result = iris_trace:end_span(<<"orphan_span">>),
            ?assertMatch(#{trace_id := <<"unknown">>}, Result)
        end},
        
        {"Add event without active context", fun() ->
            iris_trace:clear_context(),
            %% Should not crash
            ok = iris_trace:add_event(<<"event">>, #{})
        end},
        
        {"Set attribute without active context", fun() ->
            iris_trace:clear_context(),
            %% Should not crash
            ok = iris_trace:set_attribute(<<"key">>, <<"value">>)
        end},
        
        {"Extract from empty carrier", fun() ->
            iris_trace:clear_context(),
            Result = iris_trace:extract(#{}),
            ?assertEqual(undefined, Result)
        end},
        
        {"Extract with partial carrier", fun() ->
            iris_trace:clear_context(),
            %% Only trace_id, no span_id
            Result = iris_trace:extract(#{<<"trace_id">> => <<"abc">>}),
            ?assertEqual(undefined, Result)
        end},
        
        {"Inject without active context", fun() ->
            iris_trace:clear_context(),
            Carrier = #{<<"key">> => <<"value">>},
            Result = iris_trace:inject(Carrier),
            %% Should return carrier unchanged
            ?assertEqual(Carrier, Result)
        end}
    ].

%% ============================================================================
%% RPC Propagation Simulation
%% ============================================================================

rpc_propagation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"Full RPC propagation flow", fun test_rpc_propagation/0}
     ]
    }.

test_rpc_propagation() ->
    %% Simulate: Client -> Edge -> Core -> Storage
    
    %% Client starts trace
    _ClientCtx = iris_trace:new_trace(),
    ClientTraceId = iris_trace:get_trace_id(),
    
    %% Client creates span and prepares RPC
    iris_trace:new_span(<<"client_request">>),
    ClientSpan = iris_trace:get_span_id(),
    
    %% Client injects context into RPC
    RpcPayload = iris_trace:inject(#{message => <<"hello">>}),
    
    %% Simulate "sending" to Edge (new process)
    EdgeTraceId = simulate_edge_handler(RpcPayload),
    
    %% Trace ID should be preserved
    ?assertEqual(ClientTraceId, EdgeTraceId),
    
    %% Client ends span
    iris_trace:end_span(<<"client_request">>),
    
    %% Verify client context is intact
    ?assertEqual(ClientTraceId, iris_trace:get_trace_id()).

simulate_edge_handler(Payload) ->
    %% Edge extracts context
    _Ctx = iris_trace:extract(Payload),
    TraceId = iris_trace:get_trace_id(),
    
    %% Edge creates its own span
    iris_trace:new_span(<<"edge_handler">>),
    
    %% Edge processes request...
    timer:sleep(1),
    
    %% Edge ends span
    iris_trace:end_span(<<"edge_handler">>),
    
    %% Return trace ID for verification
    TraceId.
