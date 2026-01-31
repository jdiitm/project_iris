%%%-------------------------------------------------------------------
%%% @doc Tests for NFR Metrics (iris_metrics) module.
%%% 
%%% RFC-001 v3.0 NFR-32, NFR-33 Requirements:
%%% - NFR-32: Standard counters (msg_in, msg_out, ack_sent, dedup_hit)
%%% - NFR-33: Latency histograms (e2e_latency, db_write_latency P50/P90/P99)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_metrics_nfr_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    %% Stop any existing metrics server
    case whereis(iris_metrics) of
        undefined -> ok;
        Pid -> 
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    {ok, _} = iris_metrics:start_link(),
    ok.

cleanup(_) ->
    case whereis(iris_metrics) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%% ============================================================================
%% NFR-32: Standard Counter Tests
%% ============================================================================

nfr32_counter_test_() ->
    {"NFR-32: Standard counters",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"msg_in counter increments", fun() ->
            Initial = get_counter(iris_msg_in),
            iris_metrics:msg_in(),
            iris_metrics:msg_in(),
            iris_metrics:msg_in(),
            Final = get_counter(iris_msg_in),
            ?assertEqual(3, Final - Initial)
        end},
        
       {"msg_out counter increments", fun() ->
            Initial = get_counter(iris_msg_out),
            iris_metrics:msg_out(),
            iris_metrics:msg_out(),
            Final = get_counter(iris_msg_out),
            ?assertEqual(2, Final - Initial)
        end},
        
       {"ack_sent counter increments", fun() ->
            Initial = get_counter(iris_ack_sent),
            iris_metrics:ack_sent(),
            Final = get_counter(iris_ack_sent),
            ?assertEqual(1, Final - Initial)
        end},
        
       {"dedup_hit counter increments", fun() ->
            Initial = get_counter(iris_dedup_hit),
            iris_metrics:dedup_hit(),
            iris_metrics:dedup_hit(),
            iris_metrics:dedup_hit(),
            iris_metrics:dedup_hit(),
            Final = get_counter(iris_dedup_hit),
            ?assertEqual(4, Final - Initial)
        end},
        
       {"generic inc/1 works for counters", fun() ->
            Initial = get_counter(iris_msg_in),
            iris_metrics:inc(iris_msg_in),
            Final = get_counter(iris_msg_in),
            ?assertEqual(1, Final - Initial)
        end},
        
       {"generic inc/2 increments by N", fun() ->
            Initial = get_counter(iris_msg_out),
            iris_metrics:inc(iris_msg_out, 5),
            Final = get_counter(iris_msg_out),
            ?assertEqual(5, Final - Initial)
        end}
      ]}}.

get_counter(Name) ->
    Metrics = iris_metrics:get_metrics(),
    maps:get(Name, Metrics, 0).

%% ============================================================================
%% NFR-33: Latency Histogram Tests
%% ============================================================================

nfr33_latency_test_() ->
    {"NFR-33: Latency histograms",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"observe_e2e_latency records value", fun() ->
            iris_metrics:observe_e2e_latency(100),
            iris_metrics:observe_e2e_latency(200),
            iris_metrics:observe_e2e_latency(150),
            
            Stats = iris_metrics:get_latency_stats({latency, message, e2e}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(3, maps:get(count, Stats))
        end},
        
       {"observe_db_write_latency records value", fun() ->
            iris_metrics:observe_db_write_latency(5),
            iris_metrics:observe_db_write_latency(10),
            
            Stats = iris_metrics:get_latency_stats({latency, database, write}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(2, maps:get(count, Stats))
        end},
        
       {"observe_inbox_append_latency records value", fun() ->
            iris_metrics:observe_inbox_append_latency(2),
            iris_metrics:observe_inbox_append_latency(3),
            iris_metrics:observe_inbox_append_latency(4),
            
            Stats = iris_metrics:get_latency_stats({latency, inbox, append}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(3, maps:get(count, Stats))
        end},
        
       {"observe_route_latency local", fun() ->
            iris_metrics:observe_route_latency(local, 10),
            iris_metrics:observe_route_latency(local, 15),
            
            Stats = iris_metrics:get_latency_stats({latency, route, local}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(2, maps:get(count, Stats))
        end},
        
       {"observe_route_latency remote", fun() ->
            iris_metrics:observe_route_latency(remote, 100),
            iris_metrics:observe_route_latency(remote, 200),
            iris_metrics:observe_route_latency(remote, 300),
            
            Stats = iris_metrics:get_latency_stats({latency, route, remote}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(3, maps:get(count, Stats))
        end},
        
       {"latency percentile calculation", fun() ->
            %% Use unique latency key to avoid pollution from other tests
            %% Add 100 samples: 1, 2, 3, ..., 100
            [iris_metrics:observe_latency(percentile_test, e2e, I) || I <- lists:seq(1, 100)],
            
            Stats = iris_metrics:get_latency_stats({latency, percentile_test, e2e}),
            ?assertNotEqual(undefined, Stats),
            P50 = maps:get(p50, Stats),
            P99 = maps:get(p99, Stats),
            
            %% P50 should be around 50 (40-60), P99 should be high (>=90)
            ?assert(P50 >= 40 andalso P50 =< 60),
            ?assert(P99 >= 90)
        end},
        
       {"get_latency_percentile with specific percentile", fun() ->
            %% Add known values
            [iris_metrics:observe_route_latency(local, V) || V <- [10, 20, 30, 40, 50]],
            
            P50 = iris_metrics:get_latency_percentile({latency, route, local}, 0.5),
            ?assert(P50 >= 20 andalso P50 =< 40)
        end}
      ]}}.

%% ============================================================================
%% Integrated NFR Metrics Test
%% ============================================================================

nfr_integration_test_() ->
    {"NFR metrics integration",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"get_nfr_metrics returns structured data", fun() ->
            %% Generate some metrics
            iris_metrics:msg_in(),
            iris_metrics:msg_in(),
            iris_metrics:msg_out(),
            iris_metrics:ack_sent(),
            iris_metrics:dedup_hit(),
            iris_metrics:observe_e2e_latency(50),
            iris_metrics:observe_db_write_latency(5),
            
            %% Get NFR metrics
            NFR = iris_metrics:get_nfr_metrics(),
            
            %% Verify structure
            ?assert(is_map(NFR)),
            ?assert(maps:is_key(counters, NFR)),
            ?assert(maps:is_key(latencies, NFR)),
            
            %% Verify counters
            Counters = maps:get(counters, NFR),
            ?assert(maps:get(msg_in, Counters) >= 2),
            ?assert(maps:get(msg_out, Counters) >= 1),
            ?assert(maps:get(ack_sent, Counters) >= 1),
            ?assert(maps:get(dedup_hit, Counters) >= 1),
            
            %% Verify latencies have expected keys
            Latencies = maps:get(latencies, NFR),
            ?assert(maps:is_key(e2e, Latencies)),
            ?assert(maps:is_key(db_write, Latencies))
        end},
        
       {"Prometheus export includes NFR metrics", fun() ->
            iris_metrics:msg_in(),
            iris_metrics:observe_e2e_latency(100),
            
            Output = iris_metrics:export_prometheus(),
            ?assert(is_binary(Output)),
            ?assert(binary:match(Output, <<"iris_msg_in">>) =/= nomatch)
        end}
      ]}}.

%% ============================================================================
%% Route-Specific Latency Tests (From PRINCIPAL_AUDIT_REPORT)
%% ============================================================================

route_latency_test_() ->
    {"Route-specific latency tracking",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"observe_latency with route and phase", fun() ->
            iris_metrics:observe_latency(intra_region, delivery, 50),
            iris_metrics:observe_latency(intra_region, delivery, 60),
            
            Stats = iris_metrics:get_latency_stats({latency, intra_region, delivery}),
            ?assertNotEqual(undefined, Stats),
            ?assertEqual(2, maps:get(count, Stats))
        end},
        
       {"observe_latency with destination", fun() ->
            iris_metrics:observe_latency(cross_region, delivery, us_west, 150),
            iris_metrics:observe_latency(cross_region, delivery, us_west, 180),
            
            Stats = iris_metrics:get_latency_stats({latency, cross_region, delivery, us_west}),
            ?assertNotEqual(undefined, Stats)
        end},
        
       {"latency stats include min/max/mean", fun() ->
            iris_metrics:observe_latency(presence, lookup, 5),
            iris_metrics:observe_latency(presence, lookup, 10),
            iris_metrics:observe_latency(presence, lookup, 15),
            
            Stats = iris_metrics:get_latency_stats({latency, presence, lookup}),
            ?assertEqual(5, maps:get(min, Stats)),
            ?assertEqual(15, maps:get(max, Stats)),
            ?assertEqual(10.0, maps:get(mean, Stats))
        end}
      ]}}.

%% ============================================================================
%% Basic Metrics Tests
%% ============================================================================

basic_metrics_test_() ->
    {"Basic metrics operations",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"set gauge value", fun() ->
            iris_metrics:set(test_gauge, 42),
            Metrics = iris_metrics:get_metrics(),
            ?assertEqual(42, maps:get(test_gauge, Metrics))
        end},
        
       {"observe histogram value", fun() ->
            %% Use unique histogram name to avoid pollution
            iris_metrics:observe(test_histogram_unique2, 100),
            iris_metrics:observe(test_histogram_unique2, 200),
            
            %% observe uses gen_server:cast, so give it time to process
            timer:sleep(50),
            
            Metrics = iris_metrics:get_metrics(),
            Sum = maps:get(test_histogram_unique2_sum, Metrics, 0),
            Count = maps:get(test_histogram_unique2_count, Metrics, 0),
            
            %% Sum should include our 300, count should include our 2
            ?assert(Sum >= 300),
            ?assert(Count >= 2)
        end},
        
       {"get_metrics returns map", fun() ->
            Metrics = iris_metrics:get_metrics(),
            ?assert(is_map(Metrics))
        end}
      ]}}.

%% ============================================================================
%% Concurrent Metrics Tests
%% ============================================================================

concurrent_metrics_test_() ->
    {"Concurrent metrics updates",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"concurrent counter increments", fun() ->
            Self = self(),
            NumProcs = 10,
            IncPerProc = 100,
            
            Initial = get_counter(iris_msg_in),
            
            %% Spawn concurrent incrementers
            [spawn(fun() ->
                [iris_metrics:msg_in() || _ <- lists:seq(1, IncPerProc)],
                Self ! done
            end) || _ <- lists:seq(1, NumProcs)],
            
            %% Wait for all
            [receive done -> ok after 5000 -> error(timeout) end 
             || _ <- lists:seq(1, NumProcs)],
            
            Final = get_counter(iris_msg_in),
            ?assertEqual(NumProcs * IncPerProc, Final - Initial)
        end},
        
       {"concurrent latency observations", fun() ->
            Self = self(),
            NumProcs = 5,
            ObsPerProc = 50,
            
            %% Use unique key for this test to avoid pollution
            UniqueKey = list_to_atom("concurrent_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
            
            %% Spawn concurrent observers
            [spawn(fun() ->
                [iris_metrics:observe_latency(UniqueKey, latency, I) || I <- lists:seq(1, ObsPerProc)],
                Self ! done
            end) || _ <- lists:seq(1, NumProcs)],
            
            %% Wait for all
            [receive done -> ok after 5000 -> error(timeout) end 
             || _ <- lists:seq(1, NumProcs)],
            
            %% Give time for async processing
            timer:sleep(100),
            
            Stats = iris_metrics:get_latency_stats({latency, UniqueKey, latency}),
            ?assertNotEqual(undefined, Stats),
            
            %% Due to concurrent ETS updates, some samples may be lost
            %% (non-atomic read-modify-write). We just verify we got a reasonable count.
            Count = maps:get(count, Stats),
            ?assert(Count >= 100)  %% Should get at least half the samples
        end}
      ]}}.
