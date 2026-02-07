-module(iris_metrics_slo_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% SLI/SLO Tracking Tests (RFC-001 v4.0 Appendix B)
%%
%% Verifies that SLI computation and SLO compliance reporting
%% are correctly implemented in iris_metrics.erl.
%%
%% SLIs:
%%   availability = msg_out / (msg_out + msg_lost)
%%   durability   = 1 - (msg_lost / msg_acked)
%%   latency P99  = 99th percentile of e2e delivery
%%
%% SLOs:
%%   availability >= 99.9%
%%   durability   >= 99.999%
%%   latency P99  <= 500ms
%% =============================================================================

%% =============================================================================
%% Setup / Teardown
%% =============================================================================

setup() ->
    case whereis(iris_metrics) of
        undefined ->
            {ok, Pid} = iris_metrics:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_metrics);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

slo_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Availability SLI computed correctly", fun test_availability_sli/0},
      {"Durability SLI computed correctly", fun test_durability_sli/0},
      {"Latency SLI returns P99", fun test_latency_sli/0},
      {"SLO report has all required fields", fun test_slo_report_fields/0},
      {"SLO compliance with zero traffic", fun test_slo_zero_traffic/0},
      {"SLO compliance with perfect traffic", fun test_slo_perfect_traffic/0},
      {"Error budget computation", fun test_error_budget/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_availability_sli() ->
    %% With zero traffic, availability should be 1.0
    Avail = iris_metrics:get_sli_availability(),
    ?assertEqual(1.0, Avail).

test_durability_sli() ->
    %% With zero traffic, durability should be 1.0
    Dur = iris_metrics:get_sli_durability(),
    ?assertEqual(1.0, Dur).

test_latency_sli() ->
    %% With no samples, should return undefined
    Lat = iris_metrics:get_sli_latency(),
    ?assertEqual(undefined, Lat),

    %% Add some latency samples
    lists:foreach(fun(I) ->
        iris_metrics:observe_e2e_latency(I * 10.0)
    end, lists:seq(1, 100)),
    %% Give casts time to process
    timer:sleep(100),

    %% Should now return a numeric value
    Lat2 = iris_metrics:get_sli_latency(),
    ?assert(Lat2 =/= undefined),
    ?assert(is_number(Lat2)).

test_slo_report_fields() ->
    Report = iris_metrics:get_slo_report(),
    ?assert(is_map(Report)),

    %% Must have sli, slo, compliance, error_budget sections
    ?assert(maps:is_key(sli, Report)),
    ?assert(maps:is_key(slo, Report)),
    ?assert(maps:is_key(compliance, Report)),
    ?assert(maps:is_key(error_budget, Report)),

    %% SLI section
    SLI = maps:get(sli, Report),
    ?assert(maps:is_key(availability, SLI)),
    ?assert(maps:is_key(durability, SLI)),
    ?assert(maps:is_key(latency_p99_ms, SLI)),

    %% SLO section
    SLO = maps:get(slo, Report),
    ?assertEqual(0.999, maps:get(availability_target, SLO)),
    ?assertEqual(0.99999, maps:get(durability_target, SLO)),
    ?assertEqual(500, maps:get(latency_p99_target_ms, SLO)),

    %% Compliance section
    Comp = maps:get(compliance, Report),
    ?assert(maps:is_key(availability_ok, Comp)),
    ?assert(maps:is_key(durability_ok, Comp)),
    ?assert(maps:is_key(latency_ok, Comp)),
    ?assert(maps:is_key(all_ok, Comp)).

test_slo_zero_traffic() ->
    %% With no message loss, availability and durability SLOs should pass.
    %% NOTE: latency_ok may depend on samples from prior tests, so we
    %% check availability and durability independently.
    Report = iris_metrics:get_slo_report(),
    Comp = maps:get(compliance, Report),
    ?assertEqual(true, maps:get(availability_ok, Comp)),
    ?assertEqual(true, maps:get(durability_ok, Comp)).

test_slo_perfect_traffic() ->
    %% Simulate perfect traffic: lots of output, zero loss
    lists:foreach(fun(_) -> iris_metrics:msg_out() end, lists:seq(1, 1000)),
    lists:foreach(fun(_) -> iris_metrics:msg_acked() end, lists:seq(1, 1000)),

    Report = iris_metrics:get_slo_report(),
    SLI = maps:get(sli, Report),
    ?assertEqual(1.0, maps:get(availability, SLI)),
    ?assertEqual(1.0, maps:get(durability, SLI)),

    Comp = maps:get(compliance, Report),
    ?assertEqual(true, maps:get(availability_ok, Comp)),
    ?assertEqual(true, maps:get(durability_ok, Comp)).

test_error_budget() ->
    Report = iris_metrics:get_slo_report(),
    Budget = maps:get(error_budget, Report),

    %% With perfect traffic, full budget remaining (1.0)
    AvailBudget = maps:get(availability_remaining, Budget),
    DurBudget = maps:get(durability_remaining, Budget),

    ?assert(AvailBudget >= 0.0),
    ?assert(AvailBudget =< 1.0),
    ?assert(DurBudget >= 0.0),
    ?assert(DurBudget =< 1.0).
