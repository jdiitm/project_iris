-module(iris_disk_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% H-4: Disk Space Monitoring Tests
%% =============================================================================
%% Requirement: The system must emit iris_disk_usage_percent as a gauge
%% metric so Prometheus can alert on disk space exhaustion.
%% =============================================================================

setup() ->
    %% Ensure metrics server is running
    case whereis(iris_metrics) of
        undefined -> catch iris_metrics:start_link();
        _ -> ok
    end,
    ok.

%% =============================================================================
%% Test: disk usage metric is emitted with valid value
%% =============================================================================
disk_usage_metric_emitted_test() ->
    setup(),
    iris_metrics:emit_disk_usage(),
    Metrics = iris_metrics:get_metrics(),
    ?assert(maps:is_key(iris_disk_usage_percent, Metrics)),
    Value = maps:get(iris_disk_usage_percent, Metrics),
    %% Must be a number between 0 and 100
    ?assert(is_number(Value)),
    ?assert(Value >= 0),
    ?assert(Value =< 100).

%% =============================================================================
%% Test: disk usage appears in Prometheus export
%% =============================================================================
disk_usage_exported_in_prometheus_test() ->
    setup(),
    iris_metrics:emit_disk_usage(),
    Output = iris_metrics:export_prometheus(),
    ?assertNotEqual(nomatch, binary:match(Output, <<"iris_disk_usage_percent">>)).
