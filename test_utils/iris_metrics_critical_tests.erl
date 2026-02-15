-module(iris_metrics_critical_tests).
-include_lib("eunit/include/eunit.hrl").

%% Guard tests: critical operational metrics must be present in Prometheus export.

-define(METRICS_TABLE, iris_metrics_table).

setup() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

edge_dedup_size_metric_test() ->
    setup(),
    Output = iris_metrics:export_prometheus(),
    ?assert(is_binary(Output)),
    ?assertNotEqual(nomatch, binary:match(Output, <<"iris_edge_dedup_table_size">>)).

cert_expiry_metric_test() ->
    setup(),
    Output = iris_metrics:export_prometheus(),
    ?assertNotEqual(nomatch, binary:match(Output, <<"iris_cert_expiry_seconds">>)).

partition_guard_metric_test() ->
    setup(),
    Output = iris_metrics:export_prometheus(),
    ?assertNotEqual(nomatch, binary:match(Output, <<"iris_partition_guard_state">>)).

type_directives_test() ->
    setup(),
    Output = iris_metrics:export_prometheus(),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# TYPE iris_edge_dedup_table_size gauge">>)),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# TYPE iris_cert_expiry_seconds gauge">>)),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# TYPE iris_partition_guard_state gauge">>)).

help_directives_test() ->
    setup(),
    Output = iris_metrics:export_prometheus(),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# HELP iris_edge_dedup_table_size">>)),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# HELP iris_cert_expiry_seconds">>)),
    ?assertNotEqual(nomatch, binary:match(Output, <<"# HELP iris_partition_guard_state">>)).

edge_dedup_size_returns_zero_when_no_table_test() ->
    %% Ensure table doesn't exist
    try ets:delete(iris_edge_dedup) catch error:badarg -> ok end,
    ?assertEqual(0, iris_metrics:edge_dedup_table_size()).

cert_expiry_returns_neg1_when_no_monitor_test() ->
    ?assertEqual(-1, iris_metrics:cert_expiry_seconds()).

partition_guard_returns_zero_when_no_guard_test() ->
    ?assertEqual(0, iris_metrics:partition_guard_state()).
