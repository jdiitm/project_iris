-module(iris_compression_observability_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION: Compression Fallback Observability (Finding 3)
%%
%% When zstd NIF is unavailable at runtime, the fallback should emit a metric
%% so operators can detect silent degradation via dashboards/alerts.
%% =============================================================================

%% iris_metrics uses iris_metrics_table (defined as ?METRICS_TABLE in iris_metrics.erl)
-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

ensure_metrics_table() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end.

get_metric(Key) ->
    case ets:lookup(?METRICS_TABLE, Key) of
        [{_, Val}] -> Val;
        [] -> 0
    end.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

%% Test: zstd compress fallback increments the compression_fallback_count metric
zstd_fallback_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    _Result = iris_compression:compress(zstd, <<"test data for compression">>),
    After = get_metric(iris_compression_fallback_count),
    ?assert(After > Before).

%% Test: zstd decompress fallback also increments the metric
zstd_decompress_fallback_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    _Result = iris_compression:decompress(zstd, <<"not real compressed data">>),
    After = get_metric(iris_compression_fallback_count),
    ?assert(After > Before).

%% Test: zlib compress does NOT emit fallback metric (zlib always works)
zlib_no_fallback_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    {ok, _} = iris_compression:compress(zlib, <<"test data for zlib compression">>),
    After = get_metric(iris_compression_fallback_count),
    ?assertEqual(Before, After).
