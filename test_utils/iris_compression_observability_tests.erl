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

%% Check if the zstd NIF can actually be loaded (CI has libzstd-dev).
zstd_nif_loadable() ->
    try
        _ = iris_zstd_nif:compress(<<0>>),
        true
    catch
        error:undef -> false;
        error:nif_not_loaded -> false;
        error:{nif_not_loaded, _} -> false;
        _:_ -> false
    end.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

%% Test: zstd compress fallback increments metric when NIF is absent.
%% AUDIT V2: compress(zstd, ...) now always returns {ok, _} via transparent
%% zlib fallback. Metric distinguishes NIF-native vs fallback path.
zstd_fallback_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    {ok, _} = iris_compression:compress(zstd, <<"test data for compression">>),
    After = get_metric(iris_compression_fallback_count),
    case zstd_nif_loadable() of
        true ->
            %% NIF available: compress succeeded natively, no fallback
            ?assertEqual(Before, After);
        false ->
            %% NIF absent: transparent zlib fallback bumped metric
            ?assert(After > Before)
    end.

%% Test: zstd decompress fallback also increments metric when NIF is absent.
%% AUDIT V2: decompress(zstd, ...) transparently falls back to zlib.
%% Invalid data will still return {error, _} from zlib:uncompress.
zstd_decompress_fallback_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    %% Use invalid data — will fail in both NIF and zlib, but metric
    %% should still be bumped when NIF is absent (fallback path taken).
    _Result = iris_compression:decompress(zstd, <<"not real compressed data">>),
    After = get_metric(iris_compression_fallback_count),
    case zstd_nif_loadable() of
        true  -> ?assertEqual(Before, After);
        false -> ?assert(After > Before)
    end.

%% Test: zlib compress does NOT emit fallback metric (zlib always works)
zlib_no_fallback_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    {ok, _} = iris_compression:compress(zlib, <<"test data for zlib compression">>),
    After = get_metric(iris_compression_fallback_count),
    ?assertEqual(Before, After).
