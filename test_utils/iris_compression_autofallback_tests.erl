-module(iris_compression_autofallback_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Transparent zlib Auto-Fallback
%% =============================================================================
%%
%% When the zstd NIF is unavailable, compress(zstd, Data) should transparently
%% fall back to zlib compression instead of returning {error, zstd_nif_not_available}.
%% This guarantees callers always get compressed data.
%% =============================================================================

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

%% Check if the zstd NIF can actually be loaded.
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

%% =============================================================================
%% Test: compress(zstd, Data) always returns {ok, _}, never {error, _}
%% When NIF absent, it transparently falls back to zlib.
%% =============================================================================

compress_zstd_always_returns_ok_test() ->
    TestData = <<"This is test data that should always be compressible by zlib or zstd">>,
    Result = iris_compression:compress(zstd, TestData),
    ?assertMatch({ok, _}, Result).

%% =============================================================================
%% Test: Roundtrip — compress(zstd) then decompress(zstd) recovers data
%% regardless of whether NIF is present or not.
%% =============================================================================

compress_decompress_zstd_roundtrip_test() ->
    TestData = <<"Roundtrip test data for zstd with auto-fallback to zlib">>,
    {ok, Compressed} = iris_compression:compress(zstd, TestData),
    {ok, Decompressed} = iris_compression:decompress(zstd, Compressed),
    ?assertEqual(TestData, Decompressed).

%% =============================================================================
%% Test: Fallback still emits metric when NIF is absent
%% =============================================================================

fallback_emits_metric_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {iris_compression_fallback_count, 0}),
    Before = get_metric(iris_compression_fallback_count),
    TestData = <<"Metric test data for zstd compression fallback">>,
    {ok, _} = iris_compression:compress(zstd, TestData),
    After = get_metric(iris_compression_fallback_count),
    case zstd_nif_loadable() of
        true ->
            %% NIF available: no fallback, no metric bump
            ?assertEqual(Before, After);
        false ->
            %% NIF absent: fallback path should have bumped metric
            ?assert(After > Before)
    end.

%% =============================================================================
%% Test: maybe_compress/2 never returns error, always compressed or uncompressed
%% =============================================================================

maybe_compress_never_returns_error_test() ->
    %% Large data — should be compressed
    LargeData = binary:copy(<<"A">>, 256),
    Result1 = iris_compression:maybe_compress(zstd, LargeData),
    ?assert(element(1, Result1) =:= compressed orelse
            element(1, Result1) =:= uncompressed),
    %% Small data — should be uncompressed (below 128 byte threshold)
    SmallData = <<"tiny">>,
    Result2 = iris_compression:maybe_compress(zstd, SmallData),
    ?assertMatch({uncompressed, _}, Result2).
