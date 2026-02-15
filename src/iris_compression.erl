-module(iris_compression).

%% =============================================================================
%% PD-2: Compression Negotiation (RFC-001 v4.0 Section 11.1)
%%
%% Supports zstd and zlib compression. Payloads <= 128 bytes bypass
%% compression. Compressed frames flagged with opcode | 0x80.
%% =============================================================================

-export([compress/2, decompress/2]).
-export([maybe_compress/2]).
-export([flag_compressed/1, is_compressed/1, original_opcode/1]).
-export([negotiate/2]).
-export([available_algorithms/0]).  %% Dynamic capability detection

-define(MIN_COMPRESS_SIZE, 128).  %% RFC v4.0: Skip compression below this

%% @doc Compress data with the given algorithm.
-spec compress(zstd | zlib, binary()) -> {ok, binary()} | {error, term()}.
compress(zlib, Data) ->
    try
        Compressed = zlib:compress(Data),
        {ok, Compressed}
    catch
        _:Reason -> {error, Reason}
    end;
compress(zstd, Data) ->
    %% Real zstd via NIF (RFC Section 11.1: "zstd (recommended)")
    %% Transparent fallback to zlib when NIF unavailable.
    %% Callers always get {ok, CompressedData} — no error handling needed.
    try iris_zstd_nif:compress(Data)
    catch
        error:undef ->
            bump_fallback_metric(),
            compress(zlib, Data);
        error:nif_not_loaded ->
            bump_fallback_metric(),
            compress(zlib, Data);
        error:{nif_not_loaded, _} ->
            bump_fallback_metric(),
            compress(zlib, Data)
    end.

%% @doc Decompress data with the given algorithm.
-spec decompress(zstd | zlib, binary()) -> {ok, binary()} | {error, term()}.
decompress(zlib, Compressed) ->
    try
        Data = zlib:uncompress(Compressed),
        {ok, Data}
    catch
        _:Reason -> {error, Reason}
    end;
decompress(zstd, Data) ->
    %% Real zstd via NIF (RFC Section 11.1)
    %% Transparent fallback to zlib when NIF unavailable.
    try iris_zstd_nif:decompress(Data)
    catch
        error:undef ->
            bump_fallback_metric(),
            decompress(zlib, Data);
        error:nif_not_loaded ->
            bump_fallback_metric(),
            decompress(zlib, Data);
        error:{nif_not_loaded, _} ->
            bump_fallback_metric(),
            decompress(zlib, Data)
    end.

%% @doc Maybe compress based on payload size. Skips payloads <= 128 bytes.
-spec maybe_compress(zstd | zlib, binary()) -> {compressed, binary()} | {uncompressed, binary()}.
maybe_compress(_Algo, Data) when byte_size(Data) =< ?MIN_COMPRESS_SIZE ->
    {uncompressed, Data};
maybe_compress(Algo, Data) ->
    case compress(Algo, Data) of
        {ok, Compressed} -> {compressed, Compressed};
        {error, _} -> {uncompressed, Data}
    end.

%% @doc Flag an opcode as compressed (set bit 7).
-spec flag_compressed(non_neg_integer()) -> non_neg_integer().
flag_compressed(Opcode) ->
    Opcode bor 16#80.

%% @doc Check if opcode has compression flag.
-spec is_compressed(non_neg_integer()) -> boolean().
is_compressed(Opcode) ->
    (Opcode band 16#80) =/= 0.

%% @doc Get original opcode from compressed opcode.
-spec original_opcode(non_neg_integer()) -> non_neg_integer().
original_opcode(Opcode) ->
    Opcode band 16#7F.

%% @doc Negotiate compression capabilities. Returns intersection.
-spec negotiate([binary()], [binary()]) -> [binary()].
negotiate(ClientCaps, ServerCaps) ->
    [C || C <- ClientCaps, lists:member(C, ServerCaps)].

%% @doc Return the list of compression algorithms available at runtime.
%% zlib is always present (OTP built-in). zstd is included only if the
%% NIF .so exists on disk (priv/iris_zstd_nif.so).
-spec available_algorithms() -> [binary()].
available_algorithms() ->
    Base = [<<"zlib">>],
    case zstd_nif_available() of
        true -> Base ++ [<<"zstd">>];
        false -> Base
    end.

%% Verify NIF actually loads (not just file existence) and cache result.
zstd_nif_available() ->
    case persistent_term:get(iris_zstd_nif_available, undefined) of
        undefined ->
            Result = try_zstd_nif(),
            persistent_term:put(iris_zstd_nif_available, Result),
            Result;
        Cached ->
            Cached
    end.

try_zstd_nif() ->
    case code:priv_dir(iris_edge) of
        {error, _} -> false;
        PrivDir ->
            NifPath = filename:join(PrivDir, "iris_zstd_nif.so"),
            case filelib:is_file(NifPath) of
                false -> false;
                true ->
                    %% File exists — verify it actually loads and works
                    try
                        _ = iris_zstd_nif:compress(<<0>>),
                        true
                    catch
                        error:undef -> false;
                        error:nif_not_loaded -> false;
                        error:{nif_not_loaded, _} -> false;
                        _:_ -> false
                    end
            end
    end.

%% Emit metric when zstd NIF fallback triggers at runtime.
%% This makes silent compression degradation observable via dashboards/alerts.
bump_fallback_metric() ->
    try
        iris_metrics:inc(iris_compression_fallback_count)
    catch
        _:_ -> ok  %% Metrics module may not be running in all environments
    end.
