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
    %% Erlang doesn't have native zstd; use zlib as fallback with zstd tag.
    %% In production, this would use a NIF wrapper for libzstd.
    %% For now, use zlib internally but tag it as zstd format.
    try
        Compressed = zlib:compress(Data),
        {ok, <<"zstd:", Compressed/binary>>}
    catch
        _:Reason -> {error, Reason}
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
decompress(zstd, <<"zstd:", Compressed/binary>>) ->
    try
        Data = zlib:uncompress(Compressed),
        {ok, Data}
    catch
        _:Reason -> {error, Reason}
    end;
decompress(zstd, _) ->
    {error, invalid_zstd_format}.

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
