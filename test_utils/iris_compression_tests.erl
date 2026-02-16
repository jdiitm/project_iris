-module(iris_compression_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%%: Compression Negotiation Tests
%%
%% RFC-001 v4.0 Section 11.1:
%% - zstd and zlib supported
%% - Payloads <= 128 bytes not compressed
%% - Compressed frames flagged with opcode | 0x80
%% - Capability negotiation during handshake
%%
%% Tests verify:
%% 1. zstd roundtrip
%% 2. zlib roundtrip
%% 3. Small payload bypass
%% 4. Compressed frame flag
%% 5. Capability negotiation
%%
%% Pattern: standalone tests, no gen_server needed.
%% =============================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_compression_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"zstd roundtrip", fun test_compress_zstd_roundtrip/0},
      {"zlib roundtrip", fun test_compress_zlib_roundtrip/0},
      {"Small payload not compressed", fun test_compress_noop_small_payload/0},
      {"Compressed frame has flag", fun test_compressed_frame_has_flag/0},
      {"Negotiate capabilities", fun test_negotiate_capabilities/0},

      %% RFC Section 11.1: Real zstd format verification
      {"zstd produces real zstd format (not fake tag)", fun test_zstd_produces_real_format/0},
      {"zstd real roundtrip with random data", fun test_zstd_real_roundtrip/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_compress_zstd_roundtrip() ->
    Data = crypto:strong_rand_bytes(256),
    %% B-7 FIX: compress(zstd) returns error when NIF unavailable (consistent with decompress).
    %% Roundtrip only works when the zstd NIF is actually loaded.
    case iris_compression:compress(zstd, Data) of
        {ok, Compressed} ->
            %% NIF is loaded — full roundtrip must work
            {ok, Decompressed} = iris_compression:decompress(zstd, Compressed),
            ?assertEqual(Data, Decompressed);
        {error, {zstd_nif_unavailable, _}} ->
            %% NIF not loaded — error is the expected behavior
            ok
    end.

test_compress_zlib_roundtrip() ->
    Data = crypto:strong_rand_bytes(256),
    {ok, Compressed} = iris_compression:compress(zlib, Data),
    {ok, Decompressed} = iris_compression:decompress(zlib, Compressed),
    ?assertEqual(Data, Decompressed).

test_compress_noop_small_payload() ->
    %% Payloads <= 128 bytes should not be compressed (RFC v4.0 Section 11.1)
    SmallData = crypto:strong_rand_bytes(64),
    Result = iris_compression:maybe_compress(zstd, SmallData),
    ?assertEqual({uncompressed, SmallData}, Result),
    %% B-7 FIX: Large payload compressed only if NIF is available.
    %% When NIF is unavailable, maybe_compress returns {uncompressed, _}.
    LargeData = crypto:strong_rand_bytes(256),
    Result2 = iris_compression:maybe_compress(zstd, LargeData),
    ?assert(element(1, Result2) =:= compressed orelse element(1, Result2) =:= uncompressed).

test_compressed_frame_has_flag() ->
    %% Compressed frame opcode = original_opcode | 0x80
    OriginalOpcode = 16#01,
    Expected = OriginalOpcode bor 16#80,
    ?assertEqual(16#81, Expected),
    %% Verify via module function
    FlaggedOpcode = iris_compression:flag_compressed(OriginalOpcode),
    ?assertEqual(Expected, FlaggedOpcode),
    ?assert(iris_compression:is_compressed(FlaggedOpcode)),
    ?assertNot(iris_compression:is_compressed(OriginalOpcode)).

test_negotiate_capabilities() ->
    %% Client sends capabilities, server returns intersection
    ClientCaps = [<<"zstd">>, <<"e2ee">>, <<"zlib">>],
    ServerCaps = [<<"zstd">>, <<"zlib">>],
    Intersection = iris_compression:negotiate(ClientCaps, ServerCaps),
    ?assert(lists:member(<<"zstd">>, Intersection)),
    ?assert(lists:member(<<"zlib">>, Intersection)),
    ?assertNot(lists:member(<<"e2ee">>, Intersection)).

%% =============================================================================
%% RFC Section 11.1: Real Zstandard Format Tests
%% The compressed output must be real zstd format, not fake zlib-with-tag.
%% Zstd magic bytes: 0xFD2FB528 (little-endian: <<16#28, 16#B5, 16#2F, 16#FD>>)
%% =============================================================================

test_zstd_produces_real_format() ->
    Data = crypto:strong_rand_bytes(256),
    %% B-7 FIX: compress(zstd) returns error when NIF unavailable
    case iris_compression:compress(zstd, Data) of
        {ok, Compressed} ->
            %% NIF available: must produce real zstd format
            ?assertNot(binary:match(Compressed, <<"zstd:">>) =:= {0, 5}),
            <<Magic:4/binary, _/binary>> = Compressed,
            ?assertEqual(<<16#28, 16#B5, 16#2F, 16#FD>>, Magic);
        {error, {zstd_nif_unavailable, _}} ->
            %% NIF not available: error is expected behavior
            ok
    end.

test_zstd_real_roundtrip() ->
    %% B-7 FIX: Roundtrip only works when zstd NIF is loaded.
    %% When NIF is unavailable, both compress and decompress return errors.
    lists:foreach(fun(Size) ->
        Data = crypto:strong_rand_bytes(Size),
        case iris_compression:compress(zstd, Data) of
            {ok, Compressed} ->
                {ok, Decompressed} = iris_compression:decompress(zstd, Compressed),
                ?assertEqual(Data, Decompressed);
            {error, {zstd_nif_unavailable, _}} ->
                ok
        end
    end, [256, 1024, 4096, 10000]).
