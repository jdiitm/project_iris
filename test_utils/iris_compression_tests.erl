-module(iris_compression_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-11 (PD-2): Compression Negotiation Tests
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
      {"Negotiate capabilities", fun test_negotiate_capabilities/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_compress_zstd_roundtrip() ->
    Data = crypto:strong_rand_bytes(256),
    {ok, Compressed} = iris_compression:compress(zstd, Data),
    {ok, Decompressed} = iris_compression:decompress(zstd, Compressed),
    ?assertEqual(Data, Decompressed).

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
    %% Large payload should compress
    LargeData = crypto:strong_rand_bytes(256),
    Result2 = iris_compression:maybe_compress(zstd, LargeData),
    ?assertMatch({compressed, _}, Result2).

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
