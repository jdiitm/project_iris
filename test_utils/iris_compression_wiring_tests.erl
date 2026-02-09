-module(iris_compression_wiring_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% TDD: Compression Wiring Tests (Audit Finding — P3)
%% =============================================================================
%% Written BEFORE implementation. These tests define the contract for
%% iris_edge_conn:maybe_compress_outbound/2 and maybe_decompress_inbound/2.
%%
%% Packet format: <<Opcode:8, Payload/binary>>
%% Compressed:    <<(Opcode bor 0x80):8, CompressedPayload/binary>>
%% =============================================================================

%% Test: Payload > 128 bytes is compressed when zlib is negotiated
compress_outbound_when_negotiated_test() ->
    Caps = [<<"zlib">>],
    %% Create a packet with opcode 0x11 (reliable_msg) and a 200-byte payload
    Payload = binary:copy(<<$A>>, 200),
    Packet = <<16#11, Payload/binary>>,
    Result = iris_edge_conn:maybe_compress_outbound(Caps, Packet),
    %% Opcode must have high bit set (0x11 | 0x80 = 0x91)
    <<ResultOpcode:8, ResultPayload/binary>> = Result,
    ?assertEqual(16#91, ResultOpcode),
    %% Payload must be different (compressed)
    ?assertNotEqual(Payload, ResultPayload),
    %% Decompressing must recover the original
    {ok, Decompressed} = iris_compression:decompress(zlib, ResultPayload),
    ?assertEqual(Payload, Decompressed).

%% Test: No compression when capabilities are empty
no_compress_when_not_negotiated_test() ->
    Caps = [],
    Payload = binary:copy(<<$B>>, 200),
    Packet = <<16#11, Payload/binary>>,
    Result = iris_edge_conn:maybe_compress_outbound(Caps, Packet),
    %% Packet must be returned unchanged
    ?assertEqual(Packet, Result).

%% Test: Small payload (<=128 bytes) is not compressed even with negotiated caps
no_compress_small_payload_test() ->
    Caps = [<<"zlib">>],
    Payload = binary:copy(<<$C>>, 100),
    Packet = <<16#11, Payload/binary>>,
    Result = iris_edge_conn:maybe_compress_outbound(Caps, Packet),
    %% Packet must be returned unchanged
    ?assertEqual(Packet, Result).

%% Test: Inbound packet with compression flag is decompressed
decompress_inbound_when_flagged_test() ->
    Caps = [<<"zlib">>],
    OriginalPayload = binary:copy(<<$D>>, 200),
    {ok, CompressedPayload} = iris_compression:compress(zlib, OriginalPayload),
    %% Build a compressed packet (opcode 0x11 | 0x80 = 0x91)
    CompressedPacket = <<16#91, CompressedPayload/binary>>,
    Result = iris_edge_conn:maybe_decompress_inbound(Caps, CompressedPacket),
    ?assertEqual(<<16#11, OriginalPayload/binary>>, Result).

%% Test: Inbound packet without compression flag is returned unchanged
no_decompress_when_not_flagged_test() ->
    Caps = [<<"zlib">>],
    Payload = binary:copy(<<$E>>, 200),
    Packet = <<16#11, Payload/binary>>,
    Result = iris_edge_conn:maybe_decompress_inbound(Caps, Packet),
    ?assertEqual(Packet, Result).

%% Test: zstd negotiation also triggers compression
compress_outbound_zstd_test() ->
    Caps = [<<"zstd">>],
    Payload = binary:copy(<<$F>>, 200),
    Packet = <<16#11, Payload/binary>>,
    Result = iris_edge_conn:maybe_compress_outbound(Caps, Packet),
    <<ResultOpcode:8, _/binary>> = Result,
    ?assertEqual(16#91, ResultOpcode).
