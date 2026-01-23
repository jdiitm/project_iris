-module(iris_cbor_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% CBOR Codec Unit Tests (RFC-001-AMENDMENT-001)
%% Per TEST_CONTRACT.md: Tests must be deterministic
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Integer Encoding/Decoding
%% -----------------------------------------------------------------------------

encode_small_uint_test() ->
    %% Small unsigned integers (0-23) encode in 1 byte
    ?assertEqual(<<0>>, iris_proto:cbor_encode(0)),
    ?assertEqual(<<1>>, iris_proto:cbor_encode(1)),
    ?assertEqual(<<23>>, iris_proto:cbor_encode(23)).

encode_uint8_test() ->
    %% Unsigned integers 24-255 encode in 2 bytes
    ?assertEqual(<<24, 24>>, iris_proto:cbor_encode(24)),
    ?assertEqual(<<24, 100>>, iris_proto:cbor_encode(100)),
    ?assertEqual(<<24, 255>>, iris_proto:cbor_encode(255)).

encode_uint16_test() ->
    %% Unsigned integers 256-65535 encode in 3 bytes
    ?assertEqual(<<25, 1, 0>>, iris_proto:cbor_encode(256)),
    ?assertEqual(<<25, 255, 255>>, iris_proto:cbor_encode(65535)).

encode_uint32_test() ->
    %% Unsigned integers 65536-4294967295 encode in 5 bytes
    ?assertEqual(<<26, 0, 1, 0, 0>>, iris_proto:cbor_encode(65536)),
    ?assertEqual(<<26, 255, 255, 255, 255>>, iris_proto:cbor_encode(4294967295)).

encode_uint64_test() ->
    %% Unsigned integers > 4294967295 encode in 9 bytes
    ?assertEqual(<<27, 0, 0, 0, 1, 0, 0, 0, 0>>, iris_proto:cbor_encode(4294967296)).

encode_negative_int_test() ->
    %% Negative integers
    ?assertEqual(<<32>>, iris_proto:cbor_encode(-1)),     %% Major 1, value 0 = -1
    ?assertEqual(<<56, 99>>, iris_proto:cbor_encode(-100)).  %% Major 1, value 99 = -100

%% -----------------------------------------------------------------------------
%% Binary/Text Encoding/Decoding
%% -----------------------------------------------------------------------------

encode_empty_binary_test() ->
    ?assertEqual(<<64>>, iris_proto:cbor_encode(<<>>)).   %% Major 2, len 0

encode_binary_test() ->
    ?assertEqual(<<68, 1, 2, 3, 4>>, iris_proto:cbor_encode(<<1,2,3,4>>)).

encode_text_test() ->
    %% Strings (lists of chars) encode as text
    ?assertEqual(<<101, "hello">>, iris_proto:cbor_encode("hello")).

encode_binary_as_bytes_test() ->
    %% Binary should encode as byte string
    <<Major:3, _:5, _/binary>> = iris_proto:cbor_encode(<<"test">>),
    ?assertEqual(2, Major).  %% Major type 2 = byte string

%% -----------------------------------------------------------------------------
%% Boolean and Special Values
%% -----------------------------------------------------------------------------

encode_true_test() ->
    ?assertEqual(<<16#F5>>, iris_proto:cbor_encode(true)).

encode_false_test() ->
    ?assertEqual(<<16#F4>>, iris_proto:cbor_encode(false)).

encode_null_test() ->
    ?assertEqual(<<16#F6>>, iris_proto:cbor_encode(null)).

encode_undefined_test() ->
    ?assertEqual(<<16#F7>>, iris_proto:cbor_encode(undefined)).

%% -----------------------------------------------------------------------------
%% Float Encoding/Decoding
%% -----------------------------------------------------------------------------

encode_float_test() ->
    %% Floats encode as double-precision (major 7, additional 27)
    Encoded = iris_proto:cbor_encode(3.14159),
    <<16#FB, Float:64/float>> = Encoded,
    ?assert(abs(Float - 3.14159) < 0.00001).

%% -----------------------------------------------------------------------------
%% Array Encoding/Decoding
%% -----------------------------------------------------------------------------

encode_empty_array_test() ->
    ?assertEqual(<<128>>, iris_proto:cbor_encode([])).  %% Major 4, len 0

encode_array_test() ->
    Expected = <<131, 1, 2, 3>>,  %% Major 4, len 3, items 1,2,3
    ?assertEqual(Expected, iris_proto:cbor_encode([1, 2, 3])).

encode_nested_array_test() ->
    %% [[1,2], [3,4]]
    Encoded = iris_proto:cbor_encode([[1,2], [3,4]]),
    {ok, Decoded} = iris_proto:cbor_decode(Encoded),
    ?assertEqual([[1,2], [3,4]], Decoded).

%% -----------------------------------------------------------------------------
%% Map Encoding/Decoding
%% -----------------------------------------------------------------------------

encode_empty_map_test() ->
    ?assertEqual(<<160>>, iris_proto:cbor_encode(#{})).  %% Major 5, len 0

encode_map_test() ->
    Map = #{<<"key">> => <<"value">>},
    Encoded = iris_proto:cbor_encode(Map),
    {ok, Decoded} = iris_proto:cbor_decode(Encoded),
    ?assertEqual(Map, Decoded).

encode_complex_map_test() ->
    Map = #{
        <<"int">> => 42,
        <<"str">> => <<"hello">>,
        <<"bool">> => true,
        <<"arr">> => [1, 2, 3]
    },
    Encoded = iris_proto:cbor_encode(Map),
    {ok, Decoded} = iris_proto:cbor_decode(Encoded),
    ?assertEqual(Map, Decoded).

%% -----------------------------------------------------------------------------
%% Roundtrip Tests
%% -----------------------------------------------------------------------------

roundtrip_integer_test() ->
    Values = [0, 1, 23, 24, 100, 255, 256, 65535, 65536, 1000000],
    lists:foreach(fun(V) ->
        Encoded = iris_proto:cbor_encode(V),
        {ok, Decoded} = iris_proto:cbor_decode(Encoded),
        ?assertEqual(V, Decoded)
    end, Values).

roundtrip_negative_test() ->
    Values = [-1, -10, -100, -1000, -100000],
    lists:foreach(fun(V) ->
        Encoded = iris_proto:cbor_encode(V),
        {ok, Decoded} = iris_proto:cbor_decode(Encoded),
        ?assertEqual(V, Decoded)
    end, Values).

roundtrip_binary_test() ->
    Values = [<<>>, <<"a">>, <<"hello">>, <<"hello world">>, crypto:strong_rand_bytes(100)],
    lists:foreach(fun(V) ->
        Encoded = iris_proto:cbor_encode(V),
        {ok, Decoded} = iris_proto:cbor_decode(Encoded),
        ?assertEqual(V, Decoded)
    end, Values).

roundtrip_special_test() ->
    Values = [true, false, null, undefined],
    lists:foreach(fun(V) ->
        Encoded = iris_proto:cbor_encode(V),
        {ok, Decoded} = iris_proto:cbor_decode(Encoded),
        ?assertEqual(V, Decoded)
    end, Values).

%% -----------------------------------------------------------------------------
%% Protocol Message Tests
%% -----------------------------------------------------------------------------

encode_cbor_msg_test() ->
    Target = <<"alice">>,
    Payload = #{<<"type">> => <<"text">>, <<"body">> => <<"Hello!">>},
    
    Encoded = iris_proto:encode_cbor_msg(Target, Payload),
    
    %% Verify opcode is 0x10
    <<16#10, _/binary>> = Encoded,
    
    %% Decode and verify
    {ok, DecodedTarget, DecodedPayload} = iris_proto:decode_cbor_msg(Encoded),
    ?assertEqual(Target, DecodedTarget),
    ?assertEqual(Payload, DecodedPayload).

decode_cbor_msg_via_decode_test() ->
    Target = <<"bob">>,
    Payload = #{<<"seq">> => 42, <<"content">> => <<"test message">>},
    
    Encoded = iris_proto:encode_cbor_msg(Target, Payload),
    
    %% Decode via main decode/1 function
    {{cbor_msg, DecodedTarget, DecodedPayload}, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual(Target, DecodedTarget),
    ?assertEqual(Payload, DecodedPayload).

decode_cbor_msg_partial_test() ->
    %% Partial message should return {more, _}
    PartialMsg = <<16#10, 5:16, "alice">>,  %% Missing CBOR payload
    {more, _} = iris_proto:decode(PartialMsg).

decode_cbor_msg_too_large_test() ->
    %% CBOR payload exceeding limit should fail
    %% Create a message claiming 1MB CBOR payload
    TooLarge = <<16#10, 4:16, "test", 16#100001:32>>,  %% 1MB+ claim
    {{error, cbor_too_large}, <<>>} = iris_proto:decode(<<TooLarge/binary, 0:8>>).

%% -----------------------------------------------------------------------------
%% Error Cases
%% -----------------------------------------------------------------------------

decode_invalid_cbor_test() ->
    %% Invalid CBOR should return error
    Target = <<"test">>,
    InvalidCbor = <<255, 255, 255>>,  %% Invalid CBOR
    TLen = byte_size(Target),
    CLen = byte_size(InvalidCbor),
    Msg = <<16#10, TLen:16, Target/binary, CLen:32, InvalidCbor/binary>>,
    {{error, {cbor_decode_error, _}}, <<>>} = iris_proto:decode(Msg).

decode_cbor_not_map_test() ->
    %% CBOR that decodes to non-map should fail
    Target = <<"test">>,
    ArrayCbor = iris_proto:cbor_encode([1, 2, 3]),  %% Array, not map
    TLen = byte_size(Target),
    CLen = byte_size(ArrayCbor),
    Msg = <<16#10, TLen:16, Target/binary, CLen:32, ArrayCbor/binary>>,
    {{error, cbor_not_map}, <<>>} = iris_proto:decode(Msg).

%% -----------------------------------------------------------------------------
%% Atom Encoding Test
%% -----------------------------------------------------------------------------

encode_atom_test() ->
    %% Atoms encode as text strings
    Encoded = iris_proto:cbor_encode(hello),
    {ok, Decoded} = iris_proto:cbor_decode(Encoded),
    ?assertEqual(<<"hello">>, Decoded).
