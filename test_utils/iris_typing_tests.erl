-module(iris_typing_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Tests for Typing Indicators (RFC FR-8)
%% =============================================================================
%% Tests cover:
%% - Protocol encoding/decoding for TYPING_START (0x30)
%% - Protocol encoding/decoding for TYPING_STOP (0x31)
%% - Protocol encoding for TYPING_RELAY (0x32)
%% - Edge cases: empty targets, max length targets, invalid lengths
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Encoding Tests
%% ---------------------------------------------------------------------------

encode_typing_start_test() ->
    Target = <<"alice">>,
    Encoded = iris_proto:encode_typing_start(Target),
    %% Format: 0x30 | TargetLen(16) | Target
    Expected = <<16#30, 5:16, "alice">>,
    ?assertEqual(Expected, Encoded).

encode_typing_stop_test() ->
    Target = <<"bob">>,
    Encoded = iris_proto:encode_typing_stop(Target),
    %% Format: 0x31 | TargetLen(16) | Target
    Expected = <<16#31, 3:16, "bob">>,
    ?assertEqual(Expected, Encoded).

encode_typing_relay_start_test() ->
    Sender = <<"charlie">>,
    Encoded = iris_proto:encode_typing_relay(Sender, true),
    %% Format: 0x32 | SenderLen(16) | Sender | Status(8)
    Expected = <<16#32, 7:16, "charlie", 1>>,
    ?assertEqual(Expected, Encoded).

encode_typing_relay_stop_test() ->
    Sender = <<"dave">>,
    Encoded = iris_proto:encode_typing_relay(Sender, false),
    Expected = <<16#32, 4:16, "dave", 0>>,
    ?assertEqual(Expected, Encoded).

encode_empty_target_test() ->
    %% Empty target should still encode correctly
    Encoded = iris_proto:encode_typing_start(<<>>),
    Expected = <<16#30, 0:16>>,
    ?assertEqual(Expected, Encoded).

encode_unicode_target_test() ->
    %% Unicode username
    Target = <<"用户"/utf8>>,
    Encoded = iris_proto:encode_typing_start(Target),
    TargetLen = byte_size(Target),
    Expected = <<16#30, TargetLen:16, Target/binary>>,
    ?assertEqual(Expected, Encoded).

%% ---------------------------------------------------------------------------
%% Decoding Tests
%% ---------------------------------------------------------------------------

decode_typing_start_test() ->
    Packet = <<16#30, 5:16, "alice">>,
    {Result, Rest} = iris_proto:decode(Packet),
    ?assertEqual({typing_start, <<"alice">>}, Result),
    ?assertEqual(<<>>, Rest).

decode_typing_stop_test() ->
    Packet = <<16#31, 3:16, "bob">>,
    {Result, Rest} = iris_proto:decode(Packet),
    ?assertEqual({typing_stop, <<"bob">>}, Result),
    ?assertEqual(<<>>, Rest).

decode_typing_with_remainder_test() ->
    %% Packet with extra data at the end
    Packet = <<16#30, 5:16, "alice", "extra">>,
    {Result, Rest} = iris_proto:decode(Packet),
    ?assertEqual({typing_start, <<"alice">>}, Result),
    ?assertEqual(<<"extra">>, Rest).

decode_typing_partial_test() ->
    %% Incomplete packet - should return {more, Bin}
    Packet = <<16#30, 5:16, "ali">>,  %% Only 3 bytes of 5-byte target
    Result = iris_proto:decode(Packet),
    ?assertMatch({more, _}, Result).

decode_typing_target_too_long_test() ->
    %% Target length exceeds MAX_TARGET_LEN (256)
    Packet = <<16#30, 257:16, (binary:copy(<<"x">>, 257))/binary>>,
    {Result, _} = iris_proto:decode(Packet),
    ?assertEqual({error, target_too_long}, Result).

%% ---------------------------------------------------------------------------
%% Roundtrip Tests
%% ---------------------------------------------------------------------------

roundtrip_typing_start_test() ->
    Target = <<"test_user_123">>,
    Encoded = iris_proto:encode_typing_start(Target),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({typing_start, Target}, Decoded).

roundtrip_typing_stop_test() ->
    Target = <<"another_user">>,
    Encoded = iris_proto:encode_typing_stop(Target),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({typing_stop, Target}, Decoded).

roundtrip_long_target_test() ->
    %% Test with max allowed length (256 bytes)
    Target = binary:copy(<<"a">>, 256),
    Encoded = iris_proto:encode_typing_start(Target),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({typing_start, Target}, Decoded).

%% ---------------------------------------------------------------------------
%% Multiple Messages Test
%% ---------------------------------------------------------------------------

multiple_typing_messages_test() ->
    %% Encode multiple messages back-to-back
    Msg1 = iris_proto:encode_typing_start(<<"alice">>),
    Msg2 = iris_proto:encode_typing_stop(<<"bob">>),
    Combined = <<Msg1/binary, Msg2/binary>>,
    
    %% Decode first message
    {Result1, Rest1} = iris_proto:decode(Combined),
    ?assertEqual({typing_start, <<"alice">>}, Result1),
    
    %% Decode second message
    {Result2, Rest2} = iris_proto:decode(Rest1),
    ?assertEqual({typing_stop, <<"bob">>}, Result2),
    ?assertEqual(<<>>, Rest2).
