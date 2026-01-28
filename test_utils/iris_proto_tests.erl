-module(iris_proto_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_proto_test_() ->
    [
     %% Decode tests
     {"Decode login packet", fun test_decode_login/0},
     {"Decode login with empty user", fun test_decode_login_empty/0},
     {"Decode send_message complete", fun test_decode_send_message/0},
     {"Decode send_message partial header", fun test_decode_send_message_partial_header/0},
     {"Decode send_message partial body", fun test_decode_send_message_partial_body/0},
     {"Decode ack packet", fun test_decode_ack/0},
     {"Decode batch_send complete", fun test_decode_batch_send/0},
     {"Decode batch_send partial", fun test_decode_batch_send_partial/0},
     {"Decode get_status complete", fun test_decode_get_status/0},
     {"Decode get_status partial", fun test_decode_get_status_partial/0},
     {"Decode empty binary", fun test_decode_empty/0},
     {"Decode unknown opcode", fun test_decode_unknown/0},
     {"Decode with remainder", fun test_decode_with_remainder/0},
     
     %% Encode tests
     {"Encode status online", fun test_encode_status_online/0},
     {"Encode status offline", fun test_encode_status_offline/0},
     {"Encode status long username", fun test_encode_status_long_user/0},
     {"Encode status unicode user", fun test_encode_status_unicode/0},
     {"Encode reliable msg", fun test_encode_reliable_msg/0},
     
     %% Unpack batch tests
     {"Unpack batch multiple messages", fun test_unpack_batch_multiple/0},
     {"Unpack batch single message", fun test_unpack_batch_single/0},
     {"Unpack batch empty", fun test_unpack_batch_empty/0},
     {"Unpack batch empty", fun test_unpack_batch_empty/0},
     {"Unpack batch with trailing garbage", fun test_unpack_batch_garbage/0},
     
     %% Limit tests
     {"Unpack batch limit exceed", fun test_unpack_batch_limit/0}
    ].

%% =============================================================================
%% Decode Tests
%% =============================================================================

test_decode_login() ->
    Input = <<1, "alice">>,
    Result = iris_proto:decode(Input),
    ?assertEqual({{login, <<"alice">>}, <<>>}, Result).

test_decode_login_empty() ->
    Input = <<1>>,
    Result = iris_proto:decode(Input),
    ?assertEqual({{login, <<>>}, <<>>}, Result).

test_decode_send_message() ->
    Target = <<"bob">>,
    Msg = <<"Hello!">>,
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    Input = <<2, TLen:16, Target/binary, MLen:16, Msg/binary>>,
    
    Result = iris_proto:decode(Input),
    ?assertEqual({{send_message, Target, Msg}, <<>>}, Result).

test_decode_send_message_partial_header() ->
    %% Only opcode, not enough for target length
    Input = <<2, 0>>,
    Result = iris_proto:decode(Input),
    ?assertMatch({more, _}, Result).

test_decode_send_message_partial_body() ->
    %% Header complete but message body incomplete
    Target = <<"bob">>,
    TLen = byte_size(Target),
    %% Missing actual message bytes
    Input = <<2, TLen:16, Target/binary, 100:16>>,
    
    Result = iris_proto:decode(Input),
    ?assertMatch({more, _}, Result).

test_decode_ack() ->
    MsgId = <<"msg-123">>,
    Input = <<3, MsgId/binary>>,
    
    Result = iris_proto:decode(Input),
    ?assertEqual({{ack, MsgId}, <<>>}, Result).

test_decode_batch_send() ->
    Target = <<"target">>,
    TLen = byte_size(Target),
    Batch = <<"batch_data">>,
    BLen = byte_size(Batch),
    Input = <<4, TLen:16, Target/binary, BLen:32, Batch/binary>>,
    
    Result = iris_proto:decode(Input),
    ?assertEqual({{batch_send, Target, Batch}, <<>>}, Result).

test_decode_batch_send_partial() ->
    %% Only opcode, not enough bytes
    Input = <<4, 0, 5>>,
    Result = iris_proto:decode(Input),
    ?assertMatch({more, _}, Result).

test_decode_get_status() ->
    Target = <<"user123">>,
    TLen = byte_size(Target),
    Input = <<5, TLen:16, Target/binary>>,
    
    Result = iris_proto:decode(Input),
    ?assertEqual({{get_status, Target}, <<>>}, Result).

test_decode_get_status_partial() ->
    %% Target length says 10 bytes but only 5 provided
    Input = <<5, 10:16, "hello">>,
    Result = iris_proto:decode(Input),
    ?assertMatch({more, _}, Result).

test_decode_empty() ->
    Result = iris_proto:decode(<<>>),
    ?assertEqual({more, <<>>}, Result).

test_decode_unknown() ->
    %% Unknown opcode (e.g., 99)
    Input = <<99, "garbage">>,
    Result = iris_proto:decode(Input),
    ?assertEqual({{error, unknown_packet}, <<>>}, Result).

test_decode_with_remainder() ->
    %% send_message followed by more data
    Target = <<"bob">>,
    Msg = <<"Hi">>,
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    Remainder = <<"extra_data">>,
    Input = <<2, TLen:16, Target/binary, MLen:16, Msg/binary, Remainder/binary>>,
    
    Result = iris_proto:decode(Input),
    ?assertEqual({{send_message, Target, Msg}, Remainder}, Result).

%% =============================================================================
%% Encode Status Tests
%% =============================================================================

test_encode_status_online() ->
    User = <<"alice">>,
    ULen = byte_size(User),
    
    Result = iris_proto:encode_status(User, online, 0),
    Expected = <<6, ULen:16, User/binary, 1, 0:64>>,
    ?assertEqual(Expected, Result).

test_encode_status_offline() ->
    User = <<"bob">>,
    ULen = byte_size(User),
    LastSeen = 1704067200,  %% Some timestamp
    
    Result = iris_proto:encode_status(User, offline, LastSeen),
    Expected = <<6, ULen:16, User/binary, 0, LastSeen:64>>,
    ?assertEqual(Expected, Result).

test_encode_status_long_user() ->
    User = binary:copy(<<"x">>, 255),
    ULen = byte_size(User),
    
    Result = iris_proto:encode_status(User, online, 12345),
    <<6, RetLen:16, RetUser:RetLen/binary, 1, 12345:64>> = Result,
    ?assertEqual(ULen, RetLen),
    ?assertEqual(User, RetUser).

test_encode_status_unicode() ->
    User = <<"日本語"/utf8>>,
    ULen = byte_size(User),
    
    Result = iris_proto:encode_status(User, offline, 9999),
    <<6, RetLen:16, RetUser:RetLen/binary, 0, 9999:64>> = Result,
    ?assertEqual(ULen, RetLen),
    ?assertEqual(User, RetUser).

test_encode_reliable_msg() ->
    MsgId = <<"id_123">>,
    Msg = <<"payload">>,
    Result = iris_proto:encode_reliable_msg(MsgId, Msg),
    
    IdLen = byte_size(MsgId),
    MsgLen = byte_size(Msg),
    Expected = <<16, IdLen:16, MsgId/binary, MsgLen:32, Msg/binary>>,
    ?assertEqual(Expected, Result).

%% =============================================================================
%% Unpack Batch Tests
%% =============================================================================

test_unpack_batch_multiple() ->
    Msg1 = <<"Hello">>,
    Msg2 = <<"World">>,
    Msg3 = <<"Test">>,
    Blob = <<(byte_size(Msg1)):16, Msg1/binary,
             (byte_size(Msg2)):16, Msg2/binary,
             (byte_size(Msg3)):16, Msg3/binary>>,
    
    Result = iris_proto:unpack_batch(Blob),
    ?assertEqual([Msg1, Msg2, Msg3], Result).

test_unpack_batch_single() ->
    Msg = <<"SingleMessage">>,
    Blob = <<(byte_size(Msg)):16, Msg/binary>>,
    
    Result = iris_proto:unpack_batch(Blob),
    ?assertEqual([Msg], Result).

test_unpack_batch_empty() ->
    Result = iris_proto:unpack_batch(<<>>),
    ?assertEqual([], Result).

test_unpack_batch_garbage() ->
    %% Valid message followed by incomplete garbage
    Msg1 = <<"Valid">>,
    Blob = <<(byte_size(Msg1)):16, Msg1/binary, 255, 255>>,  %% Incomplete msg header
    
    Result = iris_proto:unpack_batch(Blob),
    %% Should return the valid message and ignore trailing garbage
    %% Should return the valid message and ignore trailing garbage
    ?assertEqual([Msg1], Result).

test_unpack_batch_limit() ->
    %% Create a batch with 5000 messages (limit is 1000)
    Msgs = [<<"msg">> || _ <- lists:seq(1, 5000)],
    BatchBlob = list_to_binary([<<3:16, M/binary>> || M <- Msgs]),
    
    %% Should fail
    ?assertEqual({error, batch_too_large}, iris_proto:unpack_batch(BatchBlob)),
    
    %% Small batch should pass
    SmallMsgs = [<<"msg">> || _ <- lists:seq(1, 10)],
    SmallBlob = list_to_binary([<<3:16, M/binary>> || M <- SmallMsgs]),
    ?assertEqual(10, length(iris_proto:unpack_batch(SmallBlob))).

%% =============================================================================
%% Roundtrip Tests
%% =============================================================================

roundtrip_test_() ->
    [
     {"Login roundtrip", fun test_roundtrip_login/0},
     {"Send message roundtrip", fun test_roundtrip_send_message/0},
     {"Large message roundtrip", fun test_roundtrip_large_message/0}
    ].

test_roundtrip_login() ->
    User = <<"roundtrip_user">>,
    Encoded = <<1, User/binary>>,
    {{login, DecodedUser}, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual(User, DecodedUser).

test_roundtrip_send_message() ->
    Target = <<"target">>,
    Msg = <<"The quick brown fox">>,
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    Encoded = <<2, TLen:16, Target/binary, MLen:16, Msg/binary>>,
    
    {{send_message, DecodedTarget, DecodedMsg}, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual(Target, DecodedTarget),
    ?assertEqual(Msg, DecodedMsg).

test_roundtrip_large_message() ->
    Target = <<"t">>,
    Msg = binary:copy(<<"A">>, 10000),  %% 10KB message
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    Encoded = <<2, TLen:16, Target/binary, MLen:16, Msg/binary>>,
    
    {{send_message, _, DecodedMsg}, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual(Msg, DecodedMsg).

%% =============================================================================
%% Edge Case Tests
%% =============================================================================

edge_cases_test_() ->
    [
     {"Max length username in status", fun test_max_length_user/0},
     {"Binary messages with nulls", fun test_binary_with_nulls/0},
     {"Sequential decodes", fun test_sequential_decodes/0}
    ].

test_max_length_user() ->
    %% Max 16-bit length = 65535 bytes
    User = binary:copy(<<"u">>, 1000),  %% 1KB user (reasonable test)
    ULen = byte_size(User),
    
    Encoded = iris_proto:encode_status(User, online, 0),
    <<6, RetLen:16, _/binary>> = Encoded,
    ?assertEqual(ULen, RetLen).

test_binary_with_nulls() ->
    Target = <<"user\0name">>,
    Msg = <<"body\0data">>,
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    Encoded = <<2, TLen:16, Target/binary, MLen:16, Msg/binary>>,
    
    {{send_message, DecodedTarget, DecodedMsg}, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual(Target, DecodedTarget),
    ?assertEqual(Msg, DecodedMsg).

test_sequential_decodes() ->
    %% Multiple packets in sequence
    Msg1 = <<3, "ack1">>,
    Msg2 = <<3, "ack2">>,
    Combined = <<Msg1/binary, Msg2/binary>>,
    
    {{ack, <<"ack1">>}, Rest} = iris_proto:decode(Msg1),
    ?assertEqual(<<>>, Rest),
    
    %% Note: ack consumes entire rest, so this tests that behavior
    {{ack, <<"ack2">>}, <<>>} = iris_proto:decode(Msg2).
