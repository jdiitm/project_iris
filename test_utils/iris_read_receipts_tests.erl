-module(iris_read_receipts_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Tests for Read Receipts (RFC FR-4)
%% =============================================================================
%% Tests cover:
%% - Protocol encoding/decoding for READ_RECEIPT (0x40)
%% - Protocol encoding for READ_RECEIPT_RELAY (0x41)
%% - Read receipts module API
%% - Disabled state handling
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Protocol Encoding Tests
%% ---------------------------------------------------------------------------

encode_read_receipt_test() ->
    MsgId = <<"msg_12345">>,
    OriginalSender = <<"alice">>,
    Encoded = iris_proto:encode_read_receipt(MsgId, OriginalSender),
    %% Format: 0x40 | MsgIdLen(16) | MsgId | SenderLen(16) | Sender
    Expected = <<16#40, 9:16, "msg_12345", 5:16, "alice">>,
    ?assertEqual(Expected, Encoded).

encode_read_receipt_relay_test() ->
    MsgId = <<"msg_67890">>,
    Reader = <<"bob">>,
    Timestamp = 1706700000000,
    Encoded = iris_proto:encode_read_receipt_relay(MsgId, Reader, Timestamp),
    %% Format: 0x41 | MsgIdLen(16) | MsgId | ReaderLen(16) | Reader | Timestamp(64)
    Expected = <<16#41, 9:16, "msg_67890", 3:16, "bob", Timestamp:64>>,
    ?assertEqual(Expected, Encoded).

encode_read_receipt_binary_msgid_test() ->
    %% Binary message ID (like our 16-byte generated IDs)
    MsgId = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Sender = <<"user">>,
    Encoded = iris_proto:encode_read_receipt(MsgId, Sender),
    MsgIdLen = byte_size(MsgId),
    SenderLen = byte_size(Sender),
    Expected = <<16#40, MsgIdLen:16, MsgId/binary, SenderLen:16, Sender/binary>>,
    ?assertEqual(Expected, Encoded).

%% ---------------------------------------------------------------------------
%% Protocol Decoding Tests
%% ---------------------------------------------------------------------------

decode_read_receipt_test() ->
    Packet = <<16#40, 9:16, "msg_12345", 5:16, "alice">>,
    {Result, Rest} = iris_proto:decode(Packet),
    ?assertEqual({read_receipt, <<"msg_12345">>, <<"alice">>}, Result),
    ?assertEqual(<<>>, Rest).

decode_read_receipt_with_remainder_test() ->
    Packet = <<16#40, 5:16, "msg_1", 3:16, "bob", "extra">>,
    {Result, Rest} = iris_proto:decode(Packet),
    ?assertEqual({read_receipt, <<"msg_1">>, <<"bob">>}, Result),
    ?assertEqual(<<"extra">>, Rest).

decode_read_receipt_partial_test() ->
    %% Incomplete packet
    Packet = <<16#40, 10:16, "msg_">>,  %% Only 4 bytes of 10-byte MsgId
    Result = iris_proto:decode(Packet),
    ?assertMatch({more, _}, Result).

decode_read_receipt_msgid_too_long_test() ->
    %% MsgId length exceeds MAX_MSGID_LEN (64)
    Packet = <<16#40, 65:16, (binary:copy(<<"x">>, 65))/binary, 5:16, "alice">>,
    {Result, _} = iris_proto:decode(Packet),
    ?assertEqual({error, msgid_too_long}, Result).

%% ---------------------------------------------------------------------------
%% Roundtrip Tests
%% ---------------------------------------------------------------------------

roundtrip_read_receipt_test() ->
    MsgId = <<"unique_msg_id_abc123">>,
    Sender = <<"original_sender">>,
    Encoded = iris_proto:encode_read_receipt(MsgId, Sender),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({read_receipt, MsgId, Sender}, Decoded).

roundtrip_binary_msgid_test() ->
    %% Use a realistic 16-byte binary message ID
    MsgId = crypto:strong_rand_bytes(16),
    Sender = <<"test_sender">>,
    Encoded = iris_proto:encode_read_receipt(MsgId, Sender),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({read_receipt, MsgId, Sender}, Decoded).

%% ---------------------------------------------------------------------------
%% Module API Tests (when disabled)
%% ---------------------------------------------------------------------------

is_enabled_when_not_started_test() ->
    %% When the module is not started, should return false
    ?assertEqual(false, iris_read_receipts:is_enabled()).

record_read_when_disabled_test() ->
    %% Should return error when disabled
    Result = iris_read_receipts:record_read(<<"msg">>, <<"reader">>, <<"sender">>),
    ?assertEqual({error, disabled}, Result).

get_read_status_when_disabled_test() ->
    %% Should return error when disabled
    Result = iris_read_receipts:get_read_status(<<"msg">>, <<"user">>),
    ?assertEqual({error, disabled}, Result).

%% ---------------------------------------------------------------------------
%% Module API Tests (when enabled)
%% ---------------------------------------------------------------------------

read_receipts_lifecycle_test_() ->
    {setup,
     fun() ->
         %% Enable read receipts
         application:set_env(iris_core, read_receipts_enabled, true),
         {ok, Pid} = iris_read_receipts:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Cleanup
         gen_server:stop(Pid),
         application:unset_env(iris_core, read_receipts_enabled)
     end,
     fun(_Pid) ->
         [
          {"is_enabled returns true", fun() ->
               ?assertEqual(true, iris_read_receipts:is_enabled())
           end},
          {"record_read succeeds", fun() ->
               Result = iris_read_receipts:record_read(<<"msg1">>, <<"reader1">>, <<"sender1">>),
               ?assertEqual(ok, Result)
           end},
          {"get_read_status returns readers", fun() ->
               %% Record a read
               ok = iris_read_receipts:record_read(<<"msg2">>, <<"reader2">>, <<"sender2">>),
               timer:sleep(10),  %% Allow async cast to complete
               
               %% Get status
               Result = iris_read_receipts:get_read_status(<<"msg2">>, <<"sender2">>),
               ?assertMatch([{<<"reader2">>, _Timestamp}], Result)
           end},
          {"multiple readers tracked", fun() ->
               %% Record reads from multiple users
               ok = iris_read_receipts:record_read(<<"msg3">>, <<"alice">>, <<"sender3">>),
               ok = iris_read_receipts:record_read(<<"msg3">>, <<"bob">>, <<"sender3">>),
               timer:sleep(10),
               
               %% Get status
               Result = iris_read_receipts:get_read_status(<<"msg3">>, <<"sender3">>),
               Readers = [R || {R, _} <- Result],
               ?assert(lists:member(<<"alice">>, Readers)),
               ?assert(lists:member(<<"bob">>, Readers))
           end}
         ]
     end}.
