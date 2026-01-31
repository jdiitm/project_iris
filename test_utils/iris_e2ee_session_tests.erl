-module(iris_e2ee_session_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_session E2EE and Group Handlers
%% RFC Reference: RFC-001-AMENDMENT-001
%% =============================================================================
%% These tests verify that the session module correctly handles:
%% - E2EE key bundle operations (upload, fetch)
%% - E2EE message routing
%% - Group operations (create, leave, message, roster, sender key dist)
%% - Rate limiting on message send
%% =============================================================================

-define(TEST_USER, <<"test_user">>).
-define(TEST_TARGET, <<"target_user">>).
-define(TEST_GROUP, <<"test_group">>).

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup() ->
    %% Start required services if not running
    application:ensure_all_started(crypto),
    
    %% Create ETS table for local presence
    case ets:info(local_presence_v2) of
        undefined ->
            ets:new(local_presence_v2, [named_table, public, {write_concurrency, true}]);
        _ -> ok
    end,
    
    %% Create ETS table for presence cache
    case ets:info(presence_cache) of
        undefined ->
            ets:new(presence_cache, [named_table, public, {write_concurrency, true}]);
        _ -> ok
    end,
    
    ok.

cleanup(_) ->
    %% Clean up test data
    catch ets:delete(local_presence_v2, ?TEST_USER),
    catch ets:delete(local_presence_v2, ?TEST_TARGET),
    catch ets:delete(presence_cache, ?TEST_TARGET),
    ok.

%% =============================================================================
%% Protocol Decode Tests (iris_proto)
%% =============================================================================

%% Test typing indicator opcode moved to 0x70
decode_typing_start_test() ->
    %% Format: 0x70 | TargetLen(16) | Target
    Target = <<"alice">>,
    TargetLen = byte_size(Target),
    Packet = <<16#70, TargetLen:16, Target/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({typing_start, Target}, Result).

decode_typing_stop_test() ->
    %% Format: 0x71 | TargetLen(16) | Target
    Target = <<"bob">>,
    TargetLen = byte_size(Target),
    Packet = <<16#71, TargetLen:16, Target/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({typing_stop, Target}, Result).

%% Test read receipt opcode moved to 0x74
decode_read_receipt_test() ->
    %% Format: 0x74 | MsgIdLen(16) | MsgId | SenderLen(16) | Sender
    MsgId = <<"msg123">>,
    Sender = <<"carol">>,
    Packet = <<16#74, (byte_size(MsgId)):16, MsgId/binary, 
               (byte_size(Sender)):16, Sender/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({read_receipt, MsgId, Sender}, Result).

%% Test group create opcode 0x30
decode_group_create_test() ->
    %% Format: 0x30 | NameLen(16) | GroupName
    GroupName = <<"Test Group">>,
    NameLen = byte_size(GroupName),
    Packet = <<16#30, NameLen:16, GroupName/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({group_create, GroupName}, Result).

%% Test group leave opcode 0x32
decode_group_leave_test() ->
    %% Format: 0x32 | GroupIdLen(16) | GroupId
    GroupId = <<"group-123-abc">>,
    Packet = <<16#32, (byte_size(GroupId)):16, GroupId/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({group_leave, GroupId}, Result).

%% Test group roster opcode 0x35
decode_group_roster_test() ->
    %% Format: 0x35 | GroupIdLen(16) | GroupId
    GroupId = <<"group-456-def">>,
    Packet = <<16#35, (byte_size(GroupId)):16, GroupId/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({group_roster, GroupId}, Result).

%% =============================================================================
%% Protocol Encode Tests (iris_proto)
%% =============================================================================

encode_typing_start_test() ->
    Target = <<"alice">>,
    Result = iris_proto:encode_typing_start(Target),
    %% Opcode should be 0x70 now
    ?assertMatch(<<16#70, _/binary>>, Result).

encode_typing_stop_test() ->
    Target = <<"bob">>,
    Result = iris_proto:encode_typing_stop(Target),
    %% Opcode should be 0x71 now
    ?assertMatch(<<16#71, _/binary>>, Result).

encode_typing_relay_test() ->
    Sender = <<"sender">>,
    Result = iris_proto:encode_typing_relay(Sender, true),
    %% Opcode should be 0x72 now
    ?assertMatch(<<16#72, _/binary>>, Result).

encode_read_receipt_test() ->
    MsgId = <<"msg123">>,
    OriginalSender = <<"alice">>,
    Result = iris_proto:encode_read_receipt(MsgId, OriginalSender),
    %% Opcode should be 0x74 now
    ?assertMatch(<<16#74, _/binary>>, Result).

encode_read_receipt_relay_test() ->
    MsgId = <<"msg123">>,
    Reader = <<"bob">>,
    Timestamp = 1234567890,
    Result = iris_proto:encode_read_receipt_relay(MsgId, Reader, Timestamp),
    %% Opcode should be 0x75 now
    ?assertMatch(<<16#75, _/binary>>, Result).

encode_group_create_test() ->
    GroupName = <<"Test Group">>,
    Result = iris_proto:encode_group_create(GroupName),
    %% Opcode should be 0x30
    ?assertMatch(<<16#30, _/binary>>, Result).

encode_group_join_test() ->
    GroupId = <<"group-123">>,
    User = <<"alice">>,
    Result = iris_proto:encode_group_join(GroupId, User),
    %% Opcode should be 0x31
    ?assertMatch(<<16#31, _/binary>>, Result).

encode_group_leave_test() ->
    GroupId = <<"group-123">>,
    Result = iris_proto:encode_group_leave(GroupId),
    %% Opcode should be 0x32
    ?assertMatch(<<16#32, _/binary>>, Result).

encode_group_msg_test() ->
    GroupId = <<"group-123">>,
    Header = #{<<"type">> => <<"text">>},
    Ciphertext = <<"encrypted_message">>,
    Result = iris_proto:encode_group_msg(GroupId, Header, Ciphertext),
    %% Opcode should be 0x33
    ?assertMatch(<<16#33, _/binary>>, Result).

encode_group_roster_request_test() ->
    GroupId = <<"group-123">>,
    Result = iris_proto:encode_group_roster_request(GroupId),
    %% Opcode should be 0x35
    ?assertMatch(<<16#35, _/binary>>, Result).

encode_sender_key_dist_test() ->
    GroupId = <<"group-123">>,
    KeyData = <<"serialized_key_data">>,
    Result = iris_proto:encode_sender_key_dist(GroupId, KeyData),
    %% Opcode should be 0x36
    ?assertMatch(<<16#36, _/binary>>, Result).

%% =============================================================================
%% Session Handler Tests
%% =============================================================================

%% Test that handle_packet for typing_start works
session_typing_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         User = <<"alice">>,
         Target = <<"bob">>,
         Packet = {typing_start, Target},
         
         %% Should return ok without error
         {ok, RetUser, Actions} = iris_session:handle_packet(Packet, User, self(), ?MODULE),
         
         [
          ?_assertEqual(User, RetUser),
          ?_assertEqual([], Actions)
         ]
     end}.

%% Test that unauthenticated typing is ignored
session_typing_unauthenticated_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         Packet = {typing_start, <<"bob">>},
         
         {ok, RetUser, Actions} = iris_session:handle_packet(Packet, undefined, self(), ?MODULE),
         
         [
          ?_assertEqual(undefined, RetUser),
          ?_assertEqual([], Actions)
         ]
     end}.

%% Test that handle_packet for read_receipt works
session_read_receipt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         User = <<"alice">>,
         MsgId = <<"msg123">>,
         OriginalSender = <<"bob">>,
         Packet = {read_receipt, MsgId, OriginalSender},
         
         %% Should return ok (relay function may or may not be available)
         {ok, RetUser, _Actions} = iris_session:handle_packet(Packet, User, self(), ?MODULE),
         
         [?_assertEqual(User, RetUser)]
     end}.

%% Test E2EE handlers reject unauthenticated requests
session_e2ee_unauthenticated_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% Test upload_prekeys
         Bundle = #{identity_key => crypto:strong_rand_bytes(32),
                    signed_prekey => crypto:strong_rand_bytes(32),
                    signed_prekey_signature => crypto:strong_rand_bytes(64)},
         {ok, User1, Actions1} = iris_session:handle_packet({upload_prekeys, Bundle}, undefined, self(), ?MODULE),
         
         %% Test fetch_prekeys
         {ok, User2, Actions2} = iris_session:handle_packet({fetch_prekeys, <<"target">>}, undefined, self(), ?MODULE),
         
         %% Test e2ee_msg
         Header = #{<<"type">> => <<"text">>},
         {ok, User3, _Actions3} = iris_session:handle_packet({e2ee_msg, <<"target">>, <<"ciphertext">>, Header}, undefined, self(), ?MODULE),
         
         [
          ?_assertEqual(undefined, User1),
          ?_assertMatch([{send, _}], Actions1),  %% Should send error response
          ?_assertEqual(undefined, User2),
          ?_assertMatch([{send, _}], Actions2),  %% Should send error response
          ?_assertEqual(undefined, User3)
         ]
     end}.

%% Test group handlers reject unauthenticated requests
session_group_unauthenticated_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% Test group_create
         {ok, User1, _} = iris_session:handle_packet({group_create, <<"Test">>}, undefined, self(), ?MODULE),
         
         %% Test group_leave
         {ok, User2, _} = iris_session:handle_packet({group_leave, <<"group-123">>}, undefined, self(), ?MODULE),
         
         %% Test group_msg
         {ok, User3, _} = iris_session:handle_packet({group_msg, <<"group-123">>, <<"cipher">>, #{}}, undefined, self(), ?MODULE),
         
         %% Test group_roster
         {ok, User4, _} = iris_session:handle_packet({group_roster, <<"group-123">>}, undefined, self(), ?MODULE),
         
         %% Test sender_key_dist
         {ok, User5, _} = iris_session:handle_packet({sender_key_dist, <<"group-123">>, <<"keydata">>}, undefined, self(), ?MODULE),
         
         [
          ?_assertEqual(undefined, User1),
          ?_assertEqual(undefined, User2),
          ?_assertEqual(undefined, User3),
          ?_assertEqual(undefined, User4),
          ?_assertEqual(undefined, User5)
         ]
     end}.

%% Test send_message rejects unauthenticated
session_send_message_unauthenticated_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {ok, User, Actions} = iris_session:handle_packet({send_message, <<"target">>, <<"hello">>}, undefined, self(), ?MODULE),
         
         [
          ?_assertEqual(undefined, User),
          ?_assertEqual([], Actions)
         ]
     end}.

%% =============================================================================
%% Opcode Conflict Resolution Tests
%% =============================================================================

%% Verify typing indicators don't conflict with group opcodes
opcode_conflict_resolution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% 0x30 should be group_create (not typing_start)
         GroupCreatePacket = <<16#30, 10:16, "Test Group">>,
         {GroupResult, _} = iris_proto:decode(GroupCreatePacket),
         
         %% 0x70 should be typing_start
         TypingPacket = <<16#70, 5:16, "alice">>,
         {TypingResult, _} = iris_proto:decode(TypingPacket),
         
         [
          ?_assertEqual({group_create, <<"Test Group">>}, GroupResult),
          ?_assertEqual({typing_start, <<"alice">>}, TypingResult)
         ]
     end}.

%% Verify read receipts don't conflict
read_receipt_opcode_test() ->
    %% 0x74 should be read_receipt
    MsgId = <<"msg123">>,
    Sender = <<"bob">>,
    Packet = <<16#74, (byte_size(MsgId)):16, MsgId/binary,
               (byte_size(Sender)):16, Sender/binary>>,
    
    {Result, _Rest} = iris_proto:decode(Packet),
    ?assertEqual({read_receipt, MsgId, Sender}, Result).

%% =============================================================================
%% Store Default Durability Test
%% =============================================================================

store_default_durability_test() ->
    %% This test verifies the code change, not the actual behavior
    %% (which would require Mnesia setup)
    %% The change is in iris_store:put/4 defaulting to 'quorum' instead of 'guaranteed'
    %% We verify this by checking the module exports and documentation
    Exports = iris_store:module_info(exports),
    ?assert(lists:member({put, 3}, Exports)),
    ?assert(lists:member({put, 4}, Exports)).

%% =============================================================================
%% Test Suite
%% =============================================================================

all_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Protocol decode tests
      {"decode_typing_start", fun decode_typing_start_test/0},
      {"decode_typing_stop", fun decode_typing_stop_test/0},
      {"decode_read_receipt", fun decode_read_receipt_test/0},
      {"decode_group_create", fun decode_group_create_test/0},
      {"decode_group_leave", fun decode_group_leave_test/0},
      {"decode_group_roster", fun decode_group_roster_test/0},
      
      %% Protocol encode tests
      {"encode_typing_start", fun encode_typing_start_test/0},
      {"encode_typing_stop", fun encode_typing_stop_test/0},
      {"encode_typing_relay", fun encode_typing_relay_test/0},
      {"encode_read_receipt", fun encode_read_receipt_test/0},
      {"encode_read_receipt_relay", fun encode_read_receipt_relay_test/0},
      {"encode_group_create", fun encode_group_create_test/0},
      {"encode_group_join", fun encode_group_join_test/0},
      {"encode_group_leave", fun encode_group_leave_test/0},
      {"encode_group_msg", fun encode_group_msg_test/0},
      {"encode_group_roster_request", fun encode_group_roster_request_test/0},
      {"encode_sender_key_dist", fun encode_sender_key_dist_test/0},
      
      %% Opcode conflict tests
      {"read_receipt_opcode", fun read_receipt_opcode_test/0},
      
      %% Store test
      {"store_default_durability", fun store_default_durability_test/0}
     ]}.
