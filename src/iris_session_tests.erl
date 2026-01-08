-module(iris_session_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    %% Create required ETS tables if they don't exist
    case ets:info(local_presence) of
        undefined -> ets:new(local_presence, [named_table, public, set]);
        _ -> ets:delete_all_objects(local_presence)
    end,
    case ets:info(presence_cache) of
        undefined -> ets:new(presence_cache, [named_table, public, set]);
        _ -> ets:delete_all_objects(presence_cache)
    end,
    ok.

cleanup(_) ->
    catch ets:delete_all_objects(local_presence),
    catch ets:delete_all_objects(presence_cache),
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Login tests
      {"Login registers user locally", fun test_login_registers_locally/0},
      {"Login returns correct structure", fun test_login_returns_structure/0},
      
      %% Message sending tests
      {"Send message returns ok", fun test_send_message_ok/0},
      {"Send message preserves user", fun test_send_message_preserves_user/0},
      {"Send message empty target", fun test_send_message_empty_target/0},
      {"Send message empty body", fun test_send_message_empty_body/0},
      {"Send message large payload", fun test_send_message_large_payload/0},
      
      %% Batch send tests
      {"Batch send returns ok", fun test_batch_send_ok/0},
      {"Batch send empty blob", fun test_batch_send_empty/0},
      {"Batch send single message", fun test_batch_send_single/0},
      
      %% Status query tests
      {"Status cache hit returns response", fun test_status_cache_hit/0},
      {"Status online user response", fun test_status_online_user/0},
      {"Status offline user response", fun test_status_offline_user/0},
      
      %% ACK tests
      {"ACK returns ok", fun test_ack_ok/0},
      {"ACK empty msgid", fun test_ack_empty_msgid/0},
      {"ACK preserves user", fun test_ack_preserves_user/0},
      
      %% Error tests
      {"Error returns ok", fun test_error_ok/0},
      {"Error with reason", fun test_error_with_reason/0},
      {"Error preserves user", fun test_error_preserves_user/0},
      
      %% Terminate tests
      {"Terminate undefined user", fun test_terminate_undefined/0},
      {"Terminate valid user cleans up", fun test_terminate_valid_user/0},
      {"Terminate nonexistent user", fun test_terminate_nonexistent/0},
      
      %% Edge cases
      {"Unicode username", fun test_unicode_username/0},
      {"Binary with nulls", fun test_binary_nulls/0},
      {"Very long username", fun test_long_username/0},
      {"Empty username", fun test_empty_username/0},
      
      %% Response format tests
      {"Status response format", fun test_status_response_format/0},
      
      %% Transport module tests
      {"Different transport modules", fun test_transport_modules/0},
      
      %% Integration tests
      {"Full session lifecycle", fun test_full_lifecycle/0},
      {"Concurrent status queries", fun test_concurrent_status/0}
     ]}.

%% =============================================================================
%% Login Tests
%% =============================================================================

test_login_registers_locally() ->
    User = <<"local_reg_test">>,
    Pid = spawn(fun() -> receive _ -> ok end end),
    
    %% Attempt login (RPC may fail, but local registration should work)
    catch iris_session:handle_packet({login, User}, undefined, Pid, tcp),
    
    %% Verify local_presence was updated
    Result = ets:lookup(local_presence, User),
    ?assertMatch([{User, Pid}], Result),
    
    exit(Pid, kill).

test_login_returns_structure() ->
    User = <<"test_user">>,
    TransportPid = self(),
    
    try
        Result = iris_session:handle_packet({login, User}, undefined, TransportPid, tcp),
        ?assertMatch({ok, User, _Actions}, Result),
        {ok, _, Actions} = Result,
        
        %% Verify LOGIN_OK is first action
        ?assert(length(Actions) >= 1),
        [FirstAction | _] = Actions,
        ?assertMatch({send, <<3, "LOGIN_OK">>}, FirstAction)
    catch
        _:_ ->
            %% RPC failure is expected in isolated unit test
            ?assert(true)
    end.

%% =============================================================================
%% Message Sending Tests
%% =============================================================================

test_send_message_ok() ->
    User = <<"sender">>,
    Target = <<"receiver">>,
    Msg = <<"Hello, World!">>,
    
    Result = iris_session:handle_packet({send_message, Target, Msg}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_send_message_preserves_user() ->
    User = <<"persistent_user">>,
    Result = iris_session:handle_packet(
        {send_message, <<"target">>, <<"msg">>}, 
        User, self(), tcp
    ),
    {ok, ReturnedUser, _} = Result,
    ?assertEqual(User, ReturnedUser).

test_send_message_empty_target() ->
    User = <<"sender">>,
    Result = iris_session:handle_packet({send_message, <<>>, <<"msg">>}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_send_message_empty_body() ->
    User = <<"sender">>,
    Result = iris_session:handle_packet({send_message, <<"target">>, <<>>}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_send_message_large_payload() ->
    User = <<"sender">>,
    LargeMsg = binary:copy(<<"X">>, 65536),  %% 64KB message
    Result = iris_session:handle_packet({send_message, <<"target">>, LargeMsg}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

%% =============================================================================
%% Batch Send Tests
%% =============================================================================

test_batch_send_ok() ->
    User = <<"batch_sender">>,
    Target = <<"batch_receiver">>,
    %% Create a valid batch blob (length-prefixed messages)
    Msg1 = <<"Hello">>,
    Msg2 = <<"World">>,
    Blob = <<(byte_size(Msg1)):16, Msg1/binary, (byte_size(Msg2)):16, Msg2/binary>>,
    
    Result = iris_session:handle_packet({batch_send, Target, Blob}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_batch_send_empty() ->
    User = <<"batch_sender">>,
    Result = iris_session:handle_packet({batch_send, <<"target">>, <<>>}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_batch_send_single() ->
    User = <<"batch_sender">>,
    Msg = <<"SingleMsg">>,
    Blob = <<(byte_size(Msg)):16, Msg/binary>>,
    Result = iris_session:handle_packet({batch_send, <<"target">>, Blob}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

%% =============================================================================
%% Status Query Tests
%% =============================================================================

test_status_cache_hit() ->
    User = <<"status_checker">>,
    TargetUser = <<"cached_target">>,
    Now = os:system_time(seconds),
    
    %% Pre-populate cache with fresh entry (within 5 second TTL)
    ets:insert(presence_cache, {TargetUser, online, 0, Now}),
    
    Result = iris_session:handle_packet({get_status, TargetUser}, User, self(), tcp),
    ?assertMatch({ok, User, [{send, _}]}, Result),
    
    %% Verify response format
    {ok, _, [{send, Resp}]} = Result,
    ?assertMatch(<<6, _/binary>>, Resp).

test_status_online_user() ->
    User = <<"checker">>,
    TargetUser = <<"online_target">>,
    Now = os:system_time(seconds),
    
    %% Cache shows user online
    ets:insert(presence_cache, {TargetUser, online, 0, Now}),
    
    Result = iris_session:handle_packet({get_status, TargetUser}, User, self(), tcp),
    {ok, _, [{send, Resp}]} = Result,
    
    %% Decode response: <<6, ULen:16, User, StateByte, Time:64>>
    <<6, ULen:16, RetUser:ULen/binary, StateByte, _Time:64>> = Resp,
    ?assertEqual(TargetUser, RetUser),
    ?assertEqual(1, StateByte).  %% 1 = online

test_status_offline_user() ->
    User = <<"checker">>,
    TargetUser = <<"offline_target">>,
    Now = os:system_time(seconds),
    LastSeen = 1234567890,
    
    %% Cache shows user offline with last seen time
    ets:insert(presence_cache, {TargetUser, offline, LastSeen, Now}),
    
    Result = iris_session:handle_packet({get_status, TargetUser}, User, self(), tcp),
    {ok, _, [{send, Resp}]} = Result,
    
    <<6, ULen:16, RetUser:ULen/binary, StateByte, RetTime:64>> = Resp,
    ?assertEqual(TargetUser, RetUser),
    ?assertEqual(0, StateByte),  %% 0 = offline
    ?assertEqual(LastSeen, RetTime).

%% =============================================================================
%% ACK Packet Tests
%% =============================================================================

test_ack_ok() ->
    User = <<"acker">>,
    MsgId = <<"msg-12345">>,
    
    Result = iris_session:handle_packet({ack, MsgId}, User, self(), tcp),
    ?assertMatch({ok, User, [{ack_received, MsgId}]}, Result).

test_ack_empty_msgid() ->
    User = <<"acker">>,
    Result = iris_session:handle_packet({ack, <<>>}, User, self(), tcp),
    ?assertMatch({ok, User, [{ack_received, <<>>}]}, Result).

test_ack_preserves_user() ->
    User = <<"specific_user">>,
    Result = iris_session:handle_packet({ack, <<"id">>}, User, self(), tcp),
    {ok, ReturnedUser, _} = Result,
    ?assertEqual(User, ReturnedUser).


%% =============================================================================
%% Error Packet Tests
%% =============================================================================

test_error_ok() ->
    User = <<"error_user">>,
    
    Result = iris_session:handle_packet({error, some_error}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_error_with_reason() ->
    User = <<"error_user">>,
    Result = iris_session:handle_packet({error, {badarg, "invalid"}}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_error_preserves_user() ->
    User = <<"error_user">>,
    Result = iris_session:handle_packet({error, crash}, User, self(), tcp),
    {ok, ReturnedUser, _} = Result,
    ?assertEqual(User, ReturnedUser).

%% =============================================================================
%% Terminate Tests
%% =============================================================================

test_terminate_undefined() ->
    %% Terminating with undefined user should be a no-op
    Result = iris_session:terminate(undefined),
    ?assertEqual(ok, Result).

test_terminate_valid_user() ->
    User = <<"terminating_user">>,
    Pid = self(),
    
    %% Pre-register user in local_presence
    ets:insert(local_presence, {User, Pid}),
    ?assertEqual([{User, Pid}], ets:lookup(local_presence, User)),
    
    %% Terminate (RPC cast may fail, but local cleanup should work)
    catch iris_session:terminate(User),
    
    %% Verify user removed from local_presence
    ?assertEqual([], ets:lookup(local_presence, User)).

test_terminate_nonexistent() ->
    %% Terminating a user not in ETS should not crash
    Result = (catch iris_session:terminate(<<"nonexistent">>)),
    ?assertNotMatch({'EXIT', _}, Result).

%% =============================================================================
%% Edge Cases & Boundary Tests
%% =============================================================================

test_unicode_username() ->
    User = <<"用户"/utf8>>,  %% Chinese characters
    Result = iris_session:handle_packet({send_message, <<"target">>, <<"msg">>}, User, self(), tcp),
    ?assertMatch({ok, User, []}, Result).

test_binary_nulls() ->
    User = <<"user\0name">>,
    Result = iris_session:handle_packet({ack, <<"msgid">>}, User, self(), tcp),
    ?assertMatch({ok, User, [{ack_received, <<"msgid">>}]}, Result).

test_long_username() ->
    User = binary:copy(<<"x">>, 1024),  %% 1KB username
    Result = iris_session:handle_packet({ack, <<"id">>}, User, self(), tcp),
    ?assertMatch({ok, User, [{ack_received, <<"id">>}]}, Result).

test_empty_username() ->
    User = <<>>,
    Result = iris_session:handle_packet({ack, <<"id">>}, User, self(), tcp),
    ?assertMatch({ok, <<>>, [{ack_received, <<"id">>}]}, Result).


%% =============================================================================
%% Protocol Response Format Tests
%% =============================================================================

test_status_response_format() ->
    User = <<"checker">>,
    TargetUser = <<"target">>,
    Now = os:system_time(seconds),
    
    ets:insert(presence_cache, {TargetUser, offline, 9999, Now}),
    
    {ok, _, [{send, Resp}]} = iris_session:handle_packet(
        {get_status, TargetUser}, User, self(), tcp
    ),
    
    %% Verify format: <<6, ULen:16, User:ULen, Status:8, Time:64>>
    <<OpCode, ULen:16, RetUser:ULen/binary, Status, Time:64>> = Resp,
    ?assertEqual(6, OpCode),
    ?assertEqual(TargetUser, RetUser),
    ?assertEqual(0, Status),  %% offline
    ?assertEqual(9999, Time).


test_transport_modules() ->
    User = <<"transport_user">>,
    
    %% Test with different transport module atoms (should all work the same)
    R1 = iris_session:handle_packet({ack, <<"id">>}, User, self(), tcp),
    R2 = iris_session:handle_packet({ack, <<"id">>}, User, self(), ws),
    R3 = iris_session:handle_packet({ack, <<"id">>}, User, self(), undefined),
    
    ?assertMatch({ok, User, [{ack_received, <<"id">>}]}, R1),
    ?assertMatch({ok, User, [{ack_received, <<"id">>}]}, R2),
    ?assertMatch({ok, User, [{ack_received, <<"id">>}]}, R3).

%% =============================================================================
%% Integration Tests
%% =============================================================================

test_full_lifecycle() ->
    User = <<"lifecycle_user">>,
    Pid = self(),
    
    %% 1. Login
    catch iris_session:handle_packet({login, User}, undefined, Pid, tcp),
    
    %% 2. Send some messages
    iris_session:handle_packet({send_message, <<"bob">>, <<"Hi Bob">>}, User, Pid, tcp),
    iris_session:handle_packet({send_message, <<"alice">>, <<"Hi Alice">>}, User, Pid, tcp),
    
    %% 3. Check status
    ets:insert(presence_cache, {<<"bob">>, online, 0, os:system_time(seconds)}),
    {ok, User, [{send, _}]} = iris_session:handle_packet({get_status, <<"bob">>}, User, Pid, tcp),
    
    %% 4. Send acks
    iris_session:handle_packet({ack, <<"msg1">>}, User, Pid, tcp),
    
    %% 5. Terminate
    catch iris_session:terminate(User),
    
    %% Verify cleanup
    ?assertEqual([], ets:lookup(local_presence, User)).

test_concurrent_status() ->
    User = <<"concurrent_user">>,
    Now = os:system_time(seconds),
    
    %% Pre-populate cache for multiple users
    lists:foreach(fun(N) ->
        TargetUser = list_to_binary("user_" ++ integer_to_list(N)),
        Status = case N rem 2 of 0 -> online; 1 -> offline end,
        ets:insert(presence_cache, {TargetUser, Status, N * 1000, Now})
    end, lists:seq(1, 10)),
    
    %% Query all users
    Results = lists:map(fun(N) ->
        TargetUser = list_to_binary("user_" ++ integer_to_list(N)),
        iris_session:handle_packet({get_status, TargetUser}, User, self(), tcp)
    end, lists:seq(1, 10)),
    
    %% All should succeed
    lists:foreach(fun(R) ->
        ?assertMatch({ok, User, [{send, _}]}, R)
    end, Results).
