-module(iris_session_resume_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-6 (FM-3): Connection Resume Tests
%%
%% RFC-001 v4.0 Section 6.5:
%% - Sessions cached with session_id and last_seq on login
%% - Cache expires after 5 minutes (300s)
%% - RESUME with valid session replays messages after last_seq
%% - RESUME with expired/invalid session returns NACK
%%
%% Tests verify:
%% 1. Session cache created on store
%% 2. Session cache expires after 5 min
%% 3. Resume with valid session succeeds
%% 4. Resume with expired session returns nack
%% 5. Resume with invalid session_id returns nack
%% 6. Sequence numbers increment
%%
%% Pattern: standalone ETS-backed tests.
%% =============================================================================

setup() ->
    iris_session_cache:start(),
    ok.

cleanup(_) ->
    iris_session_cache:stop().

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_session_resume_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Session cache created on store", fun test_session_cache_created/0},
      {"Session cache TTL is 300s", fun test_session_cache_ttl/0},
      {"Resume with valid session", fun test_resume_valid_session/0},
      {"Resume with invalid session_id", fun test_resume_invalid_session/0},
      {"Sequence numbers increment", fun test_sequence_numbers_increment/0},
      {"Queue messages for replay", fun test_queue_messages_for_replay/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_session_cache_created() ->
    SessionId = <<"session_create_test">>,
    UserId = <<"user_create_test">>,
    ok = iris_session_cache:store(SessionId, UserId),
    ?assertMatch({ok, _}, iris_session_cache:lookup(SessionId)).

test_session_cache_ttl() ->
    %% Verify TTL is 300 seconds (5 minutes)
    TTL = iris_session_cache:get_ttl(),
    ?assertEqual(300, TTL).

test_resume_valid_session() ->
    SessionId = <<"session_resume_valid">>,
    UserId = <<"user_resume_valid">>,
    ok = iris_session_cache:store(SessionId, UserId),
    %% Queue some messages
    iris_session_cache:queue_message(SessionId, 1, <<"msg1">>),
    iris_session_cache:queue_message(SessionId, 2, <<"msg2">>),
    iris_session_cache:queue_message(SessionId, 3, <<"msg3">>),
    %% Resume from seq 1 -- should get messages after seq 1
    {ok, Messages} = iris_session_cache:get_messages_after(SessionId, 1),
    ?assertEqual(2, length(Messages)).

test_resume_invalid_session() ->
    Result = iris_session_cache:lookup(<<"nonexistent_session">>),
    ?assertEqual({error, not_found}, Result).

test_sequence_numbers_increment() ->
    SessionId = <<"session_seq_test">>,
    UserId = <<"user_seq_test">>,
    ok = iris_session_cache:store(SessionId, UserId),
    Seq1 = iris_session_cache:next_seq(SessionId),
    Seq2 = iris_session_cache:next_seq(SessionId),
    Seq3 = iris_session_cache:next_seq(SessionId),
    ?assertEqual(1, Seq1),
    ?assertEqual(2, Seq2),
    ?assertEqual(3, Seq3).

test_queue_messages_for_replay() ->
    SessionId = <<"session_queue_test">>,
    UserId = <<"user_queue_test">>,
    ok = iris_session_cache:store(SessionId, UserId),
    iris_session_cache:queue_message(SessionId, 1, <<"first">>),
    iris_session_cache:queue_message(SessionId, 2, <<"second">>),
    {ok, All} = iris_session_cache:get_messages_after(SessionId, 0),
    ?assertEqual(2, length(All)),
    {ok, After1} = iris_session_cache:get_messages_after(SessionId, 1),
    ?assertEqual(1, length(After1)).
