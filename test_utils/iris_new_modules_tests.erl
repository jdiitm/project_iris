-module(iris_new_modules_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for Phase 1-4 New Modules
%% =============================================================================

%% -----------------------------------------------------------------------------
%% iris_dedup Tests
%% -----------------------------------------------------------------------------

dedup_check_new_test() ->
    %% New message should not be duplicate
    MsgId = <<"test_msg_1">>,
    Result = iris_dedup:check(MsgId),
    ?assertEqual(false, Result).

dedup_check_duplicate_test() ->
    %% Same message ID should be duplicate
    MsgId = <<"test_msg_dup">>,
    _ = iris_dedup:check(MsgId),
    Result = iris_dedup:check(MsgId),
    ?assertEqual(true, Result).

%% -----------------------------------------------------------------------------
%% iris_rate_limiter Tests
%% -----------------------------------------------------------------------------

rate_limiter_allow_initial_test() ->
    %% First request should be allowed
    User = <<"rate_test_user_1">>,
    Result = iris_rate_limiter:check(User),
    ?assertEqual(allow, Result).

rate_limiter_burst_test() ->
    %% Burst within limit should be allowed
    User = <<"rate_test_user_2">>,
    Results = [iris_rate_limiter:check(User) || _ <- lists:seq(1, 10)],
    AllAllowed = lists:all(fun(R) -> R =:= allow end, Results),
    ?assertEqual(true, AllAllowed).

%% -----------------------------------------------------------------------------
%% iris_auth Tests
%% -----------------------------------------------------------------------------

auth_create_token_test() ->
    %% Should create a valid token
    UserId = <<"test_user">>,
    Token = iris_auth:create_token(UserId),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 50).  %% JWT has header.payload.sig

auth_validate_token_test() ->
    %% Valid token should validate
    UserId = <<"validate_user">>,
    Token = iris_auth:create_token(UserId),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({ok, _}, Result).

auth_invalid_token_test() ->
    %% Invalid token should fail
    Result = iris_auth:validate_token(<<"invalid.token.here">>),
    ?assertMatch({error, _}, Result).

%% -----------------------------------------------------------------------------
%% iris_backpressure Tests
%% -----------------------------------------------------------------------------

backpressure_check_normal_test() ->
    %% Normal checks should be allowed
    Result = iris_backpressure:check(message),
    ?assertMatch({allow, _}, Result).

backpressure_accept_connection_test() ->
    %% Should accept connections normally
    Result = iris_backpressure:should_accept_connection(),
    ?assertEqual(true, Result).

backpressure_client_delay_test() ->
    %% Should return delay value
    Delay = iris_backpressure:get_client_delay(<<"user">>),
    ?assert(is_integer(Delay)),
    ?assert(Delay >= 0).

%% -----------------------------------------------------------------------------
%% iris_proto Tests (validation)
%% -----------------------------------------------------------------------------

proto_length_validation_test() ->
    %% Oversized target should be rejected
    LargeTarget = binary:copy(<<"x">>, 300),  %% Over MAX_TARGET_LEN
    Result = iris_proto:encode_msg(LargeTarget, <<"msg">>),
    ?assertMatch({error, _}, Result).
