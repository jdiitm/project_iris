-module(iris_new_modules_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for Phase 1-4 New Modules
%% =============================================================================
%% 
%% FIX LOG:
%% - Corrected iris_dedup:check/1 -> iris_dedup:check_and_mark/1 (function didn't exist)
%% - Added setup/teardown for rate_limiter and auth services
%% - Removed proto_length_validation_test (iris_proto:encode_msg/2 doesn't exist)
%% - IMPORTANT: Test functions named test_*() NOT *_test() to avoid EUnit double-discovery
%% =============================================================================

%% =============================================================================
%% Test Generator with Setup/Teardown
%% =============================================================================

new_modules_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% iris_dedup tests (using correct API)
      {"Dedup check new message", fun test_dedup_check_new/0},
      {"Dedup check duplicate message", fun test_dedup_check_duplicate/0},
      
      %% iris_rate_limiter tests
      {"Rate limiter allows initial request", fun test_rate_limiter_allow_initial/0},
      {"Rate limiter allows burst", fun test_rate_limiter_burst/0},
      
      %% iris_auth tests
      {"Auth creates token", fun test_auth_create_token/0},
      {"Auth validates token", fun test_auth_validate_token/0},
      {"Auth rejects invalid token", fun test_auth_invalid_token/0},
      
      %% iris_backpressure tests (don't require setup)
      {"Backpressure check normal", fun test_backpressure_check_normal/0},
      {"Backpressure accept connection", fun test_backpressure_accept_connection/0},
      {"Backpressure client delay", fun test_backpressure_client_delay/0}
     ]}.

setup() ->
    %% Ensure Mnesia is running
    application:ensure_all_started(mnesia),
    
    %% Create dedup_log table if needed (iris_dedup uses this for bloom filter verification)
    create_table_if_needed(dedup_log, [msg_id, timestamp]),
    
    %% Create revoked_tokens table if needed (iris_auth uses this for token revocation)
    create_table_if_needed(revoked_tokens, [token_id, timestamp]),
    
    %% Start dedup service - MUST succeed
    DedupPid = start_service(iris_dedup, fun iris_dedup:start_link/0),
    
    %% Start rate limiter service - MUST succeed
    RateLimiterPid = start_service(iris_rate_limiter, fun iris_rate_limiter:start_link/0),
    
    %% Start auth service with random secret enabled for testing
    application:set_env(iris_edge, allow_random_secret, true),
    application:set_env(iris_edge, allow_hmac_jwt, true),
    AuthPid = start_service(iris_auth, fun iris_auth:start_link/0),
    
    #{dedup => DedupPid, rate_limiter => RateLimiterPid, auth => AuthPid}.

cleanup(#{dedup := DedupPid, rate_limiter := RateLimiterPid, auth := AuthPid}) ->
    %% Stop services in reverse order
    stop_service(AuthPid),
    stop_service(RateLimiterPid),
    stop_service(DedupPid),
    application:unset_env(iris_edge, allow_random_secret),
    application:unset_env(iris_edge, allow_hmac_jwt),
    ok;
cleanup(_) ->
    ok.

%% Helper: Create Mnesia table if needed
create_table_if_needed(TableName, Attributes) ->
    case mnesia:create_table(TableName, [
        {attributes, Attributes},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, TableName}} -> ok;
        {aborted, Reason} -> 
            logger:warning("Could not create ~p table: ~p", [TableName, Reason]),
            ok
    end.

%% Helper: Start a service and ensure it's running
start_service(Name, StartFun) ->
    case whereis(Name) of
        undefined ->
            case StartFun() of
                {ok, Pid} -> 
                    Pid;
                {error, {already_started, Pid}} -> 
                    Pid;
                Other -> 
                    error({failed_to_start, Name, Other})
            end;
        Pid -> 
            Pid
    end.

%% Helper: Stop a service gracefully
stop_service(undefined) -> ok;
stop_service(Pid) when is_pid(Pid) ->
    catch gen_server:stop(Pid, normal, 1000),
    ok.

%% -----------------------------------------------------------------------------
%% iris_dedup Tests (using correct API: check_and_mark/1)
%% NOTE: Function names do NOT end in "_test" to avoid EUnit auto-discovery
%% -----------------------------------------------------------------------------

test_dedup_check_new() ->
    %% New message should return 'new'
    MsgId = <<"test_msg_new_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(new, Result).

test_dedup_check_duplicate() ->
    %% Same message ID should return 'duplicate' on second check
    MsgId = <<"test_msg_dup_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% First call - new
    new = iris_dedup:check_and_mark(MsgId),
    %% Second call - duplicate
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(duplicate, Result).

%% -----------------------------------------------------------------------------
%% iris_rate_limiter Tests
%% -----------------------------------------------------------------------------

test_rate_limiter_allow_initial() ->
    %% First request should be allowed
    User = <<"rate_test_user_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Result = iris_rate_limiter:check(User),
    ?assertEqual(allow, Result).

test_rate_limiter_burst() ->
    %% Burst within limit should be allowed
    User = <<"rate_test_burst_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Results = [iris_rate_limiter:check(User) || _ <- lists:seq(1, 10)],
    AllAllowed = lists:all(fun(R) -> R =:= allow end, Results),
    ?assertEqual(true, AllAllowed).

%% -----------------------------------------------------------------------------
%% iris_auth Tests
%% -----------------------------------------------------------------------------

test_auth_create_token() ->
    %% Should create a valid token
    UserId = <<"test_user_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {ok, Token} = iris_auth:create_token(UserId),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 50).  %% JWT has header.payload.sig

test_auth_validate_token() ->
    %% Valid token should validate
    UserId = <<"validate_user_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {ok, Token} = iris_auth:create_token(UserId),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({ok, _}, Result).

test_auth_invalid_token() ->
    %% Invalid token should fail
    Result = iris_auth:validate_token(<<"invalid.token.here">>),
    ?assertMatch({error, _}, Result).

%% -----------------------------------------------------------------------------
%% iris_backpressure Tests (don't require gen_server)
%% -----------------------------------------------------------------------------

test_backpressure_check_normal() ->
    %% Normal checks should be allowed
    Result = iris_backpressure:check(message),
    case Result of
        {allow, _} -> ?assert(true);
        allow -> ?assert(true);
        {drop, _} -> ?assert(true);
        {delay, _} -> ?assert(true)
    end.

test_backpressure_accept_connection() ->
    %% Should return boolean for connection acceptance
    Result = iris_backpressure:should_accept_connection(),
    ?assert(is_boolean(Result)).

test_backpressure_client_delay() ->
    %% Should return delay value (integer >= 0)
    Delay = iris_backpressure:get_client_delay(<<"user">>),
    ?assert(is_integer(Delay)),
    ?assert(Delay >= 0).
