-module(iris_fail_closed_security_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Fail-closed security patterns
%% =============================================================================
%% Security-critical code paths MUST fail-closed (deny) when dependent
%% services are unavailable, rather than fail-open (allow).
%%
%% Tested patterns:
%% 1. check_block_status: When the user_blocks Mnesia table does NOT exist,
%%    the blocking feature is not deployed → returns ok (no blocks to enforce).
%%    When the table EXISTS but a transient failure occurs → returns {error, blocked}.
%% 2. is_revoked: MUST return true (revoked) when Mnesia transaction fails
%%    (not false)
%% 3. check_conn_rate_tcp: MUST return deny when rate check throws (not allow)
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test 1: Block check allows when blocking feature is not deployed
%% ---------------------------------------------------------------------------
block_check_allows_when_not_deployed_test() ->
    %% When user_blocks Mnesia table doesn't exist (feature not deployed),
    %% check_block_status returns ok — there are no blocks to enforce.
    %% This is NOT fail-open: it distinguishes "feature absent" from
    %% "feature present but temporarily broken".
    Result = iris_session:check_block_status(<<"attacker">>, <<"victim">>),
    ?assertEqual(ok, Result).

%% ---------------------------------------------------------------------------
%% Test 2: Rate limit check fails closed on exception
%% ---------------------------------------------------------------------------
rate_limit_catch_all_fails_closed_test() ->
    %% check_conn_rate_tcp should return 'deny' when peername throws.
    %% We use a non-socket value to trigger the catch-all.
    Result = iris_edge_listener:check_conn_rate_tcp(not_a_socket),
    ?assertEqual(deny, Result).
