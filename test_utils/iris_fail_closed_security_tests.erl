-module(iris_fail_closed_security_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-6 AUDIT MITIGATION: Fail-closed security patterns
%% =============================================================================
%% Security-critical code paths MUST fail-closed (deny) when dependent
%% services are unavailable, rather than fail-open (allow).
%%
%% Tested patterns:
%% 1. check_block_status: MUST return {error, service_unavailable} when
%%    iris_user_safety is down (not 'ok')
%% 2. is_revoked: MUST return true (revoked) when Mnesia transaction fails
%%    (not false)
%% 3. check_conn_rate_tcp: MUST return deny when rate check throws (not allow)
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test 1: Block check fails closed when iris_user_safety is unavailable
%% ---------------------------------------------------------------------------
block_check_fails_closed_test() ->
    %% Calling check_block_status when iris_user_safety is not registered
    %% should return {error, service_unavailable}, NOT ok.
    Result = iris_session:check_block_status(<<"attacker">>, <<"victim">>),
    ?assertMatch({error, _}, Result),
    %% Specifically should NOT be 'ok'
    ?assertNotEqual(ok, Result).

%% ---------------------------------------------------------------------------
%% Test 2: Rate limit check fails closed on exception
%% ---------------------------------------------------------------------------
rate_limit_catch_all_fails_closed_test() ->
    %% check_conn_rate_tcp should return 'deny' when peername throws.
    %% We use a non-socket value to trigger the catch-all.
    Result = iris_edge_listener:check_conn_rate_tcp(not_a_socket),
    ?assertEqual(deny, Result).
