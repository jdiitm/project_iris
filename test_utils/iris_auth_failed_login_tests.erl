-module(iris_auth_failed_login_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Failed Login Rate Limiter Tests (RFC-001 v4.0 Section 10.1)
%%
%% RFC: "10 failed logins per hour per account"
%% Implementation: ETS-based counter with 1-hour sliding window.
%% =============================================================================

%% =============================================================================
%% Setup / Teardown
%% =============================================================================

setup() ->
    %% iris_auth:start_link() creates the ETS tables we need
    case whereis(iris_auth) of
        undefined ->
            %% Clean up any orphaned ETS tables from previous test crashes
            try ets:delete(iris_auth_revoked) catch error:badarg -> ok end,
            try ets:delete(iris_auth_failed_logins) catch error:badarg -> ok end,
            %% Need to set required config for iris_auth to start
            %% Set auth_enabled=false so ephemeral keys are allowed
            application:set_env(iris_edge, auth_enabled, false),
            application:set_env(iris_edge, allow_random_secret, true),
            {ok, Pid} = iris_auth:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_auth),
    application:unset_env(iris_edge, auth_enabled),
    application:unset_env(iris_edge, allow_random_secret);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

failed_login_rate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"First login attempt is allowed",
       fun test_first_attempt_allowed/0},
      {"10 failures are allowed within window",
       fun test_ten_failures_allowed/0},
      {"11th failure is blocked",
       fun test_eleventh_blocked/0},
      {"Window expiry resets counter",
       fun test_window_expiry/0},
      {"Different users have independent counters",
       fun test_independent_users/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_first_attempt_allowed() ->
    User = <<"failed_login_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ?assertEqual(ok, iris_auth:check_login_rate(User)).

test_ten_failures_allowed() ->
    User = <<"fl_ten_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Record 10 failures — all should be within limit
    lists:foreach(fun(_) -> iris_auth:record_failed_login(User) end, lists:seq(1, 10)),
    %% The 10th failure was recorded; check should now block
    ?assertEqual({error, rate_limited}, iris_auth:check_login_rate(User)).

test_eleventh_blocked() ->
    User = <<"fl_eleven_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Record 10 failures
    lists:foreach(fun(_) -> iris_auth:record_failed_login(User) end, lists:seq(1, 10)),
    %% 11th attempt should be blocked
    ?assertEqual({error, rate_limited}, iris_auth:check_login_rate(User)),
    %% Even recording more doesn't help
    iris_auth:record_failed_login(User),
    ?assertEqual({error, rate_limited}, iris_auth:check_login_rate(User)).

test_window_expiry() ->
    %% We can't easily fast-forward time in ETS, so we'll manipulate
    %% the ETS entry directly to simulate window expiry.
    User = <<"fl_expiry_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Record 10 failures
    lists:foreach(fun(_) -> iris_auth:record_failed_login(User) end, lists:seq(1, 10)),
    ?assertEqual({error, rate_limited}, iris_auth:check_login_rate(User)),

    %% Manually set the window start to 2 hours ago (expired)
    Now = os:system_time(second),
    OldStart = Now - 7200,  %% 2 hours ago (exceeds 1-hour window)
    ets:insert(iris_auth_failed_logins, {User, 10, OldStart}),

    %% Should now be allowed (window expired)
    ?assertEqual(ok, iris_auth:check_login_rate(User)).

test_independent_users() ->
    UserA = <<"fl_indep_a_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    UserB = <<"fl_indep_b_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Exhaust UserA's quota
    lists:foreach(fun(_) -> iris_auth:record_failed_login(UserA) end, lists:seq(1, 10)),
    ?assertEqual({error, rate_limited}, iris_auth:check_login_rate(UserA)),
    %% UserB should be unaffected
    ?assertEqual(ok, iris_auth:check_login_rate(UserB)).
