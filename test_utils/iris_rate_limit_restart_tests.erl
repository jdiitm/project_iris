-module(iris_rate_limit_restart_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Rate limit persistence across restart
%% =============================================================================
%% Token bucket resets on process restart allow burst abuse.
%% After restart, new buckets must initialize with conservative defaults
%% (half the burst capacity) rather than full burst.
%% =============================================================================

setup() ->
    %% Stop any running instance
    case whereis(iris_rate_limiter) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    %% Set defaults
    application:set_env(iris_edge, rate_limit_default, 5),
    application:set_env(iris_edge, rate_burst_default, 20),
    {ok, _} = iris_rate_limiter:start_link(),
    ok.

cleanup(_) ->
    case whereis(iris_rate_limiter) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    application:unset_env(iris_edge, rate_limit_default),
    application:unset_env(iris_edge, rate_burst_default),
    ok.

rate_limit_restart_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"New bucket starts with conservative tokens", fun test_new_bucket_conservative/0},
        {"Rate limit works after restart", fun test_rate_limit_after_restart/0}
     ]
    }.

%% ---------------------------------------------------------------------------
%% Test: New buckets start with half-burst tokens (not full)
%% ---------------------------------------------------------------------------
test_new_bucket_conservative() ->
    User = <<"test_conservative_user">>,
    %% Send a batch of tokens. With burst=20, old behavior would allow 20.
    %% With initial tokens = burst/2 = 10, should deny after ~10.
    Results = [iris_rate_limiter:check(User) || _ <- lists:seq(1, 15)],
    Allowed = length([R || R <- Results, R =:= allow]),
    Denied = length([R || R <- Results, R =/= allow]),
    %% Must have SOME denials (not all 15 allowed like pre-fix)
    ?assert(Denied > 0, "New bucket must not allow full burst on first use"),
    %% But also some allowed (not zero)
    ?assert(Allowed > 0, "New bucket must allow some initial requests").

%% ---------------------------------------------------------------------------
%% Test: After restart, rate limiting still enforces limits
%% ---------------------------------------------------------------------------
test_rate_limit_after_restart() ->
    User = <<"test_restart_user">>,
    %% Deplete tokens
    [iris_rate_limiter:check(User) || _ <- lists:seq(1, 20)],
    %% Should be denied now
    ?assertMatch({deny, _}, iris_rate_limiter:check(User)),
    
    %% Simulate restart
    gen_server:stop(iris_rate_limiter),
    timer:sleep(10),
    {ok, _} = iris_rate_limiter:start_link(),
    
    %% After restart, user should NOT have full burst capacity
    Results = [iris_rate_limiter:check(User) || _ <- lists:seq(1, 15)],
    Denied = length([R || R <- Results, R =/= allow]),
    ?assert(Denied > 0, "After restart, user must not get full burst capacity").
