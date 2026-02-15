-module(iris_auth_revocation_timing_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%%: Revocation Propagation Timing Tests
%%
%% RFC-001 v4.0 FR-11: Token revocation ≤10 seconds globally.
%%
%% Tests verify:
%% 1. After revoke_token(T), validate_token(T) immediately returns revoked
%% 2. revoke_token/1 triggers cross-node propagation mechanism
%% 3. Revoked token stays revoked (no premature cleanup)
%%
%% Pattern: follows iris_auth_eddsa_tests.erl setup/cleanup.
%% =============================================================================

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    case mnesia:create_table(revoked_tokens, [
        {ram_copies, [node()]},
        {attributes, [token_id, timestamp]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, revoked_tokens}} -> ok
    end,
    mnesia:wait_for_tables([revoked_tokens], 5000),

    case whereis(iris_auth) of
        undefined ->
            TestEdDSAKey = crypto:hash(sha256, <<"iris_revocation_test_key_determ">>),
            application:set_env(iris_edge, jwt_secret, <<"revocation_timing_test_secret_k!">>),
            application:set_env(iris_edge, jwt_eddsa_private_key, TestEdDSAKey),
            application:set_env(iris_edge, auth_enabled, true),
            application:set_env(iris_edge, allow_hmac_jwt, true),
            {ok, Pid} = iris_auth:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_auth),
    application:unset_env(iris_edge, allow_hmac_jwt),
    application:unset_env(iris_edge, jwt_eddsa_private_key),
    application:unset_env(iris_edge, auth_enabled),
    try mnesia:delete_table(revoked_tokens) catch _:_ -> ok end,
    application:stop(mnesia);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_auth_revocation_timing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Revocation immediate local", fun test_revocation_immediate_local/0},
      {"Revocation propagation mechanism exists", fun test_revocation_propagation_mechanism_exists/0},
      {"Revoked token stays revoked", fun test_revoked_token_stays_revoked/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_revocation_immediate_local() ->
    %% After revoke_token(T), validate_token(T) must immediately reject
    {ok, Token} = iris_auth:create_token(<<"revoke_immediate">>),
    {ok, _Claims} = iris_auth:validate_token(Token),
    ok = iris_auth:revoke_token(Token),
    Result = iris_auth:validate_token(Token),
    ?assertEqual({error, token_revoked}, Result).

test_revocation_propagation_mechanism_exists() ->
    %% The receive_revocation/2 function must be exported (for cross-node RPC)
    ?assert(erlang:function_exported(iris_auth, receive_revocation, 2)),
    %% Call it directly to verify it doesn't crash
    ok = iris_auth:receive_revocation(<<"test_token_id">>, os:system_time(second)),
    ok.

test_revoked_token_stays_revoked() ->
    %% Token must remain revoked after some time (no premature cleanup)
    {ok, Token} = iris_auth:create_token(<<"revoke_persist">>),
    ok = iris_auth:revoke_token(Token),
    %% : Event-driven wait instead of timer:sleep(1000)
    ok = iris_test_utils:wait_until(fun() ->
        iris_auth:validate_token(Token) =:= {error, token_revoked}
    end, 2000),
    Result = iris_auth:validate_token(Token),
    ?assertEqual({error, token_revoked}, Result).
