-module(iris_auth_refresh_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-5 (IA-3): Refresh Token Flow Tests
%%
%% RFC-001 v4.0 FR-11a: Refresh tokens are opaque, server-stored,
%% rotated on each use. Reuse detection revokes the family.
%%
%% Tests verify:
%% 1. create_refresh_token/1 returns opaque binary
%% 2. exchange_refresh_token/1 returns new access + refresh tokens
%% 3. Second exchange of same token returns token_reused
%% 4. Reuse detection revokes token family
%% 5. Expired refresh token rejected
%% 6. Refresh token not valid as access token
%%
%% Pattern: follows iris_auth_eddsa_tests.erl.
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

    case mnesia:create_table(refresh_tokens, [
        {ram_copies, [node()]},
        {attributes, [token_id, user_id, family_id, used, created_at, expires_at]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, refresh_tokens}} -> ok
    end,

    mnesia:wait_for_tables([revoked_tokens, refresh_tokens], 5000),

    case whereis(iris_auth) of
        undefined ->
            TestEdDSAKey = crypto:hash(sha256, <<"iris_refresh_test_key_determinis">>),
            application:set_env(iris_edge, jwt_secret, <<"refresh_token_test_secret_key_32b!">>),
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
    try mnesia:delete_table(refresh_tokens) catch _:_ -> ok end,
    application:stop(mnesia);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_auth_refresh_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Create refresh token returns opaque binary", fun test_create_refresh_token/0},
      {"Exchange refresh token returns new pair", fun test_refresh_token_exchange/0},
      {"Refresh token is single-use", fun test_refresh_token_single_use/0},
      {"Reuse revokes family", fun test_refresh_token_reuse_revokes_family/0},
      {"Expired refresh token rejected", fun test_expired_refresh_token_rejected/0},
      {"Refresh token not valid as access", fun test_refresh_token_not_valid_as_access/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_create_refresh_token() ->
    {ok, RefreshToken} = iris_auth:create_refresh_token(<<"refresh_user_1">>),
    ?assert(is_binary(RefreshToken)),
    ?assert(byte_size(RefreshToken) > 16).

test_refresh_token_exchange() ->
    {ok, RefreshToken} = iris_auth:create_refresh_token(<<"refresh_user_2">>),
    {ok, NewAccess, NewRefresh} = iris_auth:exchange_refresh_token(RefreshToken),
    ?assert(is_binary(NewAccess)),
    ?assert(is_binary(NewRefresh)),
    ?assertNotEqual(RefreshToken, NewRefresh),
    %% New access token should be valid
    {ok, Claims} = iris_auth:validate_token(NewAccess),
    ?assertEqual(<<"refresh_user_2">>, maps:get(<<"sub">>, Claims)).

test_refresh_token_single_use() ->
    {ok, RefreshToken} = iris_auth:create_refresh_token(<<"refresh_user_3">>),
    {ok, _Access, _NewRefresh} = iris_auth:exchange_refresh_token(RefreshToken),
    %% Second use should fail
    Result = iris_auth:exchange_refresh_token(RefreshToken),
    ?assertEqual({error, token_reused}, Result).

test_refresh_token_reuse_revokes_family() ->
    {ok, RT1} = iris_auth:create_refresh_token(<<"refresh_user_4">>),
    {ok, _Access1, RT2} = iris_auth:exchange_refresh_token(RT1),
    %% Reuse RT1 (already used) -- triggers family revocation
    {error, token_reused} = iris_auth:exchange_refresh_token(RT1),
    %% RT2 should also be revoked (same family)
    Result = iris_auth:exchange_refresh_token(RT2),
    ?assertEqual({error, token_reused}, Result).

test_expired_refresh_token_rejected() ->
    %% Create a refresh token with 0 TTL (already expired)
    {ok, RefreshToken} = iris_auth:create_refresh_token(<<"refresh_user_5">>, 0),
    timer:sleep(100),
    Result = iris_auth:exchange_refresh_token(RefreshToken),
    ?assertEqual({error, refresh_expired}, Result).

test_refresh_token_not_valid_as_access() ->
    %% Refresh tokens are opaque, not JWT -- validate_token should fail
    {ok, RefreshToken} = iris_auth:create_refresh_token(<<"refresh_user_6">>),
    Result = iris_auth:validate_token(RefreshToken),
    ?assertMatch({error, _}, Result).
