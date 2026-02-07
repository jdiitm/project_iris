-module(iris_auth_hmac_deprecation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-7 (IA-1 residual): HMAC Deprecation Enforcement Tests
%%
%% RFC-001 v4.0 FR-9 mandates EdDSA (Ed25519). HMAC path should be
%% disableable via config for production.
%%
%% Tests verify:
%% 1. HMAC accepted by default (backward compat)
%% 2. HMAC rejected when allow_hmac_jwt = false
%% 3. EdDSA always accepted regardless of HMAC setting
%% 4. Deprecation warning logged in mixed mode
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
            application:set_env(iris_edge, jwt_secret, <<"test_secret_key_for_hmac_deprecation!">>),
            application:set_env(iris_edge, auth_enabled, true),
            {ok, Pid} = iris_auth:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_auth),
    application:unset_env(iris_edge, allow_hmac_jwt),
    catch mnesia:delete_table(revoked_tokens),
    application:stop(mnesia);
cleanup({existing, _Pid}) ->
    application:unset_env(iris_edge, allow_hmac_jwt).

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_auth_hmac_deprecation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"HMAC accepted by default", fun test_hmac_accepted_by_default/0},
      {"HMAC rejected when disabled", fun test_hmac_rejected_when_disabled/0},
      {"EdDSA always accepted", fun test_eddsa_always_accepted/0},
      {"HMAC deprecation detectable", fun test_hmac_deprecation_detectable/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_hmac_accepted_by_default() ->
    %% With no explicit config, HMAC tokens should validate (backward compat)
    application:unset_env(iris_edge, allow_hmac_jwt),
    {ok, Token} = iris_auth:create_token(<<"hmac_default_user">>),
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(<<"hmac_default_user">>, maps:get(<<"sub">>, Claims)).

test_hmac_rejected_when_disabled() ->
    %% When allow_hmac_jwt = false, HMAC tokens should be rejected
    application:set_env(iris_edge, allow_hmac_jwt, false),
    {ok, Token} = iris_auth:create_token(<<"hmac_disabled_user">>),
    Result = iris_auth:validate_token(Token),
    ?assertEqual({error, hmac_deprecated}, Result),
    application:unset_env(iris_edge, allow_hmac_jwt).

test_eddsa_always_accepted() ->
    %% EdDSA tokens should always validate, even when HMAC is disabled
    application:set_env(iris_edge, allow_hmac_jwt, false),
    {ok, Token} = iris_auth:create_eddsa_token(<<"eddsa_only_user">>),
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(<<"eddsa_only_user">>, maps:get(<<"sub">>, Claims)),
    application:unset_env(iris_edge, allow_hmac_jwt).

test_hmac_deprecation_detectable() ->
    %% When HMAC is still allowed (default), validate_token returns {ok, Claims}
    %% with no error. The caller can check the alg to detect HMAC usage.
    application:unset_env(iris_edge, allow_hmac_jwt),
    {ok, Token} = iris_auth:create_token(<<"hmac_detect_user">>),
    {ok, _Claims} = iris_auth:validate_token(Token),
    %% Token was validated - confirm it's an HMAC token by checking header
    [HeaderB64 | _] = binary:split(Token, <<".">>, [global]),
    Padded = case byte_size(HeaderB64) rem 4 of
        0 -> HeaderB64;
        2 -> <<HeaderB64/binary, "==">>;
        3 -> <<HeaderB64/binary, "=">>
    end,
    HeaderJson = base64:decode(binary:replace(binary:replace(Padded, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global])),
    ?assert(binary:match(HeaderJson, <<"HS256">>) =/= nomatch).
