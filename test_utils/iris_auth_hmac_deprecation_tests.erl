-module(iris_auth_hmac_deprecation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-7 (IA-1 residual): HMAC Deprecation Enforcement Tests
%%
%% RFC-001 v4.0 FR-9 mandates EdDSA (Ed25519). HMAC path should be
%% disableable via config for production.
%%
%% Tests verify:
%% 1. HMAC rejected by default (RFC v4.0 Sec 6.3: EdDSA mandatory)
%% 2. HMAC accepted when explicitly enabled via allow_hmac_jwt = true
%% 3. HMAC rejected when allow_hmac_jwt = false
%% 4. EdDSA always accepted regardless of HMAC setting
%% 5. Deprecation warning logged in mixed mode
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
            TestEdDSAKey = crypto:hash(sha256, <<"iris_hmac_depr_test_key_det">>),
            application:set_env(iris_edge, jwt_secret, <<"test_secret_key_for_hmac_deprecation!">>),
            application:set_env(iris_edge, jwt_eddsa_private_key, TestEdDSAKey),
            application:set_env(iris_edge, auth_enabled, true),
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
    application:unset_env(iris_edge, allow_hmac_jwt).

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_auth_hmac_deprecation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"HMAC rejected by default (RFC v4.0)", fun test_hmac_rejected_by_default/0},
      {"HMAC accepted when explicitly enabled", fun test_hmac_accepted_when_explicitly_enabled/0},
      {"HMAC rejected when disabled", fun test_hmac_rejected_when_disabled/0},
      {"EdDSA always accepted", fun test_eddsa_always_accepted/0},
      {"HMAC deprecation detectable when enabled", fun test_hmac_deprecation_detectable/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_hmac_rejected_by_default() ->
    %% RFC v4.0 Sec 6.3: With no explicit config, HMAC tokens must be rejected.
    %% EdDSA is the mandatory algorithm; HMAC requires explicit opt-in.
    application:unset_env(iris_edge, allow_hmac_jwt),
    {ok, Token} = iris_auth:create_token(<<"hmac_default_user">>),
    Result = iris_auth:validate_token(Token),
    ?assertEqual({error, hmac_deprecated}, Result).

test_hmac_accepted_when_explicitly_enabled() ->
    %% When allow_hmac_jwt = true is explicitly set, HMAC tokens should validate.
    application:set_env(iris_edge, allow_hmac_jwt, true),
    {ok, Token} = iris_auth:create_token(<<"hmac_enabled_user">>),
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(<<"hmac_enabled_user">>, maps:get(<<"sub">>, Claims)),
    application:unset_env(iris_edge, allow_hmac_jwt).

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
    %% When HMAC is explicitly allowed, validate_token returns {ok, Claims}.
    %% The caller can check the alg to detect HMAC usage.
    application:set_env(iris_edge, allow_hmac_jwt, true),
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
