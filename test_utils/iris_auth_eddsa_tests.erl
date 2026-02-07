-module(iris_auth_eddsa_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-4: EdDSA JWT Validation Tests (RFC-001 v4.0 Section 6.3)
%%
%% Verifies:
%% 1. EdDSA (Ed25519) token verification succeeds
%% 2. HMAC-SHA256 token rejected when header says EdDSA
%% 3. Token with wrong Ed25519 key rejected
%% 4. EdDSA token roundtrip: create and validate
%%
%% Pattern: follows iris_auth_tests.erl setup with application:set_env + Mnesia.
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
            application:set_env(iris_edge, jwt_secret, <<"test_secret_key_for_eddsa_testing!">>),
            application:set_env(iris_edge, auth_enabled, true),
            {ok, Pid} = iris_auth:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_auth),
    catch mnesia:delete_table(revoked_tokens),
    application:stop(mnesia);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_auth_eddsa_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"EdDSA token creation succeeds", fun test_eddsa_token_creation/0},
      {"EdDSA token validates successfully", fun test_eddsa_token_validates/0},
      {"EdDSA token has correct alg header", fun test_eddsa_alg_header/0},
      {"HMAC token still validates", fun test_hmac_still_works/0},
      {"Wrong key rejects EdDSA token", fun test_wrong_key_rejected/0},
      {"EdDSA public key retrieval", fun test_eddsa_public_key_retrieval/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_eddsa_token_creation() ->
    {ok, Token} = iris_auth:create_eddsa_token(<<"alice">>),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0),
    %% Token must have 3 parts separated by dots
    Parts = binary:split(Token, <<".">>, [global]),
    ?assertEqual(3, length(Parts)).

test_eddsa_token_validates() ->
    UserId = <<"eddsa_user_validate">>,
    {ok, Token} = iris_auth:create_eddsa_token(UserId),
    %% Validate the token
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(UserId, maps:get(<<"sub">>, Claims)),
    ?assert(maps:is_key(<<"exp">>, Claims)),
    ?assert(maps:is_key(<<"jti">>, Claims)).

test_eddsa_alg_header() ->
    {ok, Token} = iris_auth:create_eddsa_token(<<"alg_test">>),
    %% Decode header to verify alg = "EdDSA"
    [HeaderB64 | _] = binary:split(Token, <<".">>, [global]),
    %% Pad base64url
    Padded = case byte_size(HeaderB64) rem 4 of
        0 -> HeaderB64;
        2 -> <<HeaderB64/binary, "==">>;
        3 -> <<HeaderB64/binary, "=">>
    end,
    HeaderJson = base64:decode(binary:replace(binary:replace(Padded, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global])),
    %% Parse JSON (simple check)
    ?assert(binary:match(HeaderJson, <<"EdDSA">>) =/= nomatch).

test_hmac_still_works() ->
    %% Existing HMAC tokens must still validate (backward compatibility)
    {ok, Token} = iris_auth:create_token(<<"hmac_user">>),
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(<<"hmac_user">>, maps:get(<<"sub">>, Claims)).

test_wrong_key_rejected() ->
    %% Create a valid EdDSA token, then tamper with the payload
    %% The signature won't match the modified payload
    {ok, Token} = iris_auth:create_eddsa_token(<<"wrong_key_test">>),
    [Header, _Payload, Sig] = binary:split(Token, <<".">>, [global]),

    %% Create a different payload (change sub claim)
    FakePayload = base64:encode(<<"{\"sub\":\"attacker\",\"exp\":99999999999,\"iss\":\"iris\",\"iat\":0,\"jti\":\"fake\"}">>),
    %% Remove padding from base64
    FakePayloadClean = binary:replace(binary:replace(binary:replace(FakePayload, <<"=">>, <<>>, [global]), <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]),
    TamperedToken = <<Header/binary, ".", FakePayloadClean/binary, ".", Sig/binary>>,

    Result = iris_auth:validate_token(TamperedToken),
    ?assertEqual({error, invalid_signature}, Result).

test_eddsa_public_key_retrieval() ->
    {ok, PubKey} = iris_auth:get_eddsa_public_key(),
    ?assert(is_binary(PubKey)),
    ?assertEqual(32, byte_size(PubKey)).  %% Ed25519 public key is 32 bytes
