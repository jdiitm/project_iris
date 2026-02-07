-module(iris_auth_key_isolation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% JWT Key Isolation Tests (RFC-001 v4.0 Section 9.1)
%% =============================================================================
%% RFC: "Auth service holds Ed25519 private key (never exported).
%%       All nodes receive Ed25519 public key via config or JWKS endpoint."
%%
%% signer mode: holds private key, can create + validate tokens
%% verifier mode: public key only, can validate but not create EdDSA tokens
%% =============================================================================

setup_signer() ->
    %% Stop existing auth if running
    stop_auth_if_running(),
    application:set_env(iris_edge, allow_random_secret, true),
    application:set_env(iris_edge, auth_mode, signer),
    application:ensure_all_started(mnesia),
    ensure_revoked_table(),
    {ok, NewPid} = iris_auth:start_link(),
    {started, NewPid}.

setup_verifier() ->
    stop_auth_if_running(),
    application:set_env(iris_edge, allow_random_secret, true),
    application:set_env(iris_edge, auth_mode, verifier),
    application:ensure_all_started(mnesia),
    ensure_revoked_table(),
    {ok, NewPid} = iris_auth:start_link(),
    {started, NewPid}.

stop_auth_if_running() ->
    case whereis(iris_auth) of
        undefined -> ok;
        P -> gen_server:stop(P), timer:sleep(50)
    end.

ensure_revoked_table() ->
    case mnesia:create_table(revoked_tokens, [{attributes, [token_id, timestamp]}, {type, set}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        _ -> ok
    end.

cleanup({started, Pid}) ->
    gen_server:stop(Pid),
    application:unset_env(iris_edge, auth_mode),
    timer:sleep(50);
cleanup(_) -> ok.

%% =============================================================================
%% Signer Mode Tests
%% =============================================================================

signer_mode_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup/1,
     [
      {"Signer mode reported correctly", fun test_signer_mode/0},
      {"Signer can create HMAC token", fun test_signer_hmac_create/0},
      {"Signer can create EdDSA token", fun test_signer_eddsa_create/0},
      {"Signer can validate own EdDSA token", fun test_signer_eddsa_roundtrip/0}
     ]}.

test_signer_mode() ->
    ?assertEqual(signer, iris_auth:get_auth_mode()).

test_signer_hmac_create() ->
    {ok, Token} = iris_auth:create_token(<<"alice">>),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).

test_signer_eddsa_create() ->
    Result = iris_auth:create_eddsa_token(<<"alice">>),
    case Result of
        {ok, Token} ->
            ?assert(is_binary(Token)),
            ?assert(byte_size(Token) > 0);
        {error, no_eddsa_key} ->
            %% EdDSA not available on this platform
            ok
    end.

test_signer_eddsa_roundtrip() ->
    case iris_auth:create_eddsa_token(<<"bob">>) of
        {ok, Token} ->
            {ok, Claims} = iris_auth:validate_token(Token),
            ?assertEqual(<<"bob">>, maps:get(<<"sub">>, Claims));
        {error, no_eddsa_key} ->
            ok
    end.

%% =============================================================================
%% Verifier Mode Tests
%% =============================================================================

verifier_mode_test_() ->
    {setup,
     fun setup_verifier/0,
     fun cleanup/1,
     [
      {"Verifier mode reported correctly", fun test_verifier_mode/0},
      {"Verifier can still create HMAC tokens (legacy)", fun test_verifier_hmac_create/0},
      {"Verifier REJECTS EdDSA token creation", fun test_verifier_eddsa_rejected/0},
      {"Verifier has public key available", fun test_verifier_public_key/0}
     ]}.

test_verifier_mode() ->
    ?assertEqual(verifier, iris_auth:get_auth_mode()).

test_verifier_hmac_create() ->
    %% HMAC tokens use shared secret, so verifier can still create them
    {ok, Token} = iris_auth:create_token(<<"alice">>),
    ?assert(is_binary(Token)).

test_verifier_eddsa_rejected() ->
    %% RFC Section 9.1: Verifier mode MUST NOT create EdDSA tokens
    Result = iris_auth:create_eddsa_token(<<"alice">>),
    ?assertEqual({error, verifier_mode_no_signing}, Result).

test_verifier_public_key() ->
    %% Verifier should still have the public key for validation
    case iris_auth:get_eddsa_public_key() of
        {ok, PubKey} ->
            ?assert(is_binary(PubKey)),
            ?assert(byte_size(PubKey) =:= 32);
        {error, no_eddsa_key} ->
            %% EdDSA not available
            ok
    end.
