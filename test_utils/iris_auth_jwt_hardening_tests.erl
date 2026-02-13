-module(iris_auth_jwt_hardening_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% JWT Hardening Tests (Audit Mitigation)
%%
%% Validates that iris_auth explicitly rejects:
%%   - alg:none tokens (CVE-2015-9235 attack vector)
%%   - Unknown/unsupported algorithms (RS256, PS512, etc.)
%%   - Malformed base64 segments
%%   - Tokens with extra segments (>3 parts)
%%   - Truncated tokens (missing signature)
%%   - Empty/degenerate tokens
%%
%% IMPORTANT: Tests assert the SPECIFIC error reason, not just {error, _}.
%% A signature mismatch is NOT sufficient — the algorithm must be
%% explicitly validated before signature verification.
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Setup / Teardown
%% ---------------------------------------------------------------------------

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
            TestEdDSAKey = crypto:hash(sha256, <<"iris_auth_test_key_deterministic">>),
            application:set_env(iris_edge, jwt_secret, <<"test_secret_key_for_testing_only">>),
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

%% ---------------------------------------------------------------------------
%% Test Generator
%% ---------------------------------------------------------------------------

jwt_hardening_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"alg:none explicitly rejected", fun check_alg_none_rejected/0},
      {"unknown alg explicitly rejected", fun check_unknown_alg_rejected/0},
      {"malformed base64 rejected", fun check_malformed_base64/0},
      {"extra segments rejected", fun check_extra_segments/0},
      {"truncated token rejected", fun check_truncated_token/0},
      {"empty token rejected", fun check_empty_token/0},
      {"dot-dot token rejected", fun check_dot_dot_token/0}
     ]}.

%% ---------------------------------------------------------------------------
%% Helper: Craft a JWT with arbitrary header
%% ---------------------------------------------------------------------------

b64url_encode(Bin) ->
    B64 = base64:encode(Bin),
    NoPad = binary:replace(B64, <<"=">>, <<>>, [global]),
    NoPad2 = binary:replace(NoPad, <<"+">>, <<"-">>, [global]),
    binary:replace(NoPad2, <<"/">>, <<"_">>, [global]).

craft_token(HeaderJson, PayloadJson) ->
    H = b64url_encode(HeaderJson),
    P = b64url_encode(PayloadJson),
    FakeSig = b64url_encode(<<"fakesignature">>),
    <<H/binary, ".", P/binary, ".", FakeSig/binary>>.

%% ---------------------------------------------------------------------------
%% Tests
%%
%% These assert {error, unsupported_algorithm} — the server must validate
%% the algorithm BEFORE attempting signature verification. Rejecting via
%% signature mismatch ({error, invalid_signature}) is insufficient because
%% it means the attacker-controlled alg field influenced the code path.
%% ---------------------------------------------------------------------------

%% alg:none is a well-known JWT bypass (CVE-2015-9235).
check_alg_none_rejected() ->
    Token = craft_token(
        <<"{\"alg\":\"none\",\"typ\":\"JWT\"}">>,
        <<"{\"sub\":\"alice\",\"iss\":\"iris\",\"exp\":9999999999}">>
    ),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, unsupported_algorithm}, Result).

%% RS256 is not in the allowed set [HS256, EdDSA].
check_unknown_alg_rejected() ->
    Token = craft_token(
        <<"{\"alg\":\"RS256\",\"typ\":\"JWT\"}">>,
        <<"{\"sub\":\"alice\",\"iss\":\"iris\",\"exp\":9999999999}">>
    ),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, unsupported_algorithm}, Result).

%% Completely invalid base64 must not crash the server.
check_malformed_base64() ->
    Result = iris_auth:validate_token(<<"not.valid.base64!!!">>),
    ?assertMatch({error, _}, Result).

%% A valid JWT has exactly 3 dot-separated segments.
check_extra_segments() ->
    Token = <<"aaa.bbb.ccc.ddd">>,
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, _}, Result).

%% A token missing the signature segment must be rejected.
check_truncated_token() ->
    H = b64url_encode(<<"{\"alg\":\"HS256\",\"typ\":\"JWT\"}">>),
    P = b64url_encode(<<"{\"sub\":\"alice\"}">>),
    Token = <<H/binary, ".", P/binary>>,
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, _}, Result).

%% Empty string must be cleanly rejected.
check_empty_token() ->
    Result = iris_auth:validate_token(<<"">>),
    ?assertMatch({error, _}, Result).

%% ".." (two dots, empty segments) must be rejected.
check_dot_dot_token() ->
    Result = iris_auth:validate_token(<<"..">>),
    ?assertMatch({error, _}, Result).
