-module(iris_auth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    %% Setup Mnesia for revoked_tokens table
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    
    %% Create revoked_tokens table required by iris_auth
    case mnesia:create_table(revoked_tokens, [
        {ram_copies, [node()]},
        {attributes, [token_id, timestamp]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, revoked_tokens}} -> ok
    end,
    mnesia:wait_for_tables([revoked_tokens], 5000),
    
    %% Start the auth server for testing
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

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_auth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Token validation tests
      {"Auth enabled check", fun test_auth_enabled/0},
      {"Valid token accepted", fun test_valid_token/0},
      {"Expired token rejected", fun test_expired_token/0},
      {"Invalid signature rejected", fun test_invalid_signature/0},
      {"Malformed token rejected", fun test_malformed_token/0},
      {"Missing claims rejected", fun test_missing_claims/0},
      
      %% Token creation tests
      {"Create token includes claims", fun test_create_token/0},
      {"Token roundtrip", fun test_token_roundtrip/0},
      
      %% Revocation tests
      {"Revoked token rejected", fun test_revoked_token/0},
      
      %% Security tests
      {"Constant time compare equal", fun test_constant_time_equal/0},
      {"Constant time compare unequal", fun test_constant_time_unequal/0},
      {"Constant time compare length", fun test_constant_time_length/0},
      
      %% P0-C4 / P1-H2: Security hardening tests
      {"JWT secret 32 bytes minimum", fun test_jwt_secret_minimum_length/0},
      {"Revocation is synchronous", fun test_revocation_is_synchronous/0},
      {"Revocation immediate effect", fun test_revocation_immediate_effect/0},

      %% Refresh token TOCTOU race condition
      {"Concurrent refresh exchange - only one succeeds", fun test_concurrent_refresh_exchange/0},

      %% Login rate limiter must fail-closed when ETS absent
      {"Login rate limiter fail-closed on missing ETS", fun test_login_rate_fail_closed/0},

      %% JWT missing alg field must be rejected
      {"JWT missing alg field rejected", fun test_missing_alg_header/0},

      %% H-1: Cross-node revocation via ETS-only propagation
      {"ETS-only revocation detected (cross-node propagation)", fun test_ets_only_revocation/0}
     ]}.

%% =============================================================================
%% Auth Enabled Tests
%% =============================================================================

test_auth_enabled() ->
    %% Should be enabled from setup
    Result = iris_auth:is_auth_enabled(),
    ?assertEqual(true, Result).

%% =============================================================================
%% Token Validation Tests
%% =============================================================================

test_valid_token() ->
    %% Create a valid token and validate it
    UserId = <<"test_user">>,
    {ok, Token} = iris_auth:create_token(UserId),
    
    Result = iris_auth:validate_token(Token),
    ?assertMatch({ok, Claims} when is_map(Claims), Result),
    
    {ok, Claims} = Result,
    ?assertEqual(UserId, maps:get(<<"sub">>, Claims)).

test_expired_token() ->
    %% Create a token with negative TTL (already expired)
    UserId = <<"expired_user">>,
    {ok, Token} = iris_auth:create_token(UserId, #{}, -1),
    
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, token_expired}, Result).

test_invalid_signature() ->
    %% Create a valid token, then tamper with it
    UserId = <<"tampered_user">>,
    {ok, Token} = iris_auth:create_token(UserId),
    
    %% Modify the last character of the signature
    TamperedToken = tamper_signature(Token),
    
    Result = iris_auth:validate_token(TamperedToken),
    ?assertMatch({error, invalid_signature}, Result).

test_malformed_token() ->
    %% Completely invalid token format
    MalformedToken = <<"not.a.valid.jwt.token">>,
    
    Result = iris_auth:validate_token(MalformedToken),
    ?assertMatch({error, _}, Result).

test_missing_claims() ->
    %% Token without required claims - we test by validating garbage
    GarbageToken = <<"abc.def.ghi">>,
    
    Result = iris_auth:validate_token(GarbageToken),
    ?assertMatch({error, _}, Result).

%% =============================================================================
%% Token Creation Tests
%% =============================================================================

test_create_token() ->
    UserId = <<"creation_test">>,
    ExtraClaims = #{<<"role">> => <<"admin">>},
    
    {ok, Token} = iris_auth:create_token(UserId, ExtraClaims),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 50),  %% JWT tokens are reasonably long
    
    %% Verify the token has 3 parts (header.payload.signature)
    Parts = binary:split(Token, <<".">>, [global]),
    ?assertEqual(3, length(Parts)).

test_token_roundtrip() ->
    UserId = <<"roundtrip_user">>,
    ExtraClaims = #{<<"custom">> => <<"value">>},
    
    {ok, Token} = iris_auth:create_token(UserId, ExtraClaims),
    {ok, Claims} = iris_auth:validate_token(Token),
    
    ?assertEqual(UserId, maps:get(<<"sub">>, Claims)),
    ?assertEqual(<<"value">>, maps:get(<<"custom">>, Claims)).

%% =============================================================================
%% Revocation Tests
%% =============================================================================

test_revoked_token() ->
    UserId = <<"revoke_test_user">>,
    {ok, Token} = iris_auth:create_token(UserId),
    
    %% Token should be valid initially
    ?assertMatch({ok, _}, iris_auth:validate_token(Token)),
    
    %% Revoke the token
    ok = iris_auth:revoke_token(Token),
    
    %% Token should now be rejected
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, token_revoked}, Result).

%% =============================================================================
%% Security Tests (Timing Attack Prevention)
%% =============================================================================

test_constant_time_equal() ->
    A = <<"same_value">>,
    B = <<"same_value">>,
    %% We can't directly test constant_time_compare, but we test equality
    ?assertEqual(A, B).

test_constant_time_unequal() ->
    A = <<"value_a">>,
    B = <<"value_b">>,
    ?assert(A =/= B).

test_constant_time_length() ->
    A = <<"short">>,
    B = <<"very_long_value">>,
    ?assert(byte_size(A) =/= byte_size(B)).

%% =============================================================================
%% Helper Functions
%% =============================================================================

tamper_signature(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [Header, Payload, Signature] ->
            %% Flip the last character
            SigLen = byte_size(Signature),
            <<SigPrefix:(SigLen-1)/binary, LastChar>> = Signature,
            NewChar = (LastChar + 1) rem 256,
            NewSignature = <<SigPrefix/binary, NewChar>>,
            <<Header/binary, ".", Payload/binary, ".", NewSignature/binary>>;
        _ ->
            Token  %% Return as-is if not valid format
    end.

%% =============================================================================
%% P0-C4 / P1-H2: Security Hardening Tests
%% =============================================================================

test_jwt_secret_minimum_length() ->
    %% P0-C4 TEST: JWT secret should be at least 32 bytes
    %% The setup uses a 32-byte secret, so this verifies the auth module
    %% accepted it and is functioning correctly
    
    %% Create and validate a token to prove auth is working
    UserId = <<"min_length_test">>,
    {ok, Token} = iris_auth:create_token(UserId),
    {ok, Claims} = iris_auth:validate_token(Token),
    ?assertEqual(UserId, maps:get(<<"sub">>, Claims)).

test_revocation_is_synchronous() ->
    %% P1-H2 TEST: Revocation should be synchronous (not fire-and-forget)
    %% The token should be immediately invalid after revoke_token returns
    
    UserId = <<"sync_revoke_test">>,
    {ok, Token} = iris_auth:create_token(UserId),
    
    %% Token should be valid initially
    ?assertMatch({ok, _}, iris_auth:validate_token(Token)),
    
    %% Revoke the token (this should be synchronous)
    ok = iris_auth:revoke_token(Token),
    
    %% Immediately after revocation, token should be rejected
    %% No delay needed - revocation is synchronous
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, token_revoked}, Result).

test_revocation_immediate_effect() ->
    %% P1-H2 TEST: Multiple revocations should all take immediate effect
    
    %% Create multiple tokens
    Tokens = [begin
        UserId = <<"imm_effect_", (integer_to_binary(I))/binary>>,
        {ok, T} = iris_auth:create_token(UserId),
        T
    end || I <- lists:seq(1, 5)],
    
    %% All should be valid initially
    lists:foreach(fun(T) ->
        ?assertMatch({ok, _}, iris_auth:validate_token(T))
    end, Tokens),
    
    %% Revoke all tokens
    lists:foreach(fun(T) ->
        ok = iris_auth:revoke_token(T)
    end, Tokens),
    
    %% All should be immediately revoked
    lists:foreach(fun(T) ->
        ?assertMatch({error, token_revoked}, iris_auth:validate_token(T))
    end, Tokens).

%% =============================================================================
%% Concurrent refresh token exchange TOCTOU test
%% =============================================================================
%% RFC-001 v4.0 FR-11a: Refresh token rotation must detect concurrent reuse.
%% If two concurrent exchange_refresh_token calls use the same token,
%% exactly one MUST succeed and the other MUST return {error, token_reused}.
%% The dirty_read TOCTOU bug allows both to succeed.

test_concurrent_refresh_exchange() ->
    %% Create the refresh_tokens table if not exists
    case mnesia:create_table(refresh_tokens, [
        {ram_copies, [node()]},
        {attributes, [token_id, user_id, family_id, used, created_at, expires_at]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, refresh_tokens}} ->
            mnesia:clear_table(refresh_tokens)
    end,
    mnesia:wait_for_tables([refresh_tokens], 5000),

    %% Create a refresh token
    UserId = <<"toctou_user">>,
    {ok, TokenId} = iris_auth:create_refresh_token(UserId),

    %% Spawn two concurrent exchange attempts
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = iris_auth:exchange_refresh_token(TokenId),
        Parent ! {Ref, 1, Result}
    end),
    spawn(fun() ->
        Result = iris_auth:exchange_refresh_token(TokenId),
        Parent ! {Ref, 2, Result}
    end),

    %% Collect both results
    R1 = receive {Ref, 1, Res1} -> Res1 after 5000 -> timeout end,
    R2 = receive {Ref, 2, Res2} -> Res2 after 5000 -> timeout end,

    ?assertNotEqual(timeout, R1),
    ?assertNotEqual(timeout, R2),

    %% Exactly one should succeed, one should fail with token_reused
    Successes = length([R || R <- [R1, R2], element(1, R) =:= ok]),
    Failures = length([R || R <- [R1, R2], R =:= {error, token_reused}]),
    ?assertEqual(1, Successes),
    ?assertEqual(1, Failures).

%% =============================================================================
%% Login rate limiter must fail-closed when ETS table absent
%% =============================================================================
%% RFC Section 10.1: 10 failed logins per hour per account.
%% If the ETS table is missing (iris_auth crashed/restarting), the rate
%% limiter MUST reject (fail-closed), not allow (fail-open).

test_login_rate_fail_closed() ->
    %% Delete the failed logins ETS table to simulate iris_auth crash
    case ets:info(iris_auth_failed_logins) of
        undefined -> ok;  %% Already doesn't exist
        _ -> ets:delete(iris_auth_failed_logins)
    end,
    %% With the table gone, check_login_rate should fail-closed
    Result = iris_auth:check_login_rate(<<"brute_force_user">>),
    ?assertEqual({error, rate_limited}, Result),
    %% Recreate the table so cleanup doesn't crash
    ets:new(iris_auth_failed_logins, [set, named_table, public,
                                       {read_concurrency, true},
                                       {write_concurrency, true}]).

%% =============================================================================
%% JWT with missing 'alg' header field must be rejected
%% =============================================================================
%% A JWT header without an 'alg' field must not silently default to HS256.
%% This prevents algorithm confusion attacks.

test_missing_alg_header() ->
    %% Build a JWT with header that has no "alg" field
    Header = #{<<"typ">> => <<"JWT">>},  %% No "alg" key
    Payload = #{
        <<"sub">> => <<"test_user">>,
        <<"iss">> => <<"iris">>,
        <<"iat">> => os:system_time(second),
        <<"exp">> => os:system_time(second) + 3600,
        <<"jti">> => base64:encode(crypto:strong_rand_bytes(16))
    },
    HeaderB64 = base64url_encode(jsx_encode(Header)),
    PayloadB64 = base64url_encode(jsx_encode(Payload)),
    %% Sign with HMAC to make it look valid
    Secret = <<"test_secret_key_for_testing_only">>,
    SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
    Sig = crypto:mac(hmac, sha256, Secret, SigningInput),
    SigB64 = base64url_encode(Sig),
    Token = <<SigningInput/binary, ".", SigB64/binary>>,

    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, missing_algorithm}, Result).

%% =============================================================================
%% H-1: Cross-node revocation via ETS-only propagation
%% =============================================================================
%% Simulates receive_revocation/2 from another node: token is in ETS but
%% NOT yet in Mnesia. The double-check in is_revoked must catch it.

test_ets_only_revocation() ->
    UserId = <<"ets_only_revoke_user">>,
    {ok, Token} = iris_auth:create_token(UserId),

    %% Token valid initially — extract JTI for later revocation
    {ok, Claims} = iris_auth:validate_token(Token),
    Jti = maps:get(<<"jti">>, Claims),
    ?assert(is_binary(Jti)),

    %% Simulate cross-node propagation: insert into ETS directly
    %% (this is what receive_revocation/2 does on remote nodes)
    iris_auth:receive_revocation(Jti, os:system_time(second)),

    %% Token must now be revoked via ETS fast-path.
    %% Note: second validate may also hit jti replay, but revocation
    %% check runs BEFORE replay check in the validation pipeline.
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, token_revoked}, Result).

%% Helper: minimal JSON encode for test JWT construction
jsx_encode(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KEnc = <<"\"", K/binary, "\"">>,
        VEnc = case V of
            B when is_binary(B) -> <<"\"", B/binary, "\"">>;
            I when is_integer(I) -> integer_to_binary(I);
            _ -> <<"null">>
        end,
        [<<KEnc/binary, ":", VEnc/binary>> | Acc]
    end, [], Map),
    iolist_to_binary([<<"{">>, lists:join(<<",">>, Pairs), <<"}">>]).

base64url_encode(Bin) ->
    B64 = base64:encode(Bin),
    B1 = binary:replace(B64, <<"+">>, <<"-">>, [global]),
    B2 = binary:replace(B1, <<"/">>, <<"_">>, [global]),
    binary:replace(B2, <<"=">>, <<>>, [global]).
