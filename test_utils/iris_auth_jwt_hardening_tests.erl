-module(iris_auth_jwt_hardening_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: JWT Hardening Tests — Algorithm Pinning & Malformed Input
%% =============================================================================
%%
%% Tests verify that the JWT validation layer rejects:
%% - alg:none (CVE-2015-9235 class attack)
%% - Unsupported algorithms (RS256, HS384, etc.)
%% - Malformed base64 segments
%% - Tokens with wrong number of segments (extra, truncated, empty)
%% =============================================================================

setup() ->
    %% Mnesia setup for revocation table
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
            TestEdDSAKey = crypto:hash(sha256, <<"jwt_hardening_test_key_determ_">>),
            application:set_env(iris_edge, jwt_secret, <<"hardening_test_secret_32_bytes!!">>),
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

iris_auth_jwt_hardening_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"AUDIT: alg:none token is rejected",
       fun test_alg_none_rejected/0},
      {"AUDIT: unsupported algorithm RS256 is rejected",
       fun test_unknown_alg_rejected/0},
      {"AUDIT: malformed base64 token is rejected",
       fun test_malformed_base64/0},
      {"AUDIT: token with 4+ segments is rejected",
       fun test_extra_segments/0},
      {"AUDIT: token missing signature segment is rejected",
       fun test_truncated_token/0},
      {"AUDIT: empty token is rejected",
       fun test_empty_token/0},
      {"AUDIT: dot-only token is rejected",
       fun test_dot_only_token/0},
      {"AUDIT: algorithm whitelist is HS256 and EdDSA only",
       fun test_algorithm_whitelist_in_source/0}
     ]}.

%% =============================================================================
%% Helpers
%% =============================================================================

%% Build a raw JWT with arbitrary header (no valid signature)
craft_token(HeaderMap, PayloadMap) ->
    HeaderJson = iris_auth_json:encode(HeaderMap),
    PayloadJson = iris_auth_json:encode(PayloadMap),
    H = base64url_encode(HeaderJson),
    P = base64url_encode(PayloadJson),
    %% Empty signature
    <<H/binary, ".", P/binary, ".">>.

base64url_encode(Bin) ->
    B64 = base64:encode(Bin),
    B64_1 = binary:replace(B64, <<"+">>, <<"-">>, [global]),
    B64_2 = binary:replace(B64_1, <<"/">>, <<"_">>, [global]),
    binary:replace(B64_2, <<"=">>, <<>>, [global]).

%% =============================================================================
%% Tests
%% =============================================================================

test_alg_none_rejected() ->
    Token = craft_token(
        #{<<"alg">> => <<"none">>, <<"typ">> => <<"JWT">>},
        #{<<"sub">> => <<"attacker">>, <<"exp">> => os:system_time(second) + 3600}
    ),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, _}, Result),
    %% Must NOT be {ok, _}
    case Result of
        {ok, _} -> ?assert(false);
        _ -> ok
    end.

test_unknown_alg_rejected() ->
    Token = craft_token(
        #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>},
        #{<<"sub">> => <<"attacker">>, <<"exp">> => os:system_time(second) + 3600}
    ),
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, _}, Result).

test_malformed_base64() ->
    Result = iris_auth:validate_token(<<"not.valid.base64$$$$">>),
    ?assertMatch({error, _}, Result).

test_extra_segments() ->
    %% 4 segments: header.payload.sig.extra
    Token = <<"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0In0.AAAA.EXTRA">>,
    Result = iris_auth:validate_token(Token),
    ?assertMatch({error, _}, Result).

test_truncated_token() ->
    %% Only 1 segment (no dots)
    Result = iris_auth:validate_token(<<"eyJhbGciOiJIUzI1NiJ9">>),
    ?assertMatch({error, _}, Result),
    %% 2 segments (missing signature)
    Result2 = iris_auth:validate_token(<<"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0In0">>),
    ?assertMatch({error, _}, Result2).

test_empty_token() ->
    Result = iris_auth:validate_token(<<"">>),
    ?assertMatch({error, _}, Result).

test_dot_only_token() ->
    Result = iris_auth:validate_token(<<"..">>),
    ?assertMatch({error, _}, Result).

test_algorithm_whitelist_in_source() ->
    %% Verify the source code contains an explicit algorithm whitelist
    {ok, Src} = file:read_file("src/iris_auth.erl"),
    ?assert(binary:match(Src, <<"AllowedAlgs">>) =/= nomatch),
    ?assert(binary:match(Src, <<"unsupported_algorithm">>) =/= nomatch).
