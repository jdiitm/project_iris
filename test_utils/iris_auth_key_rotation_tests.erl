-module(iris_auth_key_rotation_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for JWT key rotation: kid header, key ring, zero-downtime transitions.

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
            TestKey = crypto:hash(sha256, <<"key_rotation_test_deterministic_">>),
            application:set_env(iris_edge, jwt_secret, <<"key_rotation_test_secret_32bytes!">>),
            application:set_env(iris_edge, jwt_eddsa_private_key, TestKey),
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

iris_auth_key_rotation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"kid header is present in HMAC tokens",
       fun test_kid_header_present/0},
      {"kid header is present in EdDSA tokens",
       fun test_kid_header_present_eddsa/0},
      {"old key remains valid after rotation",
       fun test_old_key_valid_after_rotation/0},
      {"new key works after rotation",
       fun test_new_key_valid/0},
      {"evicted key is rejected (ring size = 2)",
       fun test_evicted_key_rejected/0},
      {"rotate_signing_key API returns ok",
       fun test_rotate_api/0},
      {"rotate_signing_key rejects short secrets",
       fun test_rotate_rejects_short_secret/0}
     ]}.

test_kid_header_present() ->
    {ok, Token} = iris_auth:create_token(<<"user1">>),
    Header = extract_header(Token),
    ?assert(maps:is_key(<<"kid">>, Header)),
    Kid = maps:get(<<"kid">>, Header),
    ?assert(is_binary(Kid)),
    ?assert(byte_size(Kid) > 0).

test_kid_header_present_eddsa() ->
    {ok, Token} = iris_auth:create_eddsa_token(<<"user1">>),
    Header = extract_header(Token),
    ?assert(maps:is_key(<<"kid">>, Header)).

test_old_key_valid_after_rotation() ->
    %% Issue token with current key
    {ok, TokenA} = iris_auth:create_token(<<"user_old">>),
    ?assertMatch({ok, _}, iris_auth:validate_token(TokenA)),
    %% Rotate to new key
    NewSecret = <<"new_secret_for_rotation_32bytes!">>,
    ok = iris_auth:rotate_signing_key(NewSecret),
    %% Clear JTI replay table so re-validation isn't blocked by replay detection
    ets:delete_all_objects(iris_auth_jti_seen),
    %% Old token still validates (previous key is in the ring)
    ?assertMatch({ok, _}, iris_auth:validate_token(TokenA)).

test_new_key_valid() ->
    %% Rotate to a fresh key
    NewSecret = <<"fresh_key_for_new_test_32bytes!!">>,
    ok = iris_auth:rotate_signing_key(NewSecret),
    %% Issue token with the new key
    {ok, TokenB} = iris_auth:create_token(<<"user_new">>),
    ?assertMatch({ok, _}, iris_auth:validate_token(TokenB)).

test_evicted_key_rejected() ->
    %% Start fresh: issue token with key A (current)
    {ok, TokenA} = iris_auth:create_token(<<"user_evict">>),
    ?assertMatch({ok, _}, iris_auth:validate_token(TokenA)),
    %% Rotate to key B
    ok = iris_auth:rotate_signing_key(<<"key_b_for_eviction_test_32bytes!">>),
    %% Clear JTI replay table for re-validation
    ets:delete_all_objects(iris_auth_jti_seen),
    %% TokenA still valid (ring: [B, A])
    ?assertMatch({ok, _}, iris_auth:validate_token(TokenA)),
    %% Rotate to key C — this evicts key A (ring: [C, B])
    ok = iris_auth:rotate_signing_key(<<"key_c_for_eviction_test_32bytes!">>),
    ets:delete_all_objects(iris_auth_jti_seen),
    %% TokenA is now rejected — key A is no longer in the ring
    ?assertMatch({error, invalid_signature}, iris_auth:validate_token(TokenA)).

test_rotate_api() ->
    Result = iris_auth:rotate_signing_key(<<"api_test_secret_32_bytes_long!!!">>),
    ?assertEqual(ok, Result).

test_rotate_rejects_short_secret() ->
    Result = iris_auth:rotate_signing_key(<<"short">>),
    ?assertEqual({error, secret_too_short}, Result).

%% --- Helpers ---

extract_header(Token) ->
    [HeaderB64 | _] = binary:split(Token, <<".">>),
    PadLen = (4 - (byte_size(HeaderB64) rem 4)) rem 4,
    Padded = <<HeaderB64/binary, (binary:copy(<<"=">>, PadLen))/binary>>,
    B64_1 = binary:replace(Padded, <<"-">>, <<"+">>, [global]),
    B64_2 = binary:replace(B64_1, <<"_">>, <<"/">>, [global]),
    Json = base64:decode(B64_2),
    {ok, Map} = iris_auth_json:decode(Json),
    Map.
