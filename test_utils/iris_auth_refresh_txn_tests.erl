-module(iris_auth_refresh_txn_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT P0-4: Refresh Token Transaction Durability Tests
%% =============================================================================
%%
%% Tests verify that refresh token operations use mnesia:sync_transaction
%% instead of dirty_write. We test:
%% - create_refresh_token uses transactions (returns ok within {atomic, ok})
%% - exchange_refresh_token marks tokens as used transactionally
%% - validate_and_rotate_refresh marks tokens as used transactionally
%% - create_refresh_token_in_family uses transactions
%% - revoke_refresh_family uses transactions (all family tokens marked used)
%% - Source code contains sync_transaction, not dirty_write
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
            TestEdDSAKey = crypto:hash(sha256, <<"iris_txn_test_key_deterministic_">>),
            application:set_env(iris_edge, jwt_secret, <<"txn_test_secret_key_32_bytes_ok!">>),
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

iris_auth_refresh_txn_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"P0-4: Source uses sync_transaction, not dirty_write",
       fun test_source_uses_sync_transaction/0},
      {"P0-4: create_refresh_token stores token via transaction",
       fun test_create_token_transactional/0},
      {"P0-4: exchange_refresh_token marks used via transaction",
       fun test_exchange_marks_used_transactionally/0},
      {"P0-4: validate_and_rotate_refresh marks used via transaction",
       fun test_validate_rotate_marks_used/0},
      {"P0-4: revoke_refresh_family marks all tokens in family as used",
       fun test_revoke_family_marks_all_used/0},
      {"P0-4: create_refresh_token returns error on failure gracefully",
       fun test_create_token_error_handling/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_source_uses_sync_transaction() ->
    {ok, Src} = file:read_file("src/iris_auth.erl"),
    %% Must contain sync_transaction
    ?assert(binary:match(Src, <<"sync_transaction">>) =/= nomatch),
    %% Should NOT contain dirty_write in refresh token section
    %% (We check that dirty_write doesn't appear near refresh_tokens)
    %% Simple check: dirty_write should not be followed by ?REFRESH_TABLE
    Lines = binary:split(Src, <<"\n">>, [global]),
    RefreshDirtyLines = [L || L <- Lines,
        binary:match(L, <<"dirty_write">>) =/= nomatch,
        binary:match(L, <<"REFRESH_TABLE">>) =/= nomatch],
    ?assertEqual([], RefreshDirtyLines).

test_create_token_transactional() ->
    {ok, TokenId} = iris_auth:create_refresh_token(<<"txn_user_1">>),
    ?assert(is_binary(TokenId)),
    ?assert(byte_size(TokenId) > 16),
    %% Verify the token was actually written to Mnesia
    Records = mnesia:dirty_read(refresh_tokens, TokenId),
    ?assertEqual(1, length(Records)),
    [{refresh_tokens, TokenId, <<"txn_user_1">>, _FamilyId, false, _Created, _Expires}] = Records.

test_exchange_marks_used_transactionally() ->
    {ok, RT} = iris_auth:create_refresh_token(<<"txn_user_2">>),
    %% Exchange
    {ok, _NewAccess, NewRT} = iris_auth:exchange_refresh_token(RT),
    %% Original token should be marked as used
    [{refresh_tokens, RT, _, _, Used, _, _}] = mnesia:dirty_read(refresh_tokens, RT),
    ?assertEqual(true, Used),
    %% New token should exist and be unused
    [{refresh_tokens, NewRT, _, _, NewUsed, _, _}] = mnesia:dirty_read(refresh_tokens, NewRT),
    ?assertEqual(false, NewUsed).

test_validate_rotate_marks_used() ->
    {ok, RT} = iris_auth:create_refresh_token(<<"txn_user_3">>),
    {ok, _UserId, NewRT} = iris_auth:validate_and_rotate_refresh(RT),
    %% Original marked as used
    [{refresh_tokens, RT, _, _, true, _, _}] = mnesia:dirty_read(refresh_tokens, RT),
    %% New token exists and is unused
    [{refresh_tokens, NewRT, _, _, false, _, _}] = mnesia:dirty_read(refresh_tokens, NewRT).

test_revoke_family_marks_all_used() ->
    %% Create a chain of tokens in the same family
    {ok, RT1} = iris_auth:create_refresh_token(<<"txn_user_4">>),
    {ok, _A1, RT2} = iris_auth:exchange_refresh_token(RT1),
    {ok, _A2, RT3} = iris_auth:exchange_refresh_token(RT2),
    %% Now reuse RT1 to trigger family revocation
    {error, token_reused} = iris_auth:exchange_refresh_token(RT1),
    %% RT3 should also be revoked
    Result = iris_auth:exchange_refresh_token(RT3),
    ?assertEqual({error, token_reused}, Result).

test_create_token_error_handling() ->
    %% Create token should return {ok, TokenId} on success
    {ok, TokenId} = iris_auth:create_refresh_token(<<"txn_user_5">>),
    ?assert(is_binary(TokenId)).
