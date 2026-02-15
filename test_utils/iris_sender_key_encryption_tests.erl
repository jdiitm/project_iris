-module(iris_sender_key_encryption_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Sender Key Encryption Enforcement Tests (RFC Amendment 6.3)
%%
%% The server stores Group Sender Keys as raw binaries. RFC Amendment 6.3
%% requires keys be "distributed via 1:1 E2EE pairwise sessions", meaning
%% the server should only ever see E2EE-encrypted blobs, never plaintext keys.
%%
%% A raw Sender Key is ~64 bytes (32-byte chain_key + 32-byte signature_key).
%% An E2EE-encrypted blob includes IV(16) + ciphertext(64+) + MAC(32) +
%% header(16+) = 128+ bytes minimum.
%%
%% =============================================================================

-define(MIN_ENCRYPTED_BLOB_SIZE, 80).  %% Minimum for E2EE envelope

setup() ->
    catch mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(group_sender_key, [
        {attributes, [key, sender_key, created_at, chain_index]},
        {ram_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, group_sender_key}} -> ok
    end,
    mnesia:wait_for_tables([group_sender_key], 5000),
    ok.

cleanup(_) ->
    catch mnesia:delete_table(group_sender_key),
    catch mnesia:stop().

iris_sender_key_encryption_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"store_sender_key rejects plaintext-sized key (32 bytes)",
       fun test_rejects_raw_32_byte_key/0},
      {"store_sender_key rejects plaintext-sized key (64 bytes)",
       fun test_rejects_raw_64_byte_key/0},
      {"store_sender_key accepts encrypted blob (128+ bytes)",
       fun test_accepts_encrypted_blob/0}
     ]}.

test_rejects_raw_32_byte_key() ->
    %% A 32-byte key is clearly plaintext (raw chain_key only)
    RawKey = crypto:strong_rand_bytes(32),
    Result = iris_group:store_sender_key(<<"group1">>, <<"alice">>, <<"k1">>, RawKey),
    ?assertEqual({error, key_must_be_encrypted}, Result).

test_rejects_raw_64_byte_key() ->
    %% A 64-byte key is plaintext (chain_key + signature_key)
    RawKey = crypto:strong_rand_bytes(64),
    Result = iris_group:store_sender_key(<<"group1">>, <<"alice">>, <<"k2">>, RawKey),
    ?assertEqual({error, key_must_be_encrypted}, Result).

test_accepts_encrypted_blob() ->
    %% A 128+ byte blob represents an E2EE-encrypted key envelope
    EncryptedBlob = crypto:strong_rand_bytes(160),
    Result = iris_group:store_sender_key(<<"group1">>, <<"alice">>, <<"k3">>, EncryptedBlob),
    ?assertEqual(ok, Result).
