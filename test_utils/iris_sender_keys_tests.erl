-module(iris_sender_keys_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_sender_keys.erl
%% RFC Reference: FR-13 (Group Messaging)
%% =============================================================================

%% =============================================================================
%% Test Fixtures
%% =============================================================================

sender_keys_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Generate sender key", fun test_generate_sender_key/0},
         {"Serialize/deserialize roundtrip", fun test_serialization/0},
         {"Encrypt/decrypt group message", fun test_encrypt_decrypt/0},
         {"Chain advancement", fun test_chain_advancement/0},
         {"Multiple messages", fun test_multiple_messages/0},
         {"Signature verification", fun test_signature_verification/0},
         {"Key ID consistency", fun test_key_id_consistency/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%% =============================================================================
%% Test Cases
%% =============================================================================

test_generate_sender_key() ->
    %% Generate a sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    
    %% Verify key ID is correct length
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    ?assertEqual(8, byte_size(KeyId)),
    
    %% Generate another key - should be different
    SenderKey2 = iris_sender_keys:generate_sender_key(),
    KeyId2 = iris_sender_keys:get_key_id(SenderKey2),
    ?assertNotEqual(KeyId, KeyId2),
    
    ok.

test_serialization() ->
    %% Generate and serialize
    Original = iris_sender_keys:generate_sender_key(),
    Serialized = iris_sender_keys:serialize_sender_key(Original),
    
    %% Verify it's binary
    ?assert(is_binary(Serialized)),
    
    %% Expected size: 1 (version) + 8 (key_id) + 32 (chain_key) + 32 (sign_priv) + 32 (sign_pub) + 4 (chain_index)
    ?assertEqual(109, byte_size(Serialized)),
    
    %% Deserialize and compare
    Restored = iris_sender_keys:deserialize_sender_key(Serialized),
    
    %% Key ID should match
    ?assertEqual(iris_sender_keys:get_key_id(Original), 
                 iris_sender_keys:get_key_id(Restored)),
    
    %% Re-serialize should produce same binary
    Reserialized = iris_sender_keys:serialize_sender_key(Restored),
    ?assertEqual(Serialized, Reserialized),
    
    ok.

test_encrypt_decrypt() ->
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt a message
    Plaintext = <<"Hello, group!">>,
    {Ciphertext, _NewSenderKey} = iris_sender_keys:encrypt_group_msg(
        Plaintext, SenderKey, KeyId
    ),
    
    %% Ciphertext should be different from plaintext
    ?assertNotEqual(Plaintext, Ciphertext),
    
    %% Decrypt the message
    {ok, Decrypted} = iris_sender_keys:decrypt_group_msg(
        Ciphertext, SenderKey, KeyId
    ),
    
    %% Should match original
    ?assertEqual(Plaintext, Decrypted),
    
    ok.

test_chain_advancement() ->
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    
    %% Encrypt first message
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    {_Cipher1, SenderKey2} = iris_sender_keys:encrypt_group_msg(
        <<"msg1">>, SenderKey, KeyId
    ),
    
    %% Encrypt second message
    {_Cipher2, SenderKey3} = iris_sender_keys:encrypt_group_msg(
        <<"msg2">>, SenderKey2, KeyId
    ),
    
    %% Key ID should remain the same
    ?assertEqual(KeyId, iris_sender_keys:get_key_id(SenderKey2)),
    ?assertEqual(KeyId, iris_sender_keys:get_key_id(SenderKey3)),
    
    %% But serialization should differ (chain key changed)
    Serial1 = iris_sender_keys:serialize_sender_key(SenderKey),
    Serial2 = iris_sender_keys:serialize_sender_key(SenderKey2),
    Serial3 = iris_sender_keys:serialize_sender_key(SenderKey3),
    
    ?assertNotEqual(Serial1, Serial2),
    ?assertNotEqual(Serial2, Serial3),
    
    ok.

test_multiple_messages() ->
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt multiple messages
    Messages = [<<"Hello">>, <<"World">>, <<"Test">>, <<"1234567890">>],
    
    {Ciphertexts, FinalKey} = lists:foldl(
        fun(Msg, {Acc, Key}) ->
            {Cipher, NewKey} = iris_sender_keys:encrypt_group_msg(Msg, Key, KeyId),
            {[Cipher | Acc], NewKey}
        end,
        {[], SenderKey},
        Messages
    ),
    
    Ciphertexts2 = lists:reverse(Ciphertexts),
    
    %% Decrypt all messages - receiver tracks chain state too
    %% This simulates how a real group member would track sender's chain
    {Results, _} = lists:foldl(
        fun(Cipher, {Acc, RecvKey}) ->
            Result = iris_sender_keys:decrypt_group_msg(Cipher, RecvKey, KeyId),
            NewRecvKey = iris_sender_keys:advance_chain(RecvKey),
            {[Result | Acc], NewRecvKey}
        end,
        {[], SenderKey},
        Ciphertexts2
    ),
    
    Results2 = lists:reverse(Results),
    
    %% All should succeed
    Decrypted = [Msg || {ok, Msg} <- Results2],
    ?assertEqual(Messages, Decrypted),
    
    %% Final sender key should have advanced
    ?assertNotEqual(
        iris_sender_keys:serialize_sender_key(SenderKey),
        iris_sender_keys:serialize_sender_key(FinalKey)
    ),
    
    ok.

test_signature_verification() ->
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt a message
    Plaintext = <<"Signed message">>,
    {Ciphertext, _} = iris_sender_keys:encrypt_group_msg(
        Plaintext, SenderKey, KeyId
    ),
    
    %% Tamper with ciphertext
    TamperedCiphertext = tamper_bytes(Ciphertext, 20),
    
    %% Decryption should fail (signature invalid or decryption failed)
    Result = iris_sender_keys:decrypt_group_msg(TamperedCiphertext, SenderKey, KeyId),
    ?assertMatch({error, _}, Result),
    
    ok.

test_key_id_consistency() ->
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt with correct key ID
    Plaintext = <<"Test">>,
    {Ciphertext, _} = iris_sender_keys:encrypt_group_msg(
        Plaintext, SenderKey, KeyId
    ),
    
    %% Decrypt with wrong key ID should fail
    WrongKeyId = <<"wrongkey">>,
    Result = iris_sender_keys:decrypt_group_msg(Ciphertext, SenderKey, WrongKeyId),
    ?assertEqual({error, key_id_mismatch}, Result),
    
    ok.

%% =============================================================================
%% Helper Functions
%% =============================================================================

tamper_bytes(Bin, Pos) when Pos < byte_size(Bin) ->
    <<Before:Pos/binary, Byte:8, After/binary>> = Bin,
    <<Before/binary, (Byte bxor 16#FF):8, After/binary>>;
tamper_bytes(Bin, _) ->
    Bin.
