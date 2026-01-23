-module(iris_ratchet_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Double Ratchet Unit Tests (RFC-001-AMENDMENT-001)
%% Per TEST_CONTRACT.md: Tests must be deterministic (where possible)
%% =============================================================================

%% =============================================================================
%% Basic Session Tests
%% =============================================================================

init_alice_test() ->
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobRatchetPub, _BobRatchetPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, State} = iris_ratchet:init_alice(SharedSecret, BobRatchetPub),
    
    %% State should be initialized
    StateMap = iris_ratchet:get_state(State),
    ?assertEqual(32, byte_size(maps:get(root_key, StateMap))),
    ?assertEqual(32, byte_size(maps:get(dh_public, StateMap))),
    ?assertEqual(0, maps:get(send_msg_number, StateMap)).

init_bob_test() ->
    SharedSecret = crypto:strong_rand_bytes(32),
    BobKeyPair = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, State} = iris_ratchet:init_bob(SharedSecret, BobKeyPair, undefined),
    
    StateMap = iris_ratchet:get_state(State),
    ?assertEqual(SharedSecret, maps:get(root_key, StateMap)),
    ?assertEqual(undefined, maps:get(dh_remote, StateMap)).

%% =============================================================================
%% Encryption/Decryption Tests
%% =============================================================================

simple_encrypt_decrypt_test() ->
    %% Setup: Simulate X3DH output
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    %% Alice initializes with Bob's ratchet public key
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    %% Bob initializes without Alice's key initially
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice encrypts a message
    Plaintext = <<"Hello, Bob!">>,
    {ok, Ciphertext, Header, AliceState1} = iris_ratchet:encrypt(Plaintext, AliceState0),
    
    %% Verify ciphertext is different from plaintext
    ?assertNotEqual(Plaintext, Ciphertext),
    
    %% Bob decrypts the message
    {ok, Decrypted, BobState1} = iris_ratchet:decrypt(Ciphertext, Header, BobState0),
    
    %% Verify decryption matches original
    ?assertEqual(Plaintext, Decrypted).

bidirectional_communication_test() ->
    %% Setup
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    %% Initialize both parties
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice sends to Bob
    Msg1 = <<"Message 1 from Alice">>,
    {ok, Ct1, Hdr1, AliceState1} = iris_ratchet:encrypt(Msg1, AliceState0),
    {ok, Dec1, BobState1} = iris_ratchet:decrypt(Ct1, Hdr1, BobState0),
    ?assertEqual(Msg1, Dec1),
    
    %% Bob replies to Alice
    Msg2 = <<"Message 2 from Bob">>,
    {ok, Ct2, Hdr2, BobState2} = iris_ratchet:encrypt(Msg2, BobState1),
    {ok, Dec2, AliceState2} = iris_ratchet:decrypt(Ct2, Hdr2, AliceState1),
    ?assertEqual(Msg2, Dec2),
    
    %% Alice sends again
    Msg3 = <<"Message 3 from Alice">>,
    {ok, Ct3, Hdr3, AliceState3} = iris_ratchet:encrypt(Msg3, AliceState2),
    {ok, Dec3, BobState3} = iris_ratchet:decrypt(Ct3, Hdr3, BobState2),
    ?assertEqual(Msg3, Dec3).

multiple_messages_same_chain_test() ->
    %% Test sending multiple messages before reply (same send chain)
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice sends 3 messages in a row
    Msg1 = <<"First">>,
    Msg2 = <<"Second">>,
    Msg3 = <<"Third">>,
    
    {ok, Ct1, Hdr1, AliceState1} = iris_ratchet:encrypt(Msg1, AliceState0),
    {ok, Ct2, Hdr2, AliceState2} = iris_ratchet:encrypt(Msg2, AliceState1),
    {ok, Ct3, Hdr3, AliceState3} = iris_ratchet:encrypt(Msg3, AliceState2),
    
    %% Bob decrypts all 3
    {ok, Dec1, BobState1} = iris_ratchet:decrypt(Ct1, Hdr1, BobState0),
    {ok, Dec2, BobState2} = iris_ratchet:decrypt(Ct2, Hdr2, BobState1),
    {ok, Dec3, BobState3} = iris_ratchet:decrypt(Ct3, Hdr3, BobState2),
    
    ?assertEqual(Msg1, Dec1),
    ?assertEqual(Msg2, Dec2),
    ?assertEqual(Msg3, Dec3),
    
    %% Verify message numbers incremented
    AliceStateMap = iris_ratchet:get_state(AliceState3),
    ?assertEqual(3, maps:get(send_msg_number, AliceStateMap)).

%% =============================================================================
%% Security Property Tests
%% =============================================================================

different_ciphertexts_for_same_plaintext_test() ->
    %% Same plaintext should produce different ciphertexts (due to keys advancing)
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, _BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, State0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    Plaintext = <<"Same message">>,
    
    {ok, Ct1, _, State1} = iris_ratchet:encrypt(Plaintext, State0),
    {ok, Ct2, _, _State2} = iris_ratchet:encrypt(Plaintext, State1),
    
    %% Ciphertexts should be different
    ?assertNotEqual(Ct1, Ct2).

ciphertext_tampering_detected_test() ->
    %% Modified ciphertext should fail decryption
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    Plaintext = <<"Secret message">>,
    {ok, Ciphertext, Header, _} = iris_ratchet:encrypt(Plaintext, AliceState),
    
    %% Tamper with the authentication tag
    %% Format is: <<IV:12, Tag:16, Ciphertext:rest>>
    <<IV:12/binary, Tag:16/binary, CtData/binary>> = Ciphertext,
    <<TagFirst:8, TagRest/binary>> = Tag,
    TamperedTag = <<(TagFirst bxor 16#FF):8, TagRest/binary>>,
    TamperedCt = <<IV/binary, TamperedTag/binary, CtData/binary>>,
    
    %% Decryption should fail
    Result = iris_ratchet:decrypt(TamperedCt, Header, BobState),
    ?assertMatch({error, _}, Result).

%% =============================================================================
%% State Serialization Tests
%% =============================================================================

state_serialization_roundtrip_test() ->
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, _} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, OriginalState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    %% Serialize
    StateMap = iris_ratchet:get_state(OriginalState),
    
    %% Deserialize
    RestoredState = iris_ratchet:from_state(StateMap),
    
    %% Should be able to encrypt with restored state
    Plaintext = <<"Test message">>,
    {ok, _, _, _} = iris_ratchet:encrypt(Plaintext, RestoredState).

%% =============================================================================
%% Forward Secrecy Test
%% =============================================================================

forward_secrecy_key_evolution_test() ->
    %% Verify that keys change with each message (forward secrecy)
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, _} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, State0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    %% Get initial chain key
    StateMap0 = iris_ratchet:get_state(State0),
    ChainKey0 = maps:get(send_chain_key, StateMap0),
    
    %% Encrypt a message
    {ok, _, _, State1} = iris_ratchet:encrypt(<<"msg1">>, State0),
    StateMap1 = iris_ratchet:get_state(State1),
    ChainKey1 = maps:get(send_chain_key, StateMap1),
    
    %% Chain key should have changed
    ?assertNotEqual(ChainKey0, ChainKey1),
    
    %% Encrypt another message
    {ok, _, _, State2} = iris_ratchet:encrypt(<<"msg2">>, State1),
    StateMap2 = iris_ratchet:get_state(State2),
    ChainKey2 = maps:get(send_chain_key, StateMap2),
    
    %% Chain key should change again
    ?assertNotEqual(ChainKey1, ChainKey2).
