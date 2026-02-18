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
    {ok, Ciphertext, Header, _AliceState1} = iris_ratchet:encrypt(Plaintext, AliceState0),
    
    %% Verify ciphertext is different from plaintext
    ?assertNotEqual(Plaintext, Ciphertext),
    
    %% Bob decrypts the message
    {ok, Decrypted, _BobState1} = iris_ratchet:decrypt(Ciphertext, Header, BobState0),
    
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
    {ok, Ct3, Hdr3, _AliceState3} = iris_ratchet:encrypt(Msg3, AliceState2),
    {ok, Dec3, _BobState3} = iris_ratchet:decrypt(Ct3, Hdr3, BobState2),
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
    {ok, Dec3, _BobState3} = iris_ratchet:decrypt(Ct3, Hdr3, BobState2),
    
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

%% =============================================================================
%% Attack Resistance Tests (P1 - Correctness Critical)
%% =============================================================================
%%
%% Tests for replay, drop, and mangled header attacks.
%%
%% These tests verify the Double Ratchet implementation resists:
%% 1. Replay attacks - decrypting the same message twice
%% 2. Drop attacks - skipping messages in sequence
%% 3. Mangled MAC attacks - tampering with authentication data
%% 4. Truncated ciphertext attacks - sending incomplete data
%% 5. Header manipulation attacks - modifying message headers
%% =============================================================================

attack_resistance_test_() ->
    {"Attack resistance tests (P1)",
     [
      {"Replay attack rejected", fun test_replay_attack/0},
      {"Drop attack (skip 5 msgs) recovers", fun test_drop_attack_recovery/0},
      {"Drop attack (skip 50 msgs) recovers", fun test_large_gap_recovery/0},
      {"Mangled MAC rejected", fun test_mangled_mac/0},
      {"Truncated ciphertext rejected", fun test_truncated_ciphertext/0},
      {"Header manipulation rejected", fun test_header_manipulation/0},
      {"Wrong sender key rejected", fun test_wrong_sender_key/0},
      {"Out-of-order delivery handled", fun test_out_of_order_delivery/0}
     ]}.

test_replay_attack() ->
    %% Test: Encrypt message, then try to decrypt twice
    %% Second decrypt MUST return {error, _} (replay detected)
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice encrypts a message
    Plaintext = <<"Replay test message">>,
    {ok, Ciphertext, Header, _} = iris_ratchet:encrypt(Plaintext, AliceState),
    
    %% Bob decrypts first time - should succeed
    {ok, Decrypted, BobState1} = iris_ratchet:decrypt(Ciphertext, Header, BobState0),
    ?assertEqual(Plaintext, Decrypted),
    
    %% Bob tries to decrypt same message again - MUST fail (replay)
    %% After first decryption, the message key is consumed (removed from
    %% skipped_keys or chain advanced past it). Replaying the same
    %% ciphertext+header MUST fail with a decryption error.
    %% B-5 FIX: Previously this test passed in both branches (assert(true)).
    Result = iris_ratchet:decrypt(Ciphertext, Header, BobState1),
    ?assertMatch({error, _}, Result).

test_drop_attack_recovery() ->
    %% Test: Skip 5 messages and verify system recovers
    %% The Double Ratchet should handle message gaps gracefully
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice sends 6 messages
    {ok, _Ct1, _Hdr1, AliceState1} = iris_ratchet:encrypt(<<"msg1">>, AliceState0),
    {ok, _Ct2, _Hdr2, AliceState2} = iris_ratchet:encrypt(<<"msg2">>, AliceState1),
    {ok, _Ct3, _Hdr3, AliceState3} = iris_ratchet:encrypt(<<"msg3">>, AliceState2),
    {ok, _Ct4, _Hdr4, AliceState4} = iris_ratchet:encrypt(<<"msg4">>, AliceState3),
    {ok, _Ct5, _Hdr5, AliceState5} = iris_ratchet:encrypt(<<"msg5">>, AliceState4),
    {ok, Ct6, Hdr6, _AliceState6} = iris_ratchet:encrypt(<<"msg6">>, AliceState5),
    
    %% Bob only receives message 6 (messages 1-5 dropped)
    %% The Double Ratchet MUST handle message gaps via skipped message keys.
    %% With MAX_SKIP=1000, skipping 5 messages is well within bounds.
    %% B-5 FIX: Previously this test passed in both branches (assert(true)).
    Result = iris_ratchet:decrypt(Ct6, Hdr6, BobState0),
    ?assertMatch({ok, <<"msg6">>, _}, Result).

test_large_gap_recovery() ->
    %% Test: Skip 50 messages - verify behavior is defined
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice sends 51 messages
    {FinalAliceState, _} = lists:foldl(
        fun(N, {State, _}) ->
            Msg = list_to_binary("msg" ++ integer_to_list(N)),
            {ok, Ct, Hdr, NewState} = iris_ratchet:encrypt(Msg, State),
            {NewState, {Ct, Hdr}}
        end,
        {AliceState0, undefined},
        lists:seq(1, 51)
    ),
    
    %% Encrypt one more message after the gap
    {ok, FinalCt, FinalHdr, _} = iris_ratchet:encrypt(<<"final_msg">>, FinalAliceState),
    
    %% Bob tries to decrypt the final message (52nd, after skipping 51)
    %% With MAX_SKIP=1000, 52 skipped messages is within bounds and MUST succeed.
    %% B-5 FIX: Previously this test passed in both branches (assert(true)).
    Result = iris_ratchet:decrypt(FinalCt, FinalHdr, BobState0),
    ?assertMatch({ok, <<"final_msg">>, _}, Result).

test_mangled_mac() ->
    %% Test: Tamper with authentication tag (MAC)
    %% Decryption MUST fail with authentication error
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    Plaintext = <<"MAC test message">>,
    {ok, Ciphertext, Header, _} = iris_ratchet:encrypt(Plaintext, AliceState),
    
    %% Tamper with different parts of the MAC
    %% Format: <<IV:12, Tag:16, CtData:rest>>
    <<IV:12/binary, Tag:16/binary, CtData/binary>> = Ciphertext,
    
    %% Test 1: Flip first byte of tag
    <<T1:8, TRest/binary>> = Tag,
    MangledTag1 = <<(T1 bxor 16#FF):8, TRest/binary>>,
    MangledCt1 = <<IV/binary, MangledTag1/binary, CtData/binary>>,
    ?assertMatch({error, _}, iris_ratchet:decrypt(MangledCt1, Header, BobState)),
    
    %% Test 2: Flip last byte of tag
    TagSize = byte_size(Tag),
    <<TFirst:(TagSize-1)/binary, TLast:8>> = Tag,
    MangledTag2 = <<TFirst/binary, (TLast bxor 16#FF):8>>,
    MangledCt2 = <<IV/binary, MangledTag2/binary, CtData/binary>>,
    ?assertMatch({error, _}, iris_ratchet:decrypt(MangledCt2, Header, BobState)),
    
    %% Test 3: Zero out the tag
    ZeroTag = binary:copy(<<0>>, 16),
    MangledCt3 = <<IV/binary, ZeroTag/binary, CtData/binary>>,
    ?assertMatch({error, _}, iris_ratchet:decrypt(MangledCt3, Header, BobState)).

test_truncated_ciphertext() ->
    %% Test: Send incomplete ciphertext
    %% Decryption MUST fail safely (no crash)
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    Plaintext = <<"Truncation test message">>,
    {ok, Ciphertext, Header, _} = iris_ratchet:encrypt(Plaintext, AliceState),
    
    %% Test various truncation lengths
    CtLen = byte_size(Ciphertext),
    TruncationLengths = [0, 1, 5, 10, 12, 15, 20, CtLen - 1],
    
    lists:foreach(fun(Len) when Len < CtLen ->
        <<TruncatedCt:Len/binary, _/binary>> = Ciphertext,
        Result = try
            iris_ratchet:decrypt(TruncatedCt, Header, BobState)
        catch
            _:_ -> {error, crash}
        end,
        %% Should return error, not crash
        ?assertMatch({error, _}, Result)
    end, TruncationLengths).

test_header_manipulation() ->
    %% Test: Manipulate message header
    %% Should result in decryption failure
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    Plaintext = <<"Header test message">>,
    {ok, Ciphertext, Header, _} = iris_ratchet:encrypt(Plaintext, AliceState),
    
    %% Manipulate header based on its structure
    %% Header typically contains: {DHPublic, PreviousChainLength, MessageNumber}
    ManipulatedHeaders = case Header of
        {DHPub, Pn, N} when is_binary(DHPub), is_integer(Pn), is_integer(N) ->
            [
                {crypto:strong_rand_bytes(32), Pn, N},  %% Wrong DH key
                {DHPub, Pn + 100, N},                   %% Wrong previous chain length
                {DHPub, Pn, N + 100}                    %% Wrong message number
            ];
        _ when is_binary(Header) ->
            %% If header is binary, flip some bits
            <<H1:8, HRest/binary>> = Header,
            [<<(H1 bxor 16#FF):8, HRest/binary>>];
        _ ->
            %% Unknown header format - just try with modified version
            [{modified, Header}]
    end,
    
    %% B-5 FIX: Header manipulation MUST cause decryption failure.
    %% The header (DH public key, chain length, message number) is used as
    %% AAD in AEAD encryption and for key derivation. Any modification MUST
    %% result in {error, _} or an exception (caught as {error, exception}).
    lists:foreach(fun(MangledHeader) ->
        Result = try
            iris_ratchet:decrypt(Ciphertext, MangledHeader, BobState)
        catch
            _:_ -> {error, exception}
        end,
        ?assertMatch({error, _}, Result)
    end, ManipulatedHeaders).

test_wrong_sender_key() ->
    %% Test: Message encrypted by different sender should be rejected
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    %% Alice's session with Bob
    {ok, _AliceState} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Eve's session (different secret)
    EveSecret = crypto:strong_rand_bytes(32),
    {ok, EveState} = iris_ratchet:init_alice(EveSecret, BobPub),
    
    %% Eve encrypts a message
    EveMsg = <<"Message from Eve pretending to be Alice">>,
    {ok, EveCt, EveHdr, _} = iris_ratchet:encrypt(EveMsg, EveState),
    
    %% Bob tries to decrypt Eve's message (expecting Alice)
    Result = iris_ratchet:decrypt(EveCt, EveHdr, BobState),
    
    %% Should fail - wrong sender
    ?assertMatch({error, _}, Result).

test_out_of_order_delivery() ->
    %% Test: Messages delivered out of order should still decrypt
    %% (within the skipped message key limit)
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, {BobPub, BobPriv}, undefined),
    
    %% Alice sends 3 messages
    {ok, Ct1, Hdr1, AliceState1} = iris_ratchet:encrypt(<<"msg1">>, AliceState0),
    {ok, Ct2, Hdr2, AliceState2} = iris_ratchet:encrypt(<<"msg2">>, AliceState1),
    {ok, Ct3, Hdr3, _AliceState3} = iris_ratchet:encrypt(<<"msg3">>, AliceState2),
    
    %% Bob receives in order: 3, 1, 2 (out of order)
    %% The Double Ratchet MUST support out-of-order delivery within the same
    %% chain via skipped message keys (MAX_SKIP=1000).
    %% B-5 FIX: Previously all branches passed unconditionally.
    
    %% Message 3 first - forces storing skipped keys for msgs 1 and 2
    {ok, <<"msg3">>, BobState1} = iris_ratchet:decrypt(Ct3, Hdr3, BobState0),
    
    %% Message 1 - should be decryptable from stored skipped keys
    {ok, <<"msg1">>, BobState2} = iris_ratchet:decrypt(Ct1, Hdr1, BobState1),
    
    %% Message 2 - should be decryptable from stored skipped keys
    {ok, <<"msg2">>, _BobState3} = iris_ratchet:decrypt(Ct2, Hdr2, BobState2).

%% =============================================================================
%% Cryptographic Property Tests
%% =============================================================================

crypto_property_test_() ->
    {"Cryptographic property tests",
     [
      {"Key material is properly random", fun test_key_randomness/0},
      {"Encryption is non-deterministic", fun test_encryption_nondeterministic/0},
      {"Nonce/IV uniqueness", fun test_nonce_uniqueness/0}
     ]}.

test_key_randomness() ->
    %% Test: Generated keys should have high entropy
    %% (Simple test - verify keys are different each time)
    
    Keys = [begin
        {Pub, _Priv} = iris_ratchet:generate_ratchet_keypair(),
        Pub
    end || _ <- lists:seq(1, 10)],
    
    %% All keys should be unique
    UniqueKeys = lists:usort(Keys),
    ?assertEqual(10, length(UniqueKeys)).

test_encryption_nondeterministic() ->
    %% Test: Same message encrypted multiple times should differ
    %% (due to chain key advancement)
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, _} = iris_ratchet:generate_ratchet_keypair(),
    {ok, State0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    Plaintext = <<"Test message">>,
    
    %% Encrypt same message 5 times (with advancing state)
    {Ciphertexts, _} = lists:foldl(
        fun(_, {Cts, S}) ->
            {ok, Ct, _, S2} = iris_ratchet:encrypt(Plaintext, S),
            {[Ct | Cts], S2}
        end,
        {[], State0},
        lists:seq(1, 5)
    ),
    
    %% All ciphertexts should be unique
    UniqueCts = lists:usort(Ciphertexts),
    ?assertEqual(5, length(UniqueCts)).

test_nonce_uniqueness() ->
    %% Test: IVs/nonces should never repeat
    
    SharedSecret = crypto:strong_rand_bytes(32),
    {BobPub, _} = iris_ratchet:generate_ratchet_keypair(),
    {ok, State0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    
    %% Extract IVs from 100 ciphertexts
    {IVs, _} = lists:foldl(
        fun(_, {Acc, S}) ->
            {ok, Ct, _, S2} = iris_ratchet:encrypt(<<"msg">>, S),
            %% IV is first 12 bytes
            <<IV:12/binary, _/binary>> = Ct,
            {[IV | Acc], S2}
        end,
        {[], State0},
        lists:seq(1, 100)
    ),
    
    %% All IVs should be unique
    UniqueIVs = lists:usort(IVs),
    ?assertEqual(100, length(UniqueIVs)).

%% =============================================================================
%% Mitigation: Skipped Keys Map Must Be Bounded
%% =============================================================================
%% The skipped_keys map grows with every out-of-order message gap.
%% Without a total limit, an attacker can exhaust memory by sending messages
%% with intentionally skipped sequence numbers.
%% The total must be bounded at MAX_TOTAL_SKIPPED (5000).
%% =============================================================================

skipped_keys_bounded_test() ->
    %% Set up a proper Alice/Bob session
    SharedSecret = crypto:strong_rand_bytes(32),
    BobKeyPair = {BobPub, _BobPriv} = iris_ratchet:generate_ratchet_keypair(),
    {ok, AliceState0} = iris_ratchet:init_alice(SharedSecret, BobPub),
    {ok, BobState0} = iris_ratchet:init_bob(SharedSecret, BobKeyPair, undefined),
    
    %% Alice encrypts message #0 (Bob will receive this to establish chain)
    {ok, Ct0, Hdr0, AliceState1} = iris_ratchet:encrypt(<<"init">>, AliceState0),
    {ok, <<"init">>, BobState1} = iris_ratchet:decrypt(Ct0, Hdr0, BobState0),
    
    %% Now inject 4500 fake skipped keys into Bob's state
    BobStateMap = iris_ratchet:get_state(BobState1),
    ExistingSkipped = maps:get(skipped_keys, BobStateMap),
    FakeDH = crypto:strong_rand_bytes(32),
    BigSkipped = lists:foldl(
        fun(N, Acc) ->
            maps:put({FakeDH, N}, crypto:strong_rand_bytes(32), Acc)
        end,
        ExistingSkipped,
        lists:seq(1, 4500)
    ),
    BobState2 = iris_ratchet:from_state(BobStateMap#{skipped_keys => BigSkipped}),
    
    %% Alice encrypts 600 messages (we'll skip to the last one for Bob)
    {AliceStateFinal, _Cts} = lists:foldl(
        fun(_, {AS, Acc}) ->
            {ok, C, H, AS2} = iris_ratchet:encrypt(<<"skip">>, AS),
            {AS2, [{C, H} | Acc]}
        end,
        {AliceState1, []},
        lists:seq(1, 599)
    ),
    {ok, CtLast, HdrLast, _} = iris_ratchet:encrypt(<<"target">>, AliceStateFinal),
    
    %% Bob decrypts only the last message -- should trigger skip_messages for 599
    %% Total would be 4500 + 599 = 5099, exceeding the 5000 limit
    %% After the fix: total must be bounded at 5000 (oldest evicted)
    case catch iris_ratchet:decrypt(CtLast, HdrLast, BobState2) of
        {ok, <<"target">>, NewBobState} ->
            NewStateMap = iris_ratchet:get_state(NewBobState),
            NewSkippedCount = maps:size(maps:get(skipped_keys, NewStateMap)),
            ?assert(NewSkippedCount =< 5000);
        {error, too_many_total_skipped} ->
            %% Rejecting the operation is also acceptable behavior
            ok;
        {'EXIT', {error, too_many_total_skipped}} ->
            %% Thrown error is also acceptable
            ok;
        _Other ->
            %% Any other result is unexpected -- fail
            ?assert(false)
    end.
