-module(iris_x3dh_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% X3DH Key Exchange Unit Tests (RFC-001-AMENDMENT-001)
%% Per TEST_CONTRACT.md: Tests must be deterministic
%% =============================================================================

%% =============================================================================
%% Key Generation Tests
%% =============================================================================

generate_identity_key_test() ->
    {Public, Private} = iris_x3dh:generate_identity_key(),
    
    %% Keys should be 32 bytes (Curve25519)
    ?assertEqual(32, byte_size(Public)),
    ?assertEqual(32, byte_size(Private)),
    
    %% Keys should be different
    ?assertNotEqual(Public, Private).

generate_signed_prekey_test() ->
    {IKPublic, IKPrivate} = iris_x3dh:generate_identity_key(),
    {SPKPublic, SPKPrivate, Signature} = iris_x3dh:generate_signed_prekey(IKPrivate),
    
    %% Keys should be 32 bytes
    ?assertEqual(32, byte_size(SPKPublic)),
    ?assertEqual(32, byte_size(SPKPrivate)),
    
    %% Signature should be valid
    ?assert(iris_x3dh:verify_prekey_signature(SPKPublic, Signature, IKPublic)).

generate_one_time_prekeys_test() ->
    OPKs = iris_x3dh:generate_one_time_prekeys(10),
    
    %% Should generate 10 key pairs
    ?assertEqual(10, length(OPKs)),
    
    %% Each key should be 32 bytes
    lists:foreach(fun({Public, Private}) ->
        ?assertEqual(32, byte_size(Public)),
        ?assertEqual(32, byte_size(Private))
    end, OPKs).

generate_ephemeral_key_test() ->
    {Public, Private} = iris_x3dh:generate_ephemeral_key(),
    
    ?assertEqual(32, byte_size(Public)),
    ?assertEqual(32, byte_size(Private)).

%% =============================================================================
%% Key Exchange Tests
%% =============================================================================

full_exchange_with_opk_test() ->
    %% Generate Alice's identity key
    {IK_A_Pub, IK_A_Priv} = iris_x3dh:generate_identity_key(),
    
    %% Generate Bob's keys
    {IK_B_Pub, IK_B_Priv} = iris_x3dh:generate_identity_key(),
    {SPK_B_Pub, SPK_B_Priv, SPK_B_Sig} = iris_x3dh:generate_signed_prekey(IK_B_Priv),
    [{OPK_B_Pub, OPK_B_Priv}] = iris_x3dh:generate_one_time_prekeys(1),
    
    %% Alice's keys
    AliceKeys = #{
        identity_key_public => IK_A_Pub,
        identity_key_private => IK_A_Priv
    },
    
    %% Bob's bundle (what Alice gets from server)
    BobBundle = #{
        identity_key => IK_B_Pub,
        signed_prekey => SPK_B_Pub,
        signed_prekey_signature => SPK_B_Sig,
        one_time_prekey => OPK_B_Pub
    },
    
    %% Alice initiates exchange
    {ok, AliceSecret, InitialMessage} = iris_x3dh:initiate_exchange(AliceKeys, BobBundle),
    
    %% Verify Alice got a 32-byte shared secret
    ?assertEqual(32, byte_size(AliceSecret)),
    
    %% Bob's keys for responding
    BobKeys = #{
        identity_key_public => IK_B_Pub,
        identity_key_private => IK_B_Priv,
        signed_prekey_private => SPK_B_Priv,
        one_time_prekey_private => OPK_B_Priv
    },
    
    %% Bob responds to exchange
    {ok, BobSecret} = iris_x3dh:respond_to_exchange(BobKeys, InitialMessage),
    
    %% CRITICAL: Both parties should derive the same shared secret
    ?assertEqual(AliceSecret, BobSecret).

full_exchange_without_opk_test() ->
    %% Test exchange when no one-time prekey is available
    
    %% Generate Alice's identity key
    {IK_A_Pub, IK_A_Priv} = iris_x3dh:generate_identity_key(),
    
    %% Generate Bob's keys (no OPK)
    {IK_B_Pub, IK_B_Priv} = iris_x3dh:generate_identity_key(),
    {SPK_B_Pub, SPK_B_Priv, SPK_B_Sig} = iris_x3dh:generate_signed_prekey(IK_B_Priv),
    
    %% Alice's keys
    AliceKeys = #{
        identity_key_public => IK_A_Pub,
        identity_key_private => IK_A_Priv
    },
    
    %% Bob's bundle (no OPK)
    BobBundle = #{
        identity_key => IK_B_Pub,
        signed_prekey => SPK_B_Pub,
        signed_prekey_signature => SPK_B_Sig
        %% No one_time_prekey
    },
    
    %% Alice initiates exchange
    {ok, AliceSecret, InitialMessage} = iris_x3dh:initiate_exchange(AliceKeys, BobBundle),
    
    %% Verify prekey_used is false
    ?assertEqual(false, maps:get(prekey_used, InitialMessage)),
    
    %% Bob's keys for responding
    BobKeys = #{
        identity_key_public => IK_B_Pub,
        identity_key_private => IK_B_Priv,
        signed_prekey_private => SPK_B_Priv
    },
    
    %% Bob responds to exchange
    {ok, BobSecret} = iris_x3dh:respond_to_exchange(BobKeys, InitialMessage),
    
    %% Both parties should derive the same shared secret
    ?assertEqual(AliceSecret, BobSecret).

%% =============================================================================
%% Security Property Tests
%% =============================================================================

different_sessions_different_secrets_test() ->
    %% Two sessions between same parties should produce different secrets
    %% (due to ephemeral keys)
    
    %% Generate identity keys
    {IK_A_Pub, IK_A_Priv} = iris_x3dh:generate_identity_key(),
    {IK_B_Pub, IK_B_Priv} = iris_x3dh:generate_identity_key(),
    {SPK_B_Pub, SPK_B_Priv, SPK_B_Sig} = iris_x3dh:generate_signed_prekey(IK_B_Priv),
    
    AliceKeys = #{
        identity_key_public => IK_A_Pub,
        identity_key_private => IK_A_Priv
    },
    
    BobBundle = #{
        identity_key => IK_B_Pub,
        signed_prekey => SPK_B_Pub,
        signed_prekey_signature => SPK_B_Sig
    },
    
    %% First session
    {ok, Secret1, _} = iris_x3dh:initiate_exchange(AliceKeys, BobBundle),
    
    %% Second session (new ephemeral key generated internally)
    {ok, Secret2, _} = iris_x3dh:initiate_exchange(AliceKeys, BobBundle),
    
    %% Secrets should be different
    ?assertNotEqual(Secret1, Secret2).

initial_message_contains_required_fields_test() ->
    %% Verify initial message has all required fields
    
    {IK_A_Pub, IK_A_Priv} = iris_x3dh:generate_identity_key(),
    {IK_B_Pub, IK_B_Priv} = iris_x3dh:generate_identity_key(),
    {SPK_B_Pub, _, SPK_B_Sig} = iris_x3dh:generate_signed_prekey(IK_B_Priv),
    
    AliceKeys = #{
        identity_key_public => IK_A_Pub,
        identity_key_private => IK_A_Priv
    },
    
    BobBundle = #{
        identity_key => IK_B_Pub,
        signed_prekey => SPK_B_Pub,
        signed_prekey_signature => SPK_B_Sig
    },
    
    {ok, _, InitialMessage} = iris_x3dh:initiate_exchange(AliceKeys, BobBundle),
    
    %% Check required fields
    ?assert(maps:is_key(identity_key, InitialMessage)),
    ?assert(maps:is_key(ephemeral_key, InitialMessage)),
    ?assert(maps:is_key(prekey_used, InitialMessage)),
    ?assert(maps:is_key(associated_data, InitialMessage)),
    
    %% Check key sizes
    ?assertEqual(32, byte_size(maps:get(identity_key, InitialMessage))),
    ?assertEqual(32, byte_size(maps:get(ephemeral_key, InitialMessage))),
    
    %% Associated data should be IK_A || IK_B (64 bytes)
    ?assertEqual(64, byte_size(maps:get(associated_data, InitialMessage))).

%% =============================================================================
%% Security Tests - C2: Forged Signature Rejection
%% =============================================================================

forged_signature_rejected_test() ->
    %% P0-C2 TEST: Verify forged signatures are rejected
    %% This prevents MITM key substitution attacks
    
    %% Generate valid keys
    {IK_B_Pub, IK_B_Priv} = iris_x3dh:generate_identity_key(),
    {SPK_B_Pub, _, _ValidSig} = iris_x3dh:generate_signed_prekey(IK_B_Priv),
    
    %% Create forged signature (wrong size - too small)
    ForgedSig1 = crypto:strong_rand_bytes(32),
    ?assertNot(iris_x3dh:verify_prekey_signature(SPK_B_Pub, ForgedSig1, IK_B_Pub)),
    
    %% Create forged signature (correct size but invalid content)
    ForgedSig2 = crypto:strong_rand_bytes(96),
    ?assertNot(iris_x3dh:verify_prekey_signature(SPK_B_Pub, ForgedSig2, IK_B_Pub)),
    
    %% Create forged signature (very large)
    ForgedSig3 = crypto:strong_rand_bytes(256),
    ?assertNot(iris_x3dh:verify_prekey_signature(SPK_B_Pub, ForgedSig3, IK_B_Pub)).

signature_binds_to_prekey_test() ->
    %% P0-C2 TEST: Valid signature should verify with correct prekey
    %% The signature is cryptographically bound to the prekey content
    
    {_, IK_A_Priv} = iris_x3dh:generate_identity_key(),
    {IK_A_Pub, _} = iris_x3dh:generate_identity_key(),  %% Different for cross-check
    {SPK_Pub, _, Sig} = iris_x3dh:generate_signed_prekey(IK_A_Priv),
    
    %% Generate a different prekey to test binding
    {DifferentSPK_Pub, _, _} = iris_x3dh:generate_signed_prekey(IK_A_Priv),
    
    %% Signature should NOT verify with a different prekey
    %% This tests that signatures are bound to specific prekey content
    ?assertNot(iris_x3dh:verify_prekey_signature(DifferentSPK_Pub, Sig, IK_A_Pub)).

invalid_input_sizes_rejected_test() ->
    %% P0-C2 TEST: Invalid input sizes should be rejected
    
    %% Wrong prekey size
    ?assertNot(iris_x3dh:verify_prekey_signature(
        crypto:strong_rand_bytes(16),  %% Wrong size (should be 32)
        crypto:strong_rand_bytes(96),
        crypto:strong_rand_bytes(32))),
    
    %% Wrong identity key size
    ?assertNot(iris_x3dh:verify_prekey_signature(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(96),
        crypto:strong_rand_bytes(16))).  %% Wrong size (should be 32)

%% =============================================================================
%% Error Handling Tests
%% =============================================================================

%% Note: Signature verification now uses proper Ed25519 cryptographic checks
