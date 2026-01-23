#!/usr/bin/env python3
"""
Test: Group E2EE Integration
RFC Reference: FR-13 (Group Messaging), RFC-001-AMENDMENT-001

Validates the integration of:
1. Group membership management
2. Sender key generation and distribution
3. Group message encryption/decryption
4. Key rotation on member changes

This test uses Erlang RPC to test the complete flow.
"""

import os
import sys
import time
import subprocess

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# Test configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))


def log(msg):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def erl_eval(code: str, timeout: int = 30) -> str:
    """Execute Erlang code and return output."""
    cmd = [
        "erl", "-noshell", "-pa", "ebin",
        "-eval", code,
        "-eval", "init:stop()."
    ]
    try:
        result = subprocess.run(
            cmd, 
            capture_output=True, 
            text=True, 
            timeout=timeout,
            cwd=PROJECT_ROOT
        )
        return result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return "[TIMEOUT]"
    except Exception as e:
        return f"[ERROR] {e}"


def check_modules_compiled() -> bool:
    """Check if required modules are compiled."""
    required = ["iris_group.beam", "iris_sender_keys.beam"]
    for module in required:
        if not os.path.exists(os.path.join(PROJECT_ROOT, "ebin", module)):
            log(f"Missing: {module}")
            return False
    return True


def test_sender_key_generation():
    """Test: Generate and validate sender key."""
    log("=== Test: Sender Key Generation ===")
    
    code = """
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    
    %% Get key ID
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Serialize and deserialize
    Serialized = iris_sender_keys:serialize_sender_key(SenderKey),
    Restored = iris_sender_keys:deserialize_sender_key(Serialized),
    RestoredKeyId = iris_sender_keys:get_key_id(Restored),
    
    case KeyId =:= RestoredKeyId of
        true -> io:format(\"PASS: Sender key generation works~n\");
        false -> io:format(\"FAIL: Key ID mismatch~n\")
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Sender key generation works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_group_message_encryption():
    """Test: Encrypt and decrypt a group message."""
    log("=== Test: Group Message Encryption ===")
    
    code = """
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt a message
    Plaintext = <<"Hello, group members!">>,
    {Ciphertext, _NewKey} = iris_sender_keys:encrypt_group_msg(
        Plaintext, SenderKey, KeyId
    ),
    
    %% Verify encryption happened
    Encrypted = Ciphertext =/= Plaintext,
    
    %% Decrypt the message
    case iris_sender_keys:decrypt_group_msg(Ciphertext, SenderKey, KeyId) of
        {ok, Decrypted} when Decrypted =:= Plaintext, Encrypted =:= true ->
            io:format(\"PASS: Message encrypted and decrypted correctly~n\");
        {ok, Decrypted} ->
            io:format(\"FAIL: Decrypted ~p =/= ~p~n\", [Decrypted, Plaintext]);
        {error, Reason} ->
            io:format(\"FAIL: Decryption error: ~p~n\", [Reason])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Group message encryption works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_group_with_sender_keys():
    """Test: Create group and store sender keys for members."""
    log("=== Test: Group with Sender Keys ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Clear tables
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    
    %% Create group with admin
    {ok, GroupId} = iris_group:create_group(<<"E2EE Test Group">>, <<"alice">>),
    
    %% Add members
    ok = iris_group:add_member(GroupId, <<"bob">>, <<"alice">>),
    ok = iris_group:add_member(GroupId, <<"carol">>, <<"alice">>),
    
    %% Each member generates and stores their sender key
    AliceKey = iris_sender_keys:generate_sender_key(),
    BobKey = iris_sender_keys:generate_sender_key(),
    CarolKey = iris_sender_keys:generate_sender_key(),
    
    AliceKeyId = iris_sender_keys:get_key_id(AliceKey),
    BobKeyId = iris_sender_keys:get_key_id(BobKey),
    CarolKeyId = iris_sender_keys:get_key_id(CarolKey),
    
    %% Store sender keys in group
    AliceSerialized = iris_sender_keys:serialize_sender_key(AliceKey),
    BobSerialized = iris_sender_keys:serialize_sender_key(BobKey),
    CarolSerialized = iris_sender_keys:serialize_sender_key(CarolKey),
    
    ok = iris_group:store_sender_key(GroupId, <<"alice">>, AliceKeyId, AliceSerialized),
    ok = iris_group:store_sender_key(GroupId, <<"bob">>, BobKeyId, BobSerialized),
    ok = iris_group:store_sender_key(GroupId, <<"carol">>, CarolKeyId, CarolSerialized),
    
    %% Verify keys can be retrieved
    {ok, RetrievedAlice} = iris_group:get_sender_key(GroupId, <<"alice">>, AliceKeyId),
    {ok, RetrievedBob} = iris_group:get_sender_key(GroupId, <<"bob">>, BobKeyId),
    
    %% Verify member count
    {ok, Info} = iris_group:get_group(GroupId),
    MemberCount = maps:get(member_count, Info),
    
    case {RetrievedAlice =:= AliceSerialized, RetrievedBob =:= BobSerialized, MemberCount} of
        {true, true, 3} ->
            io:format(\"PASS: Group with sender keys works~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Group with sender keys works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_group_message_flow():
    """Test: Full group message flow with encryption."""
    log("=== Test: Group Message Flow ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(<<"Flow Test">>, <<"sender">>),
    ok = iris_group:add_member(GroupId, <<"receiver">>, <<"sender">>),
    
    %% Sender creates and stores sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    SenderSerialized = iris_sender_keys:serialize_sender_key(SenderKey),
    iris_group:store_sender_key(GroupId, <<"sender">>, KeyId, SenderSerialized),
    
    %% Sender encrypts message
    Plaintext = <<"Secret group message!">>,
    {Ciphertext, _} = iris_sender_keys:encrypt_group_msg(Plaintext, SenderKey, KeyId),
    
    %% Receiver retrieves sender's key and decrypts
    {ok, RetrievedSerialized} = iris_group:get_sender_key(GroupId, <<"sender">>, KeyId),
    ReceiverSenderKey = iris_sender_keys:deserialize_sender_key(RetrievedSerialized),
    
    case iris_sender_keys:decrypt_group_msg(Ciphertext, ReceiverSenderKey, KeyId) of
        {ok, Decrypted} when Decrypted =:= Plaintext ->
            io:format(\"PASS: Full message flow works~n\");
        {ok, Wrong} ->
            io:format(\"FAIL: Wrong decryption: ~p~n\", [Wrong]);
        {error, Reason} ->
            io:format(\"FAIL: Error: ~p~n\", [Reason])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Full group message flow works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_key_rotation_on_member_leave():
    """Test: Key rotation when a member leaves the group."""
    log("=== Test: Key Rotation on Member Leave ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    
    %% Create group with members
    {ok, GroupId} = iris_group:create_group(<<"Rotation Test">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"member1">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"leaver">>, <<"admin">>),
    
    %% Each member has a sender key
    AdminKey = iris_sender_keys:generate_sender_key(),
    AdminKeyId = iris_sender_keys:get_key_id(AdminKey),
    AdminSerialized = iris_sender_keys:serialize_sender_key(AdminKey),
    iris_group:store_sender_key(GroupId, <<"admin">>, AdminKeyId, AdminSerialized),
    
    %% Leaver's key
    LeaverKey = iris_sender_keys:generate_sender_key(),
    LeaverKeyId = iris_sender_keys:get_key_id(LeaverKey),
    LeaverSerialized = iris_sender_keys:serialize_sender_key(LeaverKey),
    iris_group:store_sender_key(GroupId, <<"leaver">>, LeaverKeyId, LeaverSerialized),
    
    %% Remove leaver
    ok = iris_group:remove_member(GroupId, <<"leaver">>, <<"admin">>),
    
    %% Verify leaver is gone
    LeaverIsMember = iris_group:is_member(GroupId, <<"leaver">>),
    
    %% Admin rotates their key (forward secrecy)
    {ok, NewAdminKeyId} = iris_group:rotate_sender_key(GroupId, <<"admin">>, 
        iris_sender_keys:serialize_sender_key(iris_sender_keys:generate_sender_key())),
    
    %% Verify new key is different
    KeysDifferent = NewAdminKeyId =/= AdminKeyId,
    
    %% Verify old key still exists (for message history)
    {ok, _OldKey} = iris_group:get_sender_key(GroupId, <<"admin">>, AdminKeyId),
    
    %% Verify new key exists
    {ok, _NewKey} = iris_group:get_sender_key(GroupId, <<"admin">>, NewAdminKeyId),
    
    case {LeaverIsMember, KeysDifferent} of
        {false, true} ->
            io:format(\"PASS: Key rotation on member leave works~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Key rotation on member leave works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_signature_verification():
    """Test: Signatures prevent message tampering."""
    log("=== Test: Signature Verification ===")
    
    code = """
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt a message
    Plaintext = <<"Signed message">>,
    {Ciphertext, _} = iris_sender_keys:encrypt_group_msg(Plaintext, SenderKey, KeyId),
    
    %% Tamper with ciphertext (flip a byte at position 20)
    TamperedCiphertext = case Ciphertext of
        <<H:20/binary, B:8, T/binary>> -> <<H/binary, (B bxor 255):8, T/binary>>;
        _ -> Ciphertext
    end,
    
    %% Decryption should fail
    Result = iris_sender_keys:decrypt_group_msg(TamperedCiphertext, SenderKey, KeyId),
    
    case Result of
        {error, _} ->
            io:format(\"PASS: Signature prevents tampering~n\");
        {ok, _} ->
            io:format(\"FAIL: Tampered message was accepted~n\")
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Signature verification works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_chain_advancement():
    """Test: Chain advances correctly for multiple messages."""
    log("=== Test: Chain Advancement ===")
    
    code = """
    %% Generate sender key
    SenderKey = iris_sender_keys:generate_sender_key(),
    KeyId = iris_sender_keys:get_key_id(SenderKey),
    
    %% Encrypt multiple messages
    {C1, K1} = iris_sender_keys:encrypt_group_msg(<<"msg1">>, SenderKey, KeyId),
    {C2, K2} = iris_sender_keys:encrypt_group_msg(<<"msg2">>, K1, KeyId),
    {C3, _K3} = iris_sender_keys:encrypt_group_msg(<<"msg3">>, K2, KeyId),
    
    %% All ciphertexts should be different
    AllDifferent = (C1 =/= C2) andalso (C2 =/= C3) andalso (C1 =/= C3),
    
    %% Decrypt each message with appropriate chain state
    {ok, D1} = iris_sender_keys:decrypt_group_msg(C1, SenderKey, KeyId),
    R1 = iris_sender_keys:advance_chain(SenderKey),
    {ok, D2} = iris_sender_keys:decrypt_group_msg(C2, R1, KeyId),
    R2 = iris_sender_keys:advance_chain(R1),
    {ok, D3} = iris_sender_keys:decrypt_group_msg(C3, R2, KeyId),
    
    case {AllDifferent, D1, D2, D3} of
        {true, <<"msg1">>, <<"msg2">>, <<"msg3">>} ->
            io:format(\"PASS: Chain advances correctly~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Chain advancement works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def main():
    """Run group E2EE tests."""
    log(f"=== Group E2EE Tests (profile={TEST_PROFILE}, seed={TEST_SEED}) ===")
    
    if not check_modules_compiled():
        log("[SKIP] Required modules not compiled. Run 'make' first.")
        sys.exit(2)
    
    results = []
    
    results.append(("Sender Key Generation", test_sender_key_generation()))
    results.append(("Group Message Encryption", test_group_message_encryption()))
    results.append(("Group with Sender Keys", test_group_with_sender_keys()))
    results.append(("Group Message Flow", test_group_message_flow()))
    results.append(("Key Rotation on Leave", test_key_rotation_on_member_leave()))
    results.append(("Signature Verification", test_signature_verification()))
    results.append(("Chain Advancement", test_chain_advancement()))
    
    log("\n=== Results ===")
    passed = 0
    failed = 0
    for name, result in results:
        status = "PASS" if result else "FAIL"
        log(f"  {name}: {status}")
        if result:
            passed += 1
        else:
            failed += 1
    
    log(f"\nTotal: {passed}/{len(results)} passed")
    
    if failed > 0:
        log("[FAIL] Some tests failed")
        sys.exit(1)
    else:
        log("[PASS] All group E2EE tests passed")
        sys.exit(0)


if __name__ == "__main__":
    main()
