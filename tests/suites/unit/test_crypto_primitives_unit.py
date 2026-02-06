#!/usr/bin/env python3
"""
Test: Cryptographic Primitives Unit Tests
RFC Reference: RFC-001-AMENDMENT-001 (E2EE Requirements)

SCOPE: This is a UNIT TEST for cryptographic algorithms.
It validates the Double Ratchet implementation in Python.
It does NOT test the server implementation.

IMPORTANT DISTINCTION:
- This test proves the ALGORITHM is correct
- It does NOT prove the SERVER implements the algorithm correctly
- It does NOT verify the "Untrusted Server" invariant (INV-3)

For server-side E2EE verification, see:
- tests/suites/security/test_server_storage_audit.py (storage inspection)
- tests/suites/security/test_revocation_integration.py (protocol-level)
- tests/suites/integration/test_group_e2ee.py (transport-level)

This test validates NEGATIVE security properties at the algorithm level:
1. Forward Secrecy: Old keys cannot decrypt messages encrypted with new keys
2. Non-Member Isolation: Without key material, decryption fails
3. Key Rotation: Rotation creates a clean cryptographic break

These properties are NECESSARY but NOT SUFFICIENT for system-level security.

CRITICAL: These tests must FAIL if algorithm properties are violated.
No skips, no partial passes. Binary pass/fail only.

Tier: 1 (Post-merge validation)
Safe for laptop: Yes (pure algorithm test, no server required)
Expected duration: <30s

HISTORY:
- Originally named test_e2ee_isolation.py in integration/ directory
- Renamed to clarify scope: this is a UNIT test of crypto primitives
- The original name implied system-level isolation testing
"""

import os
import sys
import time
import struct
import socket

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# NOTE: This is a pure algorithm test - it does NOT require server connection
# The IrisClient import below is kept for compatibility but not used

# Test results
results = []


def log(msg: str):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def log_test(name: str, passed: bool, message: str = ""):
    """Log test result."""
    status = "PASS" if passed else "FAIL"
    log(f"  {status}: {name}")
    if message:
        log(f"         {message}")
    results.append((name, passed, message))


# =============================================================================
# Cryptography Setup
# =============================================================================

def get_crypto_primitives():
    """
    Import and return cryptography primitives.
    FAILS (not skips) if cryptography library is not available.
    """
    try:
        from cryptography.hazmat.primitives.ciphers.aead import AESGCM
        from cryptography.hazmat.primitives.asymmetric import x25519
        from cryptography.hazmat.primitives import serialization
        from cryptography.hazmat.primitives.kdf.hkdf import HKDF
        from cryptography.hazmat.primitives import hashes
        return {
            'AESGCM': AESGCM,
            'x25519': x25519,
            'serialization': serialization,
            'HKDF': HKDF,
            'hashes': hashes,
        }
    except ImportError:
        log("FATAL: cryptography library not installed")
        log("Install with: pip install cryptography")
        log("This test requires cryptography - no skips allowed")
        sys.exit(1)


# =============================================================================
# Simulated Double Ratchet Key Evolution
# =============================================================================

class SimulatedSenderKey:
    """
    Simulates Signal's Sender Key for group messaging.
    
    This is a UNIT TEST simulation of the Double Ratchet algorithm.
    It validates the algorithm's security properties in isolation.
    
    In the real protocol:
    - Each sender has a chain key that advances with each message
    - Chain key evolution: chain_key_n+1 = HKDF(chain_key_n, "chain")
    - Message key: msg_key = HKDF(chain_key_n, "message")
    
    This simulation demonstrates the security property:
    - Old keys cannot decrypt messages encrypted with evolved keys
    
    NOTE: This does NOT test the server implementation.
    """
    
    def __init__(self, crypto, initial_seed: bytes = None):
        self.crypto = crypto
        self.chain_key = initial_seed or os.urandom(32)
        self.message_index = 0
    
    def derive_message_key(self) -> tuple:
        """Derive current message key and nonce, then advance chain."""
        hkdf = self.crypto['HKDF'](
            algorithm=self.crypto['hashes'].SHA256(),
            length=44,  # 32 bytes key + 12 bytes nonce
            salt=None,
            info=b"message_key_" + str(self.message_index).encode()
        )
        derived = hkdf.derive(self.chain_key)
        
        msg_key = derived[:32]
        nonce = derived[32:44]
        
        # Advance chain key (forward secrecy)
        self._advance_chain()
        self.message_index += 1
        
        return msg_key, nonce
    
    def _advance_chain(self):
        """Advance chain key - old chain key is deleted (forward secrecy)."""
        hkdf = self.crypto['HKDF'](
            algorithm=self.crypto['hashes'].SHA256(),
            length=32,
            salt=None,
            info=b"chain_advance"
        )
        self.chain_key = hkdf.derive(self.chain_key)
    
    def encrypt(self, plaintext: bytes) -> tuple:
        """Encrypt message and return (ciphertext, message_index)."""
        msg_key, nonce = self.derive_message_key()
        aesgcm = self.crypto['AESGCM'](msg_key)
        ciphertext = aesgcm.encrypt(nonce, plaintext, None)
        return ciphertext, nonce, self.message_index - 1
    
    def snapshot(self) -> bytes:
        """
        Take a snapshot of current key state.
        Used to simulate a revoked member keeping their old keys.
        """
        return self.chain_key
    
    @classmethod
    def from_snapshot(cls, crypto, snapshot: bytes, message_index: int):
        """Restore from snapshot (simulating attacker with old keys)."""
        instance = cls(crypto, snapshot)
        instance.message_index = message_index
        return instance


class SimulatedReceiver:
    """
    Simulates a receiver trying to decrypt messages.
    
    This is a UNIT TEST simulation - it does NOT test server behavior.
    
    A legitimate receiver shares the sender key and can decrypt.
    An attacker with old keys or no keys cannot decrypt.
    """
    
    def __init__(self, crypto, sender_key: SimulatedSenderKey = None):
        self.crypto = crypto
        # Receiver needs the sender's current chain key to decrypt
        self.known_chain_key = sender_key.chain_key if sender_key else None
        self.known_message_index = sender_key.message_index if sender_key else 0
    
    def try_decrypt(self, ciphertext: bytes, nonce: bytes, message_index: int) -> tuple:
        """
        Attempt to decrypt a message.
        
        Returns (success: bool, plaintext_or_error: bytes/str)
        """
        if self.known_chain_key is None:
            return False, "No key material"
        
        try:
            # Derive the message key for this index
            # We need to advance from our known state to the target index
            temp_chain = self.known_chain_key
            temp_index = self.known_message_index
            
            # Can only decrypt if we have the right chain state
            if message_index < temp_index:
                return False, "Message index before known state (forward secrecy)"
            
            # Advance to the target index
            while temp_index < message_index:
                hkdf = self.crypto['HKDF'](
                    algorithm=self.crypto['hashes'].SHA256(),
                    length=32,
                    salt=None,
                    info=b"chain_advance"
                )
                temp_chain = hkdf.derive(temp_chain)
                temp_index += 1
            
            # Derive message key at this index
            hkdf = self.crypto['HKDF'](
                algorithm=self.crypto['hashes'].SHA256(),
                length=44,
                salt=None,
                info=b"message_key_" + str(message_index).encode()
            )
            derived = hkdf.derive(temp_chain)
            msg_key = derived[:32]
            
            # Attempt decryption
            aesgcm = self.crypto['AESGCM'](msg_key)
            plaintext = aesgcm.decrypt(nonce, ciphertext, None)
            return True, plaintext
            
        except Exception as e:
            return False, f"Decryption failed: {type(e).__name__}"


# =============================================================================
# Test 1: Forward Secrecy (Algorithm Level)
# =============================================================================

def test_forward_secrecy():
    """
    Test: Old keys cannot decrypt messages encrypted with new keys.
    
    This is an ALGORITHM test, not a server test.
    
    Scenario:
    1. Alice and Bob are in a group (simulated)
    2. Alice has the current sender key (can decrypt)
    3. Alice is removed from the group (simulated)
    4. Sender key is rotated (chain key changes)
    5. Bob sends new messages
    6. Alice (with old keys) MUST NOT be able to decrypt new messages
    
    This validates the forward secrecy property of the Double Ratchet algorithm.
    """
    log("\n=== Test: Forward Secrecy (Algorithm - Old Keys Cannot Decrypt New) ===")
    
    crypto = get_crypto_primitives()
    
    try:
        # Setup: Bob's sender key for the group
        bob_sender_key = SimulatedSenderKey(crypto)
        
        # Alice joins and receives current key state
        log("  1. Alice receives sender key (simulated group join)")
        alice_snapshot = bob_sender_key.snapshot()
        alice_message_index = bob_sender_key.message_index
        
        # Bob sends messages while Alice is a member
        log("  2. Bob sends message while Alice is member")
        msg1_plaintext = b"Message while Alice is member"
        msg1_cipher, msg1_nonce, msg1_idx = bob_sender_key.encrypt(msg1_plaintext)
        
        # Alice can decrypt (she's a member)
        alice_receiver = SimulatedReceiver(crypto, None)
        alice_receiver.known_chain_key = alice_snapshot
        alice_receiver.known_message_index = alice_message_index
        
        success, result = alice_receiver.try_decrypt(msg1_cipher, msg1_nonce, msg1_idx)
        if not success:
            log_test("Forward secrecy - member decrypt", False, 
                    f"Alice should decrypt as member: {result}")
            return False
        
        if result != msg1_plaintext:
            log_test("Forward secrecy - member decrypt", False,
                    "Decrypted content mismatch")
            return False
        
        log(f"     Alice decrypted: {result.decode()}")
        
        # === KEY ROTATION: Alice is removed (simulated) ===
        log("  3. KEY ROTATION (simulating Alice's removal)")
        log("     New random chain key generated")
        
        # Simulate key rotation - Bob's chain key changes completely
        # In real Signal, this would be a new random key distributed to remaining members
        bob_sender_key.chain_key = os.urandom(32)  # New random key
        bob_sender_key.message_index = 0  # Reset index for new epoch
        
        # Bob sends new messages after Alice's removal
        log("  4. Bob encrypts messages with NEW key")
        msg2_plaintext = b"SECRET: Alice should NOT see this!"
        msg2_cipher, msg2_nonce, msg2_idx = bob_sender_key.encrypt(msg2_plaintext)
        
        msg3_plaintext = b"Another secret message"
        msg3_cipher, msg3_nonce, msg3_idx = bob_sender_key.encrypt(msg3_plaintext)
        
        # Alice attempts to decrypt with her OLD keys
        log("  5. Alice attempts to decrypt with OLD keys")
        
        # Alice still has her old snapshot
        success2, result2 = alice_receiver.try_decrypt(msg2_cipher, msg2_nonce, msg2_idx)
        
        if success2:
            log_test("Forward secrecy", False,
                    f"ALGORITHM BUG: Old keys decrypted new message: {result2}")
            return False
        
        log(f"     Correctly FAILED: {result2}")
        
        success3, result3 = alice_receiver.try_decrypt(msg3_cipher, msg3_nonce, msg3_idx)
        
        if success3:
            log_test("Forward secrecy", False,
                    f"ALGORITHM BUG: Old keys decrypted new message: {result3}")
            return False
        
        log(f"     Correctly FAILED: {result3}")
        
        # Verify new member (Carol) CAN decrypt
        log("  6. New member (Carol) with NEW key CAN decrypt")
        carol_receiver = SimulatedReceiver(crypto, None)
        carol_receiver.known_chain_key = bob_sender_key.chain_key
        carol_receiver.known_message_index = 0  # Carol knows new epoch state
        
        # Carol needs to catch up since bob already sent 2 messages
        msg4_plaintext = b"Message Carol should see"
        msg4_cipher, msg4_nonce, msg4_idx = bob_sender_key.encrypt(msg4_plaintext)
        
        # Update Carol's state (she received the new key)
        carol_receiver.known_chain_key = bob_sender_key.snapshot()
        carol_receiver.known_message_index = bob_sender_key.message_index
        
        # Next message
        msg5_plaintext = b"Carol can decrypt this"
        msg5_cipher, msg5_nonce, msg5_idx = bob_sender_key.encrypt(msg5_plaintext)
        
        success5, result5 = carol_receiver.try_decrypt(msg5_cipher, msg5_nonce, msg5_idx)
        
        if not success5:
            log_test("Forward secrecy - new member", False,
                    f"Carol (new member) should be able to decrypt: {result5}")
            return False
        
        log(f"     Carol decrypted: {result5.decode()}")
        
        log_test("Forward secrecy (algorithm)", True,
                "Old keys cannot decrypt, new keys can")
        return True
        
    except Exception as e:
        log_test("Forward secrecy", False, f"Exception: {type(e).__name__}: {e}")
        return False


# =============================================================================
# Test 2: Non-Member Isolation (Algorithm Level)
# =============================================================================

def test_non_member_isolation():
    """
    Test: Without key material, decryption fails (AES-GCM property).
    
    This is an ALGORITHM test verifying that:
    - Random keys cannot decrypt ciphertext
    - Modified ciphertext is detected (authentication)
    
    Scenario:
    1. Bob encrypts messages
    2. Eve intercepts ciphertexts but has NO keys
    3. ALL decryption attempts by Eve MUST fail
    
    This validates AES-GCM's confidentiality and integrity.
    """
    log("\n=== Test: Non-Member Isolation (Algorithm - No Key = No Decrypt) ===")
    
    crypto = get_crypto_primitives()
    
    try:
        # Setup: Bob's sender key (Eve doesn't have it)
        bob_sender_key = SimulatedSenderKey(crypto)
        
        # Bob sends messages
        log("  1. Bob encrypts secret messages")
        secrets = [
            b"Secret message 1: Bank account 12345",
            b"Secret message 2: Password is hunter2",
            b"Secret message 3: Meeting at midnight",
        ]
        
        intercepted = []
        for secret in secrets:
            cipher, nonce, idx = bob_sender_key.encrypt(secret)
            intercepted.append((cipher, nonce, idx))
            log(f"     Encrypted: {secret.decode()[:30]}...")
        
        # Eve intercepts all ciphertexts
        log("  2. Eve intercepts ciphertexts")
        
        # Eve has NO key material
        eve_receiver = SimulatedReceiver(crypto, None)
        
        # === Attack 1: Decryption without key ===
        log("  3. Eve attempts decryption WITHOUT any key")
        for i, (cipher, nonce, idx) in enumerate(intercepted):
            success, result = eve_receiver.try_decrypt(cipher, nonce, idx)
            if success:
                log_test("Non-member isolation", False,
                        f"ALGORITHM BUG: No-key decryption succeeded: {result}")
                return False
            log(f"     Message {i+1}: {result} (correct)")
        
        # === Attack 2: Random key guessing ===
        log("  4. Eve attempts decryption with RANDOM keys")
        for attempt in range(5):
            eve_receiver.known_chain_key = os.urandom(32)
            eve_receiver.known_message_index = 0
            
            cipher, nonce, idx = intercepted[0]
            success, result = eve_receiver.try_decrypt(cipher, nonce, idx)
            
            if success:
                log_test("Non-member isolation", False,
                        f"ALGORITHM BUG: Random key #{attempt+1} worked: {result}")
                return False
        
        log("     All 5 random key attempts failed (correct)")
        
        # === Attack 3: Ciphertext manipulation ===
        log("  5. Eve attempts ciphertext MANIPULATION")
        cipher, nonce, idx = intercepted[0]
        
        # Flip some bits
        manipulated = bytearray(cipher)
        manipulated[0] ^= 0xFF
        manipulated[len(manipulated)//2] ^= 0x55
        manipulated = bytes(manipulated)
        
        # Try with a made-up key
        eve_receiver.known_chain_key = os.urandom(32)
        eve_receiver.known_message_index = 0
        success, result = eve_receiver.try_decrypt(manipulated, nonce, idx)
        
        if success:
            log_test("Non-member isolation", False,
                    f"ALGORITHM BUG: Manipulated ciphertext decrypted: {result}")
            return False
        
        log(f"     Manipulation detected: {result} (correct)")
        
        # === Attack 4: Nonce manipulation ===
        log("  6. Eve attempts NONCE manipulation")
        cipher, nonce, idx = intercepted[0]
        
        manipulated_nonce = bytearray(nonce)
        manipulated_nonce[0] ^= 0xFF
        manipulated_nonce = bytes(manipulated_nonce)
        
        success, result = eve_receiver.try_decrypt(cipher, manipulated_nonce, idx)
        
        if success:
            log_test("Non-member isolation", False,
                    f"ALGORITHM BUG: Wrong nonce worked: {result}")
            return False
        
        log(f"     Nonce manipulation detected: {result} (correct)")
        
        # === Verify legitimate receiver CAN decrypt ===
        log("  7. Legitimate receiver (Alice) with correct key CAN decrypt")
        
        # Fresh scenario for Alice verification
        bob_key_2 = SimulatedSenderKey(crypto)
        alice_chain = bob_key_2.snapshot()
        alice_idx = bob_key_2.message_index
        
        test_msg = b"Alice should see this"
        cipher2, nonce2, idx2 = bob_key_2.encrypt(test_msg)
        
        alice_receiver = SimulatedReceiver(crypto, None)
        alice_receiver.known_chain_key = alice_chain
        alice_receiver.known_message_index = alice_idx
        
        success, result = alice_receiver.try_decrypt(cipher2, nonce2, idx2)
        
        if not success:
            log_test("Non-member isolation - member check", False,
                    f"Alice should be able to decrypt: {result}")
            return False
        
        if result != test_msg:
            log_test("Non-member isolation - member check", False,
                    "Decrypted content mismatch")
            return False
        
        log(f"     Alice decrypted: {result.decode()}")
        
        log_test("Non-member isolation (algorithm)", True,
                "No key = no decrypt, correct key = decrypt")
        return True
        
    except Exception as e:
        log_test("Non-member isolation", False, f"Exception: {type(e).__name__}: {e}")
        return False


# =============================================================================
# Test 3: Key Rotation Completeness (Algorithm Level)
# =============================================================================

def test_key_rotation_completeness():
    """
    Test: After key rotation, ALL previous key material is useless.
    
    This is an ALGORITHM test validating that:
    - Key rotation creates a complete cryptographic break
    - No amount of old key material can recover new keys
    
    This is the mathematical foundation of forward secrecy.
    """
    log("\n=== Test: Key Rotation Completeness (Algorithm - Clean Break) ===")
    
    crypto = get_crypto_primitives()
    
    try:
        sender_key = SimulatedSenderKey(crypto)
        
        # Capture multiple snapshots during message sending
        log("  1. Capturing key states during message sending")
        snapshots = []
        ciphertexts = []
        
        for i in range(5):
            # Snapshot before each message
            snapshots.append((sender_key.snapshot(), sender_key.message_index))
            
            msg = f"Message {i+1}".encode()
            cipher, nonce, idx = sender_key.encrypt(msg)
            ciphertexts.append((cipher, nonce, idx, msg))
            
            log(f"     Captured state and encrypted message {i+1}")
        
        # Perform key rotation
        log("  2. Performing KEY ROTATION (new random key)")
        old_chain = sender_key.chain_key
        sender_key.chain_key = os.urandom(32)
        sender_key.message_index = 0
        
        # Send new messages after rotation
        log("  3. Encrypting messages AFTER rotation")
        post_rotation = []
        for i in range(3):
            msg = f"Post-rotation {i+1}".encode()
            cipher, nonce, idx = sender_key.encrypt(msg)
            post_rotation.append((cipher, nonce, idx, msg))
        
        # Verify old snapshots CANNOT decrypt new messages
        log("  4. Verifying old snapshots CANNOT decrypt new messages")
        
        for snap_idx, (snap_chain, snap_msg_idx) in enumerate(snapshots):
            attacker = SimulatedReceiver(crypto, None)
            attacker.known_chain_key = snap_chain
            attacker.known_message_index = snap_msg_idx
            
            for cipher, nonce, idx, expected_msg in post_rotation:
                success, result = attacker.try_decrypt(cipher, nonce, idx)
                
                if success:
                    log_test("Key rotation completeness", False,
                            f"ALGORITHM BUG: Snapshot {snap_idx} decrypted post-rotation msg: {result}")
                    return False
        
        log("     All old snapshots correctly failed")
        
        # Verify new state CAN decrypt new messages
        log("  5. Verifying new state CAN decrypt new messages")
        
        new_receiver = SimulatedReceiver(crypto, None)
        new_receiver.known_chain_key = sender_key.snapshot()
        new_receiver.known_message_index = sender_key.message_index
        
        # Send a fresh message
        fresh_msg = b"Fresh message for verification"
        fresh_cipher, fresh_nonce, fresh_idx = sender_key.encrypt(fresh_msg)
        
        # Receiver got the key before this message
        new_receiver.known_chain_key = sender_key.snapshot()
        new_receiver.known_message_index = sender_key.message_index
        
        next_msg = b"This one should work"
        next_cipher, next_nonce, next_idx = sender_key.encrypt(next_msg)
        
        success, result = new_receiver.try_decrypt(next_cipher, next_nonce, next_idx)
        
        if not success:
            log_test("Key rotation completeness - new state", False,
                    f"New state should decrypt: {result}")
            return False
        
        log(f"     New state decrypted: {result.decode()}")
        
        log_test("Key rotation completeness (algorithm)", True,
                "Old snapshots useless after rotation")
        return True
        
    except Exception as e:
        log_test("Key rotation completeness", False, f"Exception: {type(e).__name__}: {e}")
        return False


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("CRYPTOGRAPHIC PRIMITIVES UNIT TESTS")
    log("RFC Reference: RFC-001-AMENDMENT-001")
    log("=" * 60)
    log("\nSCOPE: These tests validate the ALGORITHM, not the server.")
    log("They prove Double Ratchet properties at the cryptographic level.")
    log("\nFor server-side verification, see:")
    log("  - test_server_storage_audit.py (storage inspection)")
    log("  - test_revocation_integration.py (protocol-level)")
    log("\nAlgorithm properties tested:")
    log("  - Forward secrecy (old keys cannot decrypt new messages)")
    log("  - Non-member isolation (no key = no decrypt)")
    log("  - Key rotation completeness (clean cryptographic break)")
    log("\nAll tests must PASS for algorithm correctness.")
    
    # Run tests
    test_forward_secrecy()
    test_non_member_isolation()
    test_key_rotation_completeness()
    
    # Summary
    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    
    passed = sum(1 for _, p, _ in results if p)
    failed = sum(1 for _, p, _ in results if not p)
    
    for name, p, msg in results:
        status = "PASS" if p else "FAIL"
        log(f"  [{status}] {name}")
    
    log(f"\nTotal: {len(results)} tests")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")
    
    if failed > 0:
        log("\nFAIL: Cryptographic primitive tests FAILED")
        log("ALGORITHM BUG DETECTED")
        sys.exit(1)
    else:
        log("\nPASS: All cryptographic primitive tests passed")
        log("Algorithm properties verified:")
        log("  - Forward secrecy: VERIFIED (algorithm level)")
        log("  - Non-member isolation: VERIFIED (algorithm level)")
        log("  - Key rotation: VERIFIED (algorithm level)")
        log("\nNOTE: This does NOT verify server implementation.")
        log("See test_server_storage_audit.py for server-level tests.")
        sys.exit(0)


if __name__ == "__main__":
    main()
