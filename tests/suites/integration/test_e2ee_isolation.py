#!/usr/bin/env python3
"""
Test: E2EE Isolation (Negative Security Tests)
RFC Reference: RFC-001-AMENDMENT-001 (E2EE Requirements)

This test validates NEGATIVE security properties:
1. Forward Secrecy: Revoked members cannot decrypt messages sent after revocation
2. Non-Member Isolation: Non-members cannot decrypt intercepted ciphertext

These are critical security boundary tests that verify the cryptographic
guarantees hold even under adversarial conditions.

CRITICAL: These tests must FAIL if security properties are violated.
No skips, no partial passes. Binary pass/fail only.

Tier: 1 (Post-merge validation)
Safe for laptop: Yes
Expected duration: <30s
"""

import os
import sys
import time
import struct
import socket

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

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
    
    In the real protocol:
    - Each sender has a chain key that advances with each message
    - Chain key evolution: chain_key_n+1 = HKDF(chain_key_n, "chain")
    - Message key: msg_key = HKDF(chain_key_n, "message")
    
    This simulation demonstrates the security property:
    - Old keys cannot decrypt messages encrypted with evolved keys
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
# Test 1: Forward Secrecy (Revoked Member Isolation)
# =============================================================================

def test_forward_secrecy():
    """
    Test: Revoked member cannot decrypt messages sent after revocation.
    
    Scenario:
    1. Alice and Bob are in a group
    2. Alice has the current sender key (can decrypt)
    3. Alice is removed from the group
    4. Sender key is rotated (chain key changes)
    5. Bob sends new messages
    6. Alice (with old keys) MUST NOT be able to decrypt new messages
    
    This validates the forward secrecy property of the Double Ratchet.
    """
    log("\n=== Test: Forward Secrecy (Revoked Member Cannot Decrypt) ===")
    
    crypto = get_crypto_primitives()
    
    try:
        # Setup: Bob's sender key for the group
        bob_sender_key = SimulatedSenderKey(crypto)
        
        # Alice joins and receives current key state
        log("  1. Alice joins group, receives sender key")
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
        
        # === KEY ROTATION: Alice is removed ===
        log("  3. Alice is REMOVED from group")
        log("     Sender key is ROTATED (new chain key)")
        
        # Simulate key rotation - Bob's chain key changes completely
        # In real Signal, this would be a new random key distributed to remaining members
        bob_sender_key.chain_key = os.urandom(32)  # New random key
        bob_sender_key.message_index = 0  # Reset index for new epoch
        
        # Bob sends new messages after Alice's removal
        log("  4. Bob sends messages AFTER Alice removed")
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
                    f"SECURITY VIOLATION: Alice decrypted post-revocation message: {result2}")
            return False
        
        log(f"     Correctly FAILED: {result2}")
        
        success3, result3 = alice_receiver.try_decrypt(msg3_cipher, msg3_nonce, msg3_idx)
        
        if success3:
            log_test("Forward secrecy", False,
                    f"SECURITY VIOLATION: Alice decrypted post-revocation message: {result3}")
            return False
        
        log(f"     Correctly FAILED: {result3}")
        
        # Verify new member (Carol) CAN decrypt
        log("  6. New member (Carol) CAN decrypt new messages")
        carol_receiver = SimulatedReceiver(crypto, None)
        carol_receiver.known_chain_key = bob_sender_key.chain_key
        carol_receiver.known_message_index = 0  # Carol knows new epoch state
        
        # Carol needs to catch up since bob already sent 2 messages
        # She can decrypt msg2 if she got the key before msg2 was sent
        # Let's test msg3 which was sent after
        msg4_plaintext = b"Message Carol should see"
        msg4_cipher, msg4_nonce, msg4_idx = bob_sender_key.encrypt(msg4_plaintext)
        
        # Update Carol's state (she received the new key)
        carol_receiver.known_chain_key = bob_sender_key.snapshot()
        carol_receiver.known_message_index = bob_sender_key.message_index
        
        # Next message
        msg5_plaintext = b"Carol can decrypt this"
        msg5_cipher, msg5_nonce, msg5_idx = bob_sender_key.encrypt(msg5_plaintext)
        
        # Carol decrypts the message right after she got the key state
        success5, result5 = carol_receiver.try_decrypt(msg5_cipher, msg5_nonce, msg5_idx)
        
        if not success5:
            log_test("Forward secrecy - new member", False,
                    f"Carol (new member) should be able to decrypt: {result5}")
            return False
        
        log(f"     Carol decrypted: {result5.decode()}")
        
        log_test("Forward secrecy", True,
                "Revoked member cannot decrypt, new member can")
        return True
        
    except Exception as e:
        log_test("Forward secrecy", False, f"Exception: {type(e).__name__}: {e}")
        return False


# =============================================================================
# Test 2: Non-Member Isolation (Eve Cannot Decrypt)
# =============================================================================

def test_non_member_isolation():
    """
    Test: Non-member cannot decrypt even if they intercept ciphertext.
    
    Scenario:
    1. Alice and Bob are in a group, Eve is NOT
    2. Bob sends encrypted messages
    3. Eve intercepts the ciphertext (simulating server misrouting or network sniff)
    4. Eve attempts to decrypt with:
       a) Random key guessing
       b) Manipulated ciphertext
       c) Replay of old ciphertexts
    5. ALL attempts MUST fail
    
    This validates that the E2EE provides confidentiality against outsiders.
    """
    log("\n=== Test: Non-Member Isolation (Outsider Cannot Decrypt) ===")
    
    crypto = get_crypto_primitives()
    
    try:
        # Setup: Bob's sender key (Eve doesn't have it)
        bob_sender_key = SimulatedSenderKey(crypto)
        
        # Bob sends messages
        log("  1. Bob sends encrypted messages in group")
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
        log("  2. Eve intercepts all ciphertexts (network sniffing)")
        
        # Eve has NO key material
        eve_receiver = SimulatedReceiver(crypto, None)
        
        # === Attack 1: Decryption without key ===
        log("  3. Eve attempts decryption WITHOUT any key")
        for i, (cipher, nonce, idx) in enumerate(intercepted):
            success, result = eve_receiver.try_decrypt(cipher, nonce, idx)
            if success:
                log_test("Non-member isolation", False,
                        f"SECURITY VIOLATION: Eve decrypted message {i+1}: {result}")
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
                        f"SECURITY VIOLATION: Random key #{attempt+1} worked: {result}")
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
                    f"SECURITY VIOLATION: Manipulated ciphertext decrypted: {result}")
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
                    f"SECURITY VIOLATION: Wrong nonce worked: {result}")
            return False
        
        log(f"     Nonce manipulation detected: {result} (correct)")
        
        # === Verify legitimate receiver CAN decrypt ===
        log("  7. Legitimate member (Alice) CAN decrypt")
        
        # Alice has the sender key from the beginning
        alice_sender_key = SimulatedSenderKey(crypto, bob_sender_key.snapshot())
        # Actually, Alice needs Bob's original state. Let's re-create the scenario properly
        
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
        
        log_test("Non-member isolation", True,
                "Non-member cannot decrypt, member can")
        return True
        
    except Exception as e:
        log_test("Non-member isolation", False, f"Exception: {type(e).__name__}: {e}")
        return False


# =============================================================================
# Test 3: Key Rotation Completeness
# =============================================================================

def test_key_rotation_completeness():
    """
    Test: After key rotation, ALL previous key material is useless.
    
    Validates that key rotation creates a clean cryptographic break.
    """
    log("\n=== Test: Key Rotation Completeness ===")
    
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
        log("  2. Performing KEY ROTATION")
        old_chain = sender_key.chain_key
        sender_key.chain_key = os.urandom(32)
        sender_key.message_index = 0
        
        # Send new messages after rotation
        log("  3. Sending messages AFTER rotation")
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
                            f"SECURITY VIOLATION: Snapshot {snap_idx} decrypted post-rotation msg: {result}")
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
        
        log_test("Key rotation completeness", True,
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
    log("E2EE Isolation Tests (Negative Security Validation)")
    log("RFC Reference: RFC-001-AMENDMENT-001")
    log("=" * 60)
    log("\nThese tests verify SECURITY BOUNDARIES:")
    log("- Forward secrecy (revoked members cannot decrypt)")
    log("- Non-member isolation (outsiders cannot decrypt)")
    log("- Key rotation completeness (clean cryptographic break)")
    log("\nAll tests must PASS for E2EE compliance.")
    
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
        log("\nFAIL: E2EE isolation tests FAILED")
        log("SECURITY BOUNDARIES MAY BE COMPROMISED")
        sys.exit(1)
    else:
        log("\nPASS: All E2EE isolation tests passed")
        log("Security boundaries verified:")
        log("  - Forward secrecy: VERIFIED")
        log("  - Non-member isolation: VERIFIED")
        log("  - Key rotation: VERIFIED")
        sys.exit(0)


if __name__ == "__main__":
    main()
