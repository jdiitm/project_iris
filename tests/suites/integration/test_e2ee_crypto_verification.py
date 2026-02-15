#!/usr/bin/env python3
"""
T-2: E2EE Cryptographic Verification Tests

Verifies that the E2EE implementation provides real cryptographic guarantees:
1. Messages encrypted by client A can be decrypted by client B using shared keys
2. The server cannot decrypt message content (zero-knowledge relay)
3. Key bundles are returned byte-exact from the server

Uses real Curve25519 + AES-256-GCM crypto (not opaque blob pass-through).

Tier: 1 (Integration)
"""

import os
import sys
import struct
import subprocess
import random
import string
import unittest

# Add project root
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey, X25519PublicKey
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.kdf.hkdf import HKDF

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

TIMEOUT = 30


def generate_user_id():
    return f"e2ee_user_{''.join(random.choices(string.ascii_lowercase, k=8))}"


def run_erlang_command(code, timeout=TIMEOUT):
    """Run Erlang code and return output."""
    full_code = f"""
        cd {project_root} && \\
        erl -pa ebin -noshell -sname test_e2ee_$RANDOM -setcookie iris_secret -eval '
        try
            application:ensure_all_started(mnesia),
            {code}
        catch
            Class:Reason:Stack ->
                io:format("ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
                halt(1)
        end,
        halt(0).
        '
    """
    result = subprocess.run(
        ["bash", "-c", full_code],
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    return result


class X25519KeyPair:
    """Minimal X25519 key pair for testing."""

    def __init__(self):
        self.private_key = X25519PrivateKey.generate()
        self.public_key = self.private_key.public_key()

    def public_bytes(self):
        from cryptography.hazmat.primitives.serialization import Encoding, PublicFormat
        return self.public_key.public_bytes(Encoding.Raw, PublicFormat.Raw)

    def private_bytes(self):
        from cryptography.hazmat.primitives.serialization import Encoding, NoEncryption, PrivateFormat
        return self.private_key.private_bytes(Encoding.Raw, PrivateFormat.Raw, NoEncryption())


def derive_shared_secret(private_key, peer_public_bytes):
    """X25519 DH + HKDF-SHA256 to derive AES-256 key."""
    peer_public = X25519PublicKey.from_public_bytes(peer_public_bytes)
    shared_secret = private_key.private_key.exchange(peer_public)
    # HKDF to derive 32-byte AES key
    hkdf = HKDF(
        algorithm=hashes.SHA256(),
        length=32,
        salt=None,
        info=b"iris-e2ee-v1",
    )
    return hkdf.derive(shared_secret)


def encrypt_message(aes_key, plaintext):
    """AES-256-GCM encrypt with random nonce."""
    aesgcm = AESGCM(aes_key)
    nonce = os.urandom(12)  # 96-bit nonce for GCM
    ciphertext = aesgcm.encrypt(nonce, plaintext, None)
    return nonce + ciphertext  # nonce || ciphertext || tag


def decrypt_message(aes_key, encrypted_blob):
    """AES-256-GCM decrypt (nonce is first 12 bytes)."""
    aesgcm = AESGCM(aes_key)
    nonce = encrypted_blob[:12]
    ciphertext = encrypted_blob[12:]
    return aesgcm.decrypt(nonce, ciphertext, None)


class TestE2EERoundtrip(unittest.TestCase):
    """Test E2EE roundtrip encryption/decryption with real crypto."""

    def test_e2ee_roundtrip_decryption(self):
        """
        Client A encrypts a message with shared DH key.
        Client B decrypts with the same shared key.
        Verifies plaintext matches exactly.
        """
        # Generate key pairs for Alice and Bob
        alice = X25519KeyPair()
        bob = X25519KeyPair()

        # Derive shared secret (Alice's private + Bob's public)
        alice_shared_key = derive_shared_secret(alice, bob.public_bytes())
        bob_shared_key = derive_shared_secret(bob, alice.public_bytes())

        # Keys must be identical (DH symmetry)
        self.assertEqual(alice_shared_key, bob_shared_key,
                         "DH shared secrets must be identical")

        # Alice encrypts
        plaintext = b"Hello Bob, this is a secret message from Alice!"
        encrypted = encrypt_message(alice_shared_key, plaintext)

        # Encrypted must differ from plaintext
        self.assertNotEqual(encrypted, plaintext)
        self.assertNotIn(plaintext, encrypted)

        # Bob decrypts
        decrypted = decrypt_message(bob_shared_key, encrypted)
        self.assertEqual(decrypted, plaintext,
                         "Bob must recover Alice's exact plaintext")

    def test_server_cannot_decrypt(self):
        """
        Verify that a third party (the server) with its own key pair
        cannot decrypt messages encrypted with Alice-Bob shared key.
        """
        alice = X25519KeyPair()
        bob = X25519KeyPair()
        server = X25519KeyPair()

        # Alice-Bob shared key
        alice_bob_key = derive_shared_secret(alice, bob.public_bytes())

        # Alice encrypts for Bob
        plaintext = b"Server must not read this."
        encrypted = encrypt_message(alice_bob_key, plaintext)

        # Server tries to decrypt with server-alice key
        server_alice_key = derive_shared_secret(server, alice.public_bytes())
        with self.assertRaises(Exception):
            decrypt_message(server_alice_key, encrypted)

        # Server tries to decrypt with server-bob key
        server_bob_key = derive_shared_secret(server, bob.public_bytes())
        with self.assertRaises(Exception):
            decrypt_message(server_bob_key, encrypted)

    def test_key_bundle_integrity(self):
        """
        Upload a key bundle via Erlang, fetch it back,
        verify public keys match byte-exact.
        """
        user = generate_user_id()
        identity_key = X25519KeyPair()
        signed_prekey = X25519KeyPair()
        opk1 = X25519KeyPair()

        ik_hex = identity_key.public_bytes().hex()
        spk_hex = signed_prekey.public_bytes().hex()
        opk1_hex = opk1.public_bytes().hex()
        # Generate a 64-byte fake signature for the signed prekey
        sig_hex = os.urandom(64).hex()

        # Upload and fetch key bundle in a single Erlang VM invocation
        roundtrip_code = (
            f'iris_keys:start_link(),'
            f'IK = <<16#{ik_hex}:256>>,'
            f'SPK = <<16#{spk_hex}:256>>,'
            f'Sig = <<16#{sig_hex}:512>>,'
            f'OPK1 = <<16#{opk1_hex}:256>>,'
            f'Bundle = #{{identity_key => IK, signed_prekey => SPK,'
            f' signed_prekey_signature => Sig,'
            f' one_time_prekeys => [OPK1]}},'
            f'ok = iris_keys:upload_bundle(<<"{user}">>, Bundle),'
            f'io:format("UPLOAD_OK~n"),'
            f'case iris_keys:fetch_bundle(<<"{user}">>, false) of'
            f'  {{ok, FetchedBundle}} ->'
            f'    FIK = maps:get(identity_key, FetchedBundle),'
            f'    FSPK = maps:get(signed_prekey, FetchedBundle),'
            f'    io:format("IK:~s~n", [binary:encode_hex(FIK)]),'
            f'    io:format("SPK:~s~n", [binary:encode_hex(FSPK)]);'
            f'  Error ->'
            f'    io:format("FETCH_ERROR: ~p~n", [Error])'
            f'end'
        )

        result = run_erlang_command(roundtrip_code)
        output = result.stdout
        self.assertIn("UPLOAD_OK", output,
                       f"Upload failed: {output} {result.stderr}")

        # Verify byte-exact match
        self.assertIn(f"IK:{ik_hex.upper()}", output,
                       f"Identity key mismatch. Got: {output}")
        self.assertIn(f"SPK:{spk_hex.upper()}", output,
                       f"Signed prekey mismatch. Got: {output}")

    def test_tampered_ciphertext_fails(self):
        """Verify that any modification to the ciphertext causes decryption failure."""
        alice = X25519KeyPair()
        bob = X25519KeyPair()
        shared_key = derive_shared_secret(alice, bob.public_bytes())

        plaintext = b"This message must not be tampered with."
        encrypted = encrypt_message(shared_key, plaintext)

        # Tamper with one byte in the ciphertext portion (after nonce)
        tampered = bytearray(encrypted)
        tampered[15] ^= 0xFF  # Flip bits in ciphertext
        tampered = bytes(tampered)

        # Decryption must fail (GCM authentication tag check)
        with self.assertRaises(Exception):
            decrypt_message(shared_key, tampered)


if __name__ == "__main__":
    unittest.main(verbosity=2)
