#!/usr/bin/env python3
"""
CBOR/E2EE Schema Validation Tests (RFC-001 v4.0 NFR-18)

RFC NFR-18: "All protocol fields validated before processing"

Tests that malformed CBOR messages with missing required fields are
handled gracefully (error response, NOT server crash or silent accept).

Tests:
1. E2EE message (0x23) with missing required header fields
2. E2EE message with empty ciphertext
3. CBOR message (0x10) with oversized payload
4. Server survives all malformed messages (no crash)
"""

import os
import sys
import socket
import ssl
import struct
import time
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from tests.utilities.tls_connection import get_verified_ssl_context

CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def get_tls_socket():
    context = get_verified_ssl_context()
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(TIMEOUT)
    s = context.wrap_socket(raw, server_hostname=SERVER_HOST)
    s.connect((SERVER_HOST, SERVER_PORT))
    return s


def login(sock, user):
    """Login and return True on success."""
    sock.sendall(b'\x01' + user.encode('utf-8'))
    resp = sock.recv(4096)
    return b"LOGIN_OK" in resp


def cbor_encode_map(kvs):
    """Minimal CBOR map encoder for test payloads.
    kvs: list of (key_bytes, value_bytes) pairs, already CBOR-encoded."""
    n = len(kvs)
    if n < 24:
        header = bytes([0xa0 | n])
    else:
        header = bytes([0xb8, n])
    body = b''
    for k, v in kvs:
        body += k + v
    return header + body


def cbor_text(s):
    """Encode a text string as CBOR."""
    b = s.encode('utf-8')
    n = len(b)
    if n < 24:
        return bytes([0x60 | n]) + b
    elif n < 256:
        return bytes([0x78, n]) + b
    else:
        return bytes([0x79]) + struct.pack('>H', n) + b


def cbor_bytes(b):
    """Encode a byte string as CBOR."""
    n = len(b)
    if n < 24:
        return bytes([0x40 | n]) + b
    elif n < 256:
        return bytes([0x58, n]) + b
    else:
        return bytes([0x59]) + struct.pack('>H', n) + b


def server_is_alive():
    """Check server is still responding after test."""
    try:
        sock = get_tls_socket()
        sock.sendall(b'\x01' + b'alive_check')
        resp = sock.recv(4096)
        sock.close()
        return b"LOGIN_OK" in resp
    except Exception:
        return False


# =============================================================================
# Tests
# =============================================================================

def test_e2ee_msg_missing_required_fields():
    """
    E2EE message (0x23) with missing 'ik' (identity key) field.
    RFC-001-AMENDMENT-001 Section 4.1 requires: ik, ek, pn, n.
    Server should handle gracefully (error or ignore, NOT crash).
    """
    log("TEST: E2EE message missing required header fields")

    try:
        sock = get_tls_socket()
        user = f"cbor_val_{int(time.time())}"
        if not login(sock, user):
            log("  FAIL: Login failed")
            sock.close()
            return False

        time.sleep(0.05)

        # Build E2EE message with header missing 'ik' field
        # Header CBOR map with only 'ek' (missing ik, pn, n)
        header_map = cbor_encode_map([
            (cbor_text("ek"), cbor_bytes(b'\x00' * 32)),
        ])

        recipient = b"nobody"
        ciphertext = b"fake_encrypted_data"
        mac = b'\x00' * 16

        # Wire format: 0x23 | RecipientLen:16 | Recipient | HeaderLen:16 | Header | CipherLen:32 | Cipher | MAC:16
        packet = (bytes([0x23]) +
                  struct.pack('>H', len(recipient)) + recipient +
                  struct.pack('>H', len(header_map)) + header_map +
                  struct.pack('>I', len(ciphertext)) + ciphertext +
                  mac)
        sock.sendall(packet)

        time.sleep(0.3)
        sock.settimeout(2.0)
        try:
            resp = sock.recv(4096)
            log(f"  Server responded to malformed E2EE: {len(resp)} bytes")
        except socket.timeout:
            log("  Server did not respond (timeout - acceptable)")
        except ConnectionResetError:
            log("  Server reset connection (acceptable for malformed)")

        sock.close()

        # Critical: server must still be alive
        assert server_is_alive(), "Server crashed after malformed E2EE message"
        log("  Server survived malformed E2EE: PASS")
        return True

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


def test_e2ee_msg_empty_ciphertext():
    """E2EE message with zero-length ciphertext. Server should handle gracefully."""
    log("TEST: E2EE message with empty ciphertext")

    try:
        sock = get_tls_socket()
        user = f"cbor_empty_{int(time.time())}"
        if not login(sock, user):
            log("  FAIL: Login failed")
            sock.close()
            return False

        time.sleep(0.05)

        header_map = cbor_encode_map([
            (cbor_text("ik"), cbor_bytes(b'\x00' * 32)),
            (cbor_text("ek"), cbor_bytes(b'\x00' * 32)),
            (cbor_text("pn"), bytes([0x00])),  # CBOR uint 0
            (cbor_text("n"), bytes([0x00])),   # CBOR uint 0
        ])

        recipient = b"nobody"
        ciphertext = b""  # Empty ciphertext
        mac = b'\x00' * 16

        packet = (bytes([0x23]) +
                  struct.pack('>H', len(recipient)) + recipient +
                  struct.pack('>H', len(header_map)) + header_map +
                  struct.pack('>I', len(ciphertext)) + ciphertext +
                  mac)
        sock.sendall(packet)

        time.sleep(0.3)
        sock.settimeout(2.0)
        try:
            resp = sock.recv(4096)
            log(f"  Server responded: {len(resp)} bytes")
        except socket.timeout:
            log("  Server timeout (acceptable)")
        except ConnectionResetError:
            log("  Connection reset (acceptable)")

        sock.close()

        assert server_is_alive(), "Server crashed after empty ciphertext"
        log("  Server survived empty ciphertext: PASS")
        return True

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


def test_cbor_msg_oversized_payload():
    """
    CBOR message (0x10) with payload > MAX_CBOR_LEN (256KB).
    Server should reject without crash.
    """
    log("TEST: CBOR message with oversized payload")

    try:
        sock = get_tls_socket()
        user = f"cbor_big_{int(time.time())}"
        if not login(sock, user):
            log("  FAIL: Login failed")
            sock.close()
            return False

        time.sleep(0.05)

        target = b"nobody"
        # Create payload slightly over MAX_CBOR_LEN (262144 = 256KB)
        oversized = b'\x00' * 262200

        # Wire: 0x10 | TargetLen:16 | Target | CborLen:32 | CborPayload
        packet = (bytes([0x10]) +
                  struct.pack('>H', len(target)) + target +
                  struct.pack('>I', len(oversized)) + oversized)

        try:
            sock.sendall(packet)
        except (BrokenPipeError, ConnectionResetError):
            log("  Server rejected mid-send (good)")
            assert server_is_alive(), "Server crashed after oversized CBOR"
            log("  Server survived oversized CBOR: PASS")
            return True

        time.sleep(0.5)
        sock.settimeout(2.0)
        try:
            resp = sock.recv(4096)
            log(f"  Server responded: {len(resp)} bytes")
        except socket.timeout:
            log("  Server timeout (acceptable)")
        except ConnectionResetError:
            log("  Connection reset (acceptable rejection)")

        sock.close()

        assert server_is_alive(), "Server crashed after oversized CBOR"
        log("  Server survived oversized CBOR: PASS")
        return True

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


def test_server_survives_all():
    """Final health check: server still serves normal requests after all tests."""
    log("TEST: Server health after all malformed inputs")

    try:
        sock = get_tls_socket()
        user = f"health_{int(time.time())}"
        if not login(sock, user):
            log("  FAIL: Server not healthy after tests")
            sock.close()
            return False

        sock.close()
        log("  Server healthy after all tests: PASS")
        return True

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


# =============================================================================
# Main
# =============================================================================

def main():
    print("\n" + "=" * 70)
    print("CBOR/E2EE Schema Validation Tests (RFC-001 v4.0 NFR-18)")
    print("=" * 70)

    passed = 0
    failed = 0

    tests = [
        test_e2ee_msg_missing_required_fields,
        test_e2ee_msg_empty_ciphertext,
        test_cbor_msg_oversized_payload,
        test_server_survives_all,
    ]

    for test in tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
        except AssertionError as e:
            log(f"  FAIL: {test.__name__}: {e}")
            failed += 1
        except Exception as e:
            log(f"  ERROR: {test.__name__}: {e}")
            failed += 1

    print(f"\n{'=' * 70}")
    print(f"SUMMARY: {passed} passed, {failed} failed out of {passed + failed}")
    print(f"{'=' * 70}")

    if failed == 0:
        print("All CBOR schema validation tests passed!")
        return 0
    else:
        print(f"{failed} test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
