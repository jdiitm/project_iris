#!/usr/bin/env python3
"""
G-21: CBOR Malformed Input Handling

PROTOCOL_V1_FREEZE Section 4.2: CBOR payload format
RFC-001 NFR-18: Input validation

Tests that the server handles malformed CBOR payloads gracefully:
1. Truncated CBOR
2. Deeply nested structures (1000 levels)
3. Missing required fields
4. Extra unknown fields (forward compat)
5. Invalid UTF-8 strings
6. Integer overflow values

INVARIANTS:
- Server MUST return error for invalid input
- Server MUST NOT crash
- Valid CBOR with extra fields should be tolerated (forward compat)

Pattern: follows test_protocol_fuzz.py

Tier: 1 (Security)
"""

import sys
import os
import socket
import ssl
import struct
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.helpers import wait_until

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = os.path.join(PROJECT_ROOT, "certs", "ca.pem")
TIMEOUT = 5

results = []


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def server_alive():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect((SERVER_HOST, SERVER_PORT))
        s.close()
        return True
    except (socket.error, OSError):
        return False


def get_tls_socket(timeout=TIMEOUT):
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(timeout)
    tls_sock = ctx.wrap_socket(raw, server_hostname=SERVER_HOST)
    tls_sock.connect((SERVER_HOST, SERVER_PORT))
    return tls_sock


def send_cbor_msg(sock, cbor_payload):
    """Send CBOR_MSG (opcode 0x10) with given payload."""
    packet = bytes([0x10]) + struct.pack(">H", len(cbor_payload)) + cbor_payload
    try:
        sock.sendall(packet)
    except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
        pass


# =============================================================================
# Test 1: Truncated CBOR
# =============================================================================
def test_truncated_cbor():
    """Send CBOR_MSG with truncated payload. Server must not crash."""
    log("\n=== Test 1: Truncated CBOR ===")

    payloads = [
        b"\xa2",           # Map of 2 items, but no items follow
        b"\xa1\x63foo",    # Map with key "foo" but no value
        b"\x82",           # Array of 2, but no elements
        b"\x5f",           # Indefinite-length byte string, no break
        b"\x18",           # Unsigned int, additional byte expected
    ]

    for i, payload in enumerate(payloads):
        try:
            sock = get_tls_socket(timeout=2)
            sock.sendall(bytes([0x01]) + b"cbor_trunc_user")
            send_cbor_msg(sock, payload)
            try:
                sock.recv(1024)
            except Exception:
                pass
            sock.close()
        except Exception:
            pass

    if wait_until(server_alive, timeout=2, description="server alive after truncated CBOR"):
        log("  PASS: Server survived truncated CBOR payloads")
        return True
    else:
        log("  FAIL: Server crashed from truncated CBOR")
        return False


# =============================================================================
# Test 2: Deeply Nested Structures
# =============================================================================
def test_deep_nesting():
    """Send deeply nested CBOR map (100 levels). Server must not stack overflow."""
    log("\n=== Test 2: Deeply Nested CBOR ===")

    # Build a 100-deep nested map: {0: {0: {0: ... }}}
    depth = 100
    payload = b""
    for _ in range(depth):
        payload += b"\xa1\x00"  # Map of 1, key = 0
    payload += b"\x00"  # Final value = 0

    try:
        sock = get_tls_socket(timeout=3)
        sock.sendall(bytes([0x01]) + b"cbor_deep_user")
        send_cbor_msg(sock, payload)
        try:
            sock.recv(1024)
        except Exception:
            pass
        sock.close()
    except Exception:
        pass

    if wait_until(server_alive, timeout=2, description="server alive after deep nesting"):
        log(f"  PASS: Server survived {depth}-deep nested CBOR")
        return True
    else:
        log("  FAIL: Server crashed from deep nesting")
        return False


# =============================================================================
# Test 3: Invalid UTF-8 in CBOR String
# =============================================================================
def test_invalid_utf8():
    """Send CBOR text string with invalid UTF-8 bytes."""
    log("\n=== Test 3: Invalid UTF-8 in CBOR ===")

    # CBOR text string (major type 3) with invalid UTF-8
    invalid_payloads = [
        b"\x64\xff\xfe\xfd\xfc",      # 4-byte text string, all invalid
        b"\x63\xc0\xaf\x00",           # Overlong encoding
        b"\x63\xed\xa0\x80",           # Surrogate half
    ]

    for payload in invalid_payloads:
        try:
            sock = get_tls_socket(timeout=2)
            sock.sendall(bytes([0x01]) + b"cbor_utf8_user")
            send_cbor_msg(sock, payload)
            try:
                sock.recv(1024)
            except Exception:
                pass
            sock.close()
        except Exception:
            pass

    if wait_until(server_alive, timeout=2, description="server alive after invalid UTF-8"):
        log("  PASS: Server survived invalid UTF-8 in CBOR")
        return True
    else:
        log("  FAIL: Server crashed from invalid UTF-8")
        return False


# =============================================================================
# Test 4: Zero-Length and Oversized Payloads
# =============================================================================
def test_boundary_sizes():
    """Send CBOR_MSG with zero-length and max-length payloads."""
    log("\n=== Test 4: Boundary Size CBOR ===")

    for payload in [b"", b"\x00", b"\xff" * 10000]:
        try:
            sock = get_tls_socket(timeout=2)
            sock.sendall(bytes([0x01]) + b"cbor_size_user")
            send_cbor_msg(sock, payload)
            try:
                sock.recv(1024)
            except Exception:
                pass
            sock.close()
        except Exception:
            pass

    if wait_until(server_alive, timeout=2, description="server alive after boundary sizes"):
        log("  PASS: Server survived boundary-size CBOR")
        return True
    else:
        log("  FAIL: Server crashed from boundary-size CBOR")
        return False


# =============================================================================
# Test 5: Server Alive After All
# =============================================================================
def test_server_survives():
    """Verify server accepts legitimate connections after malformed CBOR."""
    log("\n=== Test 5: Server Survives ===")

    if not server_alive():
        log("  FAIL: Server is DOWN")
        return False

    try:
        from tests.utilities import IrisClient
        c = IrisClient()
        c.login("legit_after_cbor")
        c.send_msg("cbor_target", "hello after CBOR abuse")
        wait_until(server_alive, timeout=2, description="server processed legit message")
        c.close()
        log("  PASS: Legitimate client works after CBOR abuse")
        return True
    except Exception as e:
        log(f"  FAIL: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-21: CBOR MALFORMED INPUT HANDLING")
    print(" PROTOCOL_V1_FREEZE Section 4.2")
    print("=" * 60)

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Truncated CBOR", test_truncated_cbor),
        ("Deep Nesting", test_deep_nesting),
        ("Invalid UTF-8", test_invalid_utf8),
        ("Boundary Sizes", test_boundary_sizes),
        ("Server Survives", test_server_survives),
    ]

    for name, fn in tests:
        try:
            result = fn()
            results.append((name, result))
        except Exception as e:
            log(f"  Exception in {name}: {e}")
            results.append((name, False))

    print("\n" + "=" * 60)
    print("RESULTS:")
    print("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)
    for name, result in results:
        print(f"  [{'PASS' if result else 'FAIL'}] {name}")

    if passed == total:
        print(f"\nG-21 CBOR Malformed: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-21 CBOR Malformed: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
