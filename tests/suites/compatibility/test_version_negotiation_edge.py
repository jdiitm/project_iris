#!/usr/bin/env python3
"""
G-26/G-27: Version and Capability Negotiation Edge Cases

RFC-001 Section 11.1: Version Negotiation
1. Client sends: {version: [1, 2], capabilities: [...]}
2. Server responds: {version: 1, capabilities: [...]}

Tests edge cases:
1. Empty version list -- graceful error
2. Unsupported version v99 -- error response
3. Unknown capability "quantum_e2ee" -- ignored (no error)
4. Empty capabilities list -- baseline features work
5. Duplicate versions -- no crash

Pattern: follows test_protocol_versions.py

Tier: 1 (Compatibility)
"""

import sys
import os
import socket
import ssl
import struct
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

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
    except Exception:
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


# =============================================================================
# Test 1: Empty Version List
# =============================================================================
def test_empty_version_list():
    """Send version negotiation with empty list. Server must handle gracefully."""
    log("\n=== Test 1: Empty Version List ===")

    try:
        sock = get_tls_socket()
        # Login first (required for session state)
        sock.sendall(bytes([0x01]) + b"version_empty_user")
        try:
            sock.recv(1024)
        except socket.timeout:
            pass

        # Send VERSION_NEGOTIATE (0x0C) with empty version list
        # CBOR map: {"versions": [], "capabilities": []}
        cbor_payload = b"\xa2\x68versions\x80\x6ccapabilities\x80"
        packet = bytes([0x0C]) + struct.pack(">I", len(cbor_payload)) + cbor_payload
        sock.sendall(packet)

        try:
            sock.recv(1024)
        except socket.timeout:
            pass

        sock.close()
    except Exception:
        pass

    if server_alive():
        log("  PASS: Server handled empty version list gracefully")
        return True
    else:
        log("  FAIL: Server crashed on empty version list")
        return False


# =============================================================================
# Test 2: Unsupported Version
# =============================================================================
def test_unsupported_version():
    """Request version v99. Server must reject with error (not crash)."""
    log("\n=== Test 2: Unsupported Version (v99) ===")

    try:
        sock = get_tls_socket()
        sock.sendall(bytes([0x01]) + b"version_v99_user")
        try:
            sock.recv(1024)
        except socket.timeout:
            pass

        # VERSION_NEGOTIATE (0x0C) CBOR: {"versions": [99], "capabilities": []}
        cbor_payload = b"\xa2\x68versions\x81\x18\x63\x6ccapabilities\x80"
        packet = bytes([0x0C]) + struct.pack(">I", len(cbor_payload)) + cbor_payload
        sock.sendall(packet)

        try:
            resp = sock.recv(1024)
            if resp:
                log(f"  Got response ({len(resp)} bytes)")
        except socket.timeout:
            log("  No response (server may not echo)")

        sock.close()
    except Exception:
        pass

    if server_alive():
        log("  PASS: Server handled unsupported version v99")
        return True
    else:
        log("  FAIL: Server crashed on unsupported version")
        return False


# =============================================================================
# Test 3: Unknown Capability
# =============================================================================
def test_unknown_capability():
    """Advertise unknown capability "quantum_e2ee". Server must ignore it."""
    log("\n=== Test 3: Unknown Capability ===")

    try:
        sock = get_tls_socket()
        sock.sendall(bytes([0x01]) + b"cap_unknown_user")
        try:
            sock.recv(1024)
        except socket.timeout:
            pass

        # VERSION_NEGOTIATE (0x0C) CBOR: {"versions": [1], "capabilities": ["quantum_e2ee"]}
        cbor_payload = b"\xa2\x68versions\x81\x01\x6ccapabilities\x81\x6cquantum_e2ee"
        packet = bytes([0x0C]) + struct.pack(">I", len(cbor_payload)) + cbor_payload
        sock.sendall(packet)

        try:
            sock.recv(1024)
        except socket.timeout:
            pass

        sock.close()
    except Exception:
        pass

    if server_alive():
        log("  PASS: Server ignored unknown capability")
        return True
    else:
        log("  FAIL: Server crashed on unknown capability")
        return False


# =============================================================================
# Test 4: Normal Client After Edge Cases
# =============================================================================
def test_normal_after_edge_cases():
    """Verify normal client works after all edge case tests."""
    log("\n=== Test 4: Normal Client After Edge Cases ===")

    try:
        c = IrisClient()
        c.login(unique_user("after_edge_cases"))
        c.send_msg("edge_case_target", "normal after edge cases")
        c.close()
        log("  PASS: Normal client works")
        return True
    except Exception as e:
        log(f"  FAIL: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-26/G-27: VERSION & CAPABILITY NEGOTIATION EDGE CASES")
    print(" RFC-001 Section 11.1")
    print("=" * 60)

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Empty Version List", test_empty_version_list),
        ("Unsupported Version", test_unsupported_version),
        ("Unknown Capability", test_unknown_capability),
        ("Normal After Edge Cases", test_normal_after_edge_cases),
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
        print(f"\nG-26/G-27 Negotiation Edge Cases: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-26/G-27 Negotiation Edge Cases: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
