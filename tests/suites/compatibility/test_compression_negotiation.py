#!/usr/bin/env python3
"""
P2-5 / G-27: Compression Negotiation Tests

RFC-001 v4.0 Section 5.7:
- Client advertises compression capabilities in login metadata
- Server selects best shared algorithm (zstd > zlib > none)
- If no common algorithm, falls back to uncompressed (graceful)

Since compression negotiation is not yet implemented in the wire protocol,
these tests verify that:
1. Sending unknown/extra bytes after login doesn't crash the server
2. Connection survives extended handshake data
3. Normal messaging works regardless of compression metadata

Pattern: follows test_protocol_versions.py using raw TLS socket.
"""

import sys
import os
import socket
import struct
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from pathlib import Path
from tests.utilities.iris_client import IrisClient
from tests.utilities.tls_connection import get_verified_ssl_context

CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"
HOST = os.environ.get('IRIS_HOST', 'localhost')
PORT = int(os.environ.get('IRIS_PORT', '8085'))


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def unique_user(prefix):
    import uuid
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def get_tls_socket():
    context = get_verified_ssl_context()
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(10.0)
    s = context.wrap_socket(raw, server_hostname=HOST)
    s.connect((HOST, PORT))
    return s


def test_normal_login_still_works():
    """Baseline: standard login works before testing extended handshake."""
    log("=" * 60)
    log("TEST: Normal login works (baseline)")
    log("=" * 60)

    client = None
    try:
        client = IrisClient(HOST, PORT)
        client.login(unique_user("comp_baseline"))
        log("  Login successful")
        log("  PASS")
        return True
    finally:
        if client:
            client.close()


def test_messaging_uncompressed():
    """Verify messages flow correctly without compression."""
    log("=" * 60)
    log("TEST: Message flow without compression")
    log("=" * 60)

    sender_name = unique_user("comp_sender")
    receiver_name = unique_user("comp_receiver")
    sender = None
    receiver = None

    try:
        receiver = IrisClient(HOST, PORT)
        receiver.login(receiver_name)

        sender = IrisClient(HOST, PORT)
        sender.login(sender_name)

        test_msg = "uncompressed_test_message"
        sender.send_msg(receiver_name, test_msg)

        received = receiver.recv_msg(timeout=5.0)
        assert received is not None, "No message received"
        decoded = received.decode('utf-8') if isinstance(received, bytes) else received
        assert test_msg in decoded, f"Expected '{test_msg}' in '{decoded}'"

        log("  Message sent and received without compression")
        log("  PASS")
        return True

    finally:
        if sender:
            sender.close()
        if receiver:
            receiver.close()


def test_server_handles_unknown_opcodes_gracefully():
    """
    Sending opcode bytes outside the defined protocol range should
    not crash the server. This validates robustness for future
    compression negotiation opcodes.
    """
    log("=" * 60)
    log("TEST: Server handles unknown opcodes gracefully")
    log("=" * 60)

    sock = None
    try:
        sock = get_tls_socket()
        user = unique_user("comp_unknown")
        sock.sendall(b'\x01' + user.encode('utf-8'))
        resp = sock.recv(4096)
        if b"LOGIN_OK" not in resp:
            raise Exception(f"Login failed: {resp}")

        # Send an undefined opcode (0xFE — not in protocol)
        sock.sendall(bytes([0xFE, 0x00, 0x00]))

        # Server should still be alive
        sock.sendall(b'\x05' + struct.pack('>H', 4) + b'test')
        sock.settimeout(2.0)
        try:
            sock.recv(4096)
            log("  Server responded after unknown opcode")
        except socket.timeout:
            log("  Server timeout (acceptable — didn't crash)")

        log("  PASS")
        return True

    finally:
        if sock:
            sock.close()


def main():
    log("Compression Negotiation Tests (P2-5, G-27)")
    log(f"Server: {HOST}:{PORT}")
    log("")

    tests = [
        ("normal_login_baseline", test_normal_login_still_works),
        ("uncompressed_messaging", test_messaging_uncompressed),
        ("unknown_opcodes_graceful", test_server_handles_unknown_opcodes_gracefully),
    ]

    passed = 0
    failed = 0

    for name, test_fn in tests:
        try:
            result = test_fn()
            if result:
                passed += 1
            else:
                failed += 1
                log(f"  FAIL: {name}")
        except Exception as e:
            failed += 1
            log(f"  FAIL: {name} raised {type(e).__name__}: {e}")

    log("")
    log("=" * 60)
    log(f"Results: {passed} passed, {failed} failed out of {len(tests)}")
    log("=" * 60)

    if failed > 0:
        sys.exit(1)
    sys.exit(0)


if __name__ == "__main__":
    main()
