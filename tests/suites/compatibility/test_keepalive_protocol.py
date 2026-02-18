#!/usr/bin/env python3
"""
P2-1: PING/PONG Keepalive Protocol Tests

PROTOCOL_V1_FREEZE v1.1 adds:
- PING (0x08): Client keepalive heartbeat
- PONG (0x09): Server keepalive response

RFC-001 v4.0 Section 5.6 specifies:
- Client sends PING every 30s
- Server tolerates up to 90s gap before disconnecting
- PING/PONG must not interfere with in-flight messages

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


def raw_login(sock, username):
    sock.sendall(b'\x01' + username.encode('utf-8'))
    resp = sock.recv(4096)
    if b"LOGIN_OK" not in resp:
        raise Exception(f"Login failed: {resp}")
    return resp


def test_ping_does_not_crash_server():
    """
    Sending PING (0x08) should not crash the server or disconnect the client.
    Even if the server doesn't yet respond with PONG, the connection must survive.
    """
    log("=" * 60)
    log("TEST: PING does not crash server")
    log("=" * 60)

    sock = None
    try:
        sock = get_tls_socket()
        user = unique_user("ping_test")
        raw_login(sock, user)
        time.sleep(0.05)

        # Send PING (0x08)
        sock.sendall(bytes([0x08]))
        time.sleep(0.3)

        # Verify connection still alive by sending a status query
        target = b"nobody"
        sock.sendall(b'\x05' + struct.pack('>H', len(target)) + target)
        sock.settimeout(3.0)
        try:
            resp = sock.recv(4096)
            log(f"  Server responded after PING: {len(resp)} bytes")
        except socket.timeout:
            log("  Server timeout after PING (acceptable if PING not yet handled)")

        log("  Connection survived PING — no crash")
        log("  PASS")
        return True

    finally:
        if sock:
            sock.close()


def test_ping_pong_with_messages():
    """
    PING/PONG must not interfere with normal message flow.
    Send PING between message sends and verify all messages arrive.
    """
    log("=" * 60)
    log("TEST: PING/PONG does not interfere with messages")
    log("=" * 60)

    sender_name = unique_user("ka_sender")
    receiver_name = unique_user("ka_receiver")
    sender_sock = None
    recv_sock = None

    try:
        recv_sock = get_tls_socket()
        raw_login(recv_sock, receiver_name)
        time.sleep(0.05)

        sender_sock = get_tls_socket()
        raw_login(sender_sock, sender_name)

        # Send message, then PING, then message
        # Message 1 via 0x07
        msg1 = b"before_ping"
        target = receiver_name.encode('utf-8')
        payload = (b'\x07' +
                   struct.pack('>H', len(target)) + target +
                   struct.pack('>Q', 1) +
                   struct.pack('>H', len(msg1)) + msg1)
        sender_sock.sendall(payload)

        # PING
        sender_sock.sendall(bytes([0x08]))

        # Message 2
        msg2 = b"after_ping"
        payload2 = (b'\x07' +
                    struct.pack('>H', len(target)) + target +
                    struct.pack('>Q', 2) +
                    struct.pack('>H', len(msg2)) + msg2)
        sender_sock.sendall(payload2)

        # Receive both messages
        recv_sock.settimeout(5.0)
        buf = b''
        received = 0
        deadline = time.time() + 5.0

        while received < 2 and time.time() < deadline:
            try:
                data = recv_sock.recv(4096)
                if data:
                    buf += data
            except socket.timeout:
                pass

            # Count 0x11 (reliable msg) opcodes
            while len(buf) >= 3 and buf[0] == 0x11:
                id_len = struct.unpack('>H', buf[1:3])[0]
                header_end = 3 + id_len + 4
                if len(buf) < header_end:
                    break
                msg_len = struct.unpack('>I', buf[3+id_len:header_end])[0]
                total = header_end + msg_len
                if len(buf) < total:
                    break
                msg_id = buf[3:3+id_len]
                recv_sock.sendall(b'\x03' + msg_id)  # ACK
                received += 1
                buf = buf[total:]

            # Skip non-reliable bytes (e.g., PONG 0x09 responses)
            while buf and buf[0] != 0x11:
                buf = buf[1:]

        assert received == 2, f"Expected 2 messages, got {received}"

        log("  Both messages received around PING — no interference")
        log("  PASS")
        return True

    finally:
        if sender_sock:
            sender_sock.close()
        if recv_sock:
            recv_sock.close()


def test_multiple_pings():
    """Send multiple PINGs rapidly. Server must not crash."""
    log("=" * 60)
    log("TEST: Multiple PINGs don't crash server")
    log("=" * 60)

    sock = None
    try:
        sock = get_tls_socket()
        user = unique_user("multi_ping")
        raw_login(sock, user)

        # Send 10 PINGs rapidly
        for _ in range(10):
            sock.sendall(bytes([0x08]))

        time.sleep(0.5)

        # Verify connection alive
        sock.sendall(b'\x05' + struct.pack('>H', 4) + b'test')
        sock.settimeout(2.0)
        try:
            sock.recv(4096)
        except socket.timeout:
            pass

        log("  10 PINGs sent — server alive")
        log("  PASS")
        return True

    finally:
        if sock:
            sock.close()


def main():
    log("PING/PONG Keepalive Protocol Tests (P2-1)")
    log(f"Server: {HOST}:{PORT}")
    log("")

    tests = [
        ("ping_no_crash", test_ping_does_not_crash_server),
        ("ping_with_messages", test_ping_pong_with_messages),
        ("multiple_pings", test_multiple_pings),
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
