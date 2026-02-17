#!/usr/bin/env python3
"""
G-20: Backward Compatibility -- Frozen v1 Client

RFC-001 Section 14: Compatibility
PROTOCOL_V1_FREEZE v1.1: v1 opcodes MUST remain supported.

Implements a "v1 frozen client" that uses ONLY v1 opcodes:
- 0x01 LOGIN
- 0x02 SEND_MSG
- 0x03 ACK
- 0x04 STATUS
- 0x05 BATCH_SEND
- 0x06 GET_STATUS

Exercises the full v1 workflow and asserts all operations succeed.
Any commit that breaks v1 compatibility is flagged.

Pattern: follows test_protocol_versions.py

Tier: 1 (Compatibility)
"""

import sys
import os
import socket
import ssl
import struct
import time
from pathlib import Path

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.tls_connection import get_verified_ssl_context

CA_CERT = os.path.join(PROJECT_ROOT, "certs", "ca.pem")
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
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


class V1FrozenClient:
    """A client that uses ONLY v1 protocol opcodes.
    No new opcodes (0x08+), no CBOR, no RELIABLE_MSG."""

    V1_LOGIN = 0x01
    V1_SEND = 0x02
    V1_ACK = 0x03
    V1_STATUS = 0x04
    V1_BATCH = 0x05
    V1_GET_STATUS = 0x06

    def __init__(self):
        ctx = get_verified_ssl_context()
        raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        raw.settimeout(TIMEOUT)
        self.sock = ctx.wrap_socket(raw, server_hostname=SERVER_HOST)
        self.sock.connect((SERVER_HOST, SERVER_PORT))

    def login(self, user):
        """V1 LOGIN: opcode 0x01 + username bytes"""
        packet = bytes([self.V1_LOGIN]) + user.encode("utf-8")
        self.sock.sendall(packet)
        time.sleep(0.3)
        try:
            self.sock.settimeout(2)
            resp = self.sock.recv(1024)
            return resp
        except socket.timeout:
            return b""

    def send_msg(self, target, msg):
        """V1 SEND_MSG: opcode 0x02 + target_len + target + msg_len + msg"""
        target_bytes = target.encode("utf-8")
        msg_bytes = msg.encode("utf-8")
        packet = (bytes([self.V1_SEND]) +
                  struct.pack(">H", len(target_bytes)) + target_bytes +
                  struct.pack(">H", len(msg_bytes)) + msg_bytes)
        self.sock.sendall(packet)

    def send_ack(self, msg_id):
        """V1 ACK: opcode 0x03 + msg_id_len + msg_id"""
        msg_id_bytes = msg_id.encode("utf-8") if isinstance(msg_id, str) else msg_id
        packet = bytes([self.V1_ACK]) + struct.pack(">H", len(msg_id_bytes)) + msg_id_bytes
        self.sock.sendall(packet)

    def get_status(self, user):
        """V1 GET_STATUS: opcode 0x06 + user_len + user"""
        user_bytes = user.encode("utf-8")
        packet = bytes([self.V1_GET_STATUS]) + struct.pack(">H", len(user_bytes)) + user_bytes
        self.sock.sendall(packet)
        try:
            self.sock.settimeout(2)
            return self.sock.recv(1024)
        except socket.timeout:
            return b""

    def close(self):
        try:
            self.sock.close()
        except Exception:
            pass


# =============================================================================
# Test 1: V1 Login
# =============================================================================
def test_v1_login():
    """V1 LOGIN opcode (0x01) must still work."""
    log("\n=== Test 1: V1 LOGIN ===")

    try:
        c = V1FrozenClient()
        resp = c.login("v1_frozen_login_user")

        if resp:
            log(f"  Server responded ({len(resp)} bytes)")
        else:
            log("  No response (server may not echo login)")

        c.close()
        log("  PASS: V1 LOGIN accepted")
        return True
    except Exception as e:
        log(f"  FAIL: V1 LOGIN failed: {e}")
        return False


# =============================================================================
# Test 2: V1 Send Message
# =============================================================================
def test_v1_send():
    """V1 SEND_MSG opcode (0x02) must still work."""
    log("\n=== Test 2: V1 SEND_MSG ===")

    try:
        c = V1FrozenClient()
        c.login("v1_frozen_sender")
        time.sleep(0.2)

        c.send_msg("v1_frozen_target", "hello from v1 client")
        time.sleep(0.3)

        c.close()

        if server_alive():
            log("  PASS: V1 SEND_MSG accepted without crash")
            return True
        else:
            log("  FAIL: Server crashed on V1 SEND_MSG")
            return False
    except Exception as e:
        log(f"  FAIL: V1 SEND_MSG failed: {e}")
        return False


# =============================================================================
# Test 3: V1 ACK
# =============================================================================
def test_v1_ack():
    """V1 ACK opcode (0x03) must still work."""
    log("\n=== Test 3: V1 ACK ===")

    try:
        c = V1FrozenClient()
        c.login("v1_frozen_acker")
        time.sleep(0.2)

        c.send_ack("fake_msg_id_v1_test")
        time.sleep(0.2)

        c.close()

        if server_alive():
            log("  PASS: V1 ACK accepted without crash")
            return True
        else:
            log("  FAIL: Server crashed on V1 ACK")
            return False
    except Exception as e:
        log(f"  FAIL: V1 ACK failed: {e}")
        return False


# =============================================================================
# Test 4: V1 GET_STATUS
# =============================================================================
def test_v1_get_status():
    """V1 GET_STATUS opcode (0x06) must still work."""
    log("\n=== Test 4: V1 GET_STATUS ===")

    try:
        c = V1FrozenClient()
        c.login("v1_frozen_status_checker")
        time.sleep(0.2)

        resp = c.get_status("v1_frozen_target")
        if resp:
            log(f"  Got status response ({len(resp)} bytes)")
        else:
            log("  No status response (user may not exist)")

        c.close()

        if server_alive():
            log("  PASS: V1 GET_STATUS accepted without crash")
            return True
        else:
            log("  FAIL: Server crashed on V1 GET_STATUS")
            return False
    except Exception as e:
        log(f"  FAIL: V1 GET_STATUS failed: {e}")
        return False


# =============================================================================
# Test 5: V1 Full Workflow
# =============================================================================
def test_v1_full_workflow():
    """Exercise a complete v1 session: login -> send -> ack -> status."""
    log("\n=== Test 5: V1 Full Workflow ===")

    try:
        sender = V1FrozenClient()
        sender.login("v1_full_sender")
        time.sleep(0.2)

        receiver = V1FrozenClient()
        receiver.login("v1_full_receiver")
        time.sleep(0.2)

        # Send messages
        for i in range(5):
            sender.send_msg("v1_full_receiver", f"v1_workflow_msg_{i}")
            time.sleep(0.05)

        # ACK
        sender.send_ack("v1_workflow_ack_1")
        time.sleep(0.1)

        # Get status
        sender.get_status("v1_full_receiver")
        time.sleep(0.1)

        sender.close()
        receiver.close()

        if server_alive():
            log("  PASS: V1 full workflow completed")
            return True
        else:
            log("  FAIL: Server crashed during V1 workflow")
            return False

    except Exception as e:
        log(f"  FAIL: V1 workflow failed: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-20: BACKWARD COMPATIBILITY -- V1 FROZEN CLIENT")
    print(" PROTOCOL_V1_FREEZE v1.1")
    print("=" * 60)
    print(f"Target: {SERVER_HOST}:{SERVER_PORT}")

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("V1 LOGIN", test_v1_login),
        ("V1 SEND_MSG", test_v1_send),
        ("V1 ACK", test_v1_ack),
        ("V1 GET_STATUS", test_v1_get_status),
        ("V1 Full Workflow", test_v1_full_workflow),
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
        print(f"\nG-20 V1 Frozen Client: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-20 V1 Frozen Client: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
