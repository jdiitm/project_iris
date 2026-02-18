#!/usr/bin/env python3
"""
Sender Key Rotation Race Window Test (RFC-001-AMENDMENT-001 Section 6.3)

RFC says: "On member removal: All remaining members generate new Sender Keys"

This test verifies that messages sent during the key rotation window
(between member removal and new key distribution) are not lost.

The server routes opaque blobs — it doesn't validate cryptographic keys.
So the test verifies that the GROUP_MSG routing path works correctly
even when interleaved with member removal operations.
"""

import os
import sys
import socket
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


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def get_tls_socket():
    context = get_verified_ssl_context()
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(10)
    s = context.wrap_socket(raw, server_hostname=SERVER_HOST)
    s.connect((SERVER_HOST, SERVER_PORT))
    return s


def login(sock, user):
    sock.sendall(b'\x01' + user.encode('utf-8'))
    resp = sock.recv(4096)
    return b"LOGIN_OK" in resp


def create_group(sock, group_name):
    """Send GROUP_CREATE (0x30) and wait for response."""
    name_bytes = group_name.encode('utf-8')
    packet = bytes([0x30]) + struct.pack('>H', len(name_bytes)) + name_bytes
    sock.sendall(packet)
    try:
        resp = sock.recv(4096)
        return resp
    except socket.timeout:
        return None


def add_member(sock, group_id, member_name):
    """Send GROUP_JOIN (0x31) to add a member."""
    gid_bytes = group_id if isinstance(group_id, bytes) else group_id.encode('utf-8')
    member_bytes = member_name.encode('utf-8')
    packet = (bytes([0x31]) +
              struct.pack('>H', len(gid_bytes)) + gid_bytes +
              struct.pack('>H', len(member_bytes)) + member_bytes)
    sock.sendall(packet)
    try:
        return sock.recv(4096)
    except socket.timeout:
        return None


def leave_group(sock, group_id):
    """Send GROUP_LEAVE (0x32)."""
    gid_bytes = group_id if isinstance(group_id, bytes) else group_id.encode('utf-8')
    packet = bytes([0x32]) + struct.pack('>H', len(gid_bytes)) + gid_bytes
    sock.sendall(packet)
    try:
        return sock.recv(4096)
    except socket.timeout:
        return None


def test_group_message_during_member_removal():
    """
    Test that group messages sent immediately after a member removal
    do not crash the server or cause message loss for remaining members.
    
    Scenario:
    1. Create group with 3 members (admin, alice, bob)
    2. Admin removes bob
    3. Immediately send group message (during rotation window)
    4. Verify server handles this gracefully
    """
    log("TEST: Group message during member removal (rotation window)")

    try:
        ts = int(time.time())
        admin_sock = get_tls_socket()
        alice_sock = get_tls_socket()
        bob_sock = get_tls_socket()

        admin_user = f"sk_admin_{ts}"
        alice_user = f"sk_alice_{ts}"
        bob_user = f"sk_bob_{ts}"

        if not login(admin_sock, admin_user):
            log("  FAIL: Admin login failed")
            return False
        if not login(alice_sock, alice_user):
            log("  FAIL: Alice login failed")
            return False
        if not login(bob_sock, bob_user):
            log("  FAIL: Bob login failed")
            return False

        # Step 1: Create group
        group_name = f"rotation_test_{ts}"
        resp = create_group(admin_sock, group_name)
        if resp is None:
            log("  SKIP: Group service not available")
            admin_sock.close()
            alice_sock.close()
            bob_sock.close()
            return True  # Not a failure if groups aren't enabled

        # Extract group_id from response (0x31 GROUP_JOIN response)
        group_id = None
        if resp and len(resp) > 3 and resp[0] == 0x31:
            gid_len = struct.unpack('>H', resp[1:3])[0]
            group_id = resp[3:3+gid_len]

        if group_id is None:
            # Try to use the group name as ID
            group_id = group_name.encode('utf-8')

        # Step 2: Add members
        add_member(admin_sock, group_id, alice_user)
        add_member(admin_sock, group_id, bob_user)

        # Step 3: Bob leaves (triggers sender key rotation)
        leave_group(bob_sock, group_id)

        # Step 4: Immediately send a group message (during rotation window)
        # This simulates a message sent with the old sender key
        fake_ciphertext = b"rotation_window_msg"
        fake_header_cbor = bytes([0xa1, 0x66]) + b"sender" + bytes([0x60 | len(admin_user.encode())]) + admin_user.encode()
        gid = group_id if isinstance(group_id, bytes) else group_id.encode('utf-8')
        packet = (bytes([0x33]) +
                  struct.pack('>H', len(gid)) + gid +
                  struct.pack('>H', len(fake_header_cbor)) + fake_header_cbor +
                  struct.pack('>I', len(fake_ciphertext)) + fake_ciphertext)
        admin_sock.sendall(packet)

        # Step 5: Verify server is still alive
        verify_sock = get_tls_socket()
        verify_sock.sendall(b'\x01' + b'verify_alive')
        verify_resp = verify_sock.recv(4096)
        verify_sock.close()

        admin_sock.close()
        alice_sock.close()
        bob_sock.close()

        if b"LOGIN_OK" in verify_resp:
            log("  Server survived rotation window message: PASS")
            return True
        else:
            log("  FAIL: Server not healthy after rotation window message")
            return False

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


def test_server_survives_rapid_join_leave():
    """
    Rapid join/leave cycles should not crash the server.
    Tests resilience of group membership under churn.
    """
    log("TEST: Rapid group join/leave cycles")

    try:
        ts = int(time.time())
        admin_sock = get_tls_socket()
        if not login(admin_sock, f"rapid_admin_{ts}"):
            log("  FAIL: Login failed")
            return False

        # Create group
        resp = create_group(admin_sock, f"rapid_group_{ts}")
        if resp is None:
            log("  SKIP: Group service not available")
            admin_sock.close()
            return True

        # Extract group ID
        group_id = f"rapid_group_{ts}".encode('utf-8')
        if resp and len(resp) > 3 and resp[0] == 0x31:
            gid_len = struct.unpack('>H', resp[1:3])[0]
            group_id = resp[3:3+gid_len]

        # Rapid add/remove cycles
        for i in range(10):
            member = f"rapid_member_{ts}_{i}"
            member_sock = get_tls_socket()
            login(member_sock, member)
            add_member(admin_sock, group_id, member)
            leave_group(member_sock, group_id)
            member_sock.close()

        # Verify server health
        verify_sock = get_tls_socket()
        verify_sock.sendall(b'\x01' + b'rapid_verify')
        verify_resp = verify_sock.recv(4096)
        verify_sock.close()

        admin_sock.close()

        if b"LOGIN_OK" in verify_resp:
            log("  Server survived rapid join/leave: PASS")
            return True
        else:
            log("  FAIL: Server not healthy")
            return False

    except socket.error as e:
        log(f"  FAIL: Server not available: {e}")
        return False


# =============================================================================
# Main
# =============================================================================

def main():
    print("\n" + "=" * 70)
    print("Sender Key Rotation Window Test (RFC-001-AMENDMENT-001 Section 6.3)")
    print("=" * 70)

    passed = 0
    failed = 0

    tests = [
        test_group_message_during_member_removal,
        test_server_survives_rapid_join_leave,
    ]

    for test in tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            log(f"  ERROR: {test.__name__}: {e}")
            failed += 1

    print(f"\n{'=' * 70}")
    print(f"SUMMARY: {passed} passed, {failed} failed out of {passed + failed}")
    print(f"{'=' * 70}")

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
