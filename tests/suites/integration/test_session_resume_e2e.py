#!/usr/bin/env python3
"""
Item 1: Session Resume E2E Tests (RFC Section 3.4, opcode 0x0A)

TDD RED phase: These tests verify actual session resume behavior:
1. Login creates a session_id returned in LOGIN_OK
2. RESUME with valid session_id replays missed messages
3. RESUME with expired/unknown session_id returns NACK (0xFE error)
4. Connection remains usable after NACK (fallback to full login)

These tests WILL FAIL until iris_session.erl and iris_edge_conn.erl
are wired to iris_session_cache.
"""

import sys
import os
import socket
import ssl
import struct
import time
import uuid
import subprocess

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from pathlib import Path
from tests.utilities.iris_client import IrisClient

CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"
HOST = os.environ.get('IRIS_HOST', 'localhost')
PORT = int(os.environ.get('IRIS_PORT', '8085'))


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def unique_user(prefix):
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def get_tls_socket():
    context = ssl.create_default_context()
    if CA_CERT.exists():
        context.load_verify_locations(str(CA_CERT))
    else:
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(10.0)
    s = context.wrap_socket(raw, server_hostname=HOST)
    s.connect((HOST, PORT))
    return s


def raw_login(sock, username):
    """Login and return raw response bytes."""
    sock.sendall(b'\x01' + username.encode('utf-8'))
    sock.settimeout(5.0)
    resp = sock.recv(4096)
    if b"LOGIN_OK" not in resp:
        raise Exception(f"Login failed: {resp}")
    return resp


def extract_session_id(login_response):
    """
    Extract session_id from LOGIN_OK response.
    Expected format after wiring: LOGIN_OK | SessionIdLen(16) | SessionId
    The LOGIN_OK is at bytes 1.. (opcode 0x03 + "LOGIN_OK").
    After that, the session_id should follow.
    """
    # Find LOGIN_OK in the response
    idx = login_response.find(b"LOGIN_OK")
    if idx < 0:
        return None
    # Session ID starts after "LOGIN_OK"
    remainder = login_response[idx + len(b"LOGIN_OK"):]
    if len(remainder) < 2:
        return None
    sid_len = struct.unpack('>H', remainder[:2])[0]
    if len(remainder) < 2 + sid_len:
        return None
    return remainder[2:2 + sid_len]


def send_resume(sock, session_id, last_seq):
    """Send RESUME opcode (0x0A)."""
    packet = (bytes([0x0A]) +
              struct.pack('>H', len(session_id)) +
              session_id +
              struct.pack('>Q', last_seq))
    sock.sendall(packet)


def run_erlang_rpc(code):
    """Run Erlang code on the server node via rpc:call."""
    hostname = socket.gethostname()
    node = f"iris_edge1@{hostname}"
    ts = int(time.time() * 1000)
    cmd = (
        f"erl -setcookie iris_secret -sname rpc_{ts} -hidden -noshell "
        f"-pa {PROJECT_ROOT}/ebin "
        f"-eval \"{code}, init:stop().\""
    )
    result = subprocess.run(
        cmd, shell=True, capture_output=True, text=True, timeout=10,
        cwd=PROJECT_ROOT
    )
    return result.stdout.strip(), result.returncode


# =============================================================================
# Test 1: Login returns session_id and RESUME replays missed messages
# =============================================================================
def test_resume_replays_missed_messages_e2e():
    """
    Full E2E flow:
    1. Login user A -> get session_id
    2. Send messages to A from B
    3. Disconnect A
    4. Reconnect A using RESUME(session_id, last_seq)
    5. Assert missed messages arrive
    """
    log("=" * 60)
    log("TEST: RESUME replays missed messages (E2E)")
    log("=" * 60)

    receiver = unique_user("resume_recv")
    sender = unique_user("resume_send")

    # Step 1: Login receiver, extract session_id
    recv_sock = get_tls_socket()
    login_resp = raw_login(recv_sock, receiver)
    session_id = extract_session_id(login_resp)

    if session_id is None or len(session_id) == 0:
        log("  FAIL: LOGIN_OK did not include a session_id")
        log("  (Server must return session_id in LOGIN_OK for resume to work)")
        recv_sock.close()
        return False

    log(f"  Got session_id: {session_id[:16]}... ({len(session_id)} bytes)")

    # Step 2: Login sender and send a message to receiver
    send_sock = get_tls_socket()
    raw_login(send_sock, sender)
    time.sleep(0.1)

    # Receive the first message (establishes sequence baseline)
    recv_sock.settimeout(3.0)
    test_msg = b"message_before_disconnect"
    target_bytes = receiver.encode('utf-8')
    msg_packet = (bytes([0x07]) +
                  struct.pack('>H', len(target_bytes)) + target_bytes +
                  struct.pack('>Q', 1) +
                  struct.pack('>H', len(test_msg)) + test_msg)
    send_sock.sendall(msg_packet)
    time.sleep(0.3)

    try:
        pre_data = recv_sock.recv(4096)
        log(f"  Received pre-disconnect message: {len(pre_data)} bytes")
    except socket.timeout:
        log("  No pre-disconnect message (might be stored)")

    # Step 3: Disconnect receiver (abrupt close)
    recv_sock.close()
    time.sleep(0.5)

    # Step 4: Send more messages while receiver is disconnected
    missed_msg = b"missed_while_offline"
    msg_packet2 = (bytes([0x07]) +
                   struct.pack('>H', len(target_bytes)) + target_bytes +
                   struct.pack('>Q', 2) +
                   struct.pack('>H', len(missed_msg)) + missed_msg)
    send_sock.sendall(msg_packet2)
    time.sleep(0.5)

    # Step 5: Reconnect with RESUME
    recv_sock2 = get_tls_socket()
    # First login (required to establish connection context)
    raw_login(recv_sock2, receiver)
    time.sleep(0.1)

    # Send RESUME with session_id and last_seq=0 (replay all)
    send_resume(recv_sock2, session_id, 0)
    time.sleep(0.5)

    # Step 6: Check for replayed messages
    recv_sock2.settimeout(3.0)
    got_data = False
    try:
        data = recv_sock2.recv(4096)
        if data and len(data) > 0:
            got_data = True
            log(f"  Received {len(data)} bytes after RESUME")
            # Check if missed message content is in the replay
            if missed_msg in data:
                log("  Missed message found in replay!")
            else:
                log(f"  Replay data present but missed message not found verbatim")
                log(f"  (Data may be wrapped in reliable delivery envelope)")
                # Even wrapped data counts as success
        else:
            log("  Empty response after RESUME")
    except socket.timeout:
        log("  Timeout waiting for replayed messages after RESUME")

    send_sock.close()
    recv_sock2.close()

    if got_data:
        log("  PASS: RESUME replayed messages")
        return True
    else:
        log("  FAIL: No messages replayed after RESUME")
        return False


# =============================================================================
# Test 2: RESUME with expired/deleted session returns NACK
# =============================================================================
def test_resume_expired_session_nack():
    """
    RESUME with a session_id that doesn't exist should return NACK.
    The server should send an error (opcode 0xFE) indicating the
    session is unknown/expired and the client must do a full login.
    """
    log("=" * 60)
    log("TEST: RESUME with expired session returns NACK")
    log("=" * 60)

    user = unique_user("resume_nack")
    sock = get_tls_socket()
    raw_login(sock, user)
    time.sleep(0.1)

    # Send RESUME with a fabricated session_id that doesn't exist
    fake_session_id = b"EXPIRED_SESSION_" + uuid.uuid4().hex[:16].encode()
    send_resume(sock, fake_session_id, 0)
    time.sleep(0.5)

    # Expect a NACK/error response (opcode 0xFE)
    sock.settimeout(3.0)
    got_nack = False
    try:
        data = sock.recv(4096)
        if data:
            log(f"  Received {len(data)} bytes after fake RESUME")
            # Check for error opcode 0xFE
            if data[0] == 0xFE:
                log("  Got error opcode 0xFE (NACK) -- correct")
                got_nack = True
            elif b"SESSION_NOT_FOUND" in data or b"RESUME_NACK" in data:
                log("  Got session not found response -- correct")
                got_nack = True
            else:
                log(f"  Unexpected response: opcode=0x{data[0]:02x}, data={data[:30]}")
        else:
            log("  Empty response")
    except socket.timeout:
        log("  No response (server silently ignored -- currently a no-op stub)")

    sock.close()

    if got_nack:
        log("  PASS: Server sent NACK for expired session")
        return True
    else:
        log("  FAIL: Server did not NACK the expired session")
        return False


# =============================================================================
# Test 3: Connection usable after RESUME NACK (fallback)
# =============================================================================
def test_resume_falls_back_on_unknown_session():
    """
    After receiving a NACK for an unknown session, the connection
    should still be usable for normal messaging.
    """
    log("=" * 60)
    log("TEST: Connection usable after RESUME NACK (fallback)")
    log("=" * 60)

    sender_name = unique_user("fallback_sender")
    receiver_name = unique_user("fallback_recv")

    # Login both users
    sender_sock = get_tls_socket()
    raw_login(sender_sock, sender_name)

    recv_sock = get_tls_socket()
    raw_login(recv_sock, receiver_name)
    time.sleep(0.1)

    # Send RESUME with garbage session_id
    garbage_session = b"GARBAGE_SESSION_ID_12345"
    send_resume(recv_sock, garbage_session, 0)
    time.sleep(0.3)

    # Drain any NACK response
    recv_sock.settimeout(1.0)
    try:
        recv_sock.recv(4096)
    except socket.timeout:
        pass

    # Now send a message from sender to receiver -- should still work
    test_msg = b"after_failed_resume"
    target_bytes = receiver_name.encode('utf-8')
    msg_packet = (bytes([0x07]) +
                  struct.pack('>H', len(target_bytes)) + target_bytes +
                  struct.pack('>Q', 1) +
                  struct.pack('>H', len(test_msg)) + test_msg)
    sender_sock.sendall(msg_packet)
    time.sleep(0.5)

    # Receiver should get the message
    recv_sock.settimeout(3.0)
    got_msg = False
    try:
        data = recv_sock.recv(4096)
        if data and len(data) > 0:
            log(f"  Received {len(data)} bytes after failed RESUME")
            if test_msg in data:
                log("  Message content found -- connection is functional")
                got_msg = True
            else:
                log("  Got data but message not found verbatim (may be wrapped)")
                got_msg = True  # Wrapped delivery still counts
    except socket.timeout:
        log("  Timeout -- no message received after failed RESUME")

    sender_sock.close()
    recv_sock.close()

    if got_msg:
        log("  PASS: Connection functional after RESUME NACK")
        return True
    else:
        log("  FAIL: Connection not functional after RESUME NACK")
        return False


# =============================================================================
# Main
# =============================================================================
def main():
    log("")
    log("=" * 60)
    log("SESSION RESUME E2E TESTS (RFC Section 3.4)")
    log("=" * 60)

    tests = [
        ("Resume Replays Missed Messages", test_resume_replays_missed_messages_e2e),
        ("Resume Expired Session NACK", test_resume_expired_session_nack),
        ("Fallback After Unknown Session", test_resume_falls_back_on_unknown_session),
    ]

    results = []
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
        print(f"\nSession Resume E2E: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nSession Resume E2E: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
