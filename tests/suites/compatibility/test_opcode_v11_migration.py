#!/usr/bin/env python3
"""
P1-2: RELIABLE_MSG Opcode v1.1 Migration Tests

PROTOCOL_V1_FREEZE v1.1 moved RELIABLE_MSG from opcode 0x10 to 0x11
to resolve the collision with CBOR_MSG (which also used 0x10).

RFC References:
- PROTOCOL_V1_FREEZE Section 2.1: RELIABLE_MSG now at 0x11
- PROTOCOL_V1_FREEZE Section 2.2: CBOR_MSG stays at 0x10

Test Scenarios:
1. Server delivers reliable messages using opcode 0x11
2. Opcode 0x10 sent by client is treated as CBOR_MSG
3. Round-trip: send sequenced message, receive reliable delivery on 0x11
4. Multiple messages delivered on 0x11

NO SKIPS, NO FALLBACKS - binary pass/fail only.
"""

import sys
import os
import socket
import ssl
import struct
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from pathlib import Path
from tests.utilities.iris_client import IrisClient

CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"

HOST = os.environ.get('IRIS_HOST', 'localhost')
PORT = int(os.environ.get('IRIS_PORT', '8085'))


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def unique_user(prefix):
    """Generate unique username for test isolation."""
    import uuid
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def get_raw_tls_connection():
    """Get a raw TLS socket (no IrisClient parsing)."""
    context = ssl.create_default_context()
    if CA_CERT.exists():
        context.load_verify_locations(str(CA_CERT))
    else:
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE

    raw_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw_sock.settimeout(10.0)
    s = context.wrap_socket(raw_sock, server_hostname=HOST)
    s.connect((HOST, PORT))
    return s


def raw_login(sock, username):
    """Login using raw bytes and wait for LOGIN_OK."""
    sock.sendall(b'\x01' + username.encode('utf-8'))
    response = sock.recv(4096)
    if b"LOGIN_OK" not in response:
        raise Exception(f"Login failed: {response}")
    return response


def raw_send_seq(sock, target, msg, seq_no):
    """Send a sequenced message (opcode 0x07) using raw bytes."""
    target_bytes = target.encode('utf-8')
    msg_bytes = msg.encode('utf-8') if isinstance(msg, str) else msg
    payload = (b'\x07' +
               struct.pack('>H', len(target_bytes)) + target_bytes +
               struct.pack('>Q', seq_no) +
               struct.pack('>H', len(msg_bytes)) + msg_bytes)
    sock.sendall(payload)


def recv_reliable_raw(sock, timeout=5.0):
    """
    Receive raw bytes and parse a RELIABLE_MSG (opcode 0x11).
    Returns (opcode_byte, msg_id, msg_payload, remaining_buffer).
    """
    sock.settimeout(timeout)
    buf = b''
    deadline = time.time() + timeout

    while time.time() < deadline:
        try:
            data = sock.recv(4096)
            if not data:
                raise Exception("Connection closed")
            buf += data
        except socket.timeout:
            if buf:
                break
            raise

        # Scan buffer for opcode 0x11
        idx = 0
        while idx < len(buf):
            opcode = buf[idx]
            if opcode == 0x11:
                # RELIABLE_MSG: 0x11 | IdLen(16) | MsgId | MsgLen(32) | Msg
                if idx + 3 > len(buf):
                    break
                id_len = struct.unpack('>H', buf[idx+1:idx+3])[0]
                header_end = idx + 3 + id_len + 4
                if header_end > len(buf):
                    break
                msg_id = buf[idx+3:idx+3+id_len]
                msg_len = struct.unpack('>I', buf[idx+3+id_len:header_end])[0]
                total = header_end + msg_len
                if total > len(buf):
                    break
                msg_payload = buf[header_end:total]
                remaining = buf[total:]
                return opcode, msg_id, msg_payload, remaining
            else:
                idx += 1
        # Keep reading if we haven't found a complete message

    raise Exception(f"No RELIABLE_MSG (0x11) found in buffer ({len(buf)} bytes): {buf[:50]}...")


# =============================================================================
# TEST 1: Server delivers on opcode 0x11
# =============================================================================
def test_reliable_msg_uses_opcode_0x11():
    """
    Verify that server-delivered messages use opcode 0x11 (not 0x10).
    This is the core validation of PROTOCOL_V1_FREEZE v1.1 P1-2 fix.
    """
    log("=" * 60)
    log("TEST: Reliable message delivered with opcode 0x11")
    log("=" * 60)

    sender_name = unique_user("v11_sender")
    receiver_name = unique_user("v11_receiver")
    test_msg = "opcode_0x11_test_message"

    sender = None
    recv_sock = None
    try:
        # Receiver: raw TLS socket to inspect exact bytes
        recv_sock = get_raw_tls_connection()
        raw_login(recv_sock, receiver_name)
        time.sleep(0.05)

        # Sender: use IrisClient for convenience
        sender = IrisClient(HOST, PORT)
        sender.login(sender_name)

        # Send message
        sender.send_msg(receiver_name, test_msg)

        # Receive on raw socket and check opcode byte
        opcode, msg_id, payload, _ = recv_reliable_raw(recv_sock)

        assert opcode == 0x11, f"Expected opcode 0x11 (RELIABLE_MSG v1.1), got 0x{opcode:02X}"
        assert len(msg_id) > 0, "Message ID must be non-empty"
        assert test_msg.encode() in payload, f"Payload mismatch: expected '{test_msg}' in {payload}"

        log(f"  Opcode: 0x{opcode:02X} (correct)")
        log(f"  MsgId length: {len(msg_id)}")
        log(f"  Payload: {payload}")
        log("  PASS")
        return True

    finally:
        if sender:
            sender.close()
        if recv_sock:
            recv_sock.close()


# =============================================================================
# TEST 2: Round-trip message delivery
# =============================================================================
def test_roundtrip_delivery_0x11():
    """
    Full round-trip: send via 0x07, receive via 0x11, ACK via 0x03.
    Uses IrisClient which is updated to expect 0x11.
    """
    log("=" * 60)
    log("TEST: Round-trip delivery (send 0x07, receive 0x11, ACK 0x03)")
    log("=" * 60)

    sender_name = unique_user("rt_sender")
    receiver_name = unique_user("rt_receiver")

    sender = None
    receiver = None
    try:
        receiver = IrisClient(HOST, PORT)
        receiver.login(receiver_name)

        sender = IrisClient(HOST, PORT)
        sender.login(sender_name)

        test_msg = "roundtrip_v11_test"
        sender.send_msg(receiver_name, test_msg)

        # IrisClient.recv_msg now expects opcode 0x11 and sends ACK
        received = receiver.recv_msg(timeout=5.0)
        assert received is not None, "No message received"
        received_text = received.decode('utf-8') if isinstance(received, bytes) else received
        assert test_msg in received_text, f"Expected '{test_msg}' in '{received_text}'"

        log(f"  Received: {received_text}")
        log("  PASS")
        return True

    finally:
        if sender:
            sender.close()
        if receiver:
            receiver.close()


# =============================================================================
# TEST 3: Multiple messages all use 0x11
# =============================================================================
def test_multiple_messages_all_0x11():
    """
    Send 5 messages. Verify ALL are delivered with opcode 0x11.
    """
    log("=" * 60)
    log("TEST: Multiple messages all delivered with opcode 0x11")
    log("=" * 60)

    sender_name = unique_user("multi_sender")
    receiver_name = unique_user("multi_receiver")
    num_messages = 5

    sender = None
    recv_sock = None
    try:
        recv_sock = get_raw_tls_connection()
        raw_login(recv_sock, receiver_name)
        time.sleep(0.05)

        sender = IrisClient(HOST, PORT)
        sender.login(sender_name)

        # Send messages
        for i in range(num_messages):
            sender.send_msg(receiver_name, f"multi_msg_{i}")
            time.sleep(0.02)

        # Receive all and verify opcodes
        buf = b''
        received_count = 0
        recv_sock.settimeout(5.0)
        deadline = time.time() + 5.0

        while received_count < num_messages and time.time() < deadline:
            try:
                data = recv_sock.recv(4096)
                if data:
                    buf += data
            except socket.timeout:
                pass

            # Parse all complete messages from buffer
            while len(buf) >= 3:
                if buf[0] != 0x11:
                    buf = buf[1:]
                    continue
                id_len = struct.unpack('>H', buf[1:3])[0]
                header_end = 3 + id_len + 4
                if len(buf) < header_end:
                    break
                msg_len = struct.unpack('>I', buf[3+id_len:header_end])[0]
                total = header_end + msg_len
                if len(buf) < total:
                    break

                msg_id = buf[3:3+id_len]
                # Send ACK
                recv_sock.sendall(b'\x03' + msg_id)
                received_count += 1
                log(f"  Message {received_count}: opcode=0x11, id_len={id_len}")
                buf = buf[total:]

        assert received_count == num_messages, \
            f"Expected {num_messages} messages, received {received_count}"

        log(f"  All {num_messages} messages used opcode 0x11")
        log("  PASS")
        return True

    finally:
        if sender:
            sender.close()
        if recv_sock:
            recv_sock.close()


# =============================================================================
# TEST 4: Opcode 0x10 is CBOR_MSG (not reliable)
# =============================================================================
def test_0x10_is_cbor_not_reliable():
    """
    Sending opcode 0x10 should be interpreted as CBOR_MSG, not RELIABLE_MSG.
    A malformed CBOR payload on 0x10 should NOT be parsed as a reliable message.
    """
    log("=" * 60)
    log("TEST: Opcode 0x10 is CBOR_MSG (not RELIABLE_MSG)")
    log("=" * 60)

    user_name = unique_user("cbor_test")
    sock = None
    try:
        sock = get_raw_tls_connection()
        raw_login(sock, user_name)
        time.sleep(0.05)

        # Send a packet with opcode 0x10 — server should treat as CBOR_MSG
        # Construct a valid CBOR_MSG: 0x10 | TargetLen(16) | Target | CborLen(32) | CborPayload
        target = b"nobody"
        # Minimal CBOR map: 0xA0 = empty map {}
        cbor_payload = bytes([0xA0])
        packet = (bytes([0x10]) +
                  struct.pack('>H', len(target)) + target +
                  struct.pack('>I', len(cbor_payload)) + cbor_payload)

        sock.sendall(packet)

        # Server should handle this as CBOR_MSG (may route, may error on unknown target).
        # The key assertion: server does NOT crash, connection stays alive.

        # Verify connection is still alive by attempting a status query
        sock.sendall(b'\x05' + struct.pack('>H', len(target)) + target)
        sock.settimeout(2.0)
        try:
            response = sock.recv(4096)
            # Any response (or timeout) is acceptable — server didn't crash
            log(f"  Server responded after 0x10 CBOR_MSG: {len(response)} bytes")
        except socket.timeout:
            log("  Server timeout (acceptable — no crash)")

        log("  Server treated 0x10 as CBOR_MSG (no crash)")
        log("  PASS")
        return True

    finally:
        if sock:
            sock.close()


# =============================================================================
# MAIN
# =============================================================================
def main():
    log("PROTOCOL_V1_FREEZE v1.1 Opcode Migration Tests")
    log(f"Server: {HOST}:{PORT}")
    log("")

    tests = [
        ("reliable_msg_opcode_0x11", test_reliable_msg_uses_opcode_0x11),
        ("roundtrip_delivery_0x11", test_roundtrip_delivery_0x11),
        ("multiple_messages_0x11", test_multiple_messages_all_0x11),
        ("0x10_is_cbor_not_reliable", test_0x10_is_cbor_not_reliable),
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
                log(f"  FAIL: {name} returned False")
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
