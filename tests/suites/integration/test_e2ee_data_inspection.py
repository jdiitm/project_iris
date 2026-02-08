#!/usr/bin/env python3
"""
AUDIT FIX M18: E2EE Data Inspection Test

Sends an E2EE message (opcode 0x23) through the server and verifies
that the plaintext marker is NOT present in the relayed data. The server
must relay ciphertext opaquely without decrypting it.

This is an integration-level smoke test for E2EE. It does not verify
the full cryptographic protocol -- just that the server does not
transmit plaintext message bodies when using the E2EE path.

Tier: 1 (Integration)
"""

import time
import sys
import os
import struct
import socket
import ssl

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from utilities.iris_client import IrisClient

SERVER_HOST = os.environ.get('IRIS_HOST', 'localhost')
SERVER_PORT = int(os.environ.get('IRIS_PORT', '8085'))


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def cbor_encode_text(s):
    """Encode a UTF-8 string as CBOR text string (major type 3)."""
    data = s.encode('utf-8')
    length = len(data)
    if length < 24:
        return bytes([0x60 | length]) + data
    elif length < 256:
        return bytes([0x78, length]) + data
    else:
        return bytes([0x79]) + struct.pack('>H', length) + data


def cbor_encode_map(pairs):
    """Encode a list of (key, value) pairs as a CBOR map (major type 5).
    Keys and values must already be CBOR-encoded bytes."""
    n = len(pairs)
    if n < 24:
        header = bytes([0xa0 | n])
    else:
        header = bytes([0xb8, n])
    body = b''
    for k, v in pairs:
        body += k + v
    return header + body


def cbor_encode_bytes(b):
    """Encode raw bytes as CBOR byte string (major type 2)."""
    n = len(b)
    if n < 24:
        return bytes([0x40 | n]) + b
    elif n < 256:
        return bytes([0x58, n]) + b
    else:
        return bytes([0x59]) + struct.pack('>H', n) + b


def build_e2ee_header_cbor():
    """Build a minimal valid CBOR map for the E2EE header.
    Server validates structural integrity: required keys ik and ek must be
    present (RFC-001-AMENDMENT-001 Section 4.1). Values are dummy 32-byte
    keys since the server never decrypts — it only checks presence."""
    return cbor_encode_map([
        (cbor_encode_text("ik"), cbor_encode_bytes(b'\x00' * 32)),
        (cbor_encode_text("ek"), cbor_encode_bytes(b'\x00' * 32)),
    ])


def build_e2ee_packet(recipient, ciphertext):
    """Build opcode 0x23 E2EE message packet.
    Format: 0x23 | RecipientLen(16) | Recipient | HeaderLen(16) | Header(CBOR) | CipherLen(32) | Ciphertext
    """
    recipient_bytes = recipient.encode('utf-8')
    header_cbor = build_e2ee_header_cbor()
    return (bytes([0x23])
            + struct.pack('>H', len(recipient_bytes)) + recipient_bytes
            + struct.pack('>H', len(header_cbor)) + header_cbor
            + struct.pack('>I', len(ciphertext)) + ciphertext)


def test_e2ee_data_not_plaintext():
    """
    Send an E2EE message (opcode 0x23) with known "ciphertext" that is
    actually an encrypted blob. Verify the server relays it as-is via
    opcode 0x24, and that the PLAINTEXT marker (which was "encrypted"
    into the ciphertext) does NOT appear as plaintext in the delivery.

    The server must relay ciphertext opaquely.
    """
    log("=" * 60)
    log("TEST: E2EE data is not transmitted as plaintext")
    log("=" * 60)

    sender = None
    receiver = None

    try:
        ts = int(time.time())
        sender_name = f"e2ee_sender_{ts}"
        receiver_name = f"e2ee_receiver_{ts}"

        # Login receiver first so they're registered
        receiver = IrisClient(SERVER_HOST, SERVER_PORT)
        receiver.login(receiver_name)

        sender = IrisClient(SERVER_HOST, SERVER_PORT)
        sender.login(sender_name)

        # The plaintext we're "encrypting" -- must NOT appear in the relay
        plaintext_marker = b"SUPER_SECRET_PLAINTEXT_MARKER_e2ee_12345"

        # Simulate encryption: XOR with a repeating key (NOT real crypto,
        # but sufficient to verify the server doesn't inject plaintext)
        key = b'\xAB\xCD\xEF\x01\x23\x45\x67\x89'
        ciphertext = bytes([plaintext_marker[i] ^ key[i % len(key)]
                            for i in range(len(plaintext_marker))])

        # Sanity: ciphertext must NOT contain the plaintext
        assert plaintext_marker not in ciphertext, "XOR encryption failed sanity check"

        # Build and send E2EE packet (opcode 0x23)
        e2ee_packet = build_e2ee_packet(receiver_name, ciphertext)
        sender.sock.sendall(e2ee_packet)
        log(f"  Sent E2EE message ({len(e2ee_packet)} bytes) via opcode 0x23")

        time.sleep(1.0)

        # Receive raw data on receiver side
        try:
            receiver.sock.settimeout(5.0)
            raw_data = receiver.sock.recv(4096)
        except socket.timeout:
            log("  FAIL: No E2EE message received (timeout)")
            return False

        if raw_data is None or len(raw_data) == 0:
            log("  FAIL: No data received")
            return False

        log(f"  Received {len(raw_data)} bytes")

        # The server wraps deliveries in a reliable message envelope:
        # 0x11 | IdLen(16) | MsgId | MsgLen(32) | InnerPayload
        # The inner payload should start with 0x24 (E2EE delivery).
        payload = raw_data
        if raw_data[0] == 0x11:
            log(f"  Received opcode 0x11 (reliable wrapper) -- unwrapping")
            # Parse: 0x11 | IdLen(16) | MsgId(IdLen) | MsgLen(32) | Inner
            if len(raw_data) < 4:
                log(f"  FAIL: Reliable wrapper too short")
                return False
            id_len = struct.unpack('>H', raw_data[1:3])[0]
            header_size = 1 + 2 + id_len + 4  # opcode + idlen + id + msglen
            if len(raw_data) < header_size:
                log(f"  FAIL: Reliable wrapper truncated")
                return False
            payload = raw_data[header_size:]
            log(f"  Inner payload: {len(payload)} bytes, opcode 0x{payload[0]:02x}")

        # Verify inner opcode is 0x24 (E2EE delivery)
        if payload[0] == 0x24:
            log(f"  E2EE delivery opcode 0x24 confirmed")
        elif raw_data[0] == 0x24:
            log(f"  Direct E2EE delivery opcode 0x24 (no wrapper)")
        else:
            log(f"  Received opcode 0x{payload[0]:02x} -- expected 0x24")
            log(f"  FAIL: Server did not use E2EE delivery opcode")
            return False

        # CRITICAL ASSERTION: plaintext marker must NOT be in relayed data
        if plaintext_marker in raw_data:
            log(f"  FAIL: Plaintext marker found in relayed E2EE data!")
            log(f"  The server is transmitting plaintext instead of ciphertext")
            return False

        # Verify the ciphertext IS in the relayed data (server relayed it intact)
        if ciphertext in raw_data:
            log(f"  Ciphertext found intact in delivery -- server relayed opaquely")
            log(f"  PASS: E2EE data is NOT plaintext; ciphertext preserved")
            return True
        else:
            log(f"  WARNING: Ciphertext not found verbatim in delivery")
            log(f"  Server may have modified the E2EE payload")
            log(f"  FAIL: E2EE relay integrity broken")
            return False

    except Exception as e:
        log(f"  FAIL: Exception: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        if sender:
            sender.close()
        if receiver:
            receiver.close()


def main():
    print("\n" + "=" * 60)
    print("AUDIT FIX M18: E2EE DATA INSPECTION TEST")
    print("=" * 60)

    tests = [
        ("E2EE data not plaintext", test_e2ee_data_not_plaintext),
    ]

    passed = 0
    failed = 0

    for name, fn in tests:
        try:
            if fn():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"  ERROR: {name}: {e}")
            failed += 1

    print(f"\n{passed}/{passed + failed} tests passed")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
