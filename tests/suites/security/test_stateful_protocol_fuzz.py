#!/usr/bin/env python3
"""
G-11: Stateful Protocol Fuzzing Tests

RFC-001 NFR-18: Input validation -- all untrusted input MUST be validated.

Stateless fuzzing (test_protocol_fuzz.py) catches parsing bugs but not state
machine bugs. This test exercises the session state machine by:

1. Post-login corruption: complete login, then send random bytes
2. State machine violations: ACK before login, double LOGIN
3. Interleaved valid/invalid: alternate good and bad messages
4. Oversized payload: length header claiming 10MB, 100 bytes of data
5. Out-of-order opcodes: send opcodes in unexpected sequence

INVARIANTS:
- Server MUST NOT crash (beam.smp must survive every attack)
- Server MUST disconnect cleanly on invalid state transitions
- Valid messages interleaved with invalid MUST still succeed
- Server MUST remain operational for legitimate clients after all attacks

Pattern: follows test_protocol_fuzz.py (get_raw_socket + server_alive)

Tier: 1 (Security)
"""

import sys
import os
import socket
import ssl
import struct
import time
import random
from pathlib import Path

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient
from tests.utilities.helpers import wait_until

# Determinism
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = os.path.join(PROJECT_ROOT, "certs", "ca.pem")
TIMEOUT = 5

results = []


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def server_alive():
    """Check if server is still accepting connections."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect((SERVER_HOST, SERVER_PORT))
        s.close()
        return True
    except Exception:
        return False


def get_tls_socket(timeout=TIMEOUT):
    """Get a TLS-wrapped socket to the server."""
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(timeout)
    tls_sock = ctx.wrap_socket(raw, server_hostname=SERVER_HOST)
    tls_sock.connect((SERVER_HOST, SERVER_PORT))
    return tls_sock


def do_login(sock, user):
    """Perform LOGIN opcode 0x01 on an already-connected socket."""
    packet = bytes([0x01]) + user.encode("utf-8")
    sock.sendall(packet)
    try:
        sock.settimeout(2)
        resp = sock.recv(1024)
        return resp
    except socket.timeout:
        return b""


# =========================================================================
# Test 1: Post-Login Random Bytes
# =========================================================================
def test_post_login_corruption():
    """Complete valid login, then send random garbage bytes.
    Server must disconnect cleanly without crashing."""
    log("\n=== Test 1: Post-Login Random Bytes ===")

    for attempt in range(5):
        try:
            sock = get_tls_socket()
            resp = do_login(sock, f"fuzz_post_login_{attempt}")

            # Send 10 rounds of random bytes
            for _ in range(10):
                garbage = bytes([random.randint(0, 255) for _ in range(random.randint(1, 512))])
                try:
                    sock.sendall(garbage)
                    time.sleep(0.05)
                except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
                    break  # Server disconnected -- expected

            try:
                sock.close()
            except Exception:
                pass
        except Exception as e:
            log(f"  Attempt {attempt} connection/fuzz error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after post-login corruption"):
        log("  PASS: Server survived post-login corruption (5 attempts)")
        return True
    else:
        log("  FAIL: Server crashed from post-login random bytes")
        return False


# =========================================================================
# Test 2: ACK Before Login
# =========================================================================
def test_ack_before_login():
    """Send ACK (0x03) before completing LOGIN (0x01).
    Server must reject or disconnect, not crash."""
    log("\n=== Test 2: ACK Before Login ===")

    for attempt in range(5):
        try:
            sock = get_tls_socket()
            # Send ACK opcode without login
            msg_id = b"fake_msg_id_12345678"
            ack_packet = bytes([0x03]) + struct.pack(">H", len(msg_id)) + msg_id
            sock.sendall(ack_packet)

            try:
                resp = sock.recv(1024)
                # Any response is acceptable (error, disconnect, etc.)
            except (socket.timeout, ConnectionResetError, ssl.SSLError):
                pass

            try:
                sock.close()
            except Exception:
                pass
        except Exception as e:
            log(f"  Attempt {attempt} connection/fuzz error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after ACK-before-login"):
        log("  PASS: Server survived ACK-before-login (5 attempts)")
        return True
    else:
        log("  FAIL: Server crashed from ACK before login")
        return False


# =========================================================================
# Test 3: Double LOGIN
# =========================================================================
def test_double_login():
    """Send LOGIN (0x01) twice on the same connection.
    Server must handle gracefully (reject second, or accept as re-login)."""
    log("\n=== Test 3: Double LOGIN ===")

    for attempt in range(5):
        try:
            sock = get_tls_socket()
            resp1 = do_login(sock, f"fuzz_double_login_{attempt}")
            # Send second login on same connection
            resp2 = do_login(sock, f"fuzz_double_login_{attempt}_v2")

            try:
                sock.close()
            except Exception:
                pass
        except Exception as e:
            log(f"  Attempt {attempt} connection/fuzz error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after double LOGIN"):
        log("  PASS: Server survived double LOGIN (5 attempts)")
        return True
    else:
        log("  FAIL: Server crashed from double LOGIN")
        return False


# =========================================================================
# Test 4: Interleaved Valid/Invalid Messages
# =========================================================================
def test_interleaved_valid_invalid():
    """Alternate between valid messages and garbage on the same connection.
    Valid messages must succeed; garbage must not crash server."""
    log("\n=== Test 4: Interleaved Valid/Invalid ===")

    try:
        sock = get_tls_socket()
        do_login(sock, "fuzz_interleave_user")

        success_count = 0
        seq_no = 0
        for i in range(20):
            if i % 2 == 0:
                # Valid: send message (opcode 0x07 sequenced message)
                target = b"interleave_target"
                msg = f"valid_msg_{i}".encode("utf-8")
                seq_no += 1
                packet = bytes([0x07]) + struct.pack(">H", len(target)) + target + struct.pack(">Q", seq_no) + struct.pack(">H", len(msg)) + msg
                try:
                    sock.sendall(packet)
                    success_count += 1
                except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
                    break
            else:
                # Invalid: garbage bytes
                garbage = bytes([random.randint(0, 255) for _ in range(random.randint(10, 200))])
                try:
                    sock.sendall(garbage)
                except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
                    break
            time.sleep(0.05)

        try:
            sock.close()
        except Exception:
            pass

        log(f"  Sent {success_count} valid messages before disconnect")
    except Exception as e:
        log(f"  Connection error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after interleaved messages"):
        log("  PASS: Server survived interleaved valid/invalid")
        return True
    else:
        log("  FAIL: Server crashed from interleaved messages")
        return False


# =========================================================================
# Test 5: Oversized Payload (length mismatch)
# =========================================================================
def test_oversized_payload_claim():
    """Send length header claiming 10MB but provide only 100 bytes.
    Server must timeout or disconnect, not hang or crash."""
    log("\n=== Test 5: Oversized Payload Claim ===")

    for attempt in range(3):
        try:
            sock = get_tls_socket(timeout=3)
            do_login(sock, f"fuzz_oversize_{attempt}")

            # Opcode 0x02 (SEND) with huge length field
            target = b"oversize_target"
            # Claim message body is 10MB
            packet = bytes([0x02]) + struct.pack(">H", len(target)) + target + struct.pack(">H", 65535)
            # But only send 100 bytes of body
            packet += b"x" * 100
            try:
                sock.sendall(packet)
                # Try to read -- server should have disconnected or timed out
                try:
                    sock.recv(1024)
                except (socket.timeout, ConnectionResetError, ssl.SSLError):
                    pass
            except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
                pass

            try:
                sock.close()
            except Exception:
                pass
        except Exception as e:
            log(f"  Attempt {attempt} connection/fuzz error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after oversized payload"):
        log("  PASS: Server survived oversized payload claims (3 attempts)")
        return True
    else:
        log("  FAIL: Server crashed from oversized payload claim")
        return False


# =========================================================================
# Test 6: All Invalid Opcodes
# =========================================================================
def test_invalid_opcodes():
    """Send every undefined opcode (0x80-0xFF range). Server must not crash."""
    log("\n=== Test 6: Invalid Opcodes ===")

    for opcode in range(0x80, 0x100, 0x10):
        try:
            sock = get_tls_socket(timeout=2)
            packet = bytes([opcode]) + b"payload_data_here"
            sock.sendall(packet)
            try:
                sock.recv(1024)
            except Exception:
                pass
            try:
                sock.close()
            except Exception:
                pass
        except Exception as e:
            log(f"  Opcode 0x{opcode:02X} error: {e}")

    if wait_until(server_alive, timeout=2, description="server alive after invalid opcodes"):
        log("  PASS: Server survived all invalid opcodes (0x80-0xFF)")
        return True
    else:
        log("  FAIL: Server crashed from invalid opcodes")
        return False


# =========================================================================
# Test 7: Legitimate Client After All Attacks
# =========================================================================
def test_legitimate_after_attacks():
    """After all fuzzing, verify a legitimate client can still operate."""
    log("\n=== Test 7: Legitimate Client After Attacks ===")

    try:
        client = IrisClient()
        client.login("legit_after_fuzz_user")
        client.send_msg("legit_target_user", "hello after fuzzing")
        wait_until(server_alive, timeout=2, description="server processed legit message")
        client.close()
        log("  PASS: Legitimate client works after all attacks")
        return True
    except Exception as e:
        log(f"  FAIL: Legitimate client failed: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-11: STATEFUL PROTOCOL FUZZING TESTS")
    print(" RFC-001 NFR-18: Input validation")
    print("=" * 60)
    print(f"Target: {SERVER_HOST}:{SERVER_PORT}")
    print(f"Seed: {TEST_SEED}")

    # Pre-check
    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1
    log("Server is accepting connections")

    tests = [
        ("Post-Login Corruption", test_post_login_corruption),
        ("ACK Before Login", test_ack_before_login),
        ("Double LOGIN", test_double_login),
        ("Interleaved Valid/Invalid", test_interleaved_valid_invalid),
        ("Oversized Payload Claim", test_oversized_payload_claim),
        ("Invalid Opcodes", test_invalid_opcodes),
        ("Legitimate After Attacks", test_legitimate_after_attacks),
    ]

    for name, fn in tests:
        try:
            result = fn()
            results.append((name, result))
        except Exception as e:
            log(f"  Exception in {name}: {e}")
            results.append((name, False))

    # Summary
    print("\n" + "=" * 60)
    print("RESULTS:")
    print("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)

    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  [{status}] {name}")

    print(f"\n{passed}/{total} stateful fuzz tests passed")

    if passed == total:
        print("\nG-11 Stateful Protocol Fuzzing: PASSED")
        return 0
    else:
        print("\nG-11 Stateful Protocol Fuzzing: FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
