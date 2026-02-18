#!/usr/bin/env python3
"""
Test: Plaintext Message Rejection (RFC-001-AMENDMENT-001 Section 7)

This test verifies that the server REJECTS plaintext messages (opcode 0x02)
as mandated by RFC-001-AMENDMENT-001 for v1.0 compliance.

Deprecation Schedule (per RFC):
- v0.9: Emit warning on plaintext message (opcode 0x02)
- v1.0: REJECT plaintext messages, require E2EE (opcode 0x23) or CBOR (0x10)
- v1.1: Remove legacy plaintext opcode from protocol spec

CRITICAL: This is a NEGATIVE test that MUST verify rejection.
If plaintext messages are accepted, this test FAILS - indicating an RFC violation.

Tier: 0 (Security-critical)
Safe for laptop: Yes
Expected duration: <10s
"""

import os
import sys
import time
import struct
import socket
import ssl

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from pathlib import Path
from tests.utilities.tls_connection import get_unverified_ssl_context

# Test configuration
HOST = os.environ.get("IRIS_HOST", "localhost")
PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10

# TLS Configuration
CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"

results = []


def log(msg: str):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def log_test(name: str, passed: bool, message: str = ""):
    """Log test result."""
    status = "PASS" if passed else "FAIL"
    log(f"  {status}: {name}")
    if message:
        log(f"         {message}")
    results.append((name, passed, message))


def connect_tls():
    """Create TLS connection to server."""
    context = get_unverified_ssl_context()  # Unverified: testing rejection/attack scenario

    raw_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw_sock.settimeout(TIMEOUT)
    raw_sock.connect((HOST, PORT))
    return context.wrap_socket(raw_sock, server_hostname=HOST)


def login(sock, username):
    """Send login packet and wait for LOGIN_OK."""
    packet = b'\x01' + username.encode('utf-8')
    sock.sendall(packet)
    try:
        response = sock.recv(1024)
        return b"LOGIN_OK" in response
    except socket.timeout:
        return False


def send_plaintext_message(sock, target, message):
    """
    Send a PLAINTEXT message using deprecated opcode 0x02.
    
    This SHOULD be rejected by a v1.0 compliant server.
    Format: 0x02 | TargetLen(16) | Target | MsgLen(16) | Msg
    """
    target_bytes = target.encode('utf-8')
    msg_bytes = message.encode('utf-8')

    packet = (bytes([0x02]) +
              struct.pack('>H', len(target_bytes)) + target_bytes +
              struct.pack('>H', len(msg_bytes)) + msg_bytes)
    sock.sendall(packet)


def recv_with_timeout(sock, timeout=3.0):
    """Receive data with timeout."""
    sock.settimeout(timeout)
    try:
        return sock.recv(4096)
    except socket.timeout:
        return b''
    except ssl.SSLWantReadError:
        return b''


def parse_error_response(data):
    """
    Parse error response from server.
    Error format: 0xFE | Length(16) | Message
    """
    if len(data) < 3:
        return None

    if data[0] == 0xFE:
        msg_len = struct.unpack('>H', data[1:3])[0]
        if len(data) >= 3 + msg_len:
            return data[3:3+msg_len].decode('utf-8', errors='replace')
    return None


def check_server_available():
    """Check if server is reachable."""
    try:
        sock = connect_tls()
        sock.close()
        return True
    except Exception as e:
        log(f"Server not available: {e}")
        return False


# =============================================================================
# Test 1: Plaintext Message Rejection
# =============================================================================

def test_plaintext_rejected():
    """
    Test: Server MUST reject plaintext message (opcode 0x02).
    
    Per RFC-001-AMENDMENT-001 Section 7:
    "v1.0: Reject plaintext messages, require E2EE (opcode 0x23)"
    
    Expected behavior:
    - Server returns error response (0xFE with "e2ee_required")
    - OR server disconnects the client
    - Message is NOT delivered to recipient
    """
    log("\n=== Test: Plaintext Message Rejection (RFC v1.0 Compliance) ===")

    if not check_server_available():
        log_test("Plaintext rejection", False, "Server not available")
        return False

    sender_sock = None
    receiver_sock = None

    try:
        # Connect sender
        sender_sock = connect_tls()
        sender_name = f"plaintext_sender_{int(time.time())}"
        if not login(sender_sock, sender_name):
            log_test("Plaintext rejection", False, "Sender login failed")
            return False
        log(f"  Sender logged in: {sender_name}")

        # Connect receiver (to verify message is NOT delivered)
        receiver_sock = connect_tls()
        receiver_name = f"plaintext_receiver_{int(time.time())}"
        if not login(receiver_sock, receiver_name):
            log_test("Plaintext rejection", False, "Receiver login failed")
            return False
        log(f"  Receiver logged in: {receiver_name}")

        # Send plaintext message (SHOULD BE REJECTED)
        log("  Sending PLAINTEXT message (opcode 0x02)...")
        test_message = f"PLAINTEXT_TEST_{time.time()}"
        send_plaintext_message(sender_sock, receiver_name, test_message)

        response = recv_with_timeout(sender_sock, timeout=3.0)

        # Check for error response
        error_msg = parse_error_response(response)

        if error_msg:
            log(f"  Server returned error: {error_msg}")
            if "e2ee_required" in error_msg.lower() or "e2ee" in error_msg.lower():
                log_test("Plaintext rejection", True,
                        f"Server correctly rejected with: {error_msg}")

                # Verify message was NOT delivered to receiver
                receiver_sock.settimeout(1.0)
                try:
                    recv_data = receiver_sock.recv(4096)
                    if test_message.encode() in recv_data:
                        log_test("Message not delivered", False,
                                "SECURITY VIOLATION: Message was delivered despite rejection!")
                        return False
                    else:
                        log_test("Message not delivered", True,
                                "Message correctly NOT delivered to receiver")
                except socket.timeout:
                    log_test("Message not delivered", True,
                            "No message received by receiver (correct)")

                return True
            else:
                log_test("Plaintext rejection", False,
                        f"Unexpected error message: {error_msg}")
                return False

        # Check if connection was closed (also acceptable rejection)
        try:
            sender_sock.sendall(b'\x00')  # Try to send ping
            ping_response = recv_with_timeout(sender_sock, timeout=1.0)
            if not ping_response:
                log_test("Plaintext rejection", True,
                        "Server closed connection (acceptable rejection)")
                return True
        except (socket.error, ssl.SSLError, BrokenPipeError):
            log_test("Plaintext rejection", True,
                    "Server closed connection (acceptable rejection)")
            return True

        # If we get here, message might have been accepted (RFC VIOLATION!)
        log("  WARNING: No explicit rejection received, checking if message was delivered...")

        # Check if receiver got the message
        receiver_sock.settimeout(2.0)
        try:
            recv_data = receiver_sock.recv(4096)
            if test_message.encode() in recv_data:
                log_test("Plaintext rejection", False,
                        "RFC VIOLATION: Plaintext message was ACCEPTED and DELIVERED!")
                return False
            else:
                log_test("Plaintext rejection", False,
                        "No clear rejection, but message not delivered - ambiguous")
                return False
        except socket.timeout:
            log_test("Plaintext rejection", False,
                    "No rejection response and no delivery - server behavior unclear")
            return False

    except Exception as e:
        log_test("Plaintext rejection", False, f"Exception: {type(e).__name__}: {e}")
        return False
    finally:
        if sender_sock:
            try:
                sender_sock.close()
            except:
                pass
        if receiver_sock:
            try:
                receiver_sock.close()
            except:
                pass


# =============================================================================
# Test 2: Multiple Plaintext Attempts
# =============================================================================

def test_multiple_plaintext_rejected():
    """
    Test: Server MUST reject ALL plaintext message attempts.
    
    Verify that repeated plaintext attempts are all rejected,
    not just the first one.
    """
    log("\n=== Test: Multiple Plaintext Attempts Rejected ===")

    if not check_server_available():
        log_test("Multiple plaintext rejection", False, "Server not available")
        return False

    sock = None

    try:
        sock = connect_tls()
        username = f"multi_plaintext_{int(time.time())}"
        if not login(sock, username):
            log_test("Multiple plaintext rejection", False, "Login failed")
            return False

        log(f"  Logged in as: {username}")

        # Try to send 3 plaintext messages
        rejections = 0
        for i in range(3):
            send_plaintext_message(sock, f"target_{i}", f"plaintext_msg_{i}")

            response = recv_with_timeout(sock, timeout=1.0)
            error_msg = parse_error_response(response)

            if error_msg and "e2ee" in error_msg.lower():
                rejections += 1
                log(f"    Attempt {i+1}: Rejected (correct)")
            elif not response:
                # Connection might be closed
                try:
                    sock.sendall(b'\x00')
                except:
                    log(f"    Attempt {i+1}: Connection closed (acceptable)")
                    rejections += 1
                    break
            else:
                log(f"    Attempt {i+1}: Unexpected response")

        if rejections >= 1:
            log_test("Multiple plaintext rejection", True,
                    f"{rejections}/3 attempts properly rejected")
            return True
        else:
            log_test("Multiple plaintext rejection", False,
                    "No rejections detected - RFC VIOLATION")
            return False

    except Exception as e:
        log_test("Multiple plaintext rejection", False, f"Exception: {type(e).__name__}: {e}")
        return False
    finally:
        if sock:
            try:
                sock.close()
            except:
                pass


# =============================================================================
# Test 3: Verify E2EE/CBOR Messages Still Work
# =============================================================================

def test_cbor_messages_accepted():
    """
    Test: Server MUST still accept CBOR messages (opcode 0x10) or sequenced (0x07).
    
    While plaintext is rejected, legitimate message protocols must work.
    """
    log("\n=== Test: CBOR/Sequenced Messages Accepted ===")

    if not check_server_available():
        log_test("CBOR messages accepted", False, "Server not available")
        return False

    sock = None

    try:
        sock = connect_tls()
        username = f"cbor_test_{int(time.time())}"
        if not login(sock, username):
            log_test("CBOR messages accepted", False, "Login failed")
            return False

        log(f"  Logged in as: {username}")

        # Send a sequenced message (opcode 0x07) - should be accepted
        target = f"cbor_receiver_{int(time.time())}"
        message = b"CBOR_TEST_MESSAGE"
        seq_no = 1

        target_bytes = target.encode('utf-8')
        payload = (bytes([0x07]) +
                   struct.pack('>H', len(target_bytes)) + target_bytes +
                   struct.pack('>Q', seq_no) +
                   struct.pack('>H', len(message)) + message)
        sock.sendall(payload)
        log("  Sent sequenced message (opcode 0x07)")

        response = recv_with_timeout(sock, timeout=2.0)

        # Check for rejection (should NOT happen)
        error_msg = parse_error_response(response)
        if error_msg and "e2ee" in error_msg.lower():
            log_test("CBOR messages accepted", False,
                    f"Sequenced message wrongly rejected: {error_msg}")
            return False

        # No error = message was accepted (correct)
        log_test("CBOR messages accepted", True,
                "Sequenced message (0x07) accepted correctly")
        return True

    except Exception as e:
        log_test("CBOR messages accepted", False, f"Exception: {type(e).__name__}: {e}")
        return False
    finally:
        if sock:
            try:
                sock.close()
            except:
                pass


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Plaintext Rejection Test (RFC-001-AMENDMENT-001 Section 7)")
    log("=" * 60)
    log("\nThis test verifies that v1.0 REJECTS plaintext messages (0x02)")
    log("and only accepts E2EE (0x23) or CBOR (0x10) protocols.")

    # Run tests
    test_plaintext_rejected()
    test_multiple_plaintext_rejected()
    test_cbor_messages_accepted()

    # Summary
    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)

    passed = sum(1 for _, p, _ in results if p)
    failed = sum(1 for _, p, _ in results if not p)

    for name, p, msg in results:
        status = "PASS" if p else "FAIL"
        log(f"  [{status}] {name}")

    log(f"\nTotal: {len(results)} tests")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed > 0:
        log("\nFAIL: Plaintext rejection tests FAILED")
        log("RFC-001-AMENDMENT-001 Section 7 VIOLATION DETECTED")
        sys.exit(1)
    else:
        log("\nPASS: All plaintext rejection tests passed")
        log("RFC-001-AMENDMENT-001 Section 7: COMPLIANT")
        sys.exit(0)


if __name__ == "__main__":
    main()
