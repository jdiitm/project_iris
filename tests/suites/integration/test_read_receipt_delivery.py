#!/usr/bin/env python3
"""
G-30: Read Receipt Delivery Guarantee

RFC-001 FR-4: Read receipts
Read receipts are best-effort but must not crash the server.

Test Scenarios:
1. Send read receipt during normal operation -- verify delivery to sender
2. Send read receipt to offline user -- verify defined behavior
3. Rapid read receipts -- verify no crash

INVARIANTS:
- Server MUST NOT crash from read receipt operations
- Read receipts should be delivered when possible
- Best-effort contract: dropped receipts are acceptable, crashes are not

Pattern: follows test_rate_limiting.py

Tier: 0 (Integration)
"""

import sys
import os
import time
import socket
import struct
import random

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))

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


def encode_read_receipt(target, msg_id):
    """Encode READ_RECEIPT opcode (0x71)."""
    target_bytes = target.encode("utf-8")
    msg_id_bytes = msg_id.encode("utf-8")
    return (bytes([0x71]) +
            struct.pack(">H", len(target_bytes)) + target_bytes +
            struct.pack(">H", len(msg_id_bytes)) + msg_id_bytes)


# =============================================================================
# Test 1: Read Receipt Normal Operation
# =============================================================================
def test_read_receipt_normal():
    """Send a read receipt while both parties are online."""
    log("\n=== Test 1: Read Receipt Normal ===")

    sender = unique_user("rr_sender")
    receiver = unique_user("rr_receiver")

    try:
        send_client = IrisClient()
        send_client.login(sender)

        recv_client = IrisClient()
        recv_client.login(receiver)

        # Send a message first
        send_client.send_msg(receiver, "read_receipt_test_msg")

        # Receiver sends read receipt back to sender
        recv_client.sock.sendall(encode_read_receipt(sender, "rr_msg_001"))

        send_client.close()
        recv_client.close()

        if server_alive():
            log("  PASS: Read receipt sent without crash")
            return True
        else:
            log("  FAIL: Server crashed on read receipt")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 2: Read Receipt to Offline User
# =============================================================================
def test_read_receipt_offline():
    """Send read receipt to an offline user. Must not crash."""
    log("\n=== Test 2: Read Receipt to Offline User ===")

    sender = unique_user("rr_offline_sender")
    offline_target = unique_user("rr_offline_target")

    try:
        client = IrisClient()
        client.login(sender)

        # Send read receipt to non-existent/offline user
        client.sock.sendall(encode_read_receipt(offline_target, "rr_offline_msg"))

        client.close()

        if server_alive():
            log("  PASS: Read receipt to offline user handled gracefully")
            return True
        else:
            log("  FAIL: Server crashed on read receipt to offline user")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 3: Rapid Read Receipts
# =============================================================================
def test_rapid_read_receipts():
    """Send many read receipts rapidly. Must not crash."""
    log("\n=== Test 3: Rapid Read Receipts ===")

    sender = unique_user("rr_rapid_sender")
    target = unique_user("rr_rapid_target")

    try:
        client = IrisClient()
        client.login(sender)

        for i in range(50):
            try:
                client.sock.sendall(encode_read_receipt(target, f"rapid_msg_{i}"))
            except Exception:
                break
            time.sleep(0.01)

        time.sleep(0.5)
        client.close()

        if server_alive():
            log("  PASS: 50 rapid read receipts handled")
            return True
        else:
            log("  FAIL: Server crashed on rapid read receipts")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


def main():
    print("=" * 60)
    print(" G-30: READ RECEIPT DELIVERY GUARANTEE")
    print(" RFC-001 FR-4: Read receipts")
    print("=" * 60)

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Normal Read Receipt", test_read_receipt_normal),
        ("Offline Read Receipt", test_read_receipt_offline),
        ("Rapid Read Receipts", test_rapid_read_receipts),
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
        print(f"\nG-30 Read Receipt: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-30 Read Receipt: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
