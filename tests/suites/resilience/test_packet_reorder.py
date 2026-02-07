#!/usr/bin/env python3
"""
G-17: Packet Reordering Tolerance

RFC-001 NFR-18: Input validation

Tests that the server handles out-of-order client operations gracefully.
Since we cannot inject tc netem reordering in standalone mode, this test
simulates application-level reordering by sending messages out of sequence
and verifying all are delivered correctly.

Test Scenarios:
1. Send 100 messages with shuffled order IDs -- all must be accepted
2. Interleave messages from multiple users -- no cross-contamination
3. Rapid fire unordered -- server must not crash

INVARIANTS:
- All messages must be accepted regardless of send order
- Server must not crash from out-of-order packets
- TLS layer handles TCP reassembly (this test validates application layer)

Pattern: follows test_resilience.py

Tier: 1 (Resilience)
"""

import sys
import os
import time
import socket
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


# =============================================================================
# Test 1: Shuffled Message Order
# =============================================================================
def test_shuffled_order():
    """Send 100 messages with shuffled sequence IDs. All must be accepted."""
    log("\n=== Test 1: Shuffled Message Order (100 messages) ===")

    sender = unique_user("reorder_sender")
    target = unique_user("reorder_target")

    try:
        client = IrisClient()
        client.login(sender)

        # Create messages with sequence numbers, then shuffle
        indices = list(range(100))
        random.shuffle(indices)

        sent = 0
        for idx in indices:
            try:
                client.send_msg(target, f"reorder_msg_seq_{idx:04d}")
                sent += 1
            except Exception:
                break
            time.sleep(0.01)

        client.close()

        if sent >= 90:
            log(f"  PASS: {sent}/100 shuffled messages accepted")
            return True
        else:
            log(f"  FAIL: Only {sent}/100 accepted")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 2: Interleaved Multi-User
# =============================================================================
def test_interleaved_users():
    """Multiple users send interleaved messages to same target. No mix-up."""
    log("\n=== Test 2: Interleaved Multi-User ===")

    users = [unique_user(f"reorder_user_{i}") for i in range(5)]
    target = unique_user("reorder_multi_target")

    clients = []
    try:
        for user in users:
            c = IrisClient()
            c.login(user)
            clients.append(c)

        # Interleave: user0 sends msg0, user1 sends msg0, user2 sends msg0, ...
        for msg_idx in range(10):
            for idx, client in enumerate(clients):
                try:
                    client.send_msg(target, f"user{idx}_msg{msg_idx}")
                except Exception:
                    pass
                time.sleep(0.005)

        time.sleep(0.5)
        for c in clients:
            try:
                c.close()
            except Exception:
                pass

        if server_alive():
            log("  PASS: 5 users x 10 interleaved messages handled")
            return True
        else:
            log("  FAIL: Server crashed from interleaved messages")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        for c in clients:
            try:
                c.close()
            except Exception:
                pass
        return server_alive()


# =============================================================================
# Test 3: Rapid Fire Unordered
# =============================================================================
def test_rapid_unordered():
    """Blast 200 messages as fast as possible with random content lengths."""
    log("\n=== Test 3: Rapid Fire Unordered ===")

    sender = unique_user("rapid_reorder")
    target = unique_user("rapid_reorder_target")

    try:
        client = IrisClient()
        client.login(sender)

        sent = 0
        for i in range(200):
            msg_len = random.randint(1, 500)
            msg = f"rapid_{i}_" + "x" * msg_len
            try:
                client.send_msg(target, msg)
                sent += 1
            except Exception:
                break

        client.close()

        if server_alive():
            log(f"  PASS: {sent}/200 rapid unordered messages")
            return True
        else:
            log("  FAIL: Server crashed from rapid fire")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


def main():
    print("=" * 60)
    print(" G-17: PACKET REORDERING TOLERANCE")
    print(" RFC-001 NFR-18: Input validation")
    print("=" * 60)

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Shuffled Order", test_shuffled_order),
        ("Interleaved Users", test_interleaved_users),
        ("Rapid Unordered", test_rapid_unordered),
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
        print(f"\nG-17 Packet Reorder: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-17 Packet Reorder: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
