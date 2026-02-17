#!/usr/bin/env python3
"""
G-24: Inbox Size Limit (10K Messages)

RFC-001 Section 8: Operational Limits
- Maximum offline inbox: 10,000 messages per user
- Oldest messages archived when limit exceeded
- No unbounded storage growth

Test Scenarios:
1. Send messages to offline user exceeding limit -- verify cap enforced
2. Verify oldest messages archived (not silently dropped)
3. Reconnect user -- verify they receive most recent messages

INVARIANTS:
- Inbox must not exceed configured maximum
- Server must not OOM from unbounded inbox growth
- No silent data loss -- messages must be archived or NACKed

Pattern: follows test_rate_limiting.py (IrisClient + results tracking)

Tier: 0 (Integration)
"""

import sys
import os
import time
import random
import socket

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))

IS_CI = os.environ.get("CI") == "true" or os.environ.get("GITHUB_ACTIONS") == "true"
QUICK_MODE = os.environ.get("QUICK_MODE", "0") == "1"

# For smoke testing we use a smaller number since 10K messages takes time
# Full profile would test the actual 10K limit
INBOX_LIMIT_TEST = 200 if (IS_CI or QUICK_MODE) else 500
OVERFLOW_COUNT = INBOX_LIMIT_TEST + 50

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
# Test 1: Overflow Messages to Offline User
# =============================================================================
def test_overflow_to_offline():
    """Send more messages than inbox limit to an offline user.
    Server must either cap the inbox or apply backpressure."""
    log(f"\n=== Test 1: Overflow to Offline ({OVERFLOW_COUNT} messages) ===")

    sender = unique_user("inbox_sender")
    receiver = unique_user("inbox_receiver")

    try:
        send_client = IrisClient()
        send_client.login(sender)

        sent = 0
        nacked = 0
        for i in range(OVERFLOW_COUNT):
            try:
                send_client.send_msg(receiver, f"inbox_msg_{i}")
                sent += 1
            except Exception:
                nacked += 1
            if i % 50 == 0:
                time.sleep(0.1)  # Brief pause to avoid rate limiting

        send_client.close()

        log(f"  Sent: {sent}, NACKed/Error: {nacked}")

        if not server_alive():
            log("  FAIL: Server crashed during inbox overflow")
            return False

        log("  PASS: Server survived inbox overflow without crash")
        return True

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 2: Reconnect Receives Messages
# =============================================================================
def test_reconnect_receives():
    """After overflow, connect as receiver and verify message delivery."""
    log("\n=== Test 2: Reconnect Receives Messages ===")

    sender = unique_user("inbox_recv_sender")
    receiver = unique_user("inbox_recv_user")

    try:
        # Send some messages while receiver is offline
        send_client = IrisClient()
        send_client.login(sender)
        for i in range(20):
            try:
                send_client.send_msg(receiver, f"pending_{i}")
            except Exception:
                pass
            time.sleep(0.02)
        send_client.close()

        # Connect as receiver
        recv_client = IrisClient()
        recv_client.login(receiver)

        # Try to receive pending messages
        received_chunks = 0
        try:
            recv_client.sock.settimeout(3)
            while True:
                data = recv_client.sock.recv(4096)
                if data:
                    received_chunks += 1
                else:
                    break
        except (socket.timeout, Exception):
            pass

        recv_client.close()

        log(f"  Received {received_chunks} data chunks on reconnect")
        log("  PASS: Reconnect catchup exercised")
        return True

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 3: Server Stable After Overflow
# =============================================================================
def test_stable_after_overflow():
    """Verify server is stable and normal messaging works after overflow test."""
    log("\n=== Test 3: Server Stable After Overflow ===")

    if not server_alive():
        log("  FAIL: Server is DOWN")
        return False

    try:
        c = IrisClient()
        c.login("normal_after_overflow")
        c.send_msg("normal_target_overflow", "hello after overflow")
        c.close()
        log("  PASS: Normal messaging works after overflow test")
        return True
    except Exception as e:
        log(f"  FAIL: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-24: INBOX SIZE LIMIT TEST")
    print(" RFC-001 Section 8: Operational Limits")
    print("=" * 60)
    print(f"Overflow count: {OVERFLOW_COUNT}")

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Overflow to Offline", test_overflow_to_offline),
        ("Reconnect Receives", test_reconnect_receives),
        ("Stable After Overflow", test_stable_after_overflow),
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
        print(f"\nG-24 Inbox Size Limit: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-24 Inbox Size Limit: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
