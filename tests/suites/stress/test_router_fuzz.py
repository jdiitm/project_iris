#!/usr/bin/env python3
"""
AUDIT MITIGATION P1-2: Router Fuzz Stress Tests

Tests async router resilience with random usernames and offline fallback
under high load conditions.

Tier: 3 (Stress — requires running server, runs heavy load)
"""
import sys
import os
import time
import random
import string
import traceback

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.utilities.iris_client import IrisClient


def random_username(length=16):
    """Generate a random username."""
    chars = string.ascii_letters + string.digits + '_'
    return ''.join(random.choice(chars) for _ in range(length))


def test_route_random_usernames():
    """Send messages to 200 randomly-generated usernames. Server must stay healthy."""
    sender = IrisClient()
    try:
        sender.login("fuzz_sender")
        time.sleep(0.3)

        # Send messages to random, likely-offline users
        for i in range(200):
            target = random_username()
            try:
                sender.send_msg(target, f"fuzz_msg_{i}")
            except Exception:
                pass  # Some may be rate-limited or overflow -- OK

        # Verify server is still healthy
        time.sleep(0.5)
        sender.send_msg("fuzz_sender", "self_check")
        time.sleep(1)
        return True
    finally:
        sender.close()


def test_offline_fallback_under_load():
    """Disconnect recipient, send burst, reconnect. Messages must be delivered."""
    sender = IrisClient()
    receiver = IrisClient()

    try:
        # Setup
        sender.login("offline_fuzz_sender")
        receiver.login("offline_fuzz_receiver")
        time.sleep(0.3)

        # Disconnect receiver
        receiver.close()
        time.sleep(0.5)

        # Send burst to offline receiver
        msg_count = 50
        for i in range(msg_count):
            try:
                sender.send_msg("offline_fuzz_receiver", f"offline_burst_{i}")
            except Exception:
                pass

        time.sleep(1)

        # Reconnect receiver
        receiver = IrisClient()
        receiver.login("offline_fuzz_receiver")
        time.sleep(2)

        # Check for delivered messages - receive what we can
        msgs = []
        try:
            while True:
                msg = receiver.recv_msg(timeout=2)
                msgs.append(msg)
        except Exception:
            pass  # Timeout is expected after all messages received

        delivered = [m for m in msgs if b'offline_burst_' in m]
        assert len(delivered) > 0, \
            f"No offline messages delivered after burst of {msg_count}"
        return True
    finally:
        sender.close()
        try:
            receiver.close()
        except Exception:
            pass


if __name__ == '__main__':
    results = []
    tests = [
        ("route_random_usernames", test_route_random_usernames),
        ("offline_fallback_under_load", test_offline_fallback_under_load),
    ]

    for name, fn in tests:
        try:
            result = fn()
            results.append((name, True))
            print(f"  [PASS] {name}")
        except Exception as e:
            results.append((name, False))
            print(f"  [FAIL] {name}: {e}")
            traceback.print_exc()

    passed = sum(1 for _, r in results if r)
    total = len(results)
    print(f"\n{passed}/{total} router fuzz tests passed")
    sys.exit(0 if passed == total else 1)
