#!/usr/bin/env python3
"""
AUDIT FIX M15: Negative Delivery Test

Verifies that messages sent to an offline (non-existent) user are NOT
delivered to any other connected user. This is a message isolation test.

A messaging system must guarantee that messages are only delivered to
the intended recipient, never leaked to other users.

Tier: 1 (Integration)
"""

import time
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from utilities.iris_client import IrisClient


def test_message_not_delivered_to_wrong_user():
    """
    Scenario:
    1. User A connects
    2. User B connects
    3. A sends a message to "nonexistent_user_xyz" (offline / never registered)
    4. Wait briefly
    5. Verify B did NOT receive the message
    6. Verify A did NOT receive the message echoed back
    """
    print("=" * 60)
    print("TEST: Messages to offline user not delivered to others")
    print("=" * 60)

    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))

    user_a = None
    user_b = None

    try:
        user_a = IrisClient(host, port)
        user_a.login(f"neg_delivery_a_{int(time.time())}")

        user_b = IrisClient(host, port)
        user_b.login(f"neg_delivery_b_{int(time.time())}")

        # Send message to a non-existent user
        offline_target = f"nonexistent_user_{int(time.time())}_xyz"
        user_a.send_msg(offline_target, "secret_message_for_offline_user")
        time.sleep(1.0)

        # Check that user B did NOT receive the message
        leaked_to_b = False
        try:
            msg = user_b.recv_msg(timeout=2.0)
            if msg and b"secret_message_for_offline_user" in msg:
                leaked_to_b = True
                print(f"  ✗ Message leaked to user B: {msg[:50]}")
        except Exception:
            pass  # Timeout = no message received = correct

        # Check that user A did NOT receive an echo
        leaked_to_a = False
        try:
            msg = user_a.recv_msg(timeout=1.0)
            if msg and b"secret_message_for_offline_user" in msg:
                leaked_to_a = True
                print(f"  ✗ Message echoed back to user A: {msg[:50]}")
        except Exception:
            pass  # Timeout = no message received = correct

        if leaked_to_b:
            print("  FAIL: Message to offline user was delivered to another user")
            return False
        if leaked_to_a:
            print("  FAIL: Message to offline user was echoed back to sender")
            return False

        print("  ✓ Message to offline user was not delivered to any other user")
        print("  PASS")
        return True

    except Exception as e:
        print(f"  FAIL: Exception: {e}")
        return False
    finally:
        if user_a:
            user_a.close()
        if user_b:
            user_b.close()


def main():
    print("\n" + "=" * 60)
    print("AUDIT FIX M15: NEGATIVE DELIVERY TEST")
    print("=" * 60)

    tests = [
        ("Message isolation", test_message_not_delivered_to_wrong_user),
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
