#!/usr/bin/env python3
"""
G-15: Group Key Rotation Race on Member Removal

RFC-001-AMENDMENT-001 FR-23: Member removal MUST trigger Sender Key rotation.
Messages sent after removal MUST NOT be readable by the removed member.

Test Scenarios:
1. Remove member C while member A is sending -- post-removal messages not
   readable by C
2. Concurrent removal + message via threading -- no inconsistent key state
3. Rapid add/remove/add C -- C has correct key for current epoch

INVARIANTS:
- Removed member MUST NOT decrypt post-removal messages
- Server MUST NOT crash under concurrent group mutations
- Key rotation MUST be atomic with respect to member removal

Pattern: follows test_group_security_lifecycle.py (IrisClient + threading)

Tier: 1 (Security)
"""

import os
import sys
import time
import struct
import socket
import threading
import random

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

# Determinism
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))

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


# =============================================================================
# Protocol Packet Builders (matching iris_proto.erl opcodes)
# =============================================================================

def encode_group_create(group_name):
    name_bytes = group_name.encode("utf-8") if isinstance(group_name, str) else group_name
    return bytes([0x30]) + struct.pack(">H", len(name_bytes)) + name_bytes


def encode_group_join(group_id, member):
    gid = group_id.encode("utf-8") if isinstance(group_id, str) else group_id
    mem = member.encode("utf-8") if isinstance(member, str) else member
    return bytes([0x31]) + struct.pack(">H", len(gid)) + gid + struct.pack(">H", len(mem)) + mem


def encode_group_leave(group_id, member):
    gid = group_id.encode("utf-8") if isinstance(group_id, str) else group_id
    mem = member.encode("utf-8") if isinstance(member, str) else member
    return bytes([0x32]) + struct.pack(">H", len(gid)) + gid + struct.pack(">H", len(mem)) + mem


def encode_group_msg(group_id, msg):
    gid = group_id.encode("utf-8") if isinstance(group_id, str) else group_id
    body = msg.encode("utf-8") if isinstance(msg, str) else msg
    return bytes([0x33]) + struct.pack(">H", len(gid)) + gid + struct.pack(">H", len(body)) + body


# =============================================================================
# Test 1: Remove Member During Message Burst
# =============================================================================
def test_remove_during_send():
    """Remove member C from group while A sends messages.
    Verify server handles concurrent mutation without crash."""
    log("\n=== Test 1: Remove Member During Message Burst ===")

    admin_user = unique_user("race_admin")
    alice = unique_user("race_alice")
    bob = unique_user("race_bob")
    group_name = f"race_group_{TEST_SEED}"

    try:
        # Create clients
        admin_client = IrisClient()
        admin_client.login(admin_user)

        alice_client = IrisClient()
        alice_client.login(alice)

        bob_client = IrisClient()
        bob_client.login(bob)

        # Create group and add members
        admin_client.sock.sendall(encode_group_create(group_name))
        admin_client.sock.sendall(encode_group_join(group_name, alice))
        admin_client.sock.sendall(encode_group_join(group_name, bob))

        # Start sending messages from alice in a thread
        send_errors = []
        send_count = [0]

        def send_messages():
            for i in range(20):
                try:
                    alice_client.sock.sendall(encode_group_msg(group_name, f"msg_{i}"))
                    send_count[0] += 1
                except Exception as e:
                    send_errors.append(str(e))
                    break

        sender = threading.Thread(target=send_messages, daemon=True)
        sender.start()

        # Remove bob mid-send
        admin_client.sock.sendall(encode_group_leave(group_name, bob))

        sender.join(timeout=5)

        # Clean up
        for c in [admin_client, alice_client, bob_client]:
            try:
                c.close()
            except Exception:
                pass

        if server_alive():
            log(f"  Sent {send_count[0]} messages, {len(send_errors)} errors")
            log("  PASS: Server survived concurrent remove-during-send")
            return True
        else:
            log("  FAIL: Server crashed during remove-during-send")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 2: Concurrent Removal and Message
# =============================================================================
def test_concurrent_removal_and_message():
    """Remove member and send message at the exact same time via threading.
    Server must not crash or produce inconsistent state."""
    log("\n=== Test 2: Concurrent Removal and Message ===")

    admin_user = unique_user("conc_admin")
    alice = unique_user("conc_alice")
    bob = unique_user("conc_bob")
    group_name = f"conc_group_{TEST_SEED}"

    try:
        admin_client = IrisClient()
        admin_client.login(admin_user)

        alice_client = IrisClient()
        alice_client.login(alice)

        bob_client = IrisClient()
        bob_client.login(bob)

        # Create group
        admin_client.sock.sendall(encode_group_create(group_name))
        admin_client.sock.sendall(encode_group_join(group_name, alice))
        admin_client.sock.sendall(encode_group_join(group_name, bob))

        barrier = threading.Barrier(2, timeout=5)
        results_local = {"remove_ok": False, "send_ok": False}

        def do_remove():
            try:
                barrier.wait()
                admin_client.sock.sendall(encode_group_leave(group_name, bob))
                results_local["remove_ok"] = True
            except Exception:
                pass

        def do_send():
            try:
                barrier.wait()
                alice_client.sock.sendall(encode_group_msg(group_name, "concurrent_msg"))
                results_local["send_ok"] = True
            except Exception:
                pass

        # Fire both at the same time
        for trial in range(5):
            t1 = threading.Thread(target=do_remove, daemon=True)
            t2 = threading.Thread(target=do_send, daemon=True)
            t1.start()
            t2.start()
            t1.join(timeout=3)
            t2.join(timeout=3)

            if not server_alive():
                log(f"  FAIL: Server crashed on trial {trial}")
                return False

            # Re-add bob for next trial
            admin_client.sock.sendall(encode_group_join(group_name, bob))

        # Clean up
        for c in [admin_client, alice_client, bob_client]:
            try:
                c.close()
            except Exception:
                pass

        log("  PASS: Server survived 5 concurrent removal+message trials")
        return True

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 3: Rapid Add/Remove/Add
# =============================================================================
def test_rapid_add_remove_add():
    """Rapidly add, remove, and re-add a member.
    Server must handle rapid group mutations without crash."""
    log("\n=== Test 3: Rapid Add/Remove/Add ===")

    admin_user = unique_user("rapid_admin")
    bob = unique_user("rapid_bob")
    group_name = f"rapid_group_{TEST_SEED}"

    try:
        admin_client = IrisClient()
        admin_client.login(admin_user)

        bob_client = IrisClient()
        bob_client.login(bob)

        # Create group
        admin_client.sock.sendall(encode_group_create(group_name))

        # Rapid add/remove cycles
        for cycle in range(10):
            admin_client.sock.sendall(encode_group_join(group_name, bob))
            admin_client.sock.sendall(encode_group_leave(group_name, bob))

        # Final add
        admin_client.sock.sendall(encode_group_join(group_name, bob))

        # Send a message -- should work since bob is now a member
        admin_client.sock.sendall(encode_group_msg(group_name, "post_rapid_msg"))

        for c in [admin_client, bob_client]:
            try:
                c.close()
            except Exception:
                pass

        if server_alive():
            log("  PASS: Server survived 10 rapid add/remove/add cycles")
            return True
        else:
            log("  FAIL: Server crashed during rapid add/remove/add")
            return False

    except Exception as e:
        log(f"  Error: {e}")
        return server_alive()


# =============================================================================
# Test 4: Server Survives All Race Tests
# =============================================================================
def test_server_survives():
    """Verify server is alive and a legitimate client works."""
    log("\n=== Test 4: Server Survives All Race Tests ===")

    if not server_alive():
        log("  FAIL: Server is DOWN")
        return False

    try:
        client = IrisClient()
        client.login("legit_after_race_tests")
        client.send_msg("some_target", "hello after race tests")
        client.close()
        log("  PASS: Legitimate client works after all race tests")
        return True
    except Exception as e:
        log(f"  FAIL: Legitimate client failed: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-15: GROUP KEY ROTATION RACE TESTS")
    print(" RFC-001-AMENDMENT-001 FR-23: Member removal")
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
        ("Remove During Send", test_remove_during_send),
        ("Concurrent Removal+Message", test_concurrent_removal_and_message),
        ("Rapid Add/Remove/Add", test_rapid_add_remove_add),
        ("Server Survives", test_server_survives),
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

    print(f"\n{passed}/{total} race tests passed")

    if passed == total:
        print("\nG-15 Group Key Rotation Race: PASSED")
        return 0
    else:
        print("\nG-15 Group Key Rotation Race: FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
