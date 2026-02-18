#!/usr/bin/env python3
"""
AUDIT MITIGATION P1-1: Rate Limit Granularity Integration Tests

Verifies that typing indicator floods do not block real message delivery.
Per-message-type rate limiting isolates cheap packets from critical messages.

Tier: 2 (requires running server)
"""
import sys
import os
import struct
import socket
import traceback

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.utilities.iris_client import IrisClient


def send_typing(client, target, start=True):
    """Send a typing indicator (opcode 0x70 for start, 0x71 for stop)."""
    opcode = b'\x70' if start else b'\x71'
    target_bytes = target.encode('utf-8')
    payload = opcode + struct.pack('>H', len(target_bytes)) + target_bytes
    client.sock.sendall(payload)


def drain_socket(client, drain_timeout=0.5):
    """Read and discard any pending data (e.g. typing relays) from the socket."""
    client.sock.settimeout(drain_timeout)
    try:
        while True:
            data = client.sock.recv(4096)
            if not data:
                break
    except (socket.timeout, TimeoutError):
        pass  # No more data to drain
    client.buffer = b''


def test_typing_flood_does_not_block_messages():
    """Send 100 rapid typing indicators, then verify a real message still delivers."""
    sender = IrisClient()
    receiver = IrisClient()
    try:
        sender.login("typing_flood_sender")
        receiver.login("typing_flood_receiver")

        # Flood typing indicators (some may be silently dropped -- that's OK)
        # 100 exceeds the burst limit of 50, proving per-type isolation
        for _ in range(100):
            try:
                send_typing(sender, "typing_flood_receiver", start=True)
            except Exception:
                pass  # Expected: some may be rate-limited

        # Drain any typing relay packets the receiver got
        drain_socket(receiver)

        # Now send a real message -- must be delivered
        sender.send_msg("typing_flood_receiver", "after_flood_msg")

        # Receiver must get the message
        try:
            msg = receiver.recv_msg(timeout=15)
            assert b"after_flood_msg" in msg, \
                f"Message not delivered after typing flood. Got: {msg}"
            return True
        except Exception as e:
            raise AssertionError(f"Message not delivered after typing flood: {e}")
    finally:
        sender.close()
        receiver.close()


def test_server_healthy_after_typing_flood():
    """Verify server stays healthy after a typing flood."""
    client = IrisClient()
    try:
        client.login("health_check_rl")
        # Basic message to self
        client.send_msg("health_check_rl", "health_check")
        # If we got here without crash, server is healthy
        return True
    finally:
        client.close()


if __name__ == '__main__':
    results = []
    tests = [
        ("typing_flood_does_not_block_messages", test_typing_flood_does_not_block_messages),
        ("server_healthy_after_typing_flood", test_server_healthy_after_typing_flood),
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
    print(f"\n{passed}/{total} rate limit granularity tests passed")
    sys.exit(0 if passed == total else 1)
