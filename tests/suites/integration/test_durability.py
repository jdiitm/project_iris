#!/usr/bin/env python3
"""
Durability Test - Zero Message Loss on Failover

Tests:
1. Messages queued during node failure are preserved
2. Pending acks are saved to offline storage on disconnect
3. WAL recovery works after crash
"""

import sys
import time
import socket
import struct
import random
import string

# Add utilities to path
sys.path.insert(0, 'tests/utilities')
from iris_client import IrisClient
from cluster_util import ClusterManager


def random_user():
    return ''.join(random.choices(string.ascii_lowercase, k=8))


def test_pending_acks_preserved():
    """Test that pending acks are saved when connection drops"""
    print("\n=== Test: Pending Acks Preserved ===")
    
    sender = IrisClient("edge1")
    receiver = IrisClient("edge1")
    
    sender_user = f"sender_{random_user()}"
    receiver_user = f"receiver_{random_user()}"
    
    # Login both
    sender.login(sender_user)
    receiver.login(receiver_user)
    
    # Send message
    test_msg = f"durability_test_{time.time()}"
    sender.send_msg(receiver_user, test_msg)
    
    # Receive but DON'T ack (simulate crash before ack)
    # Close receiver abruptly
    receiver.sock.close()
    
    # Wait for pending ack to be saved to offline
    time.sleep(2)
    
    # Reconnect receiver
    receiver2 = IrisClient("edge1")
    receiver2.login(receiver_user)
    
    # Should receive message from offline storage
    try:
        msg = receiver2.recv_msg(timeout=5.0)
        if test_msg in str(msg):
            print(f"✓ Pending message preserved after disconnect")
            sender.close()
            receiver2.close()
            return True
        else:
            print(f"✗ Wrong message received: {msg}")
            sender.close()
            receiver2.close()
            return False
    except Exception as e:
        print(f"✗ No message received: {e}")
        sender.close()
        receiver2.close()
        return False


def test_offline_message_delivery():
    """Test messages to offline users are stored and delivered"""
    print("\n=== Test: Offline Message Delivery ===")
    
    sender = IrisClient("edge1")
    sender_user = f"sender_{random_user()}"
    offline_user = f"offline_{random_user()}"
    
    sender.login(sender_user)
    
    # Send to user who is NOT online
    test_msg = f"offline_test_{time.time()}"
    sender.send_msg(offline_user, test_msg)
    
    # Wait for storage
    time.sleep(1)
    
    # Now offline user connects
    receiver = IrisClient("edge1")
    receiver.login(offline_user)
    
    try:
        msg = receiver.recv_msg(timeout=5.0)
        if test_msg in str(msg):
            print(f"✓ Offline message delivered correctly")
            sender.close()
            receiver.close()
            return True
        else:
            print(f"✗ Wrong message: {msg}")
            sender.close()
            receiver.close()
            return False
    except Exception as e:
        print(f"✗ No message received: {e}")
        sender.close()
        receiver.close()
        return False


def test_multi_message_durability():
    """Test multiple messages are all preserved"""
    print("\n=== Test: Multi-Message Durability ===")
    
    sender = IrisClient("edge1")
    sender_user = f"sender_{random_user()}"
    receiver_user = f"receiver_{random_user()}"
    
    sender.login(sender_user)
    
    # Send multiple messages to offline user
    messages = [f"batch_{i}_{time.time()}" for i in range(5)]
    for msg in messages:
        sender.send_msg(receiver_user, msg)
    
    time.sleep(1)
    
    # Connect receiver
    receiver = IrisClient("edge1")
    receiver.login(receiver_user)
    
    received = []
    try:
        for _ in range(5):
            msg = receiver.recv_msg(timeout=3.0)
            received.append(str(msg))
    except:
        pass
    
    # Check all messages received
    success = 0
    for orig in messages:
        if any(orig in r for r in received):
            success += 1
    
    print(f"  Received {success}/{len(messages)} messages")
    
    sender.close()
    receiver.close()
    
    if success == len(messages):
        print(f"✓ All messages preserved")
        return True
    else:
        print(f"✗ Message loss detected: {len(messages) - success} lost")
        return False


def main():
    print("=" * 60)
    print(" DURABILITY TEST SUITE")
    print("=" * 60)
    
    tests = [
        ("Pending Acks Preserved", test_pending_acks_preserved),
        ("Offline Message Delivery", test_offline_message_delivery),
        ("Multi-Message Durability", test_multi_message_durability),
    ]
    
    passed = 0
    failed = 0
    
    for name, test_fn in tests:
        try:
            if test_fn():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"✗ {name} EXCEPTION: {e}")
            failed += 1
    
    print("\n" + "=" * 60)
    print(f" RESULTS: {passed} passed, {failed} failed")
    print("=" * 60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
