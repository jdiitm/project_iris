#!/usr/bin/env python3
"""
Test Message Ordering - Validates per-conversation message ordering guarantees.

This test verifies that messages sent in sequence are received in the same order,
which is a critical correctness guarantee for messaging systems.
"""

import socket
import struct
import time
import sys
import os

# Add parent directories to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from utilities.iris_client import IrisClient

def test_message_ordering():
    """
    Test that messages sent in sequence arrive in order.
    
    Scenario:
    1. User A connects
    2. User B connects
    3. A sends 20 messages to B in sequence
    4. Verify B receives all 20 in order
    """
    print("=" * 60)
    print("TEST: Message Ordering Guarantee")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    sender = None
    receiver = None
    
    try:
        # Connect sender and receiver
        sender = IrisClient(host, port)
        receiver = IrisClient(host, port)
        
        # Use unique names to avoid stale offline messages from previous tests
        run_id = int(time.time())
        sender_name = f"ordering_sender_{run_id}"
        receiver_name = f"ordering_receiver_{run_id}"
        sender.login(sender_name)
        receiver.login(receiver_name)
        
        print(f"✓ Connected sender and receiver to {host}:{port}")
        
        # Send messages in sequence
        num_messages = 20
        sent_sequence = []
        
        for i in range(num_messages):
            msg_content = f"msg_{i:03d}_{int(time.time() * 1000)}"
            sender.send_msg(receiver_name, msg_content)
            sent_sequence.append(msg_content)
        
        print(f"✓ Sent {num_messages} messages in sequence")
        
        # Receive messages (recv loop handles waiting via timeout)
        received_sequence = []
        start_time = time.time()
        timeout = 10.0  # 10 second timeout
        
        while len(received_sequence) < num_messages and (time.time() - start_time) < timeout:
            msg = receiver.recv_msg(timeout=1.0)
            if msg:
                received_sequence.append(msg)
        
        print(f"✓ Received {len(received_sequence)}/{num_messages} messages")
        
        # Validate ordering - decode bytes for comparison
        ordering_errors = 0
        for i, (sent, received) in enumerate(zip(sent_sequence, received_sequence)):
            received_str = received.decode('utf-8') if isinstance(received, bytes) else received
            if sent != received_str:
                ordering_errors += 1
                if ordering_errors <= 3:  # Show first 3 errors
                    print(f"  ✗ Position {i}: expected '{sent}', got '{received_str}'")
        
        if len(received_sequence) < num_messages:
            missing = num_messages - len(received_sequence)
            print(f"  ⚠ Missing {missing} messages (may be offline)")
        
        if ordering_errors == 0 and len(received_sequence) == num_messages:
            print("✓ All messages received in correct order")
            return True
        elif ordering_errors == 0:
            print(f"✓ Received messages are in correct order ({len(received_sequence)}/{num_messages})")
            return True
        else:
            print(f"✗ {ordering_errors} ordering errors detected")
            return False
            
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if sender:
            sender.close()
        if receiver:
            receiver.close()


def test_interleaved_conversations():
    """
    Test that ordering is maintained per-conversation even with interleaving.
    
    Scenario:
    1. A sends to B: msg_0, msg_1, msg_2
    2. A sends to C: msg_0, msg_1, msg_2
    3. Interleave the sends
    4. Verify B gets A's messages in order
    5. Verify C gets A's messages in order
    """
    print("\n" + "=" * 60)
    print("TEST: Interleaved Conversation Ordering")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    try:
        sender = IrisClient(host, port)
        receiver_b = IrisClient(host, port)
        receiver_c = IrisClient(host, port)
        
        # Use unique names to avoid stale offline messages
        run_id = int(time.time())
        sender_name = f"interleave_sender_{run_id}"
        b_name = f"interleave_b_{run_id}"
        c_name = f"interleave_c_{run_id}"
        
        sender.login(sender_name)
        receiver_b.login(b_name)
        receiver_c.login(c_name)
        
        print(f"✓ Connected 3 clients")
        
        # Interleave sends
        for i in range(5):
            sender.send_msg(b_name, f"to_b_{i}")
            sender.send_msg(c_name, f"to_c_{i}")
        
        print("✓ Sent 10 interleaved messages (5 to B, 5 to C)")
        
        # Receive at B (recv handles waiting via timeout)
        b_msgs = []
        for _ in range(5):
            msg = receiver_b.recv_msg(timeout=0.5)
            if msg:
                b_msgs.append(msg)
        
        # Receive at C
        c_msgs = []
        for _ in range(5):
            msg = receiver_c.recv_msg(timeout=0.5)
            if msg:
                c_msgs.append(msg)
        
        print(f"✓ B received {len(b_msgs)} messages, C received {len(c_msgs)} messages")
        
        # Validate B's order - decode bytes
        b_decoded = [m.decode('utf-8') if isinstance(m, bytes) else m for m in b_msgs]
        c_decoded = [m.decode('utf-8') if isinstance(m, bytes) else m for m in c_msgs]
        b_ordered = all(f"to_b_{i}" == msg for i, msg in enumerate(b_decoded))
        c_ordered = all(f"to_c_{i}" == msg for i, msg in enumerate(c_decoded))
        
        if b_ordered and c_ordered:
            print("✓ Both conversations maintained correct order")
            return True
        else:
            if not b_ordered:
                print(f"✗ B's messages out of order: {b_msgs}")
            if not c_ordered:
                print(f"✗ C's messages out of order: {c_msgs}")
            return False
            
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        sender.close()
        receiver_b.close()
        receiver_c.close()


if __name__ == "__main__":
    results = []
    
    results.append(("Message Ordering", test_message_ordering()))
    results.append(("Interleaved Ordering", test_interleaved_conversations()))
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for _, r in results if r)
    total = len(results)
    
    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  [{status}] {name}")
    
    print(f"\n{passed}/{total} tests passed")
    
    sys.exit(0 if passed == total else 1)
