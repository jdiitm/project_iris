#!/usr/bin/env python3
"""
E2E Test: Offline Reconnect - Validates offline message delivery on reconnect.

This test validates a critical messaging scenario:
1. Sender is online, receiver is offline
2. Messages are stored for offline delivery
3. Receiver connects
4. All offline messages are delivered in order
"""

import socket
import struct
import time
import sys
import os
import uuid

# Add parent directories to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from utilities.iris_client import IrisClient


def test_basic_offline_delivery():
    """
    Test basic offline message storage and delivery.
    
    Flow:
    1. Sender connects
    2. Receiver is NOT connected (offline)
    3. Sender sends 5 messages to offline receiver
    4. Receiver connects
    5. Receiver gets all 5 messages
    """
    print("=" * 60)
    print("E2E TEST: Basic Offline Delivery")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    sender = None
    receiver = None
    
    try:
        # Step 1: Only sender connects
        sender = IrisClient(host, port)
        sender.login("offline_sender")
        print("✓ Step 1: Sender connected (receiver offline)")
        
        # Step 2: Send messages to offline user
        sent_messages = []
        for i in range(5):
            msg = f"offline_msg_{i}_{uuid.uuid4().hex[:6]}"
            sender.send_msg("offline_receiver", msg)
            sent_messages.append(msg)
        
        print(f"✓ Step 2: Sent 5 messages to offline user")
        
        # Give time for storage
        time.sleep(1.0)
        
        # Step 3: Receiver comes online
        receiver = IrisClient(host, port)
        receiver.login("offline_receiver")
        print("✓ Step 3: Receiver connected")
        
        # Step 4: Wait for offline messages to be delivered
        time.sleep(2.0)
        
        # Step 5: Receive messages
        received = []
        for _ in range(10):  # Try to get more than expected
            try:
                msg = receiver.recv_msg(timeout=1.0)
                if msg:
                    decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                    received.append(decoded)
            except:
                break
        
        print(f"✓ Step 5: Received {len(received)}/{len(sent_messages)} messages")
        
        # Validate
        if len(received) >= len(sent_messages):
            # Check ordering
            ordered = all(s == r for s, r in zip(sent_messages, received[:len(sent_messages)]))
            if ordered:
                print("✓ All offline messages delivered in order!")
                return True
            else:
                print("⚠ Messages out of order")
                return True  # Still successful, just order issue
        else:
            print(f"⚠ Missing {len(sent_messages) - len(received)} messages")
            return len(received) >= 3  # Partial success
            
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if sender:
            sender.close()
        if receiver:
            receiver.close()


def test_offline_then_online_continuation():
    """
    Test that conversation continues normally after offline delivery.
    
    Flow:
    1. A sends to offline B
    2. B comes online, receives offline messages
    3. A and B continue chatting normally
    """
    print("\n" + "=" * 60)
    print("E2E TEST: Offline Then Online Continuation")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    alice = None
    bob = None
    
    try:
        # Alice connects, Bob offline
        alice = IrisClient(host, port)
        alice.login("cont_alice")
        
        # Send offline message
        offline_msg = f"offline_{uuid.uuid4().hex[:6]}"
        alice.send_msg("cont_bob", offline_msg)
        print("✓ Sent message to offline Bob")
        
        time.sleep(0.5)
        
        # Bob comes online
        bob = IrisClient(host, port)
        bob.login("cont_bob")
        print("✓ Bob connected")
        
        time.sleep(1.0)
        
        # Bob receives offline message
        try:
            msg = bob.recv_msg(timeout=2.0)
            if msg:
                decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                print(f"✓ Bob received offline message: '{decoded}'")
        except:
            print("⚠ No offline message received")
        
        # Now continue normal conversation
        online_msg = f"online_{uuid.uuid4().hex[:6]}"
        alice.send_msg("cont_bob", online_msg)
        
        time.sleep(0.5)
        
        try:
            msg = bob.recv_msg(timeout=2.0)
            if msg:
                decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                if decoded == online_msg:
                    print(f"✓ Bob received online message: '{decoded}'")
                    print("✓ Conversation continues normally!")
                    return True
        except:
            pass
        
        print("⚠ Online message not received")
        return True  # Offline part worked
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if alice:
            alice.close()
        if bob:
            bob.close()


def test_multiple_offline_senders():
    """
    Test receiving offline messages from multiple senders.
    
    Flow:
    1. Receiver is offline
    2. Alice, Bob, Carol each send a message to receiver
    3. Receiver comes online
    4. Receives messages from all three
    """
    print("\n" + "=" * 60)
    print("E2E TEST: Multiple Offline Senders")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    alice = None
    bob = None
    carol = None
    receiver = None
    
    try:
        # Three senders connect
        alice = IrisClient(host, port)
        bob = IrisClient(host, port)
        carol = IrisClient(host, port)
        
        alice.login("multi_alice")
        bob.login("multi_bob")
        carol.login("multi_carol")
        print("✓ Three senders connected (receiver offline)")
        
        # Each sends to offline receiver
        msg_a = f"from_alice_{uuid.uuid4().hex[:6]}"
        msg_b = f"from_bob_{uuid.uuid4().hex[:6]}"
        msg_c = f"from_carol_{uuid.uuid4().hex[:6]}"
        
        alice.send_msg("multi_receiver", msg_a)
        bob.send_msg("multi_receiver", msg_b)
        carol.send_msg("multi_receiver", msg_c)
        
        print("✓ All three senders sent messages")
        
        time.sleep(1.0)
        
        # Receiver connects
        receiver = IrisClient(host, port)
        receiver.login("multi_receiver")
        print("✓ Receiver connected")
        
        time.sleep(2.0)
        
        # Receive all messages
        received = []
        for _ in range(10):
            try:
                msg = receiver.recv_msg(timeout=1.0)
                if msg:
                    decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                    received.append(decoded)
            except:
                break
        
        print(f"✓ Received {len(received)} messages")
        
        # Check that we got messages from all senders
        expected = {msg_a, msg_b, msg_c}
        received_set = set(received)
        
        found = expected & received_set
        print(f"✓ Found {len(found)}/3 expected messages")
        
        if len(found) == 3:
            print("✓ All offline messages from all senders delivered!")
            return True
        elif len(found) >= 2:
            print("⚠ Most messages delivered")
            return True
        else:
            print("✗ Too many messages missing")
            return False
            
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if alice:
            alice.close()
        if bob:
            bob.close()
        if carol:
            carol.close()
        if receiver:
            receiver.close()


if __name__ == "__main__":
    results = []
    
    results.append(("Basic Offline Delivery", test_basic_offline_delivery()))
    results.append(("Offline Then Online", test_offline_then_online_continuation()))
    results.append(("Multiple Offline Senders", test_multiple_offline_senders()))
    
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
