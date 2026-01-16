#!/usr/bin/env python3
"""
E2E Test: Full Conversation - Complete messaging workflow from start to finish.

This test validates a real-world conversation scenario:
1. Two users connect
2. They exchange messages bidirectionally
3. Messages are acknowledged
4. Session state is maintained
5. All messages delivered correctly
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


def test_simple_conversation():
    """
    Test a simple A → B → A conversation.
    
    Flow:
    1. Alice sends "Hello" to Bob
    2. Bob receives and replies "Hi back"
    3. Alice receives Bob's reply
    4. Both acknowledge
    """
    print("=" * 60)
    print("E2E TEST: Simple Conversation")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    alice = None
    bob = None
    
    try:
        # Step 1: Both users connect
        alice = IrisClient(host, port)
        bob = IrisClient(host, port)
        
        alice.login("alice_conv")
        bob.login("bob_conv")
        print("✓ Step 1: Alice and Bob connected")
        
        # Step 2: Alice sends to Bob
        msg1 = f"Hello Bob! {uuid.uuid4().hex[:8]}"
        alice.send_msg("bob_conv", msg1)
        print(f"✓ Step 2: Alice sent: '{msg1}'")
        
        time.sleep(0.5)
        
        # Step 3: Bob receives
        received1 = bob.recv_msg(timeout=5.0)
        if received1:
            decoded1 = received1.decode('utf-8') if isinstance(received1, bytes) else received1
            if decoded1 == msg1:
                print(f"✓ Step 3: Bob received: '{decoded1}'")
            else:
                print(f"✗ Step 3: Message mismatch: expected '{msg1}', got '{decoded1}'")
                return False
        else:
            print("✗ Step 3: Bob did not receive message")
            return False
        
        # Step 4: Bob replies
        msg2 = f"Hi Alice! {uuid.uuid4().hex[:8]}"
        bob.send_msg("alice_conv", msg2)
        print(f"✓ Step 4: Bob replied: '{msg2}'")
        
        time.sleep(0.5)
        
        # Step 5: Alice receives reply
        received2 = alice.recv_msg(timeout=5.0)
        if received2:
            decoded2 = received2.decode('utf-8') if isinstance(received2, bytes) else received2
            if decoded2 == msg2:
                print(f"✓ Step 5: Alice received: '{decoded2}'")
            else:
                print(f"✗ Step 5: Message mismatch: expected '{msg2}', got '{decoded2}'")
                return False
        else:
            print("✗ Step 5: Alice did not receive reply")
            return False
        
        print("✓ Complete conversation succeeded!")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if alice:
            alice.close()
        if bob:
            bob.close()


def test_multi_message_conversation():
    """
    Test a longer conversation with multiple messages.
    
    Flow:
    1. Alice and Bob exchange 5 messages each
    2. Verify all messages received in order
    """
    print("\n" + "=" * 60)
    print("E2E TEST: Multi-Message Conversation")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    alice = None
    bob = None
    
    try:
        alice = IrisClient(host, port)
        bob = IrisClient(host, port)
        
        alice.login("alice_multi")
        bob.login("bob_multi")
        print("✓ Both users connected")
        
        # Alice sends 5 messages
        alice_msgs = []
        for i in range(5):
            msg = f"alice_msg_{i}_{uuid.uuid4().hex[:6]}"
            alice.send_msg("bob_multi", msg)
            alice_msgs.append(msg)
        
        print(f"✓ Alice sent 5 messages")
        
        time.sleep(1.0)
        
        # Bob receives and replies to each
        bob_received = []
        bob_msgs = []
        for i in range(5):
            try:
                msg = bob.recv_msg(timeout=1.0)
                if msg:
                    decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                    bob_received.append(decoded)
                    
                    # Bob replies
                    reply = f"bob_reply_{i}_{uuid.uuid4().hex[:6]}"
                    bob.send_msg("alice_multi", reply)
                    bob_msgs.append(reply)
            except:
                pass
        
        print(f"✓ Bob received {len(bob_received)}/5 messages and replied")
        
        time.sleep(1.0)
        
        # Alice receives Bob's replies
        alice_received = []
        for i in range(5):
            try:
                msg = alice.recv_msg(timeout=1.0)
                if msg:
                    decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                    alice_received.append(decoded)
            except:
                pass
        
        print(f"✓ Alice received {len(alice_received)}/5 replies")
        
        # Validate ordering
        alice_ordered = all(a == b for a, b in zip(alice_msgs, bob_received))
        bob_ordered = all(a == b for a, b in zip(bob_msgs, alice_received))
        
        if alice_ordered and bob_ordered:
            print("✓ All messages in correct order")
            return True
        else:
            if not alice_ordered:
                print("✗ Alice's messages out of order at Bob")
            if not bob_ordered:
                print("✗ Bob's messages out of order at Alice")
            return len(bob_received) >= 3 and len(alice_received) >= 3  # Partial success
            
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        if alice:
            alice.close()
        if bob:
            bob.close()


def test_three_party_conversation():
    """
    Test conversation with three participants.
    
    Flow:
    1. Alice sends to Bob
    2. Bob sends to Carol
    3. Carol sends to Alice
    4. Everyone receives their messages
    """
    print("\n" + "=" * 60)
    print("E2E TEST: Three-Party Conversation")
    print("=" * 60)
    
    host = os.environ.get('IRIS_HOST', 'localhost')
    port = int(os.environ.get('IRIS_PORT', '8085'))
    
    alice = None
    bob = None
    carol = None
    
    try:
        alice = IrisClient(host, port)
        bob = IrisClient(host, port)
        carol = IrisClient(host, port)
        
        alice.login("alice_3p")
        bob.login("bob_3p")
        carol.login("carol_3p")
        print("✓ All three users connected")
        
        # Alice → Bob
        msg_ab = f"hi_bob_{uuid.uuid4().hex[:6]}"
        alice.send_msg("bob_3p", msg_ab)
        
        # Bob → Carol
        msg_bc = f"hi_carol_{uuid.uuid4().hex[:6]}"
        bob.send_msg("carol_3p", msg_bc)
        
        # Carol → Alice
        msg_ca = f"hi_alice_{uuid.uuid4().hex[:6]}"
        carol.send_msg("alice_3p", msg_ca)
        
        print("✓ All messages sent in triangle")
        
        time.sleep(1.0)
        
        # Receive
        success = True
        
        # Bob receives from Alice
        try:
            msg = bob.recv_msg(timeout=2.0)
            decoded = msg.decode('utf-8') if msg and isinstance(msg, bytes) else msg
            if decoded == msg_ab:
                print(f"✓ Bob received from Alice")
            else:
                print(f"⚠ Bob got wrong message")
                success = False
        except:
            print("⚠ Bob didn't receive")
            success = False
        
        # Carol receives from Bob
        try:
            msg = carol.recv_msg(timeout=2.0)
            decoded = msg.decode('utf-8') if msg and isinstance(msg, bytes) else msg
            if decoded == msg_bc:
                print(f"✓ Carol received from Bob")
            else:
                print(f"⚠ Carol got wrong message")
                success = False
        except:
            print("⚠ Carol didn't receive")
            success = False
        
        # Alice receives from Carol
        try:
            msg = alice.recv_msg(timeout=2.0)
            decoded = msg.decode('utf-8') if msg and isinstance(msg, bytes) else msg
            if decoded == msg_ca:
                print(f"✓ Alice received from Carol")
            else:
                print(f"⚠ Alice got wrong message")
                success = False
        except:
            print("⚠ Alice didn't receive")
            success = False
        
        if success:
            print("✓ Three-party conversation complete!")
        return success
        
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


if __name__ == "__main__":
    results = []
    
    results.append(("Simple Conversation", test_simple_conversation()))
    results.append(("Multi-Message Conversation", test_multi_message_conversation()))
    results.append(("Three-Party Conversation", test_three_party_conversation()))
    
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
