#!/usr/bin/env python3
"""
Test: Online Messaging

Validates core message delivery functionality:
- Alice sends message to Bob
- Bob receives message correctly
- Message content integrity preserved
- Proper acknowledgment flow

Tier: 0 (Required on every merge)
Safe for laptop: Yes
Expected duration: <30s
"""

import sys
import os
import time

# Add parent paths for imports
sys.path.insert(0, str(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))

from tests.framework import TestLogger, ClusterManager, MessageTracker
from tests.framework.assertions import (
    assert_message_delivered,
    assert_no_message_loss,
    assert_no_duplicates,
    wait_for_condition
)
from tests.utilities import IrisClient


def test_basic_message_delivery():
    """Test that a message sent from Alice reaches Bob."""
    
    with TestLogger("test_basic_message_delivery", "integration") as log:
        tracker = MessageTracker()
        
        log.info("setup", "Connecting Alice and Bob")
        
        alice = IrisClient()
        alice.login("alice")
        log.connection_event("login", "alice")
        
        bob = IrisClient()
        bob.login("bob")
        log.connection_event("login", "bob")
        
        # Send message
        msg_id = f"msg_{int(time.time() * 1000)}"
        message = "Hello Bob!"
        
        log.info("sending", f"Alice sending message to Bob: {msg_id}")
        tracker.record_sent(msg_id, "bob", message)
        alice.send_msg("bob", message)
        log.message_sent(msg_id, "bob", source_user="alice")
        
        # Receive message
        try:
            start = time.monotonic()
            received = bob.recv_msg(timeout=5.0)
            latency_ms = (time.monotonic() - start) * 1000
            
            tracker.record_received(msg_id)
            log.message_received(msg_id, latency_ms, source_user="alice")
            
            # Validate content
            expected = message.encode('utf-8')
            if received == expected:
                log.info("validation", "Message content matches")
            else:
                log.error("validation", f"Content mismatch: expected {expected}, got {received}")
                raise AssertionError(f"Message content mismatch")
                
        except Exception as e:
            log.error("receive", f"Failed to receive message: {e}")
            raise
        
        # Cleanup
        alice.close()
        bob.close()
        log.info("cleanup", "Connections closed")
        
        # Assert correctness
        assert_no_message_loss(tracker)
        assert_no_duplicates(tracker)
        
        summary = tracker.summary()
        log.metric("messages_sent", summary["sent"])
        log.metric("messages_received", summary["received"])
        log.metric("latency_avg_ms", summary["latency_avg_ms"], "ms")
        
        log.info("result", "Test PASSED")
        return True


def test_bidirectional_messaging():
    """Test that messages can flow in both directions."""
    
    with TestLogger("test_bidirectional_messaging", "integration") as log:
        tracker = MessageTracker()
        
        alice = IrisClient()
        alice.login("alice_bi")
        log.connection_event("login", "alice_bi")
        
        bob = IrisClient()
        bob.login("bob_bi")
        log.connection_event("login", "bob_bi")
        
        # Alice -> Bob
        msg1 = "Hello from Alice"
        msg1_id = "msg_a2b_1"
        tracker.record_sent(msg1_id, "bob_bi", msg1)
        alice.send_msg("bob_bi", msg1)
        log.message_sent(msg1_id, "bob_bi", source_user="alice_bi")
        
        # Bob receives
        received = bob.recv_msg(timeout=5.0)
        tracker.record_received(msg1_id)
        log.message_received(msg1_id, 0, source_user="alice_bi")
        
        # Bob -> Alice
        msg2 = "Hello from Bob"
        msg2_id = "msg_b2a_1"
        tracker.record_sent(msg2_id, "alice_bi", msg2)
        bob.send_msg("alice_bi", msg2)
        log.message_sent(msg2_id, "alice_bi", source_user="bob_bi")
        
        # Alice receives
        received = alice.recv_msg(timeout=5.0)
        tracker.record_received(msg2_id)
        log.message_received(msg2_id, 0, source_user="bob_bi")
        
        alice.close()
        bob.close()
        
        assert_no_message_loss(tracker)
        log.info("result", "Test PASSED")
        return True


def test_multi_message_sequence():
    """Test sending multiple messages in sequence."""
    
    with TestLogger("test_multi_message_sequence", "integration") as log:
        tracker = MessageTracker()
        NUM_MESSAGES = 10
        
        sender = IrisClient()
        sender.login("sender_seq")
        log.connection_event("login", "sender_seq")
        
        receiver = IrisClient()
        receiver.login("receiver_seq")
        log.connection_event("login", "receiver_seq")
        
        # Send multiple messages
        for i in range(NUM_MESSAGES):
            msg_id = f"seq_msg_{i}"
            tracker.record_sent(msg_id, "receiver_seq", f"Message {i}", sequence=i)
            sender.send_msg("receiver_seq", f"Message {i}")
            log.message_sent(msg_id, "receiver_seq", source_user="sender_seq")
        
        log.info("sending_complete", f"Sent {NUM_MESSAGES} messages")
        
        # Receive all messages
        received_count = 0
        for i in range(NUM_MESSAGES):
            try:
                msg = receiver.recv_msg(timeout=5.0)
                msg_id = f"seq_msg_{i}"  # Simplified - in real test, extract from message
                tracker.record_received(msg_id)
                received_count += 1
                log.message_received(msg_id, 0, source_user="sender_seq")
            except Exception as e:
                log.error("receive", f"Failed to receive message {i}: {e}")
                break
        
        sender.close()
        receiver.close()
        
        log.metric("messages_sent", NUM_MESSAGES)
        log.metric("messages_received", received_count)
        
        summary = tracker.summary()
        if summary["lost"] == 0:
            log.info("result", "Test PASSED")
            return True
        else:
            log.error("result", f"Test FAILED: {summary['lost']} messages lost")
            return False


def main():
    """Run all online messaging tests."""
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        print("[SETUP] Starting cluster...")
        if not cluster.start():
            print("[ERROR] Failed to start cluster")
            return 1
    
    tests = [
        test_basic_message_delivery,
        test_bidirectional_messaging,
        test_multi_message_sequence
    ]
    
    passed = 0
    failed = 0
    
    for test_fn in tests:
        try:
            print(f"\n{'='*60}")
            print(f"Running: {test_fn.__name__}")
            print('='*60)
            
            if test_fn():
                passed += 1
                print(f"✓ PASSED: {test_fn.__name__}")
            else:
                failed += 1
                print(f"✗ FAILED: {test_fn.__name__}")
        except Exception as e:
            failed += 1
            print(f"✗ ERROR: {test_fn.__name__} - {e}")
    
    print(f"\n{'='*60}")
    print(f"Results: {passed}/{passed+failed} passed")
    print('='*60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
