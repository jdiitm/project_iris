#!/usr/bin/env python3
"""
Test: Offline Message Storage and Delivery

Validates offline messaging functionality:
- Messages stored when recipient offline
- Messages delivered on login
- Delete-after-read behavior
- No message loss during offline periods

Tier: 0 (Required on every merge)
Safe for laptop: Yes
Expected duration: <60s
"""

import sys
import os
import time

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework import TestLogger, ClusterManager
from tests.framework.assertions import MessageTracker, assert_no_message_loss, wait_for_condition
from tests.utilities import IrisClient


def test_offline_message_storage():
    """Test that messages to offline users are stored."""
    
    with TestLogger("test_offline_message_storage", "integration") as log:
        tracker = MessageTracker()
        
        # Charlie is offline
        charlie_user = f"charlie_{int(time.time())}"
        
        # Alice sends message to offline Charlie
        alice = IrisClient()
        alice.login(f"alice_off_{int(time.time())}")
        log.connection_event("login", "alice")
        
        msg = "Hello offline Charlie!"
        msg_id = "offline_msg_1"
        tracker.record_sent(msg_id, charlie_user, msg)
        
        alice.send_msg(charlie_user, msg)
        log.message_sent(msg_id, charlie_user, source_user="alice")
        log.info("sent_to_offline", f"Sent message to offline user {charlie_user}")
        
        alice.close()
        
        # Wait a moment for storage
        time.sleep(1)
        
        # Charlie comes online
        log.info("coming_online", f"{charlie_user} logging in")
        charlie = IrisClient()
        charlie.login(charlie_user)
        log.connection_event("login", charlie_user)
        
        # Charlie should receive the offline message
        try:
            start = time.monotonic()
            received = charlie.recv_msg(timeout=10.0)
            latency_ms = (time.monotonic() - start) * 1000
            
            tracker.record_received(msg_id)
            log.message_received(msg_id, latency_ms, source_user="alice")
            
            if msg.encode() in received or received == msg.encode():
                log.info("validation", "Offline message content correct")
            else:
                log.info("validation", f"Received: {received}")
                
        except Exception as e:
            log.error("receive", f"Failed to receive offline message: {e}")
            charlie.close()
            raise
        
        charlie.close()
        
        assert_no_message_loss(tracker)
        log.info("result", "Test PASSED")
        return True


def test_delete_after_read():
    """Test that offline messages are deleted after delivery."""
    
    with TestLogger("test_delete_after_read", "integration") as log:
        
        dave_user = f"dave_{int(time.time())}"
        
        # Send message while Dave is offline
        sender = IrisClient()
        sender.login(f"sender_dar_{int(time.time())}")
        sender.send_msg(dave_user, "Message for Dave")
        log.message_sent("dar_msg_1", dave_user)
        sender.close()
        
        time.sleep(1)
        
        # Dave comes online and receives
        dave = IrisClient()
        dave.login(dave_user)
        msg = dave.recv_msg(timeout=10.0)
        log.message_received("dar_msg_1", 0)
        log.info("first_login", f"Dave received: {msg}")
        dave.close()
        
        time.sleep(1)
        
        # Dave reconnects - should NOT receive the same message again
        dave2 = IrisClient()
        dave2.login(dave_user)
        log.connection_event("login", f"{dave_user}_second")
        
        try:
            # Should timeout waiting for message (none should exist)
            dave2.sock.settimeout(3.0)
            try:
                msg = dave2.recv_msg(timeout=3.0)
                # If we get here, we received something unexpected
                log.error("duplicate", f"Unexpected message on re-login: {msg}")
                dave2.close()
                return False
            except Exception:
                # Expected - no message
                log.info("validation", "No duplicate message on re-login (expected)")
        finally:
            dave2.close()
        
        log.info("result", "Test PASSED - delete-after-read verified")
        return True


def test_multiple_offline_messages():
    """Test that multiple offline messages are all delivered."""
    
    with TestLogger("test_multiple_offline_messages", "integration") as log:
        tracker = MessageTracker()
        NUM_MESSAGES = 5
        
        eve_user = f"eve_{int(time.time())}"
        
        # Send multiple messages while Eve is offline
        for i in range(NUM_MESSAGES):
            sender = IrisClient()
            sender.login(f"sender_{i}_{int(time.time())}")
            msg_id = f"multi_off_{i}"
            tracker.record_sent(msg_id, eve_user, f"Message {i}")
            sender.send_msg(eve_user, f"Message {i}")
            log.message_sent(msg_id, eve_user)
            sender.close()
            time.sleep(0.2)  # Small delay between senders
        
        log.info("sending_complete", f"Sent {NUM_MESSAGES} messages to offline {eve_user}")
        time.sleep(1)
        
        # Eve comes online
        eve = IrisClient()
        eve.login(eve_user)
        log.connection_event("login", eve_user)
        
        # Receive all messages
        received_count = 0
        for i in range(NUM_MESSAGES):
            try:
                msg = eve.recv_msg(timeout=5.0)
                msg_id = f"multi_off_{i}"
                tracker.record_received(msg_id)
                received_count += 1
                log.message_received(msg_id, 0)
            except Exception as e:
                log.error("receive", f"Failed to receive message {i}: {e}")
                break
        
        eve.close()
        
        log.metric("messages_sent", NUM_MESSAGES)
        log.metric("messages_received", received_count)
        
        if received_count == NUM_MESSAGES:
            log.info("result", "Test PASSED")
            return True
        else:
            log.error("result", f"Only received {received_count}/{NUM_MESSAGES}")
            return False


def main():
    """Run all offline storage tests."""
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        print("[SETUP] Starting cluster...")
        if not cluster.start():
            print("[ERROR] Failed to start cluster")
            return 1
    
    tests = [
        test_offline_message_storage,
        test_delete_after_read,
        test_multiple_offline_messages
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
            import traceback
            traceback.print_exc()
    
    print(f"\n{'='*60}")
    print(f"Results: {passed}/{passed+failed} passed")
    print('='*60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
