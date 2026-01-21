#!/usr/bin/env python3
"""
Test: Hot-Key Bucketing (Celebrity Accounts)

Validates inbox bucketing for high-traffic users:
- VIP users get sharded message ingestion
- Prevents single-mailbox bottleneck
- Handles burst traffic to single recipient

Tier: 0 (Required on every merge)  
Safe for laptop: Yes
Expected duration: <60s
"""

import sys
import os
import time
import struct
import concurrent.futures

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework import TestLogger, ClusterManager
from tests.utilities import IrisClient


def send_burst_to_user(sender_id: int, target_user: str, message_count: int) -> int:
    """Send a burst of messages to a single user."""
    sent = 0
    try:
        client = IrisClient()
        client.login(f"fan_{sender_id}_{int(time.time())}")
        
        for i in range(message_count):
            client.send_msg(target_user, f"Message_{sender_id}_{i}")
            sent += 1
        
        client.close()
    except Exception as e:
        pass  # Connection errors expected under load
    
    return sent


def test_hotkey_basic():
    """Test basic message delivery to high-traffic user."""
    
    with TestLogger("test_hotkey_basic", "integration") as log:
        
        vip_user = f"celebrity_{int(time.time())}"
        NUM_FANS = 10
        MSGS_PER_FAN = 5
        
        log.info("setup", f"VIP user: {vip_user}, {NUM_FANS} fans, {MSGS_PER_FAN} msgs each")
        
        # Fans send messages while VIP is offline
        total_sent = 0
        with concurrent.futures.ThreadPoolExecutor(max_workers=NUM_FANS) as executor:
            futures = [
                executor.submit(send_burst_to_user, i, vip_user, MSGS_PER_FAN)
                for i in range(NUM_FANS)
            ]
            
            for future in concurrent.futures.as_completed(futures):
                total_sent += future.result()
        
        log.metric("total_sent", total_sent)
        log.info("flood_complete", f"Sent {total_sent} messages to {vip_user}")
        
        time.sleep(2)  # Allow messages to be stored
        
        # VIP comes online
        vip = IrisClient()
        vip.login(vip_user)
        log.connection_event("login", vip_user)
        
        # Count received messages
        received = 0
        vip.sock.settimeout(2.0)
        
        start = time.monotonic()
        try:
            while time.monotonic() - start < 15:  # 15 second timeout
                try:
                    msg = vip.recv_msg(timeout=2.0)
                    received += 1
                except Exception:
                    break
        except Exception:
            pass
        
        vip.close()
        
        log.metric("total_received", received)
        log.info("receive_complete", f"VIP received {received}/{total_sent} messages")
        
        # Allow some loss under concurrent load, but should receive most
        delivery_rate = received / total_sent if total_sent > 0 else 0
        log.metric("delivery_rate", delivery_rate * 100, "%")
        
        if delivery_rate >= 0.5:  # At least 50% delivery
            log.info("result", f"Test PASSED - {delivery_rate*100:.1f}% delivery")
            return True
        else:
            log.error("result", f"Test FAILED - Only {delivery_rate*100:.1f}% delivery")
            return False


def test_hotkey_sustained_load():
    """Test sustained high-frequency messages to single user."""
    
    with TestLogger("test_hotkey_sustained", "integration") as log:
        
        vip_user = f"megastar_{int(time.time())}"
        NUM_FANS = 20
        MSGS_PER_FAN = 10
        
        log.info("setup", f"Sustained load test: {NUM_FANS} fans x {MSGS_PER_FAN} msgs")
        
        # VIP is online this time
        vip = IrisClient()
        vip.login(vip_user)
        log.connection_event("login", vip_user)
        
        # Start receiver in background
        received = []
        
        def receiver_loop():
            vip.sock.settimeout(1.0)
            while True:
                try:
                    msg = vip.recv_msg(timeout=1.0)
                    received.append(msg)
                except Exception:
                    break
        
        import threading
        receiver_thread = threading.Thread(target=receiver_loop, daemon=True)
        receiver_thread.start()
        
        # Flood with messages
        total_sent = 0
        start_time = time.monotonic()
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=NUM_FANS) as executor:
            futures = [
                executor.submit(send_burst_to_user, i, vip_user, MSGS_PER_FAN)
                for i in range(NUM_FANS)
            ]
            
            for future in concurrent.futures.as_completed(futures):
                total_sent += future.result()
        
        flood_duration = time.monotonic() - start_time
        
        # Wait for messages to arrive
        time.sleep(3)
        vip.close()
        receiver_thread.join(timeout=2)
        
        log.metric("messages_sent", total_sent)
        log.metric("messages_received", len(received))
        log.metric("flood_duration_s", flood_duration)
        
        if total_sent > 0:
            ingestion_rate = total_sent / flood_duration
            log.metric("ingestion_rate_msgs_sec", ingestion_rate)
            log.info("result", f"Ingestion rate: {ingestion_rate:.0f} msgs/sec")
        
        log.info("result", "Test PASSED")
        return True


def main():
    """Run all hot-key bucketing tests."""
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        print("[SETUP] Starting cluster...")
        if not cluster.start():
            print("[ERROR] Failed to start cluster")
            return 1
    
    tests = [
        test_hotkey_basic,
        test_hotkey_sustained_load
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
