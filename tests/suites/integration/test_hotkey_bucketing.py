#!/usr/bin/env python3
"""
Test: Hot-Key Bucketing (Celebrity Accounts)

Validates inbox bucketing for high-traffic users:
- VIP users get sharded message ingestion
- Prevents single-mailbox bottleneck
- Handles burst traffic to single recipient

INVARIANTS:
- Delivery rate must be >= 95% for basic test
- Sustained load must achieve >= 80% delivery
- Ingestion rate must exceed 10 msgs/sec minimum
- No silent message loss without logging

Tier: 0 (Required on every merge)  
Safe for laptop: Yes
Expected duration: <60s
"""

import sys
import os
import time
import struct
import concurrent.futures
import socket
import threading

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework import TestLogger, ClusterManager
from tests.utilities import IrisClient

# Thresholds
MIN_DELIVERY_RATE_BASIC = 0.95  # 95% delivery for basic test
MIN_DELIVERY_RATE_SUSTAINED = 0.80  # 80% delivery under sustained load
MIN_INGESTION_RATE = 10  # msgs/sec minimum


def log_global(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def send_burst_to_user(sender_id: int, target_user: str, message_count: int) -> tuple:
    """
    Send a burst of messages to a single user.
    
    Returns:
        Tuple of (sent_count, error_count, error_details)
    """
    sent = 0
    errors = 0
    error_details = []
    
    try:
        client = IrisClient()
        client.login(f"fan_{sender_id}_{int(time.time())}")
        
        for i in range(message_count):
            try:
                client.send_msg(target_user, f"Message_{sender_id}_{i}")
                sent += 1
            except socket.timeout:
                errors += 1
                error_details.append(f"sender_{sender_id}: send timeout on msg {i}")
            except socket.error as e:
                errors += 1
                error_details.append(f"sender_{sender_id}: socket error - {e}")
                break  # Socket likely dead, stop trying
        
        client.close()
    except socket.error as e:
        errors += 1
        error_details.append(f"sender_{sender_id}: connection error - {e}")
    except Exception as e:
        errors += 1
        error_details.append(f"sender_{sender_id}: unexpected error - {type(e).__name__}: {e}")
    
    return (sent, errors, error_details)


def test_hotkey_basic():
    """Test basic message delivery to high-traffic user."""
    
    with TestLogger("test_hotkey_basic", "integration") as log:
        
        vip_user = f"celebrity_{int(time.time())}"
        NUM_FANS = 10
        MSGS_PER_FAN = 5
        
        log.info("setup", f"VIP user: {vip_user}, {NUM_FANS} fans, {MSGS_PER_FAN} msgs each")
        log.info("threshold", f"Required delivery rate: {MIN_DELIVERY_RATE_BASIC*100:.0f}%")
        
        # Fans send messages while VIP is offline
        total_sent = 0
        total_errors = 0
        all_error_details = []
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=NUM_FANS) as executor:
            futures = [
                executor.submit(send_burst_to_user, i, vip_user, MSGS_PER_FAN)
                for i in range(NUM_FANS)
            ]
            
            for future in concurrent.futures.as_completed(futures):
                sent, errors, details = future.result()
                total_sent += sent
                total_errors += errors
                all_error_details.extend(details)
        
        log.metric("total_sent", total_sent)
        log.metric("send_errors", total_errors)
        log.info("flood_complete", f"Sent {total_sent} messages to {vip_user} ({total_errors} errors)")
        
        # Log send errors if any
        if all_error_details:
            log.info("send_errors_detail", f"{len(all_error_details)} errors during send")
            for detail in all_error_details[:5]:
                log.info("error", detail)
        
        time.sleep(2)  # Allow messages to be stored
        
        # VIP comes online
        vip = IrisClient()
        vip.login(vip_user)
        log.connection_event("login", vip_user)
        
        # Count received messages
        received = 0
        receive_errors = 0
        vip.sock.settimeout(2.0)
        
        start = time.monotonic()
        while time.monotonic() - start < 15:  # 15 second timeout
            try:
                msg = vip.recv_msg(timeout=2.0)
                if msg:
                    received += 1
            except socket.timeout:
                # No more messages available
                break
            except socket.error as e:
                log.info("receive_error", f"Socket error during receive: {e}")
                receive_errors += 1
                break
            except Exception as e:
                log.info("receive_error", f"Unexpected error: {type(e).__name__}: {e}")
                receive_errors += 1
                break
        
        vip.close()
        
        log.metric("total_received", received)
        log.metric("receive_errors", receive_errors)
        log.info("receive_complete", f"VIP received {received}/{total_sent} messages")
        
        # Calculate delivery rate
        delivery_rate = received / total_sent if total_sent > 0 else 0
        log.metric("delivery_rate", delivery_rate * 100, "%")
        
        # ASSERTION: delivery rate must meet threshold
        if delivery_rate >= MIN_DELIVERY_RATE_BASIC:
            log.info("result", f"PASS - Delivery rate {delivery_rate*100:.1f}% >= {MIN_DELIVERY_RATE_BASIC*100:.0f}%")
            return True
        else:
            log.error("result", f"FAIL - Delivery rate {delivery_rate*100:.1f}% < {MIN_DELIVERY_RATE_BASIC*100:.0f}%")
            return False


def test_hotkey_sustained_load():
    """Test sustained high-frequency messages to single user."""
    
    with TestLogger("test_hotkey_sustained", "integration") as log:
        
        vip_user = f"megastar_{int(time.time())}"
        NUM_FANS = 20
        MSGS_PER_FAN = 10
        
        log.info("setup", f"Sustained load test: {NUM_FANS} fans x {MSGS_PER_FAN} msgs")
        log.info("threshold", f"Required delivery rate: {MIN_DELIVERY_RATE_SUSTAINED*100:.0f}%")
        
        # VIP is online this time
        vip = IrisClient()
        vip.login(vip_user)
        log.connection_event("login", vip_user)
        
        # Start receiver in background
        received = []
        receive_errors = []
        receiver_running = threading.Event()
        receiver_running.set()
        
        def receiver_loop():
            vip.sock.settimeout(1.0)
            while receiver_running.is_set():
                try:
                    msg = vip.recv_msg(timeout=1.0)
                    if msg:
                        received.append(msg)
                except socket.timeout:
                    # Normal - no message available
                    continue
                except socket.error as e:
                    receive_errors.append(f"Socket error: {e}")
                    break
                except Exception as e:
                    receive_errors.append(f"Unexpected error: {type(e).__name__}: {e}")
                    break
        
        receiver_thread = threading.Thread(target=receiver_loop, daemon=True)
        receiver_thread.start()
        
        # Flood with messages
        total_sent = 0
        total_errors = 0
        all_error_details = []
        start_time = time.monotonic()
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=NUM_FANS) as executor:
            futures = [
                executor.submit(send_burst_to_user, i, vip_user, MSGS_PER_FAN)
                for i in range(NUM_FANS)
            ]
            
            for future in concurrent.futures.as_completed(futures):
                sent, errors, details = future.result()
                total_sent += sent
                total_errors += errors
                all_error_details.extend(details)
        
        flood_duration = time.monotonic() - start_time
        
        # Wait for messages to arrive
        time.sleep(3)
        receiver_running.clear()
        vip.close()
        receiver_thread.join(timeout=2)
        
        log.metric("messages_sent", total_sent)
        log.metric("messages_received", len(received))
        log.metric("send_errors", total_errors)
        log.metric("flood_duration_s", flood_duration)
        
        # Log errors if any
        if all_error_details:
            log.info("send_errors_detail", f"{len(all_error_details)} errors during send")
            for detail in all_error_details[:5]:
                log.info("error", detail)
        
        if receive_errors:
            log.info("receive_errors", f"{len(receive_errors)} errors during receive")
            for err in receive_errors[:5]:
                log.info("error", err)
        
        # Calculate metrics
        delivery_rate = len(received) / total_sent if total_sent > 0 else 0
        ingestion_rate = total_sent / flood_duration if flood_duration > 0 else 0
        
        log.metric("delivery_rate", delivery_rate * 100, "%")
        log.metric("ingestion_rate_msgs_sec", ingestion_rate)
        
        log.info("stats", f"Delivery: {delivery_rate*100:.1f}%, Ingestion: {ingestion_rate:.0f} msgs/sec")
        
        # ASSERTIONS
        passed = True
        
        # Assertion 1: Delivery rate
        if delivery_rate >= MIN_DELIVERY_RATE_SUSTAINED:
            log.info("assertion", f"PASS - Delivery {delivery_rate*100:.1f}% >= {MIN_DELIVERY_RATE_SUSTAINED*100:.0f}%")
        else:
            log.error("assertion", f"FAIL - Delivery {delivery_rate*100:.1f}% < {MIN_DELIVERY_RATE_SUSTAINED*100:.0f}%")
            passed = False
        
        # Assertion 2: Ingestion rate
        if ingestion_rate >= MIN_INGESTION_RATE:
            log.info("assertion", f"PASS - Ingestion {ingestion_rate:.0f} msgs/sec >= {MIN_INGESTION_RATE}")
        else:
            log.error("assertion", f"FAIL - Ingestion {ingestion_rate:.0f} msgs/sec < {MIN_INGESTION_RATE}")
            passed = False
        
        if passed:
            log.info("result", "Test PASSED")
        else:
            log.error("result", "Test FAILED")
        
        return passed


def main():
    """Run all hot-key bucketing tests."""
    log_global("--- HOT-KEY BUCKETING TESTS ---")
    
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        log_global("[SETUP] Starting cluster...")
        if not cluster.start():
            log_global("[ERROR] Failed to start cluster")
            return 1
    
    tests = [
        test_hotkey_basic,
        test_hotkey_sustained_load
    ]
    
    passed = 0
    failed = 0
    
    for test_fn in tests:
        try:
            log_global(f"\n{'='*60}")
            log_global(f"Running: {test_fn.__name__}")
            log_global('='*60)
            
            if test_fn():
                passed += 1
                log_global(f"PASSED: {test_fn.__name__}")
            else:
                failed += 1
                log_global(f"FAILED: {test_fn.__name__}")
        except Exception as e:
            failed += 1
            log_global(f"ERROR: {test_fn.__name__} - {type(e).__name__}: {e}")
            import traceback
            traceback.print_exc()
    
    log_global(f"\n{'='*60}")
    log_global(f"Results: {passed}/{passed+failed} passed")
    log_global('='*60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
