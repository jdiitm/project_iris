#!/usr/bin/env python3
"""
Backpressure Tests

Tests:
1. Connection acceptance during load
2. Graceful degradation under pressure
3. Recovery after overload

INVARIANTS:
- Connection acceptance rate >= 90%
- Message throughput >= 500 msg/s
- Concurrent load success rate >= 80%
- System must recover after load

Tier: 0 (Required on every merge)
"""

import sys
import os
import time
import random
import string
import socket
import threading
import concurrent.futures

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework import TestLogger, ClusterManager
from tests.utilities import IrisClient

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Thresholds
MIN_CONNECTION_RATE = 0.90  # 90% connection success
MIN_THROUGHPUT = 500  # 500 msg/s minimum
MIN_CONCURRENT_SUCCESS_RATE = 0.80  # 80% success under concurrent load


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def random_user():
    """Generate a random username with deterministic seed offset."""
    return ''.join(random.choices(string.ascii_lowercase, k=8))


def test_connection_acceptance():
    """Test that connections are accepted under normal load."""
    log("\n=== Test: Connection Acceptance ===")
    log(f"Threshold: {MIN_CONNECTION_RATE*100:.0f}% connection success")
    
    clients = []
    success = 0
    failed = 0
    errors = []
    
    TARGET_CONNECTIONS = 20
    
    # Try to establish multiple connections
    for i in range(TARGET_CONNECTIONS):
        try:
            client = IrisClient()
            client.login(f"bp_user_{random_user()}")
            clients.append(client)
            success += 1
        except socket.timeout as e:
            failed += 1
            errors.append(f"Connection {i}: timeout")
        except socket.error as e:
            failed += 1
            errors.append(f"Connection {i}: socket error - {e}")
        except Exception as e:
            failed += 1
            errors.append(f"Connection {i}: unexpected - {type(e).__name__}: {e}")
    
    log(f"Connections: {success}/{TARGET_CONNECTIONS} success, {failed} failed")
    
    # Log errors if any
    if errors:
        for err in errors[:5]:
            log(f"  Error: {err}")
    
    # Cleanup
    for c in clients:
        try:
            c.close()
        except socket.error as e:
            log(f"  Cleanup warning: socket error closing client - {e}")
        except Exception as e:
            log(f"  Cleanup warning: {type(e).__name__} closing client - {e}")
    
    # ASSERTION
    connection_rate = success / TARGET_CONNECTIONS
    if connection_rate >= MIN_CONNECTION_RATE:
        log(f"PASS: Connection rate {connection_rate*100:.0f}% >= {MIN_CONNECTION_RATE*100:.0f}%")
        return True
    else:
        log(f"FAIL: Connection rate {connection_rate*100:.0f}% < {MIN_CONNECTION_RATE*100:.0f}%")
        return False


def test_message_throughput():
    """Test message throughput under load."""
    log("\n=== Test: Message Throughput ===")
    log(f"Threshold: {MIN_THROUGHPUT} msg/s minimum")
    
    try:
        sender = IrisClient()
        receiver = IrisClient()
    except Exception as e:
        log(f"FAIL: Could not create clients - {type(e).__name__}: {e}")
        return False
    
    sender_user = f"sender_{random_user()}"
    receiver_user = f"receiver_{random_user()}"
    
    try:
        sender.login(sender_user)
        receiver.login(receiver_user)
    except Exception as e:
        log(f"FAIL: Login failed - {type(e).__name__}: {e}")
        try:
            sender.close()
            receiver.close()
        except Exception:
            pass
        return False
    
    # Send messages rapidly
    count = 100
    errors = 0
    
    start = time.time()
    for i in range(count):
        try:
            sender.send_msg(receiver_user, f"throughput_{i}")
        except socket.error as e:
            errors += 1
            if errors == 1:
                log(f"  Send error at msg {i}: {e}")
        except Exception as e:
            errors += 1
            if errors == 1:
                log(f"  Send error at msg {i}: {type(e).__name__}: {e}")
    
    elapsed = time.time() - start
    successful_sends = count - errors
    rate = successful_sends / elapsed if elapsed > 0 else 0
    
    log(f"Sent {successful_sends}/{count} msgs in {elapsed:.2f}s ({rate:.1f} msg/s)")
    
    try:
        sender.close()
        receiver.close()
    except Exception as e:
        log(f"  Cleanup warning: {e}")
    
    # ASSERTION
    if rate >= MIN_THROUGHPUT:
        log(f"PASS: Throughput {rate:.0f} msg/s >= {MIN_THROUGHPUT}")
        return True
    else:
        log(f"FAIL: Throughput {rate:.0f} msg/s < {MIN_THROUGHPUT}")
        return False


def test_concurrent_load():
    """Test behavior under concurrent connection load."""
    log("\n=== Test: Concurrent Load ===")
    log(f"Threshold: {MIN_CONCURRENT_SUCCESS_RATE*100:.0f}% success rate")
    
    results = {"success": 0, "failed": 0, "errors": []}
    lock = threading.Lock()
    
    TARGET_CONCURRENT = 30
    
    def connect_and_send(worker_id):
        # Per-worker random for thread safety
        worker_random = random.Random(TEST_SEED + worker_id)
        user_suffix = ''.join(worker_random.choices(string.ascii_lowercase, k=8))
        
        try:
            client = IrisClient()
            user = f"concurrent_{user_suffix}"
            client.login(user)
            
            # Send a few messages
            for i in range(5):
                target_suffix = ''.join(worker_random.choices(string.ascii_lowercase, k=8))
                client.send_msg(f"target_{target_suffix}", f"msg_{i}")
            
            client.close()
            
            with lock:
                results["success"] += 1
            return True
        except socket.timeout as e:
            with lock:
                results["failed"] += 1
                results["errors"].append(f"Worker {worker_id}: timeout")
            return False
        except socket.error as e:
            with lock:
                results["failed"] += 1
                results["errors"].append(f"Worker {worker_id}: socket error - {e}")
            return False
        except Exception as e:
            with lock:
                results["failed"] += 1
                results["errors"].append(f"Worker {worker_id}: {type(e).__name__}: {e}")
            return False
    
    # Run concurrent connections
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = [executor.submit(connect_and_send, i) for i in range(TARGET_CONCURRENT)]
        concurrent.futures.wait(futures)
    
    log(f"Concurrent: {results['success']}/{TARGET_CONCURRENT} success, {results['failed']} failed")
    
    # Log errors if any
    if results["errors"]:
        for err in results["errors"][:5]:
            log(f"  Error: {err}")
    
    # ASSERTION
    success_rate = results["success"] / TARGET_CONCURRENT
    if success_rate >= MIN_CONCURRENT_SUCCESS_RATE:
        log(f"PASS: Success rate {success_rate*100:.0f}% >= {MIN_CONCURRENT_SUCCESS_RATE*100:.0f}%")
        return True
    else:
        log(f"FAIL: Success rate {success_rate*100:.0f}% < {MIN_CONCURRENT_SUCCESS_RATE*100:.0f}%")
        return False


def test_recovery_after_load():
    """Test that system recovers after high load."""
    log("\n=== Test: Recovery After Load ===")
    
    # First, create some load
    clients = []
    load_errors = 0
    
    for i in range(15):
        try:
            c = IrisClient()
            c.login(f"load_{random_user()}")
            clients.append(c)
        except socket.error as e:
            load_errors += 1
            log(f"  Load client {i} failed: socket error - {e}")
        except Exception as e:
            load_errors += 1
            log(f"  Load client {i} failed: {type(e).__name__}: {e}")
    
    log(f"Created {len(clients)} load clients ({load_errors} failed)")
    
    # Close all (simulate load drop)
    for i, c in enumerate(clients):
        try:
            c.close()
        except socket.error as e:
            log(f"  Cleanup client {i}: socket error - {e}")
        except Exception as e:
            log(f"  Cleanup client {i}: {type(e).__name__}: {e}")
    
    # Brief wait for system recovery
    time.sleep(0.5)
    
    # System should accept new connections normally
    try:
        new_client = IrisClient()
        new_client.login(f"recovery_{random_user()}")
        new_client.send_msg(f"target_{random_user()}", "recovery_test")
        new_client.close()
        log("PASS: System recovered - new connection accepted")
        return True
    except socket.timeout as e:
        log(f"FAIL: System not recovered - timeout: {e}")
        return False
    except socket.error as e:
        log(f"FAIL: System not recovered - socket error: {e}")
        return False
    except Exception as e:
        log(f"FAIL: System not recovered - {type(e).__name__}: {e}")
        return False


def main():
    log("=" * 60)
    log(" BACKPRESSURE TEST SUITE")
    log("=" * 60)
    log(f"Random seed: {TEST_SEED}")
    
    tests = [
        ("Connection Acceptance", test_connection_acceptance),
        ("Message Throughput", test_message_throughput),
        ("Concurrent Load", test_concurrent_load),
        ("Recovery After Load", test_recovery_after_load),
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
            log(f"ERROR in {name}: {type(e).__name__}: {e}")
            import traceback
            traceback.print_exc()
            failed += 1
    
    log("\n" + "=" * 60)
    log(f" RESULTS: {passed} passed, {failed} failed")
    log("=" * 60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
