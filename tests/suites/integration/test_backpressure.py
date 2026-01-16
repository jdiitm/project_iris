#!/usr/bin/env python3
"""
Backpressure Tests

Tests:
1. Connection acceptance during load
2. Graceful degradation under pressure
3. Recovery after overload
"""

import sys
import time
import random
import string
import threading
import concurrent.futures

sys.path.insert(0, 'tests/utilities')
from iris_client import IrisClient


def random_user():
    return ''.join(random.choices(string.ascii_lowercase, k=8))


def test_connection_acceptance():
    """Test that connections are accepted under normal load"""
    print("\n=== Test: Connection Acceptance ===")
    
    clients = []
    success = 0
    failed = 0
    
    # Try to establish multiple connections
    for i in range(20):
        try:
            client = IrisClient("edge1")
            client.login(f"bp_user_{random_user()}")
            clients.append(client)
            success += 1
        except Exception as e:
            failed += 1
    
    print(f"  Connections: {success} success, {failed} failed")
    
    # Cleanup
    for c in clients:
        try:
            c.close()
        except:
            pass
    
    if success >= 18:  # 90% success
        print(f"✓ Connection acceptance normal")
        return True
    else:
        print(f"✗ Too many connection failures")
        return False


def test_message_throughput():
    """Test message throughput under load"""
    print("\n=== Test: Message Throughput ===")
    
    sender = IrisClient("edge1")
    receiver = IrisClient("edge1")
    
    sender_user = f"sender_{random_user()}"
    receiver_user = f"receiver_{random_user()}"
    
    sender.login(sender_user)
    receiver.login(receiver_user)
    
    # Send messages rapidly
    start = time.time()
    count = 100
    
    for i in range(count):
        sender.send_msg(receiver_user, f"throughput_{i}")
    
    elapsed = time.time() - start
    rate = count / elapsed if elapsed > 0 else 0
    
    print(f"  Sent {count} msgs in {elapsed:.2f}s ({rate:.1f} msg/s)")
    
    sender.close()
    receiver.close()
    
    if rate > 50:  # At least 50 msg/s
        print(f"✓ Throughput acceptable")
        return True
    else:
        print(f"✗ Throughput too low")
        return False


def test_concurrent_load():
    """Test behavior under concurrent connection load"""
    print("\n=== Test: Concurrent Load ===")
    
    results = {"success": 0, "failed": 0}
    lock = threading.Lock()
    
    def connect_and_send():
        try:
            client = IrisClient("edge1")
            user = f"concurrent_{random_user()}"
            client.login(user)
            
            # Send a few messages
            for i in range(5):
                client.send_msg(f"target_{random_user()}", f"msg_{i}")
            
            client.close()
            
            with lock:
                results["success"] += 1
            return True
        except Exception as e:
            with lock:
                results["failed"] += 1
            return False
    
    # Run concurrent connections
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = [executor.submit(connect_and_send) for _ in range(30)]
        concurrent.futures.wait(futures)
    
    print(f"  Concurrent: {results['success']} success, {results['failed']} failed")
    
    if results["success"] >= 25:  # 83% success
        print(f"✓ Concurrent load handled")
        return True
    else:
        print(f"✗ Too many failures under load")
        return False


def test_recovery_after_load():
    """Test that system recovers after high load"""
    print("\n=== Test: Recovery After Load ===")
    
    # First, create some load
    clients = []
    for i in range(15):
        try:
            c = IrisClient("edge1")
            c.login(f"load_{random_user()}")
            clients.append(c)
        except:
            pass
    
    # Close all (simulate load drop)
    for c in clients:
        try:
            c.close()
        except:
            pass
    
    # Wait for recovery
    time.sleep(1)
    
    # System should accept new connections normally
    new_client = IrisClient("edge1")
    try:
        new_client.login(f"recovery_{random_user()}")
        new_client.send_msg(f"target_{random_user()}", "recovery_test")
        new_client.close()
        print(f"✓ System recovered after load")
        return True
    except Exception as e:
        print(f"✗ System not recovered: {e}")
        return False


def main():
    print("=" * 60)
    print(" BACKPRESSURE TEST SUITE")
    print("=" * 60)
    
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
            print(f"✗ {name} EXCEPTION: {e}")
            failed += 1
    
    print("\n" + "=" * 60)
    print(f" RESULTS: {passed} passed, {failed} failed")
    print("=" * 60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
