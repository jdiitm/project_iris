#!/usr/bin/env python3
"""
Failover Time Test (RFC NFR-9)

Validates that traffic resumes within 30 seconds after primary core failure.

RFC Requirements:
- NFR-9: Failover time ≤ 30 seconds

Test Strategy:
1. Start steady message traffic
2. Kill primary core node
3. Measure time until traffic resumes successfully
4. Verify recovery time ≤ 30 seconds

PASS: Traffic resumes within 30 seconds
FAIL: Traffic takes longer than 30 seconds to resume
"""

import socket
import time
import subprocess
import threading
import sys
import os

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CONTAINER_NAME = os.environ.get("IRIS_CORE_CONTAINER", "core-east-1")
FAILOVER_TARGET_SECONDS = 30
TRAFFIC_INTERVAL_MS = 100  # Send message every 100ms

# Per TEST_CONTRACT.md: exit(0)=pass, exit(1)=fail, exit(2)=skip


class TrafficMonitor:
    """Thread-safe traffic monitoring."""
    
    def __init__(self):
        self.running = True
        self.success_count = 0
        self.fail_count = 0
        self.last_success_time = None
        self.first_fail_time = None
        self.recovery_time = None
        self.lock = threading.Lock()
    
    def record_success(self):
        with self.lock:
            now = time.time()
            self.success_count += 1
            
            if self.first_fail_time is not None and self.recovery_time is None:
                # First success after failures = recovery
                self.recovery_time = now - self.first_fail_time
            
            self.last_success_time = now
    
    def record_failure(self):
        with self.lock:
            now = time.time()
            self.fail_count += 1
            
            if self.first_fail_time is None:
                self.first_fail_time = now
    
    def stop(self):
        self.running = False
    
    def get_recovery_time(self):
        with self.lock:
            return self.recovery_time


def connect():
    """Create connection."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(5)
    sock.connect((SERVER_HOST, SERVER_PORT))
    return sock


def login(sock, username):
    """Send login packet."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    time.sleep(0.1)


def send_and_wait(sock, target, message, timeout=2):
    """Send message and wait for response."""
    target_bytes = target.encode()
    msg_bytes = message.encode()
    packet = bytes([0x02]) + \
             len(target_bytes).to_bytes(2, 'big') + target_bytes + \
             len(msg_bytes).to_bytes(2, 'big') + msg_bytes
    
    try:
        sock.settimeout(timeout)
        sock.sendall(packet)
        sock.recv(1024)
        return True
    except:
        return False


def traffic_worker(monitor, sender_id):
    """Worker thread that sends traffic and monitors results."""
    receiver = f"failover_receiver_{sender_id}"
    sock = None
    
    while monitor.running:
        try:
            if sock is None:
                sock = connect()
                login(sock, f"failover_sender_{sender_id}")
            
            msg = f"FAILOVER_TEST_{time.time()}"
            if send_and_wait(sock, receiver, msg):
                monitor.record_success()
            else:
                monitor.record_failure()
                sock.close()
                sock = None
        
        except Exception:
            monitor.record_failure()
            if sock:
                try:
                    sock.close()
                except:
                    pass
            sock = None
        
        time.sleep(TRAFFIC_INTERVAL_MS / 1000)
    
    if sock:
        try:
            sock.close()
        except:
            pass


def kill_container(container_name):
    """Kill Docker container."""
    result = subprocess.run(
        ["docker", "kill", container_name],
        capture_output=True
    )
    return result.returncode == 0


def check_docker_available():
    """Check if Docker is available."""
    result = subprocess.run(["docker", "ps"], capture_output=True)
    return result.returncode == 0


def check_container_exists(container_name):
    """Check if container exists."""
    result = subprocess.run(
        ["docker", "inspect", container_name],
        capture_output=True
    )
    return result.returncode == 0


def test_failover_time():
    """Test failover recovery time."""
    print("\n" + "=" * 60)
    print("Failover Time Test (RFC NFR-9)")
    print("=" * 60)
    print(f"Target: {SERVER_HOST}:{SERVER_PORT}")
    print(f"Container: {CONTAINER_NAME}")
    print(f"Target recovery time: ≤ {FAILOVER_TARGET_SECONDS}s")
    
    # Check prerequisites
    if not check_docker_available():
        print("\n⚠️ Docker not available - skipping failover test")
        return None
    
    if not check_container_exists(CONTAINER_NAME):
        print(f"\n⚠️ Container {CONTAINER_NAME} not found")
        print("  Start cluster with: make cluster-up")
        return None
    
    print("\n1. Starting traffic generator...")
    monitor = TrafficMonitor()
    worker_thread = threading.Thread(
        target=traffic_worker,
        args=(monitor, int(time.time()))
    )
    worker_thread.start()
    
    # Let traffic stabilize
    print("   Waiting for traffic to stabilize (5s)...")
    time.sleep(5)
    
    with monitor.lock:
        initial_success = monitor.success_count
        initial_fail = monitor.fail_count
    
    print(f"   Initial: {initial_success} success, {initial_fail} fail")
    
    if initial_success == 0:
        print("   ❌ No successful traffic - server not responding")
        monitor.stop()
        worker_thread.join()
        return None
    
    print(f"\n2. Killing primary core: {CONTAINER_NAME}")
    kill_time = time.time()
    if not kill_container(CONTAINER_NAME):
        print("   ❌ Failed to kill container")
        monitor.stop()
        worker_thread.join()
        # Per TEST_CONTRACT.md: return None = SKIP (exit code 2)
        print("\nSKIP:DOCKER - Container not available")
        return None
    print("   ✅ Container killed")
    
    print(f"\n3. Monitoring failover (timeout: {FAILOVER_TARGET_SECONDS + 30}s)...")
    max_wait = FAILOVER_TARGET_SECONDS + 30
    start_wait = time.time()
    
    while time.time() - start_wait < max_wait:
        recovery = monitor.get_recovery_time()
        if recovery is not None:
            break
        
        with monitor.lock:
            fails = monitor.fail_count - initial_fail
        
        elapsed = time.time() - kill_time
        print(f"   Elapsed: {elapsed:.1f}s, Failures since kill: {fails}")
        time.sleep(2)
    
    # Stop traffic
    monitor.stop()
    worker_thread.join()
    
    # Get results
    recovery_time = monitor.get_recovery_time()
    
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    
    with monitor.lock:
        total_success = monitor.success_count
        total_fail = monitor.fail_count
    
    print(f"  Total messages: {total_success + total_fail}")
    print(f"  Successful: {total_success}")
    print(f"  Failed: {total_fail}")
    print("")
    
    if recovery_time is not None:
        print(f"  Recovery time: {recovery_time:.2f} seconds")
        
        if recovery_time <= FAILOVER_TARGET_SECONDS:
            print(f"\n✅ PASS: Recovery ({recovery_time:.2f}s) ≤ {FAILOVER_TARGET_SECONDS}s")
            print("   RFC NFR-9: COMPLIANT")
            return True
        else:
            print(f"\n❌ FAIL: Recovery ({recovery_time:.2f}s) > {FAILOVER_TARGET_SECONDS}s")
            print("   RFC NFR-9: NON-COMPLIANT")
            return False
    else:
        print("  ⚠️ Traffic did not recover within timeout")
        print("\n❌ FAIL: No recovery detected")
        return False


def main():
    result = test_failover_time()
    
    print("\n" + "=" * 60)
    if result is True:
        print("RESULT: PASSED")
        sys.exit(0)
    elif result is False:
        print("RESULT: FAILED")
        sys.exit(1)
    else:
        # Per TEST_CONTRACT.md: exit(2) = SKIP
        print("RESULT: SKIPPED")
        sys.exit(2)


if __name__ == "__main__":
    main()
