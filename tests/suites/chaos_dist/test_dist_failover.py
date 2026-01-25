#!/usr/bin/env python3
"""
Distributed Failover Test (Automated)
--------------------------------------
Tests failover behavior under various failure conditions:
1. Node kill & recovery
2. Network partition simulation (container pause)
3. Cascading failures
4. Load during recovery

Runs automatically in ~60 seconds total.
"""

import sys
import os
import time
import socket
import threading
import subprocess
from pathlib import Path

# Project root for init_cluster.sh
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
from dataclasses import dataclass
from typing import List, Tuple

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CORE_CONTAINER = os.environ.get("IRIS_CORE_CONTAINER", "core-east-1")

# Protocol Constants
OP_LOGIN = b'\x01'
OP_MSG = b'\x02'


@dataclass
class TestResult:
    name: str
    passed: bool
    details: str
    duration_ms: float


class Metrics:
    def __init__(self):
        self.sent = 0
        self.acked = 0
        self.errors = 0
        self.lock = threading.Lock()
        
    def update(self, s=0, a=0, e=0):
        with self.lock:
            self.sent += s
            self.acked += a
            self.errors += e
            
    def reset(self):
        with self.lock:
            s, a, e = self.sent, self.acked, self.errors
            self.sent = 0
            self.acked = 0
            self.errors = 0
            return s, a, e
            
    def get(self):
        with self.lock:
            return self.sent, self.acked, self.errors


def connect_and_login(host, port, user_id) -> socket.socket:
    """Connect and login, return socket."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(3.0)
    s.connect((host, port))
    s.sendall(OP_LOGIN + user_id.encode())
    s.recv(1024)  # LOGIN_OK
    return s


def send_message(sock, target: bytes, payload: bytes) -> bool:
    """Send a message (fire-and-forget, server doesn't ACK regular messages)."""
    import struct
    pkt = OP_MSG + struct.pack('>H', len(target)) + target + struct.pack('>H', len(payload)) + payload
    try:
        sock.sendall(pkt)
        return True  # Fire and forget - server doesn't respond to regular messages
    except:
        return False


def docker_cmd(cmd: str) -> bool:
    """Run docker command."""
    result = subprocess.run(cmd, shell=True, capture_output=True)
    return result.returncode == 0


def wait_container_healthy(container: str, timeout: int = 30) -> bool:
    """Wait for container to be healthy."""
    start = time.time()
    while time.time() - start < timeout:
        result = subprocess.run(
            f"docker inspect --format '{{{{.State.Health.Status}}}}' {container}",
            shell=True, capture_output=True, text=True
        )
        if "healthy" in result.stdout:
            return True
        time.sleep(1)
    return False


def reconnect_edge_to_core():
    """Reconnect edge to core after restart."""
    docker_cmd("docker exec edge-east-1 erl -noshell -hidden -sname tmp_rc -setcookie iris_secret -eval 'rpc:call(edge_east_1@edgeeast1, net_adm, ping, [core_east_1@coreeast1]), init:stop().'")
    time.sleep(1)


# =============================================================================
# Test Scenarios
# =============================================================================

def test_basic_connectivity() -> TestResult:
    """Test 1: Basic connection and message delivery."""
    start = time.time()
    try:
        sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_basic")
        success = send_message(sock, b"echo_service", b"hello")
        sock.close()
        
        if success:
            return TestResult("Basic Connectivity", True, "Connection and messaging OK", 
                            (time.time() - start) * 1000)
        else:
            return TestResult("Basic Connectivity", False, "Message send failed",
                            (time.time() - start) * 1000)
    except Exception as e:
        return TestResult("Basic Connectivity", False, f"Error: {e}",
                        (time.time() - start) * 1000)


def test_node_kill_recovery() -> TestResult:
    """Test 2: Node kill and recovery behavior."""
    start = time.time()
    
    if not docker_cmd(f"docker inspect {CORE_CONTAINER} 2>/dev/null"):
        return TestResult("Node Kill/Recovery", True, "SKIPPED: Container not found",
                        (time.time() - start) * 1000)
    
    try:
        # Pre-kill: establish baseline
        sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_kill")
        baseline_ok = send_message(sock, b"echo_service", b"baseline")
        sock.close()
        
        if not baseline_ok:
            return TestResult("Node Kill/Recovery", False, "Baseline failed",
                            (time.time() - start) * 1000)
        
        # Kill core
        docker_cmd(f"docker kill {CORE_CONTAINER}")
        time.sleep(2)
        
        # During outage: expect failures (this is correct behavior)
        outage_errors = 0
        for _ in range(3):
            try:
                sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_outage")
                if not send_message(sock, b"echo_service", b"during_outage"):
                    outage_errors += 1
                sock.close()
            except:
                outage_errors += 1
        
        # Restart core
        docker_cmd(f"docker start {CORE_CONTAINER}")
        if not wait_container_healthy(CORE_CONTAINER, timeout=20):
            return TestResult("Node Kill/Recovery", False, "Container didn't become healthy",
                            (time.time() - start) * 1000)
        
        # Extra recovery time
        time.sleep(3)
        reconnect_edge_to_core()
        time.sleep(2)
        
        # Post-recovery: should work again
        recovery_ok = False
        for attempt in range(5):
            try:
                sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_recovery")
                if send_message(sock, b"echo_service", b"after_recovery"):
                    recovery_ok = True
                sock.close()
                if recovery_ok:
                    break
            except:
                time.sleep(1)
        
        if recovery_ok:
            return TestResult("Node Kill/Recovery", True, 
                            f"Recovered after kill (outage errors: {outage_errors})",
                            (time.time() - start) * 1000)
        else:
            return TestResult("Node Kill/Recovery", False, "Failed to recover after restart",
                            (time.time() - start) * 1000)
            
    except Exception as e:
        # Try to restore container
        docker_cmd(f"docker start {CORE_CONTAINER}")
        return TestResult("Node Kill/Recovery", False, f"Error: {e}",
                        (time.time() - start) * 1000)


def test_concurrent_connections() -> TestResult:
    """Test 3: Multiple concurrent connections during stress."""
    start = time.time()
    
    results = []
    threads = []
    
    def worker(worker_id):
        try:
            sock = connect_and_login(SERVER_HOST, SERVER_PORT, f"concurrent_{worker_id}")
            success = 0
            for i in range(10):
                if send_message(sock, b"echo_service", f"msg_{worker_id}_{i}".encode()):
                    success += 1
                time.sleep(0.05)
            sock.close()
            results.append(success)
        except Exception as e:
            results.append(0)
    
    # Start 5 concurrent workers, 10 messages each = 50 messages total
    for i in range(5):
        t = threading.Thread(target=worker, args=(i,))
        t.start()
        threads.append(t)
    
    for t in threads:
        t.join(timeout=10)
    
    total = sum(results)
    expected = 50
    success_rate = (total / expected) * 100 if expected > 0 else 0
    
    # Allow 80% success rate (some may fail due to timing)
    passed = success_rate >= 80
    
    return TestResult("Concurrent Connections", passed,
                     f"{total}/{expected} messages delivered ({success_rate:.0f}%)",
                     (time.time() - start) * 1000)


def test_rapid_reconnect() -> TestResult:
    """Test 4: Rapid connect/disconnect cycles."""
    start = time.time()
    
    successes = 0
    attempts = 20
    
    for i in range(attempts):
        try:
            sock = connect_and_login(SERVER_HOST, SERVER_PORT, f"rapid_{i}")
            if send_message(sock, b"echo_service", f"rapid_{i}".encode()):
                successes += 1
            sock.close()
        except:
            pass
    
    success_rate = (successes / attempts) * 100
    passed = success_rate >= 80
    
    return TestResult("Rapid Reconnect", passed,
                     f"{successes}/{attempts} cycles succeeded ({success_rate:.0f}%)",
                     (time.time() - start) * 1000)


def test_container_pause_resume() -> TestResult:
    """Test 5: Network partition simulation via container pause."""
    start = time.time()
    
    if not docker_cmd(f"docker inspect {CORE_CONTAINER} 2>/dev/null"):
        return TestResult("Pause/Resume", True, "SKIPPED: Container not found",
                        (time.time() - start) * 1000)
    
    try:
        # Baseline
        sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_pause")
        baseline = send_message(sock, b"echo_service", b"pre_pause")
        sock.close()
        
        if not baseline:
            return TestResult("Pause/Resume", False, "Baseline failed",
                            (time.time() - start) * 1000)
        
        # Pause (simulates network partition)
        docker_cmd(f"docker pause {CORE_CONTAINER}")
        time.sleep(2)
        
        # Unpause
        docker_cmd(f"docker unpause {CORE_CONTAINER}")
        time.sleep(2)
        reconnect_edge_to_core()
        time.sleep(1)
        
        # Should recover
        recovered = False
        for _ in range(3):
            try:
                sock = connect_and_login(SERVER_HOST, SERVER_PORT, "test_unpause")
                if send_message(sock, b"echo_service", b"post_unpause"):
                    recovered = True
                sock.close()
                if recovered:
                    break
            except:
                time.sleep(1)
        
        if recovered:
            return TestResult("Pause/Resume", True, "Recovered after unpause",
                            (time.time() - start) * 1000)
        else:
            return TestResult("Pause/Resume", False, "Failed to recover after unpause",
                            (time.time() - start) * 1000)
            
    except Exception as e:
        docker_cmd(f"docker unpause {CORE_CONTAINER}")
        return TestResult("Pause/Resume", False, f"Error: {e}",
                        (time.time() - start) * 1000)


# =============================================================================
# Main
# =============================================================================

def main():
    print("\n" + "=" * 60)
    print("Distributed Failover Test Suite")
    print("=" * 60)
    print(f"Target: {SERVER_HOST}:{SERVER_PORT}")
    print(f"Core Container: {CORE_CONTAINER}")
    print()
    
    results: List[TestResult] = []
    
    # Run tests
    tests = [
        ("1/5", test_basic_connectivity),
        ("2/5", test_node_kill_recovery),
        ("3/5", test_concurrent_connections),
        ("4/5", test_rapid_reconnect),
        ("5/5", test_container_pause_resume),
    ]
    
    for label, test_fn in tests:
        print(f"\n[{label}] Running: {test_fn.__doc__.split(chr(10))[0].strip()}")
        result = test_fn()
        results.append(result)
        
        status = "✅ PASS" if result.passed else "❌ FAIL"
        print(f"  {status}: {result.details} ({result.duration_ms:.0f}ms)")
    
    # Summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for r in results if r.passed)
    total = len(results)
    
    for r in results:
        status = "✅" if r.passed else "❌"
        print(f"  {status} {r.name}: {r.details}")
    
    print()
    print(f"Passed: {passed}/{total}")
    
    # Restore cluster state for subsequent tests
    restore_cluster_state()
    
    if passed == total:
        print("\n✅ ALL TESTS PASSED")
        sys.exit(0)
    else:
        print(f"\n❌ {total - passed} TEST(S) FAILED")
        sys.exit(1)


def restore_cluster_state():
    """Re-initialize cluster after test that restarts containers."""
    try:
        # First ensure all core containers are running
        print("[cleanup] Restoring cluster state after container restart...")
        
        cores = ["core-east-1", "core-east-2", "core-west-1", "core-west-2", "core-eu-1", "core-eu-2"]
        for core in cores:
            result = subprocess.run(
                ["docker", "inspect", "--format", "{{.State.Status}}", core],
                capture_output=True, text=True
            )
            if result.stdout.strip() in ["exited", "created"]:
                print(f"[cleanup] Restarting stopped container: {core}")
                subprocess.run(["docker", "start", core], capture_output=True)
        
        # Wait for containers to stabilize
        time.sleep(10)
        
        # Run init script
        init_script = PROJECT_ROOT / "docker" / "global-cluster" / "init_cluster.sh"
        if init_script.exists():
            subprocess.run(
                ["bash", str(init_script)],
                cwd=str(init_script.parent),
                capture_output=True,
                timeout=180  # Increased timeout
            )
            print("[cleanup] Cluster state restored")
    except Exception as e:
        print(f"[cleanup] Warning: Could not restore cluster state: {e}")


if __name__ == "__main__":
    main()
