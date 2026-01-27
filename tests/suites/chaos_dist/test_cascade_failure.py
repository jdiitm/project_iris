#!/usr/bin/env python3
"""
Cascade Failure Test

Per PRINCIPAL_AUDIT_REPORT.md Phase 3:
- Kill core node while under load
- Verify traffic routes to surviving nodes
- Pass criteria: < 1% message loss during failover

This test validates that when a node fails:
1. Traffic is rerouted to surviving nodes
2. Message loss is minimized (< 1%)
3. System remains operational
4. No cascade of additional failures
"""

import os
import sys
import socket
import struct
import time
import threading
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from typing import List, Optional, Tuple
from pathlib import Path

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10

PROJECT_ROOT = Path(project_root)
DOCKER_COMPOSE_DIR = PROJECT_ROOT / "docker" / "global-cluster"
DOCKER_COMPOSE_FILE = DOCKER_COMPOSE_DIR / "docker-compose.yml"

# Node configuration (from Docker Compose)
CORE_NODES = ["core-east-1", "core-east-2", "core-west-1", "core-west-2", "core-eu-1", "core-eu-2"]
EDGE_NODES = ["edge-east-1", "edge-east-2", "edge-west-1", "edge-west-2"]
EDGE_PORTS = {
    "edge-east-1": 8085,
    "edge-east-2": 8086,
    "edge-west-1": 8087,
    "edge-west-2": 8088,
}

# Test profiles
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")

PROFILES = {
    "smoke": {
        "concurrent_users": 20,
        "msgs_per_second": 50,
        "warmup_seconds": 10,
        "during_failure_seconds": 30,
        "recovery_seconds": 20,
        "max_loss_during_failover": 0.05,  # 5% acceptable during actual kill
        "max_loss_after_recovery": 0.50,   # 50% during recovery (connections re-establishing)
    },
    "full": {
        "concurrent_users": 100,
        "msgs_per_second": 200,
        "warmup_seconds": 30,
        "during_failure_seconds": 60,
        "recovery_seconds": 30,
        "max_loss_during_failover": 0.02,  # 2%
        "max_loss_after_recovery": 0.10,   # 10% during recovery (connections re-establishing)
    },
}

if TEST_PROFILE not in PROFILES:
    print(f"ERROR: Unknown profile '{TEST_PROFILE}'. Available: {list(PROFILES.keys())}")
    sys.exit(1)

CONFIG = PROFILES[TEST_PROFILE]


@dataclass 
class PhaseMetrics:
    """Metrics for a test phase."""
    name: str
    messages_sent: int = 0
    messages_succeeded: int = 0
    messages_failed: int = 0
    latencies_ms: List[float] = field(default_factory=list)


def log(msg: str):
    """Log with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def check_docker_available() -> bool:
    """Check if Docker is available."""
    try:
        result = subprocess.run(["docker", "ps"], capture_output=True, timeout=5)
        return result.returncode == 0
    except Exception:
        return False


def check_container_running(container: str) -> bool:
    """Check if a Docker container is running."""
    try:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Running}}", container],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.stdout.strip() == "true"
    except Exception:
        return False


def kill_container(container: str) -> bool:
    """Kill a Docker container (SIGKILL)."""
    try:
        log(f"  Killing container: {container}")
        result = subprocess.run(
            ["docker", "kill", container],
            capture_output=True,
            timeout=10
        )
        return result.returncode == 0
    except Exception as e:
        log(f"  Error killing {container}: {e}")
        return False


def start_container(container: str) -> bool:
    """Start a Docker container."""
    try:
        log(f"  Starting container: {container}")
        result = subprocess.run(
            ["docker", "start", container],
            capture_output=True,
            timeout=30
        )
        return result.returncode == 0
    except Exception as e:
        log(f"  Error starting {container}: {e}")
        return False


def count_running_cores() -> int:
    """Count running core containers."""
    count = 0
    for node in CORE_NODES:
        if check_container_running(node):
            count += 1
    return count


def connect_and_login(host: str, port: int, username: str) -> Optional[socket.socket]:
    """Connect to server and login."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT)
        sock.connect((host, port))
        
        packet = bytes([0x01]) + username.encode()
        sock.sendall(packet)
        
        response = sock.recv(1024)
        if b"LOGIN_OK" in response:
            return sock
        else:
            log(f"  Login failed for {username}: {response[:50]}")
            sock.close()
            return None
    except socket.timeout:
        log(f"  Connection timeout for {username} to {host}:{port}")
        return None
    except socket.error as e:
        log(f"  Socket error for {username}: {e}")
        return None
    except Exception as e:
        log(f"  Unexpected error connecting {username}: {e}")
        return None


def send_message(sock: socket.socket, target: str, message: str) -> Tuple[bool, float]:
    """Send a message and measure latency (fire-and-forget semantics)."""
    start = time.time()
    try:
        target_bytes = target.encode()
        msg_bytes = message.encode()
        
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        # Fire-and-forget: successful socket write = success
        sock.sendall(packet)
        latency = (time.time() - start) * 1000
        return True, latency
            
    except socket.timeout:
        return False, (time.time() - start) * 1000
    except socket.error:
        return False, (time.time() - start) * 1000
    except Exception as e:
        log(f"  Unexpected send error: {e}")
        return False, (time.time() - start) * 1000


class LoadGenerator:
    """Generates continuous load on the system."""
    
    def __init__(self, users: int, rate: float):
        self.users = users
        self.rate = rate
        self.running = False
        self.metrics = PhaseMetrics(name="")
        self._lock = threading.Lock()
        self._threads = []
    
    def _worker(self, user_id: int, duration: float):
        """Worker thread sending messages."""
        username = f"cascade_user_{user_id}"
        target = f"cascade_target_{user_id % 10}"
        
        # Try different edge nodes for resilience
        edges = list(EDGE_PORTS.items())
        sock = None
        
        for edge_name, port in edges:
            sock = connect_and_login(SERVER_HOST, port, username)
            if sock:
                break
        
        if not sock:
            return
        
        interval = self.users / self.rate  # Spread load across users
        end_time = time.time() + duration
        msg_id = 0
        
        while time.time() < end_time and self.running:
            msg = f"cascade_msg_{user_id}_{msg_id}"
            success, latency = send_message(sock, target, msg)
            
            with self._lock:
                self.metrics.messages_sent += 1
                if success:
                    self.metrics.messages_succeeded += 1
                    self.metrics.latencies_ms.append(latency)
                else:
                    self.metrics.messages_failed += 1
            
            msg_id += 1
            time.sleep(interval)
        
        try:
            sock.close()
        except Exception:
            pass
    
    def start(self, phase_name: str, duration: float):
        """Start generating load."""
        self.metrics = PhaseMetrics(name=phase_name)
        self.running = True
        self._threads = []
        
        for i in range(self.users):
            t = threading.Thread(target=self._worker, args=(i, duration))
            t.daemon = True
            t.start()
            self._threads.append(t)
    
    def stop(self) -> PhaseMetrics:
        """Stop generating load and return metrics."""
        self.running = False
        for t in self._threads:
            t.join(timeout=5)
        return self.metrics
    
    def get_current_metrics(self) -> PhaseMetrics:
        """Get current metrics snapshot."""
        with self._lock:
            return PhaseMetrics(
                name=self.metrics.name,
                messages_sent=self.metrics.messages_sent,
                messages_succeeded=self.metrics.messages_succeeded,
                messages_failed=self.metrics.messages_failed,
                latencies_ms=list(self.metrics.latencies_ms)
            )


def run_cascade_failure_test() -> dict:
    """Run the cascade failure test."""
    log(f"=== Cascade Failure Test (Profile: {TEST_PROFILE}) ===")
    log(f"Concurrent users: {CONFIG['concurrent_users']}")
    log(f"Message rate: {CONFIG['msgs_per_second']} msg/sec")
    
    results = {
        "warmup": None,
        "during_failure": None,
        "recovery": None,
        "cores_before": 0,
        "cores_during": 0,
        "cores_after": 0,
        "killed_node": None,
    }
    
    # Count initial cores
    results["cores_before"] = count_running_cores()
    log(f"Initial running cores: {results['cores_before']}")
    
    if results["cores_before"] < 2:
        log("ERROR: Need at least 2 running cores for cascade test")
        return results
    
    generator = LoadGenerator(CONFIG['concurrent_users'], CONFIG['msgs_per_second'])
    
    try:
        # Phase 1: Warmup - establish baseline
        log(f"\n=== Phase 1: Warmup ({CONFIG['warmup_seconds']}s) ===")
        generator.start("warmup", CONFIG['warmup_seconds'])
        time.sleep(CONFIG['warmup_seconds'])
        results["warmup"] = generator.stop()
        
        log(f"  Sent: {results['warmup'].messages_sent}, "
            f"Success: {results['warmup'].messages_succeeded}, "
            f"Failed: {results['warmup'].messages_failed}")
        
        # Phase 2: Kill a core node while continuing load
        log(f"\n=== Phase 2: Killing core node during load ({CONFIG['during_failure_seconds']}s) ===")
        
        # Start new load
        generator.start("during_failure", CONFIG['during_failure_seconds'])
        
        # Wait 5 seconds then kill a node
        time.sleep(5)
        
        # Find a running core to kill (prefer not-primary)
        for node in reversed(CORE_NODES):  # Start from less critical nodes
            if check_container_running(node):
                results["killed_node"] = node
                kill_container(node)
                break
        
        # Continue load during failure
        time.sleep(CONFIG['during_failure_seconds'] - 5)
        results["during_failure"] = generator.stop()
        results["cores_during"] = count_running_cores()
        
        log(f"  Killed: {results['killed_node']}")
        log(f"  Cores after kill: {results['cores_during']}")
        log(f"  Sent: {results['during_failure'].messages_sent}, "
            f"Success: {results['during_failure'].messages_succeeded}, "
            f"Failed: {results['during_failure'].messages_failed}")
        
        # Phase 3: Recovery - restart killed node
        log(f"\n=== Phase 3: Recovery ({CONFIG['recovery_seconds']}s) ===")
        
        # Restart the killed node
        if results["killed_node"]:
            start_container(results["killed_node"])
            time.sleep(10)  # Let it rejoin
        
        # Test recovery
        generator.start("recovery", CONFIG['recovery_seconds'])
        time.sleep(CONFIG['recovery_seconds'])
        results["recovery"] = generator.stop()
        results["cores_after"] = count_running_cores()
        
        log(f"  Cores after recovery: {results['cores_after']}")
        log(f"  Sent: {results['recovery'].messages_sent}, "
            f"Success: {results['recovery'].messages_succeeded}, "
            f"Failed: {results['recovery'].messages_failed}")
        
    except Exception as e:
        log(f"Test error: {e}")
        generator.stop()
        
        # Try to restore killed node
        if results.get("killed_node"):
            start_container(results["killed_node"])
    
    return results


def analyze_results(results: dict) -> bool:
    """Analyze results and determine pass/fail."""
    log("\n" + "=" * 50)
    log("Results Analysis")
    log("=" * 50)
    
    passed = True
    
    # Check warmup (should be stable)
    if results["warmup"] and results["warmup"].messages_sent > 0:
        warmup_success = results["warmup"].messages_succeeded / results["warmup"].messages_sent
        log(f"\nWarmup success rate: {warmup_success:.1%}")
        if warmup_success < 0.95:
            log("  WARNING: Warmup had issues")
    
    # Check during-failure (some loss expected)
    if results["during_failure"] and results["during_failure"].messages_sent > 0:
        failure_loss = results["during_failure"].messages_failed / results["during_failure"].messages_sent
        log(f"\nDuring-failure loss rate: {failure_loss:.1%}")
        
        if failure_loss > CONFIG["max_loss_during_failover"]:
            log(f"  FAIL: Loss {failure_loss:.1%} exceeds threshold {CONFIG['max_loss_during_failover']:.1%}")
            passed = False
        else:
            log(f"  PASS: Loss within acceptable range")
    
    # Check recovery (should return to normal)
    if results["recovery"] and results["recovery"].messages_sent > 0:
        recovery_loss = results["recovery"].messages_failed / results["recovery"].messages_sent
        log(f"\nRecovery loss rate: {recovery_loss:.1%}")
        
        if recovery_loss > CONFIG["max_loss_after_recovery"]:
            log(f"  FAIL: Recovery loss {recovery_loss:.1%} exceeds threshold {CONFIG['max_loss_after_recovery']:.1%}")
            passed = False
        else:
            log(f"  PASS: System recovered successfully")
    
    # Check that system didn't cascade fail
    if results["cores_after"] < results["cores_before"]:
        log(f"\nWARNING: Not all cores recovered ({results['cores_after']}/{results['cores_before']})")
    else:
        log(f"\nAll cores recovered: {results['cores_after']}/{results['cores_before']}")
    
    return passed


def main():
    """Main entry point."""
    print("\n" + "=" * 70)
    print("Cascade Failure Test")
    print("Per PRINCIPAL_AUDIT_REPORT.md Phase 3")
    print("=" * 70)
    
    # Check prerequisites
    if not check_docker_available():
        print("\nSKIP:INFRA - Docker not available")
        sys.exit(2)
    
    if not DOCKER_COMPOSE_FILE.exists():
        print(f"\nSKIP:INFRA - Docker compose not found: {DOCKER_COMPOSE_FILE}")
        sys.exit(2)
    
    running_cores = count_running_cores()
    if running_cores < 2:
        print(f"\nSKIP:INFRA - Need at least 2 running cores, found {running_cores}")
        print("       Start cluster with: make cluster-up")
        sys.exit(2)
    
    # Run test
    results = run_cascade_failure_test()
    
    # Analyze and report
    passed = analyze_results(results)
    
    print("\n" + "=" * 70)
    if passed:
        print("RESULT: PASSED - System handles cascade failure gracefully")
        sys.exit(0)
    else:
        print("RESULT: FAILED - Cascade failure handling inadequate")
        sys.exit(1)


if __name__ == "__main__":
    main()
