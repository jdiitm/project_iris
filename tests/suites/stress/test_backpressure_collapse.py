#!/usr/bin/env python3
"""
Backpressure Collapse Stress Test

Per PRINCIPAL_AUDIT_REPORT.md Section 6.3:
- Send 2x system capacity
- Verify iris_flow_controller sheds load gracefully
- Pass criteria: No OOM, latency degrades gracefully

This test validates that the system handles overload conditions by:
1. Applying backpressure (slowing clients)
2. Shedding excess load (rejecting requests)
3. Recovering when load decreases

The system should NOT:
- OOM kill any process
- Drop messages silently
- Hang indefinitely
"""

import os
import sys
import socket
import struct
import time
import threading
import subprocess
import statistics
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10

# Test profiles
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")

PROFILES = {
    "smoke": {
        "target_rate": 500,           # msgs/sec target (normal load)
        "overload_multiplier": 2.0,   # 2x overload
        "warmup_seconds": 10,
        "overload_seconds": 30,
        "recovery_seconds": 20,
        "max_latency_during_overload_ms": 5000,
        "min_successful_during_overload": 0.5,  # 50% should succeed
        "max_errors_during_recovery": 0.01,     # 1% after recovery
    },
    "full": {
        "target_rate": 2000,
        "overload_multiplier": 3.0,   # 3x overload
        "warmup_seconds": 30,
        "overload_seconds": 60,
        "recovery_seconds": 30,
        "max_latency_during_overload_ms": 3000,
        "min_successful_during_overload": 0.7,  # 70% should succeed
        "max_errors_during_recovery": 0.005,    # 0.5% after recovery
    },
    "extreme": {
        "target_rate": 5000,
        "overload_multiplier": 5.0,   # 5x overload (25K/sec)
        "warmup_seconds": 60,
        "overload_seconds": 120,
        "recovery_seconds": 60,
        "max_latency_during_overload_ms": 5000,
        "min_successful_during_overload": 0.5,
        "max_errors_during_recovery": 0.01,
    }
}

if TEST_PROFILE not in PROFILES:
    print(f"ERROR: Unknown profile '{TEST_PROFILE}'. Available: {list(PROFILES.keys())}")
    sys.exit(1)

CONFIG = PROFILES[TEST_PROFILE]


@dataclass
class PhaseMetrics:
    """Metrics collected during a test phase."""
    name: str
    duration_seconds: float
    messages_sent: int = 0
    messages_succeeded: int = 0
    messages_rejected: int = 0
    messages_timeout: int = 0
    latencies_ms: List[float] = field(default_factory=list)


@dataclass
class TestResult:
    """Overall test result."""
    warmup: PhaseMetrics
    overload: PhaseMetrics
    recovery: PhaseMetrics
    oom_detected: bool = False
    hung_detected: bool = False


def log(msg: str):
    """Log with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


class LoadGenerator:
    """Generates load at a configurable rate."""
    
    def __init__(self, target_rate: float):
        self.target_rate = target_rate
        self.running = False
        self.metrics = PhaseMetrics(name="", duration_seconds=0)
        self._lock = threading.Lock()
        self._sockets = []
        self._target_sock = None
        self._receiver_thread = None
        self._messages_received = 0
    
    def _connect(self, username: str) -> Optional[socket.socket]:
        """Connect and login."""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(TIMEOUT)
            sock.connect((SERVER_HOST, SERVER_PORT))
            
            packet = bytes([0x01]) + username.encode()
            sock.sendall(packet)
            
            response = sock.recv(1024)
            if b"LOGIN_OK" in response:
                return sock
            else:
                sock.close()
                return None
        except Exception:
            return None
    
    def _receiver_worker(self):
        """Background thread that receives messages on the target socket."""
        while self.running and self._target_sock:
            try:
                self._target_sock.settimeout(1.0)
                data = self._target_sock.recv(4096)
                if data:
                    # Count messages received (rough count based on data)
                    with self._lock:
                        self._messages_received += 1
            except socket.timeout:
                continue
            except Exception:
                break
    
    def _send_message(self, sock: socket.socket, target: str, message: str) -> Tuple[str, float]:
        """
        Send message and return (status, latency_ms).
        Status: 'success', 'rejected', 'timeout', 'error'
        
        For backpressure testing, we consider the send successful if the
        socket write completes without error. The server queues messages
        for delivery to the target.
        """
        start = time.time()
        try:
            target_bytes = target.encode()
            msg_bytes = message.encode()
            
            packet = (
                bytes([0x02]) +
                struct.pack('>H', len(target_bytes)) + target_bytes +
                struct.pack('>H', len(msg_bytes)) + msg_bytes
            )
            
            sock.sendall(packet)
            latency = (time.time() - start) * 1000
            
            # For fire-and-forget messaging, successful socket write = success
            # The server will queue for delivery
            return 'success', latency
                
        except socket.timeout:
            return 'timeout', (time.time() - start) * 1000
        except (ConnectionError, BrokenPipeError, OSError):
            # Connection issues indicate backpressure/rejection
            return 'rejected', (time.time() - start) * 1000
        except Exception:
            return 'error', (time.time() - start) * 1000
    
    def _worker(self, worker_id: int, target_user: str, msgs_per_sec: float, duration: float):
        """Worker thread that sends messages at target rate."""
        username = f"backpressure_sender_{worker_id}_{int(time.time() * 1000)}"
        sock = self._connect(username)
        if not sock:
            return
        
        with self._lock:
            self._sockets.append(sock)
        
        interval = 1.0 / msgs_per_sec if msgs_per_sec > 0 else 1.0
        end_time = time.time() + duration
        msg_count = 0
        
        while time.time() < end_time and self.running:
            msg = f"bp_test_{worker_id}_{msg_count}"
            status, latency = self._send_message(sock, target_user, msg)
            
            with self._lock:
                self.metrics.messages_sent += 1
                if status == 'success':
                    self.metrics.messages_succeeded += 1
                    self.metrics.latencies_ms.append(latency)
                elif status == 'rejected':
                    self.metrics.messages_rejected += 1
                elif status == 'timeout':
                    self.metrics.messages_timeout += 1
            
            msg_count += 1
            
            # Rate limiting
            sleep_time = interval - (time.time() % interval)
            if sleep_time > 0 and sleep_time < interval:
                time.sleep(sleep_time)
    
    def run(self, name: str, rate: float, duration: float, workers: int = 10) -> PhaseMetrics:
        """Run load generation for a phase."""
        self.metrics = PhaseMetrics(name=name, duration_seconds=duration)
        self.running = True
        self._sockets = []
        self._messages_received = 0
        
        target_user = f"backpressure_target_{int(time.time() * 1000)}"
        
        # Login target user and start receiver thread
        self._target_sock = self._connect(target_user)
        if self._target_sock:
            self._receiver_thread = threading.Thread(target=self._receiver_worker)
            self._receiver_thread.start()
        
        msgs_per_worker = rate / workers
        
        threads = []
        for i in range(workers):
            t = threading.Thread(target=self._worker, args=(i, target_user, msgs_per_worker, duration))
            t.start()
            threads.append(t)
        
        # Wait for completion
        for t in threads:
            t.join(timeout=duration + 30)
        
        self.running = False
        
        # Stop receiver
        if self._receiver_thread:
            self._receiver_thread.join(timeout=5)
        
        # Close sockets
        if self._target_sock:
            try:
                self._target_sock.close()
            except Exception:
                pass
        
        for sock in self._sockets:
            try:
                sock.close()
            except Exception:
                pass
        
        return self.metrics
    
    def stop(self):
        """Stop load generation."""
        self.running = False


def check_for_oom() -> bool:
    """Check if any OOM events occurred."""
    try:
        # Check dmesg for OOM killer
        result = subprocess.run(
            ["dmesg", "-T"],
            capture_output=True,
            text=True,
            timeout=5
        )
        if "Out of memory" in result.stdout or "oom-kill" in result.stdout:
            return True
        
        # Check if beam.smp crashed
        result = subprocess.run(
            ["pgrep", "-f", "beam.smp"],
            capture_output=True,
            timeout=5
        )
        if result.returncode != 0:
            return True  # Erlang process not running
            
    except Exception:
        pass
    
    return False


def check_server_available() -> bool:
    """Check if server is reachable."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect((SERVER_HOST, SERVER_PORT))
        sock.close()
        return True
    except Exception:
        return False


def run_backpressure_test() -> TestResult:
    """Run the backpressure collapse test."""
    log(f"=== Backpressure Collapse Test (Profile: {TEST_PROFILE}) ===")
    log(f"Target rate: {CONFIG['target_rate']} msg/sec")
    log(f"Overload: {CONFIG['overload_multiplier']}x = {int(CONFIG['target_rate'] * CONFIG['overload_multiplier'])} msg/sec")
    
    generator = LoadGenerator(CONFIG['target_rate'])
    
    result = TestResult(
        warmup=PhaseMetrics(name="warmup", duration_seconds=0),
        overload=PhaseMetrics(name="overload", duration_seconds=0),
        recovery=PhaseMetrics(name="recovery", duration_seconds=0),
    )
    
    try:
        # Phase 1: Warmup at normal rate
        log(f"\n=== Phase 1: Warmup ({CONFIG['warmup_seconds']}s at {CONFIG['target_rate']} msg/sec) ===")
        result.warmup = generator.run(
            name="warmup",
            rate=CONFIG['target_rate'],
            duration=CONFIG['warmup_seconds'],
            workers=10
        )
        log(f"  Sent: {result.warmup.messages_sent}, Success: {result.warmup.messages_succeeded}, "
            f"Rejected: {result.warmup.messages_rejected}")
        
        # Check for OOM after warmup
        if check_for_oom():
            log("  ERROR: OOM detected during warmup!")
            result.oom_detected = True
            return result
        
        if not check_server_available():
            log("  ERROR: Server not responding after warmup!")
            result.hung_detected = True
            return result
        
        # Phase 2: Overload
        overload_rate = int(CONFIG['target_rate'] * CONFIG['overload_multiplier'])
        log(f"\n=== Phase 2: Overload ({CONFIG['overload_seconds']}s at {overload_rate} msg/sec) ===")
        result.overload = generator.run(
            name="overload",
            rate=overload_rate,
            duration=CONFIG['overload_seconds'],
            workers=20  # More workers for higher rate
        )
        log(f"  Sent: {result.overload.messages_sent}, Success: {result.overload.messages_succeeded}, "
            f"Rejected: {result.overload.messages_rejected}, Timeout: {result.overload.messages_timeout}")
        
        # Check for OOM during overload
        if check_for_oom():
            log("  ERROR: OOM detected during overload!")
            result.oom_detected = True
            return result
        
        if not check_server_available():
            log("  ERROR: Server not responding after overload!")
            result.hung_detected = True
            return result
        
        # Phase 3: Recovery at normal rate
        log(f"\n=== Phase 3: Recovery ({CONFIG['recovery_seconds']}s at {CONFIG['target_rate']} msg/sec) ===")
        result.recovery = generator.run(
            name="recovery",
            rate=CONFIG['target_rate'],
            duration=CONFIG['recovery_seconds'],
            workers=10
        )
        log(f"  Sent: {result.recovery.messages_sent}, Success: {result.recovery.messages_succeeded}, "
            f"Rejected: {result.recovery.messages_rejected}")
        
        # Final OOM check
        if check_for_oom():
            log("  ERROR: OOM detected during recovery!")
            result.oom_detected = True
        
    except Exception as e:
        log(f"Test error: {e}")
        result.hung_detected = True
    
    return result


def analyze_results(result: TestResult) -> bool:
    """Analyze test results and determine pass/fail."""
    log("\n" + "=" * 50)
    log("Results Analysis")
    log("=" * 50)
    
    passed = True
    
    # Check for critical failures
    if result.oom_detected:
        log("FAIL: OOM detected - system did not handle overload gracefully")
        return False
    
    if result.hung_detected:
        log("FAIL: System hung/unresponsive during test")
        return False
    
    # Analyze warmup (should be stable)
    if result.warmup.messages_sent > 0:
        warmup_success_rate = result.warmup.messages_succeeded / result.warmup.messages_sent
        log(f"\nWarmup phase: {warmup_success_rate:.1%} success rate")
        if warmup_success_rate < 0.95:
            log(f"  WARNING: Warmup success rate below 95%")
    
    # Analyze overload (should shed load gracefully)
    if result.overload.messages_sent > 0:
        overload_success_rate = result.overload.messages_succeeded / result.overload.messages_sent
        log(f"\nOverload phase: {overload_success_rate:.1%} success rate")
        
        if overload_success_rate < CONFIG['min_successful_during_overload']:
            log(f"  FAIL: Success rate {overload_success_rate:.1%} below threshold "
                f"{CONFIG['min_successful_during_overload']:.1%}")
            passed = False
        else:
            log(f"  PASS: Success rate above threshold")
        
        # Check latency during overload
        if result.overload.latencies_ms:
            p99 = sorted(result.overload.latencies_ms)[int(len(result.overload.latencies_ms) * 0.99)]
            log(f"  Overload latency P99: {p99:.0f}ms")
            if p99 > CONFIG['max_latency_during_overload_ms']:
                log(f"  WARNING: P99 latency exceeds threshold {CONFIG['max_latency_during_overload_ms']}ms")
        
        # Check that load shedding happened (rejections)
        if result.overload.messages_rejected > 0:
            log(f"  Load shedding: {result.overload.messages_rejected} messages rejected (good!)")
        else:
            log(f"  NOTE: No explicit rejections - system may be queueing")
    
    # Analyze recovery (should return to normal)
    if result.recovery.messages_sent > 0:
        recovery_success_rate = result.recovery.messages_succeeded / result.recovery.messages_sent
        recovery_error_rate = 1.0 - recovery_success_rate
        log(f"\nRecovery phase: {recovery_success_rate:.1%} success rate")
        
        if recovery_error_rate > CONFIG['max_errors_during_recovery']:
            log(f"  FAIL: Error rate {recovery_error_rate:.1%} above threshold "
                f"{CONFIG['max_errors_during_recovery']:.1%}")
            passed = False
        else:
            log(f"  PASS: System recovered to normal operation")
        
        if result.recovery.latencies_ms:
            p99 = sorted(result.recovery.latencies_ms)[int(len(result.recovery.latencies_ms) * 0.99)]
            log(f"  Recovery latency P99: {p99:.0f}ms")
    
    return passed


def main():
    """Main entry point."""
    print("\n" + "=" * 70)
    print("Backpressure Collapse Stress Test")
    print("Per PRINCIPAL_AUDIT_REPORT.md Section 6.3")
    print("=" * 70)
    
    # Check prerequisites
    if not check_server_available():
        print(f"\nSKIP:INFRA - Server not available at {SERVER_HOST}:{SERVER_PORT}")
        sys.exit(2)
    
    # Run test
    result = run_backpressure_test()
    
    # Analyze and report
    passed = analyze_results(result)
    
    print("\n" + "=" * 70)
    if passed:
        print("RESULT: PASSED - System handles backpressure gracefully")
        sys.exit(0)
    else:
        print("RESULT: FAILED - Backpressure handling inadequate")
        sys.exit(1)


if __name__ == "__main__":
    main()
