#!/usr/bin/env python3
"""
Connection Scale Test - 100K Connection Load Test Harness (Phase 0)

Tests Iris ability to handle 100K concurrent connections per edge node.
Per RFC-001 NFR-10: Connections per edge node ≥100K (target 200K)

Per TEST_CONTRACT.md:
- exit(0) = PASS - Target connection count achieved
- exit(1) = FAIL - Target not met
- exit(2) = SKIP - Infrastructure not available

Per TEST_DETERMINISM.md:
- Uses TEST_SEED for reproducibility
- Uses TEST_PROFILE for scale adjustment
"""

import os
import sys
import time
import socket
import random
import struct
import threading
import queue
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from typing import List, Optional

# Setup paths
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework.cluster import ClusterManager

# =============================================================================
# Configuration
# =============================================================================

TEST_SEED = int(os.environ.get("TEST_SEED", "42"))
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
random.seed(TEST_SEED)

# Server configuration
EDGE_HOST = os.environ.get("EDGE_HOST", "127.0.0.1")
EDGE_PORT = int(os.environ.get("EDGE_PORT", "8085"))

# Scale configuration based on profile
if TEST_PROFILE == "smoke":
    # CI/smoke: Reduced scale for quick validation
    TARGET_CONNECTIONS = 1000
    RAMP_RATE = 100          # connections/second
    HOLD_DURATION = 30       # seconds to hold peak
    TIMEOUT_PER_CONN = 5.0   # connection timeout
elif TEST_PROFILE == "full":
    # Full scale: Production-like test
    TARGET_CONNECTIONS = 100000
    RAMP_RATE = 1000         # connections/second
    HOLD_DURATION = 60       # seconds to hold peak
    TIMEOUT_PER_CONN = 10.0  # connection timeout
else:
    # Default to smoke
    TARGET_CONNECTIONS = 1000
    RAMP_RATE = 100
    HOLD_DURATION = 30
    TIMEOUT_PER_CONN = 5.0


# =============================================================================
# Data Structures
# =============================================================================

@dataclass
class ConnectionStats:
    """Statistics for connection attempts."""
    attempted: int = 0
    successful: int = 0
    failed: int = 0
    timeouts: int = 0
    refused: int = 0
    errors: List[str] = field(default_factory=list)
    connect_times: List[float] = field(default_factory=list)
    
    @property
    def success_rate(self) -> float:
        if self.attempted == 0:
            return 0.0
        return self.successful / self.attempted * 100
    
    @property
    def avg_connect_time_ms(self) -> float:
        if not self.connect_times:
            return 0.0
        return sum(self.connect_times) / len(self.connect_times) * 1000


@dataclass
class TestResult:
    """Overall test result."""
    passed: bool
    peak_connections: int
    target_connections: int
    stats: ConnectionStats
    duration_seconds: float
    memory_mb: Optional[float] = None
    cpu_pct: Optional[float] = None


# =============================================================================
# Connection Management
# =============================================================================

class IrisConnection:
    """Single Iris connection."""
    
    def __init__(self, user_id: str):
        self.user_id = user_id
        self.sock: Optional[socket.socket] = None
        self.connected = False
    
    def connect(self, host: str, port: int, timeout: float) -> bool:
        """Establish connection and login."""
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.settimeout(timeout)
            self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            self.sock.connect((host, port))
            
            # Send login packet: 0x01 | username
            login_packet = bytes([0x01]) + self.user_id.encode('utf-8')
            self.sock.sendall(login_packet)
            
            self.connected = True
            return True
        except Exception as e:
            self.close()
            raise e
    
    def close(self):
        """Close connection."""
        if self.sock:
            try:
                self.sock.close()
            except:
                pass
        self.sock = None
        self.connected = False
    
    def is_alive(self) -> bool:
        """Check if connection is still alive."""
        if not self.sock or not self.connected:
            return False
        try:
            # Send a small ping (empty read with peek)
            self.sock.setblocking(False)
            try:
                data = self.sock.recv(1, socket.MSG_PEEK | socket.MSG_DONTWAIT)
                return True  # Data available or connection open
            except BlockingIOError:
                return True  # No data but connection open
            except:
                return False
        finally:
            if self.sock:
                self.sock.setblocking(True)


class ConnectionPool:
    """Pool of Iris connections."""
    
    def __init__(self):
        self.connections: List[IrisConnection] = []
        self.lock = threading.Lock()
        self.stats = ConnectionStats()
    
    def add_connections(self, count: int, host: str, port: int, 
                       timeout: float, progress_callback=None) -> int:
        """Add connections to the pool."""
        successful = 0
        
        for i in range(count):
            user_id = f"scale_user_{len(self.connections) + i}_{TEST_SEED}"
            conn = IrisConnection(user_id)
            
            start_time = time.monotonic()
            try:
                self.stats.attempted += 1
                if conn.connect(host, port, timeout):
                    connect_time = time.monotonic() - start_time
                    self.stats.connect_times.append(connect_time)
                    self.stats.successful += 1
                    
                    with self.lock:
                        self.connections.append(conn)
                    successful += 1
            except socket.timeout:
                self.stats.timeouts += 1
                self.stats.failed += 1
            except ConnectionRefusedError:
                self.stats.refused += 1
                self.stats.failed += 1
            except Exception as e:
                self.stats.errors.append(str(e)[:100])
                self.stats.failed += 1
            
            if progress_callback and (i + 1) % 100 == 0:
                progress_callback(i + 1, successful)
        
        return successful
    
    def get_active_count(self) -> int:
        """Count active connections."""
        with self.lock:
            active = sum(1 for c in self.connections if c.is_alive())
        return active
    
    def close_all(self):
        """Close all connections."""
        with self.lock:
            for conn in self.connections:
                conn.close()
            self.connections.clear()


# =============================================================================
# Load Test
# =============================================================================

def check_server_available() -> bool:
    """Check if server is accepting connections."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((EDGE_HOST, EDGE_PORT))
        sock.close()
        return result == 0
    except:
        return False


def get_system_metrics() -> dict:
    """Get system metrics (memory, CPU)."""
    metrics = {}
    try:
        import subprocess
        result = subprocess.run(
            ["ps", "aux"], 
            capture_output=True, text=True, timeout=5
        )
        for line in result.stdout.split('\n'):
            if 'beam.smp' in line.lower() or 'iris' in line.lower():
                parts = line.split()
                if len(parts) >= 6:
                    metrics['cpu_pct'] = float(parts[2])
                    metrics['rss_kb'] = int(parts[5])
                    break
    except:
        pass
    return metrics


def run_load_test(target: int, ramp_rate: int, hold_duration: int) -> TestResult:
    """Run the connection scale test."""
    print(f"\n{'='*60}")
    print(f" CONNECTION SCALE TEST")
    print(f" Target: {target:,} connections")
    print(f" Ramp rate: {ramp_rate}/sec")
    print(f" Hold duration: {hold_duration}s")
    print(f"{'='*60}\n")
    
    pool = ConnectionPool()
    start_time = time.monotonic()
    
    try:
        # Phase 1: Ramp up
        print("[PHASE 1] Ramping up connections...")
        
        batches = (target + ramp_rate - 1) // ramp_rate  # Ceiling division
        
        for batch in range(batches):
            batch_size = min(ramp_rate, target - batch * ramp_rate)
            batch_start = time.monotonic()
            
            def progress(done, successful):
                pass  # Quiet progress
            
            successful = pool.add_connections(
                batch_size, EDGE_HOST, EDGE_PORT, 
                TIMEOUT_PER_CONN, progress
            )
            
            current = pool.get_active_count()
            elapsed = time.monotonic() - start_time
            
            print(f"  Batch {batch+1}/{batches}: +{successful} conns "
                  f"(total: {current:,}, elapsed: {elapsed:.1f}s)")
            
            # Rate limiting - wait for remaining time in this second
            batch_duration = time.monotonic() - batch_start
            if batch_duration < 1.0:
                time.sleep(1.0 - batch_duration)
            
            # Early exit if server is refusing connections
            if pool.stats.refused > ramp_rate * 3:
                print(f"[WARN] Too many refused connections, stopping ramp")
                break
        
        # Peak connections after ramp
        peak_connections = pool.get_active_count()
        print(f"\n[PHASE 1 COMPLETE] Peak connections: {peak_connections:,}")
        
        # Phase 2: Hold
        print(f"\n[PHASE 2] Holding connections for {hold_duration}s...")
        
        hold_start = time.monotonic()
        samples = []
        
        while time.monotonic() - hold_start < hold_duration:
            active = pool.get_active_count()
            metrics = get_system_metrics()
            
            samples.append({
                'active': active,
                'metrics': metrics
            })
            
            elapsed = time.monotonic() - hold_start
            print(f"  t={elapsed:.0f}s: {active:,} active", end="")
            if metrics:
                print(f", CPU={metrics.get('cpu_pct', 'N/A')}%, "
                      f"RSS={metrics.get('rss_kb', 0) // 1024}MB", end="")
            print()
            
            time.sleep(5)
        
        # Calculate averages
        avg_active = sum(s['active'] for s in samples) / len(samples) if samples else 0
        
        cpu_samples = [s['metrics'].get('cpu_pct') for s in samples 
                       if s['metrics'] and 'cpu_pct' in s['metrics']]
        avg_cpu = sum(cpu_samples) / len(cpu_samples) if cpu_samples else None
        
        mem_samples = [s['metrics'].get('rss_kb') for s in samples 
                       if s['metrics'] and 'rss_kb' in s['metrics']]
        avg_mem = sum(mem_samples) / len(mem_samples) / 1024 if mem_samples else None
        
        duration = time.monotonic() - start_time
        
        # Determine pass/fail
        # Pass if we achieved at least 90% of target during hold phase
        min_required = int(target * 0.9)
        passed = avg_active >= min_required
        
        return TestResult(
            passed=passed,
            peak_connections=peak_connections,
            target_connections=target,
            stats=pool.stats,
            duration_seconds=duration,
            memory_mb=avg_mem,
            cpu_pct=avg_cpu
        )
        
    finally:
        # Phase 3: Cleanup
        print(f"\n[PHASE 3] Closing connections...")
        pool.close_all()


def print_results(result: TestResult):
    """Print test results."""
    print(f"\n{'='*60}")
    print(" TEST RESULTS")
    print(f"{'='*60}")
    
    print(f"\nConnections:")
    print(f"  Target:     {result.target_connections:,}")
    print(f"  Peak:       {result.peak_connections:,}")
    print(f"  Success rate: {result.stats.success_rate:.1f}%")
    
    print(f"\nConnection Stats:")
    print(f"  Attempted:  {result.stats.attempted:,}")
    print(f"  Successful: {result.stats.successful:,}")
    print(f"  Failed:     {result.stats.failed:,}")
    print(f"  Timeouts:   {result.stats.timeouts:,}")
    print(f"  Refused:    {result.stats.refused:,}")
    
    if result.stats.connect_times:
        print(f"\nLatency:")
        print(f"  Avg connect: {result.stats.avg_connect_time_ms:.1f}ms")
    
    if result.memory_mb:
        print(f"\nServer Resources:")
        print(f"  Avg Memory: {result.memory_mb:.1f} MB")
        if result.peak_connections > 0:
            mem_per_conn = (result.memory_mb * 1024) / result.peak_connections
            print(f"  Memory/conn: {mem_per_conn:.2f} KB")
    
    if result.cpu_pct is not None:
        print(f"  Avg CPU:    {result.cpu_pct:.1f}%")
    
    print(f"\nDuration: {result.duration_seconds:.1f}s")
    
    print(f"\n{'='*60}")
    if result.passed:
        print(f" RESULT: PASS (≥90% of target achieved)")
    else:
        print(f" RESULT: FAIL (did not achieve 90% of target)")
    print(f"{'='*60}\n")


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description="Connection Scale Test")
    parser.add_argument("--target", type=int, default=TARGET_CONNECTIONS,
                        help=f"Target connections (default: {TARGET_CONNECTIONS})")
    parser.add_argument("--rate", type=int, default=RAMP_RATE,
                        help=f"Ramp rate/sec (default: {RAMP_RATE})")
    parser.add_argument("--hold", type=int, default=HOLD_DURATION,
                        help=f"Hold duration sec (default: {HOLD_DURATION})")
    args = parser.parse_args()
    
    print(f"[INFO] Connection Scale Test")
    print(f"[INFO] Profile: {TEST_PROFILE}, Seed: {TEST_SEED}")
    print(f"[INFO] Target: {args.target:,} connections @ {args.rate}/sec")
    
    # Use ClusterManager to ensure server is running
    with ClusterManager(project_root=PROJECT_ROOT) as cluster:
        # Verify server is available
        if not check_server_available():
            print(f"\n[FAIL] Server not available at {EDGE_HOST}:{EDGE_PORT} after cluster start")
            sys.exit(1)
        
        # Run test
        result = run_load_test(args.target, args.rate, args.hold)
        
        # Print results
        print_results(result)
        
        # Exit with appropriate code
        if result.passed:
            sys.exit(0)
        else:
            sys.exit(1)


if __name__ == "__main__":
    main()
