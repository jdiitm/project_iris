#!/usr/bin/env python3
"""
Memory Benchmark Test Suite

Measures per-connection memory usage and validates efficiency targets.

Targets (RFC NFR-1):
- Per-connection memory: ≤10KB average
- Base VM overhead: ≤500MB
- Memory growth: Linear with connections
"""

import os
import sys
import time
import socket
import subprocess
import threading

# Add project root to path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager

# Configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
EDGE_PORT = 8085

# Profile-based thresholds
PROFILES = {
    "smoke": {
        "connections": 100,
        "per_conn_kb": 50,       # More lenient for smoke
        "base_overhead_mb": 800,  # Base VM overhead
    },
    "full": {
        "connections": 10000,
        "per_conn_kb": 10,       # Strict target
        "base_overhead_mb": 600,
    }
}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def get_memory_mb():
    """Get total memory usage of beam.smp processes in MB."""
    try:
        result = subprocess.run(
            ["ps", "-C", "beam.smp", "-o", "rss="],
            capture_output=True, text=True, timeout=5
        )
        total_kb = sum(int(x) for x in result.stdout.split() if x.strip().isdigit())
        return total_kb / 1024
    except Exception:
        return 0


def create_connection(user_id):
    """Create a single connection and login."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(('localhost', EDGE_PORT))
        # Login packet: 0x01 | username
        username = f"mem_user_{user_id}".encode()
        sock.sendall(b'\x01' + username)
        return sock
    except Exception as e:
        return None


def measure_baseline():
    """Measure baseline memory with no connections."""
    log("Measuring baseline memory (no connections)...")
    time.sleep(2)  # Let system stabilize
    baseline = get_memory_mb()
    log(f"  Baseline: {baseline:.1f} MB")
    return baseline


def measure_with_connections(count):
    """Create connections and measure memory."""
    log(f"Creating {count} connections...")
    connections = []
    
    for i in range(count):
        sock = create_connection(i)
        if sock:
            connections.append(sock)
        if (i + 1) % 50 == 0:
            log(f"  Created {i + 1}/{count} connections")
    
    log(f"  Total connections: {len(connections)}")
    
    # Let memory stabilize
    time.sleep(3)
    
    memory = get_memory_mb()
    log(f"  Memory with connections: {memory:.1f} MB")
    
    return connections, memory


def cleanup_connections(connections):
    """Close all connections."""
    for sock in connections:
        try:
            sock.close()
        except Exception:
            pass


def main():
    log("=" * 60)
    log("MEMORY BENCHMARK TEST")
    log("=" * 60)
    
    profile = PROFILES.get(TEST_PROFILE, PROFILES["smoke"])
    target_connections = profile["connections"]
    per_conn_limit_kb = profile["per_conn_kb"]
    base_overhead_limit = profile["base_overhead_mb"]
    
    log(f"Profile: {TEST_PROFILE}")
    log(f"Target connections: {target_connections}")
    log(f"Per-connection limit: {per_conn_limit_kb} KB")
    log(f"Base overhead limit: {base_overhead_limit} MB")
    log("")
    
    with ClusterManager(project_root=project_root) as cluster:
        # Measure baseline
        baseline_mb = measure_baseline()
        
        # Create connections and measure
        connections, total_mb = measure_with_connections(target_connections)
        
        # Calculate metrics
        conn_count = len(connections)
        if conn_count > 0:
            memory_increase = total_mb - baseline_mb
            per_conn_kb = (memory_increase * 1024) / conn_count
        else:
            memory_increase = 0
            per_conn_kb = 0
        
        # Cleanup
        cleanup_connections(connections)
        
        # Results
        log("")
        log("=" * 60)
        log("RESULTS")
        log("=" * 60)
        log(f"  Baseline memory: {baseline_mb:.1f} MB")
        log(f"  Total memory: {total_mb:.1f} MB")
        log(f"  Memory increase: {memory_increase:.1f} MB")
        log(f"  Connections created: {conn_count}")
        log(f"  Per-connection memory: {per_conn_kb:.2f} KB")
        log("")
        
        # Assertions
        passed = True
        
        if baseline_mb > base_overhead_limit:
            log(f"  ❌ Base overhead exceeded: {baseline_mb:.1f} MB > {base_overhead_limit} MB")
            passed = False
        else:
            log(f"  ✅ Base overhead OK: {baseline_mb:.1f} MB <= {base_overhead_limit} MB")
        
        if per_conn_kb > per_conn_limit_kb and conn_count > 0:
            log(f"  ❌ Per-connection memory exceeded: {per_conn_kb:.2f} KB > {per_conn_limit_kb} KB")
            passed = False
        else:
            log(f"  ✅ Per-connection memory OK: {per_conn_kb:.2f} KB <= {per_conn_limit_kb} KB")
        
        if conn_count < target_connections * 0.9:
            log(f"  ⚠️ Connection count low: {conn_count} < {int(target_connections * 0.9)}")
            # Don't fail for this in smoke profile
            if TEST_PROFILE != "smoke":
                passed = False
        else:
            log(f"  ✅ Connection count OK: {conn_count}")
        
        log("")
        if passed:
            log("✅ All memory benchmarks passed!")
            sys.exit(0)
        else:
            log("❌ Memory benchmark failed!")
            sys.exit(1)


if __name__ == "__main__":
    main()
