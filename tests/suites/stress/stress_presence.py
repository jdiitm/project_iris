#!/usr/bin/env python3
"""
Consolidated Presence Stress Test

Tests presence system scalability:
- Hotspot mode: Multiple fans polling a single celebrity's status
- Global mode: Mixed traffic (10% hotspot, 90% random users)

INVARIANTS:
- Error rate must be below 5%
- Throughput must exceed minimum threshold (100 ops/sec per worker)
- Hotspot latency P99 must be below 100ms
- Total requests completed must be > 0

Modes:
  --mode hotspot : Single target presence polling
  --mode global  : Mixed hotspot + random presence queries

Tier: 1 (Stress testing)
"""

import socket
import os
import struct
import time
import threading
import sys
import statistics
import random
import argparse

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Configuration
HOST = 'localhost'
PORT = 8085
TARGET_HOTSPOT = "messi_the_goat"

# Thresholds
MAX_ERROR_RATE = 0.05  # 5% max error rate
MIN_OPS_PER_WORKER = 100  # Minimum operations per worker
MAX_HOTSPOT_P99_MS = 100  # Maximum acceptable P99 latency

# Stats
stats_lock = threading.Lock()
stats = {
    "hotspot_reqs": 0, "hotspot_lats": [],
    "random_reqs": 0, "random_lats": [],
    "errors": 0,
    "error_details": []
}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


# ============================================================================
# Protocol Helpers
# ============================================================================

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_get_status(target):
    t_bytes = target.encode('utf-8')
    return b'\x05' + struct.pack('>H', len(t_bytes)) + t_bytes

# ============================================================================
# Workers
# ============================================================================

def hotspot_worker(idx, duration):
    """Worker that exclusively polls a hotspot user."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(10)
        s.connect((HOST, PORT))
        s.sendall(packet_login(f"fan_{idx}"))
        resp = s.recv(1024)
        
        if b"LOGIN_OK" not in resp:
            with stats_lock:
                stats["errors"] += 1
                stats["error_details"].append(f"Worker {idx}: login failed")
            s.close()
            return
        
        end_time = time.time() + duration
        while time.time() < end_time:
            start = time.time()
            s.sendall(packet_get_status(TARGET_HOTSPOT))
            resp = s.recv(1024)
            lat = (time.time() - start) * 1000
            
            if len(resp) > 0 and resp[0] == 6:
                with stats_lock:
                    stats["hotspot_reqs"] += 1
                    stats["hotspot_lats"].append(lat)
            else:
                with stats_lock:
                    stats["errors"] += 1
                    stats["error_details"].append(f"Worker {idx}: bad response opcode")
                break
        s.close()
    except socket.timeout as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: socket timeout")
    except socket.error as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: socket error - {e}")
    except Exception as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: unexpected error - {type(e).__name__}: {e}")

def global_worker(idx, duration, hotspot_ratio=0.10):
    """Worker that mixes hotspot and random queries."""
    # Per-worker random instance for thread safety
    worker_random = random.Random(TEST_SEED + idx)
    
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(10)
        s.connect((HOST, PORT))
        s.sendall(packet_login(f"user_{idx}"))
        resp = s.recv(1024)
        
        if b"LOGIN_OK" not in resp:
            with stats_lock:
                stats["errors"] += 1
                stats["error_details"].append(f"Worker {idx}: login failed")
            s.close()
            return
        
        end_time = time.time() + duration
        while time.time() < end_time:
            is_hotspot = worker_random.random() < hotspot_ratio
            target = TARGET_HOTSPOT if is_hotspot else f"friend_{worker_random.randint(0, 1000000)}"
            
            start = time.time()
            s.sendall(packet_get_status(target))
            resp = s.recv(1024)
            lat = (time.time() - start) * 1000
            
            if len(resp) > 0:
                with stats_lock:
                    if is_hotspot:
                        stats["hotspot_reqs"] += 1
                        stats["hotspot_lats"].append(lat)
                    else:
                        stats["random_reqs"] += 1
                        stats["random_lats"].append(lat)
            else:
                with stats_lock:
                    stats["errors"] += 1
                    stats["error_details"].append(f"Worker {idx}: empty response")
                break
        s.close()
    except socket.timeout as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: socket timeout")
    except socket.error as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: socket error - {e}")
    except Exception as e:
        with stats_lock:
            stats["errors"] += 1
            stats["error_details"].append(f"Worker {idx}: unexpected error - {type(e).__name__}: {e}")

# ============================================================================
# Test Runner
# ============================================================================

def run_test(args) -> int:
    """
    Run the presence stress test.
    
    Returns:
        0 if all assertions pass, 1 otherwise
    """
    log(f"--- PRESENCE STRESS TEST ({args.mode.upper()}) ---")
    log(f"Workers: {args.workers}, Duration: {args.duration}s, Seed: {TEST_SEED}")
    
    passed = True
    
    # Check cluster is running
    try:
        test_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        test_sock.settimeout(5)
        result = test_sock.connect_ex((HOST, PORT))
        test_sock.close()
        if result != 0:
            log("FAIL: Server not running on port 8085")
            return 1
    except Exception as e:
        log(f"FAIL: Could not connect to server - {e}")
        return 1
    
    log("PASS: Server is running")
    
    # Login the hotspot user
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(10)
        s.connect((HOST, PORT))
        s.sendall(packet_login(TARGET_HOTSPOT))
        resp = s.recv(1024)
        if b"LOGIN_OK" not in resp:
            log(f"FAIL: Could not login hotspot user {TARGET_HOTSPOT}")
            s.close()
            return 1
        log(f"PASS: {TARGET_HOTSPOT} is online")
    except Exception as e:
        log(f"FAIL: Hotspot user login error - {e}")
        return 1
    
    # Select worker function
    if args.mode == "hotspot":
        worker_fn = lambda idx: hotspot_worker(idx, args.duration)
    else:
        worker_fn = lambda idx: global_worker(idx, args.duration, args.hotspot_ratio)
    
    # Start workers
    threads = []
    start_global = time.time()
    
    for i in range(args.workers):
        t = threading.Thread(target=worker_fn, args=(i,))
        t.daemon = True
        t.start()
        threads.append(t)
    
    for t in threads:
        t.join()
    
    total_time = time.time() - start_global
    s.close()
    
    # Calculate results
    h_reqs = stats["hotspot_reqs"]
    r_reqs = stats["random_reqs"]
    total_reqs = h_reqs + r_reqs
    errors = stats["errors"]
    total_attempts = total_reqs + errors
    
    h_avg = statistics.mean(stats["hotspot_lats"]) if stats["hotspot_lats"] else 0
    r_avg = statistics.mean(stats["random_lats"]) if stats["random_lats"] else 0
    
    h_p99 = 0
    if stats["hotspot_lats"]:
        sorted_lats = sorted(stats["hotspot_lats"])
        h_p99 = sorted_lats[int(0.99 * len(sorted_lats))] if len(sorted_lats) > 0 else 0
    
    log("\n--- METRICS ---")
    log(f"Total Requests:     {total_reqs}")
    log(f"Duration:           {total_time:.2f}s")
    log(f"Throughput:         {total_reqs / total_time:.0f} ops/sec")
    log(f"Errors:             {errors}")
    log(f"Hotspot Requests:   {h_reqs} (avg: {h_avg:.2f}ms, p99: {h_p99:.2f}ms)")
    if args.mode == "global":
        log(f"Random Requests:    {r_reqs} (avg: {r_avg:.2f}ms)")
    
    # Show error details (first 5)
    if stats["error_details"]:
        log("\nError details (first 5):")
        for detail in stats["error_details"][:5]:
            log(f"  - {detail}")
    
    # ================================================================
    # ASSERTIONS
    # ================================================================
    log("\n=== ASSERTIONS ===")
    
    # Assertion 1: Total requests > 0
    if total_reqs == 0:
        log("FAIL: No requests completed successfully")
        passed = False
    else:
        log(f"PASS: {total_reqs} requests completed")
    
    # Assertion 2: Minimum throughput (per worker)
    min_expected = args.workers * MIN_OPS_PER_WORKER
    if total_reqs < min_expected:
        log(f"FAIL: Throughput {total_reqs} < {min_expected} minimum ({MIN_OPS_PER_WORKER}/worker)")
        passed = False
    else:
        log(f"PASS: Throughput {total_reqs} >= {min_expected} minimum")
    
    # Assertion 3: Error rate
    error_rate = errors / total_attempts if total_attempts > 0 else 0
    if error_rate > MAX_ERROR_RATE:
        log(f"FAIL: Error rate {error_rate:.1%} > {MAX_ERROR_RATE:.0%} threshold")
        passed = False
    else:
        log(f"PASS: Error rate {error_rate:.1%} <= {MAX_ERROR_RATE:.0%}")
    
    # Assertion 4: Hotspot latency P99
    if h_p99 > MAX_HOTSPOT_P99_MS:
        log(f"FAIL: Hotspot P99 latency {h_p99:.1f}ms > {MAX_HOTSPOT_P99_MS}ms threshold")
        passed = False
    else:
        log(f"PASS: Hotspot P99 latency {h_p99:.1f}ms <= {MAX_HOTSPOT_P99_MS}ms")
    
    # Final result
    log("\n=== RESULT ===")
    if passed:
        log("STRESS TEST PASSED")
        return 0
    else:
        log("STRESS TEST FAILED")
        return 1

# ============================================================================
# Main
# ============================================================================

def main() -> int:
    parser = argparse.ArgumentParser(description='Presence Stress Test')
    parser.add_argument('--mode', choices=['hotspot', 'global'], default='hotspot',
                        help='Test mode')
    parser.add_argument('--workers', type=int, default=10, help='Number of worker threads')
    parser.add_argument('--duration', type=int, default=15, help='Test duration (seconds)')
    parser.add_argument('--hotspot-ratio', type=float, default=0.10, 
                        help='Ratio of hotspot queries in global mode')
    parser.add_argument('--skip-restart', action='store_true', help='Skip cluster restart (ignored)')
    args = parser.parse_args()
    
    return run_test(args)

if __name__ == "__main__":
    sys.exit(main())
