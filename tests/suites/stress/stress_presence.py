#!/usr/bin/env python3
"""
Consolidated Presence Stress Test

Tests presence system scalability:
- Hotspot mode: Multiple fans polling a single celebrity's status
- Global mode: Mixed traffic (10% hotspot, 90% random users)

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

# Configuration
HOST = 'localhost'
PORT = 8085
TARGET_HOTSPOT = "messi_the_goat"

# Stats
stats_lock = threading.Lock()
stats = {
    "hotspot_reqs": 0, "hotspot_lats": [],
    "random_reqs": 0, "random_lats": [],
    "errors": 0
}

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
        s.connect((HOST, PORT))
        s.sendall(packet_login(f"fan_{idx}"))
        s.recv(1024)
        
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
                break
        s.close()
    except Exception:
        with stats_lock:
            stats["errors"] += 1

def global_worker(idx, duration, hotspot_ratio=0.10):
    """Worker that mixes hotspot and random queries."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((HOST, PORT))
        s.sendall(packet_login(f"user_{idx}"))
        s.recv(1024)
        
        end_time = time.time() + duration
        while time.time() < end_time:
            is_hotspot = random.random() < hotspot_ratio
            target = TARGET_HOTSPOT if is_hotspot else f"friend_{random.randint(0, 1000000)}"
            
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
                break
        s.close()
    except Exception:
        with stats_lock:
            stats["errors"] += 1

# ============================================================================
# Test Runners
# ============================================================================

def run_test(args):
    print(f"--- PRESENCE STRESS TEST ({args.mode.upper()}) ---")
    print(f"Workers: {args.workers}, Duration: {args.duration}s")
    
    if not args.skip_restart:
        os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
        os.system("make all >/dev/null")
        os.system("make start_core >/dev/null; sleep 2")
        os.system("make start_edge1 >/dev/null; sleep 2")
    
    # Login the hotspot user
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    s.sendall(packet_login(TARGET_HOTSPOT))
    s.recv(1024)
    print(f"[*] {TARGET_HOTSPOT} is online")
    
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
    
    # Results
    h_reqs = stats["hotspot_reqs"]
    r_reqs = stats["random_reqs"]
    total_reqs = h_reqs + r_reqs
    
    h_avg = statistics.mean(stats["hotspot_lats"]) if stats["hotspot_lats"] else 0
    r_avg = statistics.mean(stats["random_lats"]) if stats["random_lats"] else 0
    
    h_p99 = 0
    if stats["hotspot_lats"]:
        sorted_lats = sorted(stats["hotspot_lats"])
        h_p99 = sorted_lats[int(0.99 * len(sorted_lats))] if len(sorted_lats) > 0 else 0
    
    print("\n--- RESULTS ---")
    print(f"Total Requests:     {total_reqs}")
    print(f"Duration:           {total_time:.2f}s")
    print(f"Throughput:         {total_reqs / total_time:.0f} ops/sec")
    print(f"Errors:             {stats['errors']}")
    print()
    print(f"Hotspot Requests:   {h_reqs} (avg: {h_avg:.2f}ms, p99: {h_p99:.2f}ms)")
    if args.mode == "global":
        print(f"Random Requests:    {r_reqs} (avg: {r_avg:.2f}ms)")
    
    # Verdicts
    if h_avg > 5.0:
        print("\n[WARN] Hotspot latency > 5ms - cache may not be working")
    if args.mode == "global" and r_avg < h_avg and r_reqs > 0:
        print("\n[WARN] Random faster than hotspot - unexpected")
    
    if not args.skip_restart:
        os.system("make stop >/dev/null 2>&1")
    
    return 0 if stats["errors"] == 0 else 1

# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Presence Stress Test')
    parser.add_argument('--mode', choices=['hotspot', 'global'], default='hotspot',
                        help='Test mode')
    parser.add_argument('--workers', type=int, default=10, help='Number of worker threads')
    parser.add_argument('--duration', type=int, default=15, help='Test duration (seconds)')
    parser.add_argument('--hotspot-ratio', type=float, default=0.10, 
                        help='Ratio of hotspot queries in global mode')
    parser.add_argument('--skip-restart', action='store_true', help='Skip cluster restart')
    args = parser.parse_args()
    
    return run_test(args)

if __name__ == "__main__":
    sys.exit(main())
