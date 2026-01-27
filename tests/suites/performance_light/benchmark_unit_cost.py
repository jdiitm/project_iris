#!/usr/bin/env python3
"""
Unit Cost Benchmark

Measures the CPU cost per message to establish baseline performance.

INVARIANTS:
- Throughput must exceed minimum threshold (10k msg/s)
- System must handle concurrent load without crashing
- CPU cost per message must be measurable

Exit codes:
- 0: Benchmark passed (throughput >= 10k msg/s)
- 1: Benchmark failed or error
"""

import socket
import struct
import time
import os
import sys
import threading
import psutil
import argparse

HOST = 'localhost'
PORT = 8085
MSG_COUNT = 50000

# Minimum throughput threshold (messages per second)
MIN_THROUGHPUT = 10000


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def create_socket():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        s.connect((HOST, PORT))
        return s
    except socket.error as e:
        log(f"Socket connection failed: {e}")
        return None


def packet_login(user):
    return b'\x01' + user.encode('utf-8')


def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(payload)) + payload


def benchmark_worker(results, errors, pid):
    """Worker thread that sends messages and records duration."""
    sock = create_socket()
    if not sock:
        errors.append(f"Worker {pid} failed to connect")
        return
    
    try:
        # Login
        sock.sendall(packet_login(f"user_{pid}"))
        resp = sock.recv(1024)
        if b"LOGIN_OK" not in resp:
            errors.append(f"Worker {pid} login failed")
            sock.close()
            return
        
        # Send messages
        target = "recipient_0"
        payload = b"X" * 50
        pkt = packet_msg(target, payload)
        
        start = time.time()
        for _ in range(MSG_COUNT):
            sock.sendall(pkt)
        dur = time.time() - start
        
        sock.close()
        results.append(dur)
    except socket.error as e:
        errors.append(f"Worker {pid} error: {e}")
        try:
            sock.close()
        except Exception:
            pass


def measure_system_resources(pid, duration, container):
    """Monitor Erlang process CPU/RAM usage."""
    try:
        p = psutil.Process(pid)
        cpu_start = p.cpu_times()
        
        time.sleep(duration)
        
        cpu_end = p.cpu_times()
        container['cpu_user'] = cpu_end.user - cpu_start.user
        container['cpu_sys'] = cpu_end.system - cpu_start.system
        container['mem_rss'] = p.memory_info().rss
        container['success'] = True
    except psutil.NoSuchProcess:
        container['success'] = False
        container['error'] = "Process terminated during monitoring"
    except Exception as e:
        container['success'] = False
        container['error'] = str(e)


def main() -> int:
    """
    Run benchmark and return exit code.
    
    Returns:
        0 if benchmark passed, 1 if failed
    """
    log("--- UNIT COST BENCHMARK ---")
    
    passed = True
    
    # Check if server is already running
    test_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_running = test_sock.connect_ex((HOST, PORT)) == 0
    test_sock.close()
    
    if not server_running:
        log("FAIL: Server not running on port 8085")
        log("Please start the cluster before running benchmarks")
        return 1
    
    log("PASS: Server is running")
    
    # Find beam pid
    erl_pid = None
    for proc in psutil.process_iter(['pid', 'name']):
        try:
            if 'beam' in proc.info['name'].lower():
                erl_pid = proc.info['pid']
                break
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            continue
            
    if not erl_pid:
        log("FAIL: Erlang node (beam) process not found")
        return 1

    log(f"PASS: Found Erlang PID: {erl_pid}")
    
    # Configuration
    THREADS = 10
    TOTAL_MSGS = THREADS * MSG_COUNT
    
    log(f"Running benchmark: {THREADS} threads x {MSG_COUNT} msgs = {TOTAL_MSGS} total messages")
    
    # Start resource monitoring
    res_container = {}
    monitor = threading.Thread(target=measure_system_resources, args=(erl_pid, 5, res_container))
    monitor.start()
    
    # Run benchmark workers
    threads = []
    durations = []
    errors = []
    
    start_time = time.time()
    for i in range(THREADS):
        t = threading.Thread(target=benchmark_worker, args=(durations, errors, i))
        t.start()
        threads.append(t)
        
    for t in threads:
        t.join()
    monitor.join()
    
    total_time = time.time() - start_time
    
    # ================================================================
    # ASSERTIONS
    # ================================================================
    log("\n=== ASSERTIONS ===")
    
    # Check for worker errors
    if errors:
        log(f"FAIL: {len(errors)} worker errors occurred:")
        for err in errors[:5]:  # Show first 5 errors
            log(f"  - {err}")
        passed = False
    else:
        log(f"PASS: All {THREADS} workers completed successfully")
    
    # Calculate metrics
    if len(durations) > 0:
        successful_msgs = len(durations) * MSG_COUNT
        cpu_total_seconds = res_container.get('cpu_user', 0) + res_container.get('cpu_sys', 0)
        msgs_per_sec = successful_msgs / total_time if total_time > 0 else 0
        cpu_per_msg = cpu_total_seconds / successful_msgs if successful_msgs > 0 else 0
        
        log("\n--- METRICS ---")
        log(f"Total Messages: {successful_msgs}")
        log(f"Total Time:     {total_time:.4f}s")
        log(f"Throughput:     {msgs_per_sec:.2f} msgs/sec")
        log(f"Total CPU Time: {cpu_total_seconds:.4f}s")
        log(f"CPU Cost/Msg:   {cpu_per_msg*1_000_000:.2f} microseconds")
        
        if cpu_per_msg > 0:
            log(f"Est. Max RPS (1 Core): {1.0/cpu_per_msg:.2f}")
        
        log("\n=== THRESHOLD CHECK ===")
        
        # Assertion: Throughput meets minimum
        if msgs_per_sec >= MIN_THROUGHPUT:
            log(f"PASS: Throughput {msgs_per_sec:.0f} msg/s >= {MIN_THROUGHPUT} threshold")
        else:
            log(f"FAIL: Throughput {msgs_per_sec:.0f} msg/s < {MIN_THROUGHPUT} threshold")
            passed = False
    else:
        log("FAIL: No successful benchmark runs completed")
        passed = False
    
    # Final result
    log("\n=== RESULT ===")
    if passed:
        log("BENCHMARK PASSED")
        return 0
    else:
        log("BENCHMARK FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
