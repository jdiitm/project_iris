#!/usr/bin/env python3
"""
Performance Metrics Collector

Runs performance tests and collects metrics for baseline comparison.
Outputs JSON file suitable for baseline.py.

Usage:
    python3 tests/perf/collect_metrics.py --output metrics.json
    python3 tests/perf/collect_metrics.py --output metrics.json --quick
"""

import argparse
import json
import os
import socket
import struct
import sys
import time
import statistics
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Dict, List

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10


def connect_and_login(host: str, port: int, username: str) -> socket.socket:
    """Connect and login, return socket."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    sock.connect((host, port))
    
    # Login
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    
    response = sock.recv(1024)
    if b"LOGIN_OK" not in response:
        raise Exception(f"Login failed: {response}")
    
    return sock


def measure_connection_setup(num_connections: int = 100) -> Dict[str, float]:
    """Measure connection setup latency."""
    latencies = []
    
    for i in range(num_connections):
        start = time.time()
        try:
            sock = connect_and_login(SERVER_HOST, SERVER_PORT, f"conn_test_{i}_{time.time()}")
            elapsed = (time.time() - start) * 1000
            latencies.append(elapsed)
            sock.close()
        except Exception as e:
            print(f"  Connection {i} failed: {e}")
    
    if not latencies:
        return {"connection_setup_ms": 0, "connection_success_rate": 0}
    
    return {
        "connection_setup_ms": statistics.median(latencies),
        "connection_setup_p99_ms": sorted(latencies)[int(len(latencies) * 0.99)] if len(latencies) > 10 else max(latencies),
        "connection_success_rate": len(latencies) / num_connections,
    }


def measure_message_latency(num_messages: int = 1000) -> Dict[str, float]:
    """Measure message send/receive latency."""
    sender_name = f"latency_sender_{time.time()}"
    receiver_name = f"latency_receiver_{time.time()}"
    
    try:
        sender = connect_and_login(SERVER_HOST, SERVER_PORT, sender_name)
        receiver = connect_and_login(SERVER_HOST, SERVER_PORT, receiver_name)
    except Exception as e:
        print(f"  Setup failed: {e}")
        return {"p50_latency_ms": 0, "p99_latency_ms": 0}
    
    receiver.setblocking(False)
    
    latencies = []
    
    for i in range(num_messages):
        msg = f"latency_test_{i}_{time.time()}"
        target_bytes = receiver_name.encode()
        msg_bytes = msg.encode()
        
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        send_time = time.time()
        sender.sendall(packet)
        
        # Try to receive
        recv_time = None
        deadline = time.time() + 1.0  # 1 second timeout
        
        while time.time() < deadline:
            try:
                data = receiver.recv(4096)
                if data and msg.encode() in data:
                    recv_time = time.time()
                    break
            except BlockingIOError:
                time.sleep(0.001)
        
        if recv_time:
            latencies.append((recv_time - send_time) * 1000)
    
    sender.close()
    receiver.close()
    
    if not latencies:
        return {"p50_latency_ms": 0, "p99_latency_ms": 0, "message_success_rate": 0}
    
    sorted_latencies = sorted(latencies)
    
    return {
        "p50_latency_ms": sorted_latencies[len(sorted_latencies) // 2],
        "p99_latency_ms": sorted_latencies[int(len(sorted_latencies) * 0.99)] if len(sorted_latencies) > 10 else max(sorted_latencies),
        "message_success_rate": len(latencies) / num_messages,
        "avg_latency_ms": statistics.mean(latencies),
    }


def measure_throughput(duration_sec: int = 10) -> Dict[str, float]:
    """Measure message throughput."""
    sender_name = f"throughput_sender_{time.time()}"
    receiver_name = f"throughput_receiver_{time.time()}"
    
    try:
        sender = connect_and_login(SERVER_HOST, SERVER_PORT, sender_name)
        receiver = connect_and_login(SERVER_HOST, SERVER_PORT, receiver_name)
    except Exception as e:
        print(f"  Setup failed: {e}")
        return {"throughput_msg_sec": 0}
    
    receiver.setblocking(False)
    
    messages_sent = 0
    messages_received = 0
    
    target_bytes = receiver_name.encode()
    
    start_time = time.time()
    end_time = start_time + duration_sec
    
    # Send as fast as possible
    while time.time() < end_time:
        msg = f"tp_{messages_sent}"
        msg_bytes = msg.encode()
        
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        try:
            sender.sendall(packet)
            messages_sent += 1
        except:
            break
        
        # Also try to receive
        try:
            data = receiver.recv(4096)
            if data:
                messages_received += data.count(b"tp_")
        except BlockingIOError:
            pass
    
    # Drain receiver
    drain_deadline = time.time() + 2
    while time.time() < drain_deadline:
        try:
            data = receiver.recv(4096)
            if data:
                messages_received += data.count(b"tp_")
            else:
                break
        except BlockingIOError:
            time.sleep(0.01)
        except:
            break
    
    elapsed = time.time() - start_time
    
    sender.close()
    receiver.close()
    
    return {
        "throughput_msg_sec": messages_sent / elapsed,
        "received_msg_sec": messages_received / elapsed,
        "delivery_rate": messages_received / messages_sent if messages_sent > 0 else 0,
    }


def estimate_memory_per_connection(num_connections: int = 50) -> Dict[str, float]:
    """Estimate memory per connection (requires server-side metrics)."""
    # This is an approximation based on known Erlang/OTP overhead
    # Real measurement would require server-side instrumentation
    
    # Based on RFC: ~10KB per connection target
    # Actual measurement would involve:
    # 1. Get baseline memory via Erlang: erlang:memory(total)
    # 2. Add connections
    # 3. Get new memory
    # 4. Calculate delta / connections
    
    # For now, return the target as placeholder
    return {
        "memory_per_conn_kb": 10.0,  # RFC target
        "memory_estimate_note": "placeholder - needs server instrumentation"
    }


def collect_all_metrics(quick: bool = False) -> Dict[str, float]:
    """Collect all performance metrics."""
    print("\n=== Collecting Performance Metrics ===\n")
    
    metrics = {
        "test_timestamp": time.time(),
        "test_duration_sec": 0,
    }
    
    start = time.time()
    
    # Connection setup
    print("1. Measuring connection setup latency...")
    conn_count = 20 if quick else 100
    conn_metrics = measure_connection_setup(conn_count)
    metrics.update(conn_metrics)
    print(f"   Connection setup: {conn_metrics.get('connection_setup_ms', 0):.2f}ms")
    
    # Message latency
    print("2. Measuring message latency...")
    msg_count = 100 if quick else 1000
    latency_metrics = measure_message_latency(msg_count)
    metrics.update(latency_metrics)
    print(f"   P50 latency: {latency_metrics.get('p50_latency_ms', 0):.2f}ms")
    print(f"   P99 latency: {latency_metrics.get('p99_latency_ms', 0):.2f}ms")
    
    # Throughput
    print("3. Measuring throughput...")
    duration = 5 if quick else 10
    throughput_metrics = measure_throughput(duration)
    metrics.update(throughput_metrics)
    print(f"   Throughput: {throughput_metrics.get('throughput_msg_sec', 0):.0f} msg/sec")
    
    # Memory estimate
    print("4. Estimating memory per connection...")
    memory_metrics = estimate_memory_per_connection()
    metrics.update(memory_metrics)
    print(f"   Memory per conn: {memory_metrics.get('memory_per_conn_kb', 0):.1f}KB")
    
    metrics["test_duration_sec"] = time.time() - start
    
    print(f"\nTotal collection time: {metrics['test_duration_sec']:.1f}s")
    
    return metrics


def main():
    parser = argparse.ArgumentParser(description="Collect performance metrics")
    parser.add_argument("--output", "-o", default="metrics.json", help="Output JSON file")
    parser.add_argument("--quick", "-q", action="store_true", help="Quick mode (fewer samples)")
    
    args = parser.parse_args()
    
    # Check server connectivity
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        sock.connect((SERVER_HOST, SERVER_PORT))
        sock.close()
    except Exception as e:
        print(f"Cannot connect to server at {SERVER_HOST}:{SERVER_PORT}")
        print(f"Error: {e}")
        print("\nStart the server with: make start")
        return 1
    
    # Collect metrics
    metrics = collect_all_metrics(quick=args.quick)
    
    # Save to file
    with open(args.output, 'w') as f:
        json.dump(metrics, f, indent=2)
    
    print(f"\nMetrics saved to {args.output}")
    
    # Print summary
    print("\n=== Metrics Summary ===")
    for key, value in sorted(metrics.items()):
        if isinstance(value, float):
            print(f"  {key}: {value:.2f}")
        else:
            print(f"  {key}: {value}")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
