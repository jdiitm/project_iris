#!/usr/bin/env python3
"""
Benchmark: Throughput

Measures system throughput under increasing load:
- Messages per second
- Latency distribution
- Connection scalability

Tier: 1 (Nightly/manual)
Safe for laptop: Yes (uses controlled load)
Expected duration: <120s
"""

import sys
import os
import time
import threading
import statistics

sys.path.insert(0, str(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))

from tests.framework import TestLogger, ClusterManager, ResourceMonitor
from tests.utilities import IrisClient


def benchmark_single_connection():
    """Benchmark throughput with single sender/receiver pair."""
    
    with TestLogger("benchmark_single_connection", "performance_light") as log:
        with ResourceMonitor(sample_interval_seconds=1.0) as monitor:
            
            NUM_MESSAGES = 1000
            
            sender = IrisClient()
            sender.login("bench_sender")
            log.connection_event("login", "bench_sender")
            
            receiver = IrisClient()
            receiver.login("bench_receiver")
            log.connection_event("login", "bench_receiver")
            
            # Receive in background
            received = []
            stop_flag = threading.Event()
            
            def recv_loop():
                receiver.sock.settimeout(0.5)
                while not stop_flag.is_set():
                    try:
                        msg = receiver.recv_msg(timeout=0.5)
                        received.append(time.monotonic())
                    except Exception:
                        pass
            
            recv_thread = threading.Thread(target=recv_loop, daemon=True)
            recv_thread.start()
            
            # Send messages
            log.info("sending", f"Sending {NUM_MESSAGES} messages")
            start = time.monotonic()
            
            for i in range(NUM_MESSAGES):
                sender.send_msg("bench_receiver", f"bench_{i}")
                log.message_sent(f"bench_{i}", "bench_receiver")
            
            send_duration = time.monotonic() - start
            send_rate = NUM_MESSAGES / send_duration
            
            log.metric("send_rate_msgs_sec", send_rate)
            log.info("send_complete", f"Sent {NUM_MESSAGES} in {send_duration:.2f}s ({send_rate:.0f} msg/s)")
            
            # Wait for receive
            time.sleep(2)
            stop_flag.set()
            recv_thread.join(timeout=2)
            
            log.metric("messages_sent", NUM_MESSAGES)
            log.metric("messages_received", len(received))
            log.metric("send_duration_s", send_duration)
            
            sender.close()
            receiver.close()
            
            # Resource summary
            summary = monitor.get_summary()
            if summary:
                log.metric("cpu_max_pct", summary.get("cpu", {}).get("max", 0))
                log.metric("memory_max_mb", summary.get("memory_rss_mb", {}).get("max", 0))
            
            log.info("result", f"Throughput: {send_rate:.0f} msgs/sec")
            return True


def benchmark_multi_connection():
    """Benchmark with multiple concurrent connections."""
    
    with TestLogger("benchmark_multi_connection", "performance_light") as log:
        with ResourceMonitor(sample_interval_seconds=1.0) as monitor:
            
            NUM_PAIRS = 10
            MSGS_PER_PAIR = 100
            
            log.info("setup", f"Testing {NUM_PAIRS} sender/receiver pairs, {MSGS_PER_PAIR} msgs each")
            
            total_sent = 0
            lock = threading.Lock()
            
            def sender_worker(pair_id: int) -> int:
                nonlocal total_sent
                try:
                    sender = IrisClient()
                    sender.login(f"multi_sender_{pair_id}")
                    
                    sent = 0
                    for i in range(MSGS_PER_PAIR):
                        sender.send_msg(f"multi_receiver_{pair_id}", f"msg_{pair_id}_{i}")
                        sent += 1
                    
                    sender.close()
                    
                    with lock:
                        total_sent += sent
                    return sent
                except Exception as e:
                    return 0
            
            # Start receivers first
            receivers = []
            for i in range(NUM_PAIRS):
                try:
                    r = IrisClient()
                    r.login(f"multi_receiver_{i}")
                    receivers.append(r)
                except Exception:
                    pass
            
            log.info("receivers_ready", f"Started {len(receivers)} receivers")
            
            # Run senders
            start = time.monotonic()
            
            threads = []
            for i in range(NUM_PAIRS):
                t = threading.Thread(target=sender_worker, args=(i,))
                t.start()
                threads.append(t)
            
            for t in threads:
                t.join()
            
            duration = time.monotonic() - start
            
            # Cleanup receivers
            for r in receivers:
                r.close()
            
            total_expected = NUM_PAIRS * MSGS_PER_PAIR
            throughput = total_sent / duration if duration > 0 else 0
            
            log.metric("connections", NUM_PAIRS * 2)
            log.metric("messages_sent", total_sent)
            log.metric("duration_s", duration)
            log.metric("throughput_msgs_sec", throughput)
            
            summary = monitor.get_summary()
            if summary:
                log.metric("cpu_max_pct", summary.get("cpu", {}).get("max", 0))
            
            log.info("result", f"Multi-connection throughput: {throughput:.0f} msgs/sec")
            return True


def benchmark_latency():
    """Benchmark end-to-end message latency."""
    
    with TestLogger("benchmark_latency", "performance_light") as log:
        
        NUM_SAMPLES = 100
        
        sender = IrisClient()
        sender.login("latency_sender")
        
        receiver = IrisClient()
        receiver.login("latency_receiver")
        
        latencies = []
        
        log.info("measuring", f"Measuring latency for {NUM_SAMPLES} round-trips")
        
        for i in range(NUM_SAMPLES):
            start = time.monotonic()
            sender.send_msg("latency_receiver", f"lat_{i}")
            
            try:
                receiver.sock.settimeout(5.0)
                msg = receiver.recv_msg(timeout=5.0)
                latency_ms = (time.monotonic() - start) * 1000
                latencies.append(latency_ms)
                log.message_received(f"lat_{i}", latency_ms)
            except Exception as e:
                log.error("timeout", f"Message {i} timed out")
        
        sender.close()
        receiver.close()
        
        if latencies:
            latencies.sort()
            p50 = latencies[len(latencies) // 2]
            p90 = latencies[int(len(latencies) * 0.9)]
            p99 = latencies[int(len(latencies) * 0.99)]
            
            log.metric("latency_p50_ms", p50)
            log.metric("latency_p90_ms", p90)
            log.metric("latency_p99_ms", p99)
            log.metric("latency_min_ms", min(latencies))
            log.metric("latency_max_ms", max(latencies))
            log.metric("latency_avg_ms", statistics.mean(latencies))
            
            log.info("result", f"Latency P50:{p50:.2f}ms P90:{p90:.2f}ms P99:{p99:.2f}ms")
        
        return True


def main():
    """Run all throughput benchmarks."""
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        print("[SETUP] Starting cluster...")
        if not cluster.start():
            print("[ERROR] Failed to start cluster")
            return 1
    
    benchmarks = [
        benchmark_single_connection,
        benchmark_multi_connection,
        benchmark_latency
    ]
    
    for bench_fn in benchmarks:
        try:
            print(f"\n{'='*60}")
            print(f"Running: {bench_fn.__name__}")
            print('='*60)
            bench_fn()
        except Exception as e:
            print(f"ERROR: {bench_fn.__name__} - {e}")
            import traceback
            traceback.print_exc()
    
    print("\nBenchmarks complete.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
