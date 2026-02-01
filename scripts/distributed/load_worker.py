#!/usr/bin/env python3
"""
Distributed load worker - generates connections and messages to Iris server.
Works on both high-capacity machines and t2.micro instances.

Usage:
    EDGE_HOST=100.95.21.52 python3 load_worker.py --connections 10000 --duration 300
    
Environment Variables:
    EDGE_HOST: Iris server Tailscale IP (default: 100.95.21.52)
    EDGE_PORT: Iris server port (default: 8085)
    WORKER_ID: Identifier for this worker (default: hostname)
"""
import os
import sys
import socket
import struct
import time
import json
import argparse
from datetime import datetime
from typing import List, Optional, Dict, Tuple
from dataclasses import dataclass, field

# Configuration from environment
EDGE_HOST = os.environ.get("EDGE_HOST", "100.95.21.52")
EDGE_PORT = int(os.environ.get("EDGE_PORT", "8085"))
WORKER_ID = os.environ.get("WORKER_ID", socket.gethostname())


@dataclass
class Metrics:
    """Test metrics container."""
    worker_id: str = WORKER_ID
    start_time: str = ""
    end_time: str = ""
    target_host: str = ""
    target_port: int = 0
    
    # Connection metrics
    connections_target: int = 0
    connections_achieved: int = 0
    connections_failed: int = 0
    
    # Message metrics
    messages_sent: int = 0
    messages_failed: int = 0
    
    # Latency samples (capped to prevent memory issues)
    latencies_ms: List[float] = field(default_factory=list)
    
    # Errors
    errors: List[str] = field(default_factory=list)
    
    def add_latency(self, latency_ms: float):
        """Add latency sample, capped at 100K samples."""
        if len(self.latencies_ms) < 100000:
            self.latencies_ms.append(latency_ms)
    
    def add_error(self, error: str):
        """Add error, capped at 100 errors."""
        if len(self.errors) < 100:
            self.errors.append(error)
    
    def summary(self) -> Dict:
        """Generate summary statistics."""
        latencies = sorted(self.latencies_ms) if self.latencies_ms else [0]
        n = len(latencies)
        
        return {
            "worker_id": self.worker_id,
            "target": f"{self.target_host}:{self.target_port}",
            "start_time": self.start_time,
            "end_time": self.end_time,
            "duration_s": self._duration_seconds(),
            "connections": {
                "target": self.connections_target,
                "achieved": self.connections_achieved,
                "failed": self.connections_failed,
                "success_rate": round(self.connections_achieved / max(1, self.connections_target), 4),
            },
            "messages": {
                "sent": self.messages_sent,
                "failed": self.messages_failed,
                "total": self.messages_sent + self.messages_failed,
                "success_rate": round(self.messages_sent / max(1, self.messages_sent + self.messages_failed), 4),
                "throughput": round(self.messages_sent / max(1, self._duration_seconds()), 2),
            },
            "latency_ms": {
                "samples": n,
                "min": round(min(latencies), 3) if latencies else 0,
                "p50": round(latencies[n // 2], 3) if n else 0,
                "p95": round(latencies[int(n * 0.95)], 3) if n else 0,
                "p99": round(latencies[int(n * 0.99)], 3) if n else 0,
                "max": round(max(latencies), 3) if latencies else 0,
            },
            "errors": self.errors[:10],  # First 10 errors
        }
    
    def _duration_seconds(self) -> float:
        """Calculate test duration in seconds."""
        if not self.start_time or not self.end_time:
            return 0
        try:
            start = datetime.fromisoformat(self.start_time)
            end = datetime.fromisoformat(self.end_time)
            return (end - start).total_seconds()
        except:
            return 0


def connect_and_login(user_id: str, host: str, port: int, timeout: float = 10) -> Optional[socket.socket]:
    """
    Connect to Iris server and login.
    Returns socket on success, None on failure.
    """
    sock = None
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect((host, port))
        
        # Login packet: 0x01 + user_id
        packet = bytes([0x01]) + user_id.encode()
        sock.sendall(packet)
        
        # Wait for response
        response = sock.recv(1024)
        
        # Accept any non-empty response as success
        if b"LOGIN_OK" in response or len(response) > 0:
            sock.setblocking(False)
            return sock
        
        sock.close()
        return None
        
    except Exception as e:
        if sock:
            try:
                sock.close()
            except:
                pass
        return None


def send_message(sock: socket.socket, target: str, content: str) -> Tuple[bool, float]:
    """
    Send message to target user.
    Returns (success, latency_ms).
    """
    start = time.time()
    try:
        sock.setblocking(True)
        sock.settimeout(5)
        
        target_bytes = target.encode()
        content_bytes = content.encode()
        
        # Message packet: 0x02 + len(target) + target + len(content) + content
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(content_bytes)) + content_bytes
        )
        
        sock.sendall(packet)
        latency = (time.time() - start) * 1000
        return True, latency
        
    except Exception:
        return False, -1


def run_load_test(
    host: str,
    port: int,
    num_connections: int,
    msg_rate: int,
    duration: int,
    ramp_time: int = 60,
) -> Metrics:
    """
    Run load test.
    
    Args:
        host: Iris server host
        port: Iris server port
        num_connections: Target number of connections
        msg_rate: Messages per second (total across all connections)
        duration: Test duration in seconds
        ramp_time: Time to ramp up connections in seconds
    
    Returns:
        Metrics object with test results
    """
    metrics = Metrics(
        start_time=datetime.now().isoformat(),
        target_host=host,
        target_port=port,
        connections_target=num_connections,
    )
    
    connections: List[Tuple[str, socket.socket]] = []
    
    print(f"[{WORKER_ID}] Starting load test", flush=True)
    print(f"  Target: {host}:{port}", flush=True)
    print(f"  Connections: {num_connections}", flush=True)
    print(f"  Message rate: {msg_rate}/s", flush=True)
    print(f"  Duration: {duration}s", flush=True)
    print(f"", flush=True)
    
    # Phase 1: Establish connections
    print(f"[{WORKER_ID}] Phase 1: Establishing connections...", flush=True)
    
    connect_interval = ramp_time / max(1, num_connections)
    
    for i in range(num_connections):
        user_id = f"{WORKER_ID}_u{i:06d}"
        sock = connect_and_login(user_id, host, port)
        
        if sock:
            connections.append((user_id, sock))
            metrics.connections_achieved += 1
        else:
            metrics.connections_failed += 1
            metrics.add_error(f"Connection {i} failed")
        
        # Progress update
        if (i + 1) % 1000 == 0:
            rate = metrics.connections_achieved / (i + 1) * 100
            print(f"  Progress: {metrics.connections_achieved}/{i + 1} ({rate:.1f}%)", flush=True)
        
        # Ramp delay
        if connect_interval > 0.001:
            time.sleep(connect_interval)
        else:
            time.sleep(0.001)  # Minimum delay to prevent thundering herd
    
    print(f"  Final: {metrics.connections_achieved}/{num_connections} connected", flush=True)
    
    if not connections:
        print(f"[{WORKER_ID}] No connections established, aborting", flush=True)
        metrics.end_time = datetime.now().isoformat()
        return metrics
    
    # Phase 2: Send messages
    print(f"", flush=True)
    print(f"[{WORKER_ID}] Phase 2: Sending messages for {duration}s...", flush=True)
    
    start_time = time.time()
    msg_interval = 1.0 / msg_rate if msg_rate > 0 else 0.1
    msg_count = 0
    last_report = start_time
    
    while time.time() - start_time < duration:
        # Round-robin through connections
        sender_id, sender_sock = connections[msg_count % len(connections)]
        target_id, _ = connections[(msg_count + 1) % len(connections)]
        
        success, latency = send_message(sender_sock, target_id, f"msg_{msg_count}")
        
        if success:
            metrics.messages_sent += 1
            metrics.add_latency(latency)
        else:
            metrics.messages_failed += 1
        
        msg_count += 1
        
        # Progress update every 10 seconds
        now = time.time()
        if now - last_report >= 10:
            elapsed = now - start_time
            rate = msg_count / elapsed if elapsed > 0 else 0
            print(f"  {elapsed:.0f}s: {msg_count} messages ({rate:.0f}/s)", flush=True)
            last_report = now
        
        # Rate limiting
        time.sleep(msg_interval)
    
    # Phase 3: Cleanup
    print(f"", flush=True)
    print(f"[{WORKER_ID}] Phase 3: Cleanup...", flush=True)
    
    for user_id, sock in connections:
        try:
            sock.close()
        except:
            pass
    
    metrics.end_time = datetime.now().isoformat()
    
    return metrics


def main():
    parser = argparse.ArgumentParser(
        description="Iris distributed load worker",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("--host", default=EDGE_HOST, help="Iris server host")
    parser.add_argument("--port", type=int, default=EDGE_PORT, help="Iris server port")
    parser.add_argument("--connections", type=int, default=10000, help="Number of connections")
    parser.add_argument("--rate", type=int, default=100, help="Messages per second")
    parser.add_argument("--duration", type=int, default=300, help="Test duration in seconds")
    parser.add_argument("--ramp", type=int, default=60, help="Connection ramp-up time in seconds")
    args = parser.parse_args()
    
    # Run test
    metrics = run_load_test(
        host=args.host,
        port=args.port,
        num_connections=args.connections,
        msg_rate=args.rate,
        duration=args.duration,
        ramp_time=args.ramp,
    )
    
    # Print summary
    summary = metrics.summary()
    
    print(f"", flush=True)
    print(f"{'=' * 60}", flush=True)
    print(f"[{WORKER_ID}] RESULTS", flush=True)
    print(f"{'=' * 60}", flush=True)
    print(json.dumps(summary, indent=2), flush=True)
    
    # Save to file
    filename = f"results_{WORKER_ID}.json"
    with open(filename, "w") as f:
        json.dump(summary, f, indent=2)
    print(f"", flush=True)
    print(f"Saved to {filename}", flush=True)
    
    # Exit code based on success rate
    if summary["connections"]["success_rate"] < 0.5:
        sys.exit(1)
    if summary["messages"]["success_rate"] < 0.9:
        sys.exit(1)
    
    sys.exit(0)


if __name__ == "__main__":
    main()
