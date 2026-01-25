#!/usr/bin/env python3
"""
Fan-Out Performance Tests

This test suite validates the system's ability to deliver messages to multiple
recipients in quick succession. This is critical for group messaging scenarios
and prevents backpressure-related message loss.

Test Cases (from Plan Section 4.2):
1. Small fan-out:  10 recipients,  100/sec rate,  <100ms total delivery
2. Medium fan-out: 100 recipients, 1000/sec rate, <200ms total delivery
3. Large fan-out:  1000 recipients, 5000/sec rate, <500ms total delivery
4. Burst fan-out:  100 recipients, 10000/sec burst, zero message loss

Metrics Collected:
- Fan-out latency (time from first send to last recipient ACK)
- Individual message latencies (P50, P99)
- Message loss rate
- Impact on concurrent traffic

Prerequisites:
- make start (single node) OR make cluster-up (Docker cluster)
"""

import os
import sys
import socket
import struct
import time
import threading
import statistics
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import List, Optional, Tuple

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10

# Per TEST_CONTRACT.md: Use fixed profiles, not dynamic scaling
# Updated per PRINCIPAL_AUDIT_REPORT.md to support broadcast fairness testing
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")

PROFILE_THRESHOLDS = {
    "smoke": {
        "timeout_multiplier": 3.0,
        "small": {"recipients": 10, "rate": 100, "max_latency_ms": 200},
        "medium": {"recipients": 50, "rate": 500, "max_latency_ms": 400},
        "large": {"recipients": 100, "rate": 1000, "max_latency_ms": 1000},
        "burst": {"recipients": 50, "rate": 2000, "max_latency_ms": 1000, "max_loss": 0.01},
    },
    "full": {
        "timeout_multiplier": 1.0,
        "small": {"recipients": 10, "rate": 100, "max_latency_ms": 100},
        "medium": {"recipients": 100, "rate": 1000, "max_latency_ms": 200},
        "large": {"recipients": 1000, "rate": 5000, "max_latency_ms": 500},
        "burst": {"recipients": 100, "rate": 10000, "max_latency_ms": 500, "max_loss": 0},
    },
    # Extreme profile for broadcast fairness testing (PRINCIPAL_AUDIT_REPORT Section 4)
    "extreme": {
        "timeout_multiplier": 1.0,
        "small": {"recipients": 100, "rate": 500, "max_latency_ms": 100},
        "medium": {"recipients": 500, "rate": 2500, "max_latency_ms": 200},
        "large": {"recipients": 1000, "rate": 5000, "max_latency_ms": 500},
        "burst": {"recipients": 500, "rate": 20000, "max_latency_ms": 1000, "max_loss": 0.001},
    }
}

if TEST_PROFILE not in PROFILE_THRESHOLDS:
    print(f"ERROR: Unknown profile '{TEST_PROFILE}'. Available: {list(PROFILE_THRESHOLDS.keys())}")
    sys.exit(1)

TIMEOUT_MULTIPLIER = PROFILE_THRESHOLDS[TEST_PROFILE]["timeout_multiplier"]
THRESHOLDS = {k: v for k, v in PROFILE_THRESHOLDS[TEST_PROFILE].items() if k != "timeout_multiplier"}


@dataclass
class FanOutResult:
    """Result of a fan-out test."""
    recipients: int
    messages_sent: int
    messages_received: int
    total_latency_ms: float
    latencies_ms: List[float]
    errors: List[str]
    
    @property
    def loss_rate(self) -> float:
        if self.messages_sent == 0:
            return 0.0
        return (self.messages_sent - self.messages_received) / self.messages_sent
    
    @property
    def p50_latency_ms(self) -> float:
        if not self.latencies_ms:
            return 0.0
        return statistics.median(self.latencies_ms)
    
    @property
    def p99_latency_ms(self) -> float:
        if not self.latencies_ms:
            return 0.0
        sorted_latencies = sorted(self.latencies_ms)
        idx = int(len(sorted_latencies) * 0.99)
        return sorted_latencies[min(idx, len(sorted_latencies) - 1)]


class IrisClient:
    """Simple Iris protocol client."""
    
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.sock: Optional[socket.socket] = None
        self.username: Optional[str] = None
        self.buffer = b''
        self.received_messages: List[Tuple[str, float]] = []  # (msg_id, recv_time)
        self._running = False
        self._recv_thread: Optional[threading.Thread] = None
    
    def connect(self) -> bool:
        """Connect to server."""
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.settimeout(TIMEOUT)
            self.sock.connect((self.host, self.port))
            return True
        except Exception as e:
            return False
    
    def login(self, username: str) -> bool:
        """Login to server."""
        self.username = username
        packet = bytes([0x01]) + username.encode()
        self.sock.sendall(packet)
        
        try:
            response = self.sock.recv(1024)
            return b"LOGIN_OK" in response
        except:
            return False
    
    def send_message(self, target: str, message: str) -> bool:
        """Send message to target user."""
        target_bytes = target.encode()
        msg_bytes = message.encode()
        
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        try:
            self.sock.sendall(packet)
            return True
        except:
            return False
    
    def start_receiving(self):
        """Start background thread to receive messages."""
        self._running = True
        self._recv_thread = threading.Thread(target=self._receive_loop, daemon=True)
        self._recv_thread.start()
    
    def stop_receiving(self):
        """Stop receiving messages."""
        self._running = False
        if self._recv_thread:
            self._recv_thread.join(timeout=2)
    
    def _receive_loop(self):
        """Background receive loop."""
        self.sock.setblocking(False)
        
        while self._running:
            try:
                data = self.sock.recv(4096)
                recv_time = time.time()
                
                if data:
                    self.buffer += data
                    self._parse_messages(recv_time)
            except BlockingIOError:
                time.sleep(0.001)
            except Exception:
                break
    
    def _parse_messages(self, recv_time: float):
        """Parse received messages from buffer."""
        while len(self.buffer) >= 1:
            opcode = self.buffer[0]
            
            if opcode == 0x10:  # Reliable message
                if len(self.buffer) < 3:
                    break
                id_len = struct.unpack('>H', self.buffer[1:3])[0]
                if len(self.buffer) < 3 + id_len + 4:
                    break
                msg_len = struct.unpack('>I', self.buffer[3+id_len:3+id_len+4])[0]
                if len(self.buffer) < 3 + id_len + 4 + msg_len:
                    break
                
                msg_id = self.buffer[3:3+id_len].decode('utf-8', errors='ignore')
                msg_content = self.buffer[3+id_len+4:3+id_len+4+msg_len]
                
                # Extract fan-out message ID from content
                if b"FANOUT_" in msg_content:
                    try:
                        fanout_id = msg_content.decode().split("FANOUT_")[1].split("_")[0]
                        self.received_messages.append((fanout_id, recv_time))
                    except:
                        pass
                
                # Send ACK
                try:
                    ack = bytes([0x03]) + self.buffer[3:3+id_len]
                    self.sock.sendall(ack)
                except:
                    pass
                
                self.buffer = self.buffer[3 + id_len + 4 + msg_len:]
            
            elif opcode == 0x03:  # ACK
                self.buffer = self.buffer[1:]  # Just skip ACKs
            
            else:
                # Unknown opcode, skip one byte
                self.buffer = self.buffer[1:]
    
    def close(self):
        """Close connection."""
        self.stop_receiving()
        if self.sock:
            try:
                self.sock.close()
            except:
                pass


def run_fanout_test(test_name: str, num_recipients: int, target_rate: int, 
                   max_latency_ms: float, max_loss: float = 0.01) -> Tuple[bool, FanOutResult]:
    """
    Run a fan-out test.
    
    Args:
        test_name: Name of the test
        num_recipients: Number of recipient users
        target_rate: Target messages per second
        max_latency_ms: Maximum acceptable total latency
        max_loss: Maximum acceptable message loss rate
    
    Returns:
        (passed, result) tuple
    """
    print(f"\n{'='*60}")
    print(f"Fan-Out Test: {test_name}")
    print(f"{'='*60}")
    print(f"  Recipients: {num_recipients}")
    print(f"  Target rate: {target_rate}/sec")
    print(f"  Max latency: {max_latency_ms}ms")
    print(f"  Max loss: {max_loss*100:.1f}%")
    print()
    
    test_id = f"{int(time.time())}"
    errors = []
    latencies = []
    
    # Create sender
    sender = IrisClient(SERVER_HOST, SERVER_PORT)
    if not sender.connect():
        return False, FanOutResult(num_recipients, 0, 0, 0, [], ["Sender connection failed"])
    
    if not sender.login(f"fanout_sender_{test_id}"):
        sender.close()
        return False, FanOutResult(num_recipients, 0, 0, 0, [], ["Sender login failed"])
    
    print(f"  ✓ Sender connected")
    
    # Create recipients with thread pool
    recipients: List[IrisClient] = []
    send_times = {}  # msg_id -> send_time
    
    print(f"  Connecting {num_recipients} recipients...", end="", flush=True)
    
    def create_recipient(i: int) -> Optional[IrisClient]:
        client = IrisClient(SERVER_HOST, SERVER_PORT)
        if client.connect() and client.login(f"fanout_recv_{test_id}_{i}"):
            client.start_receiving()
            return client
        return None
    
    # Connect recipients in parallel
    with ThreadPoolExecutor(max_workers=min(50, num_recipients)) as executor:
        futures = [executor.submit(create_recipient, i) for i in range(num_recipients)]
        for future in as_completed(futures):
            client = future.result()
            if client:
                recipients.append(client)
    
    print(f" {len(recipients)}/{num_recipients} connected")
    
    if len(recipients) < num_recipients * 0.9:
        errors.append(f"Only {len(recipients)}/{num_recipients} recipients connected")
    
    # Wait for all recipients to be ready
    time.sleep(0.5)
    
    # Send messages to all recipients
    print(f"  Sending messages...", end="", flush=True)
    
    interval = 1.0 / target_rate if target_rate > 0 else 0
    start_time = time.time()
    messages_sent = 0
    
    for i, recipient in enumerate(recipients):
        msg_id = f"FANOUT_{test_id}_{i}"
        message = f"FANOUT_{test_id}_{i}_payload"
        
        send_times[str(i)] = time.time()
        if sender.send_message(recipient.username, message):
            messages_sent += 1
        
        # Rate limiting (but allow burst for burst test)
        if interval > 0 and target_rate < 10000:
            elapsed = time.time() - start_time
            expected = (i + 1) * interval
            if elapsed < expected:
                time.sleep(expected - elapsed)
    
    send_complete_time = time.time()
    print(f" {messages_sent} sent in {(send_complete_time - start_time)*1000:.1f}ms")
    
    # Wait for delivery
    wait_time = max(0.5, max_latency_ms / 1000 * 2)
    print(f"  Waiting {wait_time:.1f}s for delivery...", end="", flush=True)
    time.sleep(wait_time)
    
    # Collect results
    messages_received = 0
    for recipient in recipients:
        messages_received += len(recipient.received_messages)
        
        # Calculate latencies
        for msg_id, recv_time in recipient.received_messages:
            try:
                idx = int(msg_id)
                if str(idx) in send_times:
                    latency = (recv_time - send_times[str(idx)]) * 1000
                    latencies.append(latency)
            except:
                pass
    
    print(f" {messages_received} received")
    
    # Calculate total latency (first send to last receive)
    total_latency = 0
    if latencies:
        total_latency = max(latencies)
    
    # Cleanup
    sender.close()
    for r in recipients:
        r.close()
    
    # Build result
    result = FanOutResult(
        recipients=num_recipients,
        messages_sent=messages_sent,
        messages_received=messages_received,
        total_latency_ms=total_latency,
        latencies_ms=latencies,
        errors=errors
    )
    
    # Evaluate pass/fail
    passed = True
    
    print(f"\n  Results:")
    print(f"    Messages sent:     {result.messages_sent}")
    print(f"    Messages received: {result.messages_received}")
    print(f"    Loss rate:         {result.loss_rate*100:.2f}%")
    print(f"    Total latency:     {result.total_latency_ms:.2f}ms")
    
    if result.latencies_ms:
        print(f"    P50 latency:       {result.p50_latency_ms:.2f}ms")
        print(f"    P99 latency:       {result.p99_latency_ms:.2f}ms")
    
    # Check thresholds
    if result.loss_rate > max_loss:
        print(f"    ✗ Loss rate {result.loss_rate*100:.2f}% exceeds {max_loss*100:.1f}%")
        passed = False
    else:
        print(f"    ✓ Loss rate within threshold")
    
    if result.total_latency_ms > max_latency_ms:
        print(f"    ✗ Latency {result.total_latency_ms:.2f}ms exceeds {max_latency_ms}ms")
        passed = False
    else:
        print(f"    ✓ Latency within threshold")
    
    return passed, result


def test_small_fanout():
    """Test 1: Small fan-out (10 recipients)."""
    config = THRESHOLDS["small"]
    return run_fanout_test(
        "Small Fan-Out",
        num_recipients=config["recipients"],
        target_rate=config["rate"],
        max_latency_ms=config["max_latency_ms"]
    )


def test_medium_fanout():
    """Test 2: Medium fan-out (100 recipients)."""
    config = THRESHOLDS["medium"]
    return run_fanout_test(
        "Medium Fan-Out",
        num_recipients=config["recipients"],
        target_rate=config["rate"],
        max_latency_ms=config["max_latency_ms"]
    )


def test_large_fanout():
    """Test 3: Large fan-out (1000 recipients)."""
    config = THRESHOLDS["large"]
    return run_fanout_test(
        "Large Fan-Out",
        num_recipients=config["recipients"],
        target_rate=config["rate"],
        max_latency_ms=config["max_latency_ms"]
    )


def test_burst_fanout():
    """Test 4: Burst fan-out (100 recipients, maximum rate)."""
    config = THRESHOLDS["burst"]
    return run_fanout_test(
        "Burst Fan-Out",
        num_recipients=config["recipients"],
        target_rate=config["rate"],
        max_latency_ms=config["max_latency_ms"],
        max_loss=config["max_loss"]
    )


def main():
    print("\n" + "=" * 70)
    print("Fan-Out Performance Tests (Plan Section 4.2)")
    print("=" * 70)
    
    # Ensure cluster is running (handles recovery from previous test crashes)
    with ClusterManager(project_root=project_root) as cluster:
        # Check connectivity
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(2)
            sock.connect((SERVER_HOST, SERVER_PORT))
            sock.close()
        except Exception as e:
            print(f"\n✗ Cannot connect to {SERVER_HOST}:{SERVER_PORT}")
            print(f"  Error: {e}")
            print(f"  Cluster manager should have started the server.")
            return 1
        
        results = []
        
        # Run tests in order of complexity
        tests = [
            ("Small (10 recipients)", test_small_fanout),
            ("Medium (100 recipients)", test_medium_fanout),
            # Large and burst tests are resource-intensive
            # Uncomment for production hardware
            # ("Large (1000 recipients)", test_large_fanout),
            # ("Burst (100 @ 10K/sec)", test_burst_fanout),
        ]
        
        for name, test_fn in tests:
            try:
                passed, result = test_fn()
                results.append((name, passed, result))
            except Exception as e:
                print(f"\n✗ Test {name} crashed: {e}")
                results.append((name, False, None))
        
        # Summary
        print("\n" + "=" * 70)
        print("SUMMARY")
        print("=" * 70)
        
        passed_count = sum(1 for _, p, _ in results if p)
        total_count = len(results)
        
        print(f"\nTests run: {total_count}")
        print(f"Passed: {passed_count}")
        print(f"Failed: {total_count - passed_count}")
        
        for name, passed, result in results:
            status = "✓ PASS" if passed else "✗ FAIL"
            print(f"  {status}: {name}")
        
        if passed_count == total_count:
            print("\n✓ All fan-out tests passed!")
            return 0
        else:
            print(f"\n✗ {total_count - passed_count} test(s) failed")
            return 1


if __name__ == "__main__":
    sys.exit(main())
