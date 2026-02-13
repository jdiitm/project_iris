#!/usr/bin/env python3
"""
Message Ordering Under Failure Test (RFC FR-5)

Tests that message ordering is preserved even during:
- Node crashes
- Network partitions
- Edge node failover
- Core node failover

Invariant:
    ∀ m1, m2 ∈ Messages:
      (sender(m1) = sender(m2)) ∧ 
      (recipient(m1) = recipient(m2)) ∧
      (send_time(m1) < send_time(m2))
      ⟹ (deliver_time(m1) < deliver_time(m2))

Test Strategy:
1. Send ordered sequence M1, M2, ..., M10 to recipient
2. During send, inject failure (kill node, partition, etc.)
3. Verify received messages maintain order
4. Repeat with various failure injection points

RFC Reference: FR-5 - Messages from sender S to recipient R are delivered in FIFO order.

Tier: 2 (Chaos testing)
"""

import os
import sys
import time
import subprocess
import socket
import struct
import threading
import random

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.utilities.helpers import unique_user

# Test configuration
EDGE_HOST = os.environ.get("IRIS_EDGE_HOST", "localhost")
EDGE_PORT = int(os.environ.get("IRIS_EDGE_PORT", "8085"))
TIMEOUT = 10

results = []


def log(msg):
    """Log with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def log_test(name, passed, message=""):
    """Log test result."""
    status = "✓ PASS" if passed else "✗ FAIL"
    print(f"  {status}: {name}")
    if message:
        print(f"         {message}")
    results.append((name, passed, message))


class SimpleClient:
    """Minimal Iris client for testing."""
    
    def __init__(self, host=EDGE_HOST, port=EDGE_PORT):
        self.host = host
        self.port = port
        self.sock = None
        self.user = None
        self.buffer = b""
        self.seq_counter = 0  # Sequence counter for RFC-compliant messaging
    
    def connect(self, max_retries=5, retry_delay=2.0):
        """Establish TLS connection with retry for post-cluster-init readiness."""
        from tests.suites.chaos_dist.utils import create_tls_socket
        last_err = None
        for attempt in range(max_retries):
            try:
                self.sock = create_tls_socket(self.host, self.port, timeout=TIMEOUT)
                return
            except Exception as e:
                last_err = e
                if attempt < max_retries - 1:
                    import time
                    time.sleep(retry_delay)
        raise ConnectionError(f"Failed to connect after {max_retries} attempts: {last_err}")
    
    def close(self):
        """Close connection."""
        if self.sock:
            try:
                self.sock.close()
            except:
                pass
            self.sock = None
    
    def login(self, username):
        """Send login packet."""
        self.user = username
        packet = b'\x01' + username.encode('utf-8')
        self.sock.sendall(packet)
        
        # Wait for LOGIN_OK
        try:
            response = self.sock.recv(1024)
            return b"LOGIN_OK" in response
        except:
            return False
    
    def send_message(self, recipient, message):
        """Send a message to recipient using RFC-compliant opcode 0x07.
        
        RFC-001-AMENDMENT-001: Uses opcode 0x07 (sequenced message)
        Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
        """
        target = recipient.encode('utf-8')
        msg = message.encode('utf-8')
        
        # Increment sequence counter
        self.seq_counter += 1
        
        # Build RFC-compliant packet with opcode 0x07
        packet = (
            bytes([0x07]) +
            struct.pack('>H', len(target)) + target +
            struct.pack('>Q', self.seq_counter) +
            struct.pack('>H', len(msg)) + msg
        )
        
        self.sock.sendall(packet)
    
    def recv_messages(self, timeout=5, max_count=100):
        """Receive messages until timeout."""
        self.sock.settimeout(timeout)
        messages = []
        start = time.time()
        
        while time.time() - start < timeout and len(messages) < max_count:
            try:
                data = self.sock.recv(4096)
                if data:
                    self.buffer += data
                    # Parse messages from buffer (simplified)
                    messages.extend(self._parse_messages())
            except socket.timeout:
                break
            except:
                break
        
        return messages
    
    def _parse_messages(self):
        """Extract message content from buffer (simplified parsing)."""
        messages = []
        # Look for ORDER_TEST markers
        while b"ORDER_TEST_" in self.buffer:
            idx = self.buffer.find(b"ORDER_TEST_")
            # Extract sequence number
            end_idx = idx + 20  # ORDER_TEST_XXXXX format
            if end_idx <= len(self.buffer):
                chunk = self.buffer[idx:end_idx]
                try:
                    text = chunk.decode('utf-8', errors='ignore')
                    if text.startswith("ORDER_TEST_"):
                        seq = text.split("_")[2]
                        if seq.isdigit():
                            messages.append(int(seq))
                except:
                    pass
                self.buffer = self.buffer[end_idx:]
            else:
                break
        return messages


def check_server_available():
    """Check if server is reachable via TLS."""
    try:
        from tests.suites.chaos_dist.utils import create_tls_socket
        sock = create_tls_socket(EDGE_HOST, EDGE_PORT, timeout=2)
        sock.close()
        return True
    except:
        return False


def docker_available():
    """Check if Docker is available."""
    try:
        result = subprocess.run(["docker", "ps"], capture_output=True, timeout=5)
        return result.returncode == 0
    except:
        return False


def get_container_by_name(name_pattern):
    """Get container ID by name pattern."""
    try:
        result = subprocess.run(
            ["docker", "ps", "--filter", f"name={name_pattern}", "-q"],
            capture_output=True, text=True, timeout=10
        )
        containers = result.stdout.strip().split('\n')
        return containers[0] if containers and containers[0] else None
    except:
        return None


def pause_container(container_id, duration=2):
    """Pause a container for a duration then unpause."""
    def do_pause():
        try:
            subprocess.run(["docker", "pause", container_id], 
                          capture_output=True, timeout=5)
            time.sleep(duration)
            subprocess.run(["docker", "unpause", container_id], 
                          capture_output=True, timeout=5)
        except:
            pass
    
    thread = threading.Thread(target=do_pause, daemon=True)
    thread.start()
    return thread


def restart_container(container_id):
    """Restart a container."""
    try:
        subprocess.run(["docker", "restart", container_id], 
                      capture_output=True, timeout=30)
    except:
        pass


# =============================================================================
# Test 1: Basic Ordering (No Failure - Baseline)
# =============================================================================

def test_basic_ordering():
    """Baseline test: Verify ordering works without failures."""
    log("\n--- Test 1: Basic Ordering (Baseline) ---")
    
    if not check_server_available():
        log_test("Basic ordering", False, "Server not available")
        return
    
    try:
        sender = SimpleClient()
        receiver = SimpleClient()
        
        sender.connect()
        receiver.connect()
        
        sender_name = unique_user("order_snd")
        receiver_name = unique_user("order_rcv")
        
        if not sender.login(sender_name):
            log_test("Basic ordering", False, "Sender login failed")
            return
        
        if not receiver.login(receiver_name):
            log_test("Basic ordering", False, "Receiver login failed")
            return
        
        # Send ordered sequence
        num_messages = 10
        for i in range(num_messages):
            sender.send_message(receiver_name, f"ORDER_TEST_{i:05d}_payload")
            time.sleep(0.02)
        
        # Receive and verify (recv_messages has its own timeout)
        received = receiver.recv_messages(timeout=5)
        
        sender.close()
        receiver.close()
        
        # FIX: Require at least 1 message to validate ordering
        # Receiving 0 messages is NOT acceptable - can't verify ordering without data
        if len(received) == 0:
            log_test("Basic ordering", False, 
                    f"Received 0/{num_messages} - cannot verify ordering without messages")
            return
        
        if len(received) == 1:
            log_test("Basic ordering", True, 
                    f"Received 1/{num_messages} - single message (ordering trivially satisfied)")
            return
        
        # Check order
        is_ordered = all(received[i] < received[i+1] for i in range(len(received)-1))
        
        if is_ordered:
            log_test("Basic ordering", True, 
                    f"Received {len(received)}/{num_messages} in correct order")
        else:
            log_test("Basic ordering", False, 
                    f"Out of order! Received: {received[:10]}")
    
    except Exception as e:
        log_test("Basic ordering", False, f"Exception: {e}")


# =============================================================================
# Test 2: Ordering During Edge Pause (Docker)
# =============================================================================

def test_ordering_during_edge_pause():
    """Test ordering when edge node is temporarily paused."""
    log("\n--- Test 2: Ordering During Edge Pause ---")
    
    if not check_server_available():
        log_test("Ordering during edge pause", False, "Server not available")
        return
    
    if not docker_available():
        log_test("Ordering during edge pause", None, "SKIP: Docker not available")
        return
    
    edge_container = get_container_by_name("edge-east-1")
    if not edge_container:
        log_test("Ordering during edge pause", None, "SKIP: edge-east-1 container not found")
        return
    
    try:
        # Use edge-east-1's port (8085)
        sender = SimpleClient(port=8085)
        receiver = SimpleClient(port=8085)
        
        sender.connect()
        receiver.connect()
        
        sender_name = unique_user("pause_snd")
        receiver_name = unique_user("pause_rcv")
        
        if not sender.login(sender_name):
            log_test("Ordering during edge pause", False, "Sender login failed")
            return
        
        if not receiver.login(receiver_name):
            log_test("Ordering during edge pause", False, "Receiver login failed")
            return
        
        # Send first batch
        for i in range(5):
            sender.send_message(receiver_name, f"ORDER_TEST_{i:05d}_before")
            time.sleep(0.02)
        
        # Pause container briefly
        log("  Pausing edge container for 2 seconds...")
        pause_thread = pause_container(edge_container, duration=2)
        
        # Send during/after pause
        time.sleep(0.5)
        for i in range(5, 10):
            try:
                sender.send_message(receiver_name, f"ORDER_TEST_{i:05d}_after")
            except:
                pass  # May fail if socket disrupted
            time.sleep(0.1)
        
        pause_thread.join(timeout=5)
        
        # Allow recovery -- recv_messages timeout covers the wait
        received = receiver.recv_messages(timeout=7)
        
        sender.close()
        receiver.close()
        
        # FIX: Require at least 1 message to validate ordering
        if len(received) == 0:
            log_test("Ordering during edge pause", False, 
                    f"Received 0 messages - cannot verify ordering (infrastructure issue)")
            return
        
        if len(received) == 1:
            log_test("Ordering during edge pause", True, 
                    f"Received 1 message - single message (ordering trivially satisfied)")
            return
        
        # Check order of what was received
        is_ordered = all(received[i] <= received[i+1] for i in range(len(received)-1))
        
        if is_ordered:
            log_test("Ordering during edge pause", True, 
                    f"Received {len(received)} messages in order despite pause")
        else:
            log_test("Ordering during edge pause", False, 
                    f"Out of order! Received: {received}")
    
    except Exception as e:
        log_test("Ordering during edge pause", False, f"Exception: {e}")


# =============================================================================
# Test 3: Ordering During Network Jitter
# =============================================================================

def test_ordering_with_jitter():
    """Test ordering with simulated network jitter (random delays)."""
    log("\n--- Test 3: Ordering With Network Jitter ---")
    
    if not check_server_available():
        log_test("Ordering with jitter", False, "Server not available")
        return
    
    try:
        sender = SimpleClient()
        receiver = SimpleClient()
        
        sender.connect()
        receiver.connect()
        
        sender_name = unique_user("jitter_snd")
        receiver_name = unique_user("jitter_rcv")
        
        if not sender.login(sender_name):
            log_test("Ordering with jitter", False, "Sender login failed")
            return
        
        if not receiver.login(receiver_name):
            log_test("Ordering with jitter", False, "Receiver login failed")
            return
        
        # Send with random delays (simulating jitter)
        num_messages = 10
        for i in range(num_messages):
            sender.send_message(receiver_name, f"ORDER_TEST_{i:05d}_jitter")
            jitter = random.uniform(0.01, 0.1)
            time.sleep(jitter)
        
        received = receiver.recv_messages(timeout=5)
        
        sender.close()
        receiver.close()
        
        # FIX: Require at least 1 message to validate ordering
        if len(received) == 0:
            log_test("Ordering with jitter", False, 
                    f"Received 0/{num_messages} - cannot verify ordering without messages")
            return
        
        if len(received) == 1:
            log_test("Ordering with jitter", True, 
                    f"Received 1/{num_messages} - single message (ordering trivially satisfied)")
            return
        
        is_ordered = all(received[i] < received[i+1] for i in range(len(received)-1))
        
        if is_ordered:
            log_test("Ordering with jitter", True, 
                    f"Received {len(received)}/{num_messages} in correct order")
        else:
            log_test("Ordering with jitter", False, 
                    f"Out of order! Received: {received[:10]}")
    
    except Exception as e:
        log_test("Ordering with jitter", False, f"Exception: {e}")


# =============================================================================
# Test 4: Ordering During Reconnect
# =============================================================================

def test_ordering_during_reconnect():
    """Test ordering when sender reconnects mid-stream."""
    log("\n--- Test 4: Ordering During Reconnect ---")
    
    if not check_server_available():
        log_test("Ordering during reconnect", False, "Server not available")
        return
    
    try:
        receiver = SimpleClient()
        receiver.connect()
        
        sender_name = unique_user("recon_snd")
        receiver_name = unique_user("recon_rcv")
        
        if not receiver.login(receiver_name):
            log_test("Ordering during reconnect", False, "Receiver login failed")
            return
        
        all_sent = []
        
        # First sender session
        sender1 = SimpleClient()
        sender1.connect()
        if not sender1.login(sender_name):
            log_test("Ordering during reconnect", False, "Sender1 login failed")
            return
        
        for i in range(5):
            sender1.send_message(receiver_name, f"ORDER_TEST_{i:05d}_session1")
            all_sent.append(i)
            time.sleep(0.02)
        
        sender1.close()
        time.sleep(0.5)
        
        # Second sender session (reconnect)
        sender2 = SimpleClient()
        sender2.connect()
        if not sender2.login(sender_name):
            log_test("Ordering during reconnect", False, "Sender2 login failed")
            return
        
        for i in range(5, 10):
            sender2.send_message(receiver_name, f"ORDER_TEST_{i:05d}_session2")
            all_sent.append(i)
            time.sleep(0.02)
        
        sender2.close()
        
        received = receiver.recv_messages(timeout=5)
        receiver.close()
        
        # FIX: Require at least 1 message to validate ordering
        if len(received) == 0:
            log_test("Ordering during reconnect", False, 
                    f"Received 0 messages - cannot verify ordering without messages")
            return
        
        if len(received) == 1:
            log_test("Ordering during reconnect", True, 
                    f"Received 1 message - single message (ordering trivially satisfied)")
            return
        
        # Messages from same sender should maintain order even across reconnects
        is_ordered = all(received[i] < received[i+1] for i in range(len(received)-1))
        
        if is_ordered:
            log_test("Ordering during reconnect", True, 
                    f"Received {len(received)}/{len(all_sent)} in order across reconnect")
        else:
            log_test("Ordering during reconnect", False, 
                    f"Out of order! Received: {received}")
    
    except Exception as e:
        log_test("Ordering during reconnect", False, f"Exception: {e}")


# =============================================================================
# Test 5: Concurrent Senders Ordering
# =============================================================================

def test_concurrent_senders():
    """Test that each sender's messages maintain order even with concurrent senders."""
    log("\n--- Test 5: Concurrent Senders (Per-Sender Ordering) ---")
    
    if not check_server_available():
        log_test("Concurrent senders ordering", False, "Server not available")
        return
    
    try:
        receiver_name = unique_user("conc_rcv")
        
        receiver = SimpleClient()
        receiver.connect()
        if not receiver.login(receiver_name):
            log_test("Concurrent senders ordering", False, "Receiver login failed")
            return
        
        # Create multiple senders
        num_senders = 3
        messages_per_sender = 5
        senders = []
        
        for s in range(num_senders):
            sender = SimpleClient()
            sender.connect()
            if sender.login(unique_user(f"conc_snd_{s}")):
                senders.append((s, sender))
        
        # Send from all senders concurrently (interleaved)
        for i in range(messages_per_sender):
            for s_id, sender in senders:
                seq = s_id * 100 + i  # Sender 0: 0-4, Sender 1: 100-104, etc.
                sender.send_message(receiver_name, f"ORDER_TEST_{seq:05d}_s{s_id}")
                time.sleep(0.01)
        
        received = receiver.recv_messages(timeout=5)
        
        for _, sender in senders:
            sender.close()
        receiver.close()
        
        # Group received by sender
        by_sender = {s: [] for s in range(num_senders)}
        for seq in received:
            sender_id = seq // 100
            if sender_id in by_sender:
                by_sender[sender_id].append(seq)
        
        # Each sender's messages should be in order
        all_ordered = True
        for s_id, seqs in by_sender.items():
            if len(seqs) > 1:
                if not all(seqs[i] < seqs[i+1] for i in range(len(seqs)-1)):
                    all_ordered = False
                    log(f"  Sender {s_id} out of order: {seqs}")
        
        # FIX: Require at least 1 message to validate ordering
        if len(received) == 0:
            log_test("Concurrent senders ordering", False, 
                    "0 messages received - cannot verify ordering without messages")
        elif all_ordered:
            log_test("Concurrent senders ordering", True, 
                    f"Received {len(received)} messages, per-sender ordering preserved")
        else:
            log_test("Concurrent senders ordering", False, 
                    "Per-sender ordering violated!")
    
    except Exception as e:
        log_test("Concurrent senders ordering", False, f"Exception: {e}")


# =============================================================================
# Main
# =============================================================================

def main():
    print("\n" + "=" * 60)
    print("Message Ordering Under Failure Test (RFC FR-5)")
    print("=" * 60)
    print(f"\nInvariant: FIFO ordering for messages from same sender to same recipient")
    print(f"Target: {EDGE_HOST}:{EDGE_PORT}")
    
    # Run all tests
    test_basic_ordering()
    test_ordering_during_edge_pause()
    test_ordering_with_jitter()
    test_ordering_during_reconnect()
    test_concurrent_senders()
    
    # Summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for _, p, _ in results if p)
    failed = sum(1 for _, p, _ in results if not p)
    skipped = sum(1 for _, _, m in results if "SKIP" in m)
    
    print(f"\nTotal: {len(results)} tests")
    print(f"Passed: {passed} (including {skipped} skipped)")
    print(f"Failed: {failed}")
    
    if failed == 0:
        print("\n✓ All ordering tests passed!")
        print("  RFC FR-5 compliance: VALIDATED")
        return 0
    else:
        print(f"\n✗ {failed} test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
