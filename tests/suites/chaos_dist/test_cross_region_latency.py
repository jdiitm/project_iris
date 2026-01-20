#!/usr/bin/env python3
"""
Cross-Region Latency Test (RFC NFR-3)

Measures REAL end-to-end latency: US West → Sydney

RFC Requirements:
- NFR-3: Cross-region P99 latency ≤ 500ms

Test:
1. Connect receiver to Sydney edge (port 8090)
2. Connect sender to US West edge (port 8087)
3. Send timestamped messages from West
4. Receiver in Sydney measures delivery time
5. Calculate P99 from actual delivery latencies

NO FALLBACKS. NO TRICKS. REAL E2E LATENCY.
"""

import socket
import time
import statistics
import sys
import os
import threading
import struct

# Configuration
EDGE_WEST_HOST = os.environ.get("EDGE_WEST_HOST", "localhost")
EDGE_WEST_PORT = int(os.environ.get("EDGE_WEST_PORT", "8087"))
EDGE_SYDNEY_HOST = os.environ.get("EDGE_SYDNEY_HOST", "localhost")
EDGE_SYDNEY_PORT = int(os.environ.get("EDGE_SYDNEY_PORT", "8090"))

TIMEOUT = 10
MESSAGE_COUNT = 30
P99_TARGET_MS = 500


class LatencyReceiver:
    """Receiver that tracks message arrival times."""
    
    def __init__(self, host, port, username):
        self.host = host
        self.port = port
        self.username = username
        self.sock = None
        self.received = {}  # msg_id -> receive_time
        self.running = False
        self.thread = None
    
    def connect(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.settimeout(TIMEOUT)
        self.sock.connect((self.host, self.port))
        # Login
        self.sock.sendall(bytes([0x01]) + self.username.encode())
        resp = self.sock.recv(1024)
        if b"LOGIN_OK" not in resp:
            raise Exception(f"Login failed: {resp}")
        print(f"  ✓ Receiver logged in as {self.username}")
    
    def start_listening(self):
        self.running = True
        self.thread = threading.Thread(target=self._listen_loop, daemon=True)
        self.thread.start()
    
    def _listen_loop(self):
        self.sock.setblocking(False)
        buffer = b""
        while self.running:
            try:
                data = self.sock.recv(4096)
                recv_time = time.time()
                if data:
                    buffer += data
                    # Parse binary protocol: Look for LATENCY_ marker in raw bytes
                    # Message format: 0x10 (reliable msg) + MsgId (16 bytes) + payload_len (4 bytes) + payload
                    # Or simpler: just find LATENCY_ anywhere in the data
                    marker = b"LATENCY_"
                    idx = buffer.find(marker)
                    while idx >= 0:
                        # Extract the LATENCY_N_TS pattern (up to next non-alnum)
                        end_idx = idx + len(marker)
                        while end_idx < len(buffer) and (buffer[end_idx:end_idx+1].isalnum() or buffer[end_idx:end_idx+1] == b'_'):
                            end_idx += 1
                        msg_id = buffer[idx:end_idx].decode('utf-8', errors='ignore')
                        if msg_id:
                            self.received[msg_id] = recv_time
                        buffer = buffer[end_idx:]
                        idx = buffer.find(marker)
            except BlockingIOError:
                time.sleep(0.001)
            except Exception:
                pass
    
    def stop(self):
        self.running = False
        if self.thread:
            self.thread.join(timeout=2)
        if self.sock:
            self.sock.close()
    
    def get_received(self):
        return dict(self.received)


class LatencySender:
    """Sender that tracks message send times."""
    
    def __init__(self, host, port, username):
        self.host = host
        self.port = port
        self.username = username
        self.sock = None
        self.sent = {}  # msg_id -> send_time
    
    def connect(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.settimeout(TIMEOUT)
        self.sock.connect((self.host, self.port))
        # Login
        self.sock.sendall(bytes([0x01]) + self.username.encode())
        resp = self.sock.recv(1024)
        if b"LOGIN_OK" not in resp:
            raise Exception(f"Login failed: {resp}")
        print(f"  ✓ Sender logged in as {self.username}")
    
    def send_message(self, target, msg_id):
        """Send message and record send time."""
        target_bytes = target.encode()
        msg_bytes = msg_id.encode()
        packet = bytes([0x02]) + \
                 len(target_bytes).to_bytes(2, 'big') + target_bytes + \
                 len(msg_bytes).to_bytes(2, 'big') + msg_bytes
        
        send_time = time.time()
        self.sock.sendall(packet)
        self.sent[msg_id] = send_time
        return send_time
    
    def close(self):
        if self.sock:
            self.sock.close()
    
    def get_sent(self):
        return dict(self.sent)


def calculate_percentile(data, percentile):
    if not data:
        return None
    sorted_data = sorted(data)
    k = (len(sorted_data) - 1) * percentile / 100
    f = int(k)
    c = min(f + 1, len(sorted_data) - 1)
    return sorted_data[f] + (k - f) * (sorted_data[c] - sorted_data[f])


def main():
    print("\n" + "=" * 60)
    print("Cross-Region Latency Test (RFC NFR-3)")
    print("=" * 60)
    print(f"Route: US West ({EDGE_WEST_HOST}:{EDGE_WEST_PORT})")
    print(f"    →  Sydney  ({EDGE_SYDNEY_HOST}:{EDGE_SYDNEY_PORT})")
    print(f"Target: P99 ≤ {P99_TARGET_MS}ms")
    print(f"Messages: {MESSAGE_COUNT}")
    print("")
    
    ts = int(time.time() * 1000) % 100000
    sender_name = f"west_{ts}"
    receiver_name = f"sydney_{ts}"
    
    # Step 1: Connect receiver FIRST (so it's online when messages arrive)
    print("1. Connecting receiver to Sydney...")
    receiver = LatencyReceiver(EDGE_SYDNEY_HOST, EDGE_SYDNEY_PORT, receiver_name)
    try:
        receiver.connect()
    except Exception as e:
        print(f"  ❌ FAIL: Cannot connect to Sydney: {e}")
        sys.exit(1)
    
    receiver.start_listening()
    time.sleep(0.5)  # Let receiver settle
    
    # Step 2: Connect sender to US West
    print("\n2. Connecting sender to US West...")
    sender = LatencySender(EDGE_WEST_HOST, EDGE_WEST_PORT, sender_name)
    try:
        sender.connect()
    except Exception as e:
        print(f"  ❌ FAIL: Cannot connect to West: {e}")
        receiver.stop()
        sys.exit(1)
    
    # Step 3: Send messages
    print(f"\n3. Sending {MESSAGE_COUNT} messages (West → Sydney)...")
    for i in range(MESSAGE_COUNT):
        msg_id = f"LATENCY_{i}_{ts}"
        sender.send_message(receiver_name, msg_id)
        if (i + 1) % 10 == 0:
            print(f"   Sent {i + 1}/{MESSAGE_COUNT}")
        time.sleep(0.05)  # 50ms between messages
    
    # Step 4: Wait for delivery
    print("\n4. Waiting for messages to arrive in Sydney...")
    time.sleep(3)
    
    # Step 5: Collect results
    receiver.stop()
    sender.close()
    
    sent_times = sender.get_sent()
    recv_times = receiver.get_received()
    
    print(f"\n   Sent: {len(sent_times)} messages")
    print(f"   Received: {len(recv_times)} messages")
    
    # Calculate latencies
    latencies = []
    for msg_id, send_time in sent_times.items():
        if msg_id in recv_times:
            latency_ms = (recv_times[msg_id] - send_time) * 1000
            latencies.append(latency_ms)
    
    print(f"   Matched: {len(latencies)} messages")
    
    # Results
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    
    if len(latencies) == 0:
        print("\n❌ FAIL: No messages delivered from West to Sydney!")
        print("   Cross-region routing is NOT working.")
        print("\n   Debug info:")
        print(f"   - Sender connected to: {EDGE_WEST_HOST}:{EDGE_WEST_PORT}")
        print(f"   - Receiver connected to: {EDGE_SYDNEY_HOST}:{EDGE_SYDNEY_PORT}")
        print(f"   - Receiver username: {receiver_name}")
        print("\n   Check that:")
        print("   1. Both edges are connected to their cores")
        print("   2. Cores are meshed together")
        print("   3. User registration is working")
        sys.exit(1)
    
    delivery_rate = len(latencies) / MESSAGE_COUNT * 100
    
    p50 = calculate_percentile(latencies, 50)
    p90 = calculate_percentile(latencies, 90)
    p99 = calculate_percentile(latencies, 99)
    avg = statistics.mean(latencies)
    min_lat = min(latencies)
    max_lat = max(latencies)
    
    print(f"\n  Delivery Rate: {len(latencies)}/{MESSAGE_COUNT} ({delivery_rate:.1f}%)")
    print(f"\n  Latency Statistics:")
    print(f"    Min:  {min_lat:.2f} ms")
    print(f"    Avg:  {avg:.2f} ms")
    print(f"    P50:  {p50:.2f} ms")
    print(f"    P90:  {p90:.2f} ms")
    print(f"    P99:  {p99:.2f} ms")
    print(f"    Max:  {max_lat:.2f} ms")
    
    print("\n" + "-" * 60)
    
    if p99 <= P99_TARGET_MS:
        print(f"✅ PASS: P99 latency {p99:.2f}ms ≤ {P99_TARGET_MS}ms")
        print("   RFC NFR-3: COMPLIANT")
        sys.exit(0)
    else:
        print(f"❌ FAIL: P99 latency {p99:.2f}ms > {P99_TARGET_MS}ms")
        print("   RFC NFR-3: NON-COMPLIANT")
        sys.exit(1)


if __name__ == "__main__":
    main()
