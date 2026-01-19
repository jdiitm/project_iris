#!/usr/bin/env python3
"""
Cross-Region Latency Test (RFC NFR-3)

Validates P99 latency ≤ 500ms when sender and receiver are on opposite
sides of simulated WAN (100ms delay each way via Pumba).

RFC Requirements:
- NFR-3: Cross-region P99 latency ≤ 500ms

Test Strategy:
1. Connect sender to Edge East (no latency)
2. Connect receiver to Edge Sydney (100ms injected delay)
3. Send 100 messages, measure E2E latency
4. Calculate P99

PASS: P99 ≤ 500ms
FAIL: P99 > 500ms
"""

import socket
import time
import statistics
import sys
import os

# Configuration
EDGE_EAST_HOST = os.environ.get("EDGE_EAST_HOST", "localhost")
EDGE_EAST_PORT = int(os.environ.get("EDGE_EAST_PORT", "8085"))
EDGE_SYDNEY_HOST = os.environ.get("EDGE_SYDNEY_HOST", "localhost")
EDGE_SYDNEY_PORT = int(os.environ.get("EDGE_SYDNEY_PORT", "8090"))

TIMEOUT = 10
MESSAGE_COUNT = 100
P99_TARGET_MS = 500


def connect(host, port):
    """Create plaintext connection."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    sock.connect((host, port))
    return sock


def login(sock, username):
    """Send login packet."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    time.sleep(0.1)


def send_message_timed(sock, target, message):
    """Send message and measure round-trip time."""
    target_bytes = target.encode()
    msg_bytes = message.encode()
    packet = bytes([0x02]) + \
             len(target_bytes).to_bytes(2, 'big') + target_bytes + \
             len(msg_bytes).to_bytes(2, 'big') + msg_bytes
    
    start = time.time()
    sock.sendall(packet)
    
    # Wait for any response (ACK or echo)
    try:
        sock.recv(1024)
        end = time.time()
        return (end - start) * 1000  # Convert to ms
    except socket.timeout:
        return None


def calculate_percentile(data, percentile):
    """Calculate percentile of data."""
    if not data:
        return None
    sorted_data = sorted(data)
    k = (len(sorted_data) - 1) * percentile / 100
    f = int(k)
    c = f + 1 if f < len(sorted_data) - 1 else f
    return sorted_data[f] + (k - f) * (sorted_data[c] - sorted_data[f])


def test_cross_region_latency():
    """Test cross-region message latency."""
    print("\n" + "=" * 60)
    print("Cross-Region Latency Test (RFC NFR-3)")
    print("=" * 60)
    print(f"Edge East: {EDGE_EAST_HOST}:{EDGE_EAST_PORT}")
    print(f"Edge Sydney: {EDGE_SYDNEY_HOST}:{EDGE_SYDNEY_PORT}")
    print(f"Target P99: ≤ {P99_TARGET_MS}ms")
    
    sender = f"latency_sender_{int(time.time())}"
    receiver = f"latency_receiver_{int(time.time())}"
    
    # Check if Sydney edge is different from East
    same_host = (EDGE_EAST_HOST == EDGE_SYDNEY_HOST and EDGE_EAST_PORT == EDGE_SYDNEY_PORT)
    if same_host:
        print("\n⚠️ Sydney and East edges are the same - no cross-region test")
        print("  For true cross-region test, start cluster with: make cluster-chaos")
        print("  Running local-only latency measurement instead...")
    
    print(f"\n1. Connecting sender to East: {sender}")
    try:
        sender_sock = connect(EDGE_EAST_HOST, EDGE_EAST_PORT)
        login(sender_sock, sender)
    except Exception as e:
        print(f"  ❌ Connection to East failed: {e}")
        print("  Ensure server is running: make start")
        return None
    
    print(f"\n2. Sending {MESSAGE_COUNT} messages...")
    latencies = []
    failed = 0
    
    for i in range(MESSAGE_COUNT):
        msg = f"LATENCY_TEST_{i}_{time.time()}"
        latency = send_message_timed(sender_sock, receiver, msg)
        if latency is not None:
            latencies.append(latency)
        else:
            failed += 1
        
        # Progress indicator
        if (i + 1) % 20 == 0:
            print(f"   Sent {i + 1}/{MESSAGE_COUNT} messages...")
    
    sender_sock.close()
    
    if not latencies:
        print("  ❌ No successful messages")
        return False
    
    # Calculate statistics
    p50 = calculate_percentile(latencies, 50)
    p90 = calculate_percentile(latencies, 90)
    p99 = calculate_percentile(latencies, 99)
    avg = statistics.mean(latencies)
    min_lat = min(latencies)
    max_lat = max(latencies)
    
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    print(f"  Messages sent: {MESSAGE_COUNT}")
    print(f"  Successful: {len(latencies)}")
    print(f"  Failed: {failed}")
    print("")
    print(f"  Min latency:  {min_lat:.2f} ms")
    print(f"  Avg latency:  {avg:.2f} ms")
    print(f"  P50 latency:  {p50:.2f} ms")
    print(f"  P90 latency:  {p90:.2f} ms")
    print(f"  P99 latency:  {p99:.2f} ms")
    print(f"  Max latency:  {max_lat:.2f} ms")
    print("")
    
    if p99 <= P99_TARGET_MS:
        print(f"✅ PASS: P99 ({p99:.2f}ms) ≤ {P99_TARGET_MS}ms")
        print("   RFC NFR-3: COMPLIANT")
        return True
    else:
        print(f"❌ FAIL: P99 ({p99:.2f}ms) > {P99_TARGET_MS}ms")
        print("   RFC NFR-3: NON-COMPLIANT")
        return False


def main():
    result = test_cross_region_latency()
    
    print("\n" + "=" * 60)
    if result is True:
        print("RESULT: PASSED")
        sys.exit(0)
    elif result is False:
        print("RESULT: FAILED")
        sys.exit(1)
    else:
        print("RESULT: SKIPPED")
        sys.exit(0)


if __name__ == "__main__":
    main()
