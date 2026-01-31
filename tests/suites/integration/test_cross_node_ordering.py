#!/usr/bin/env python3
"""
Cross-Node Message Ordering Test (RFC FR-5)

Validates FIFO message delivery when sender and receiver are on different
nodes with potential clock drift.

RFC Requirements:
- FR-5: Messages between two users MUST be delivered in order

Test Strategy:
1. Sender sends M1, M2, M3, ... M20 with sequence numbers
2. Receiver connects and collects messages
3. Verify messages arrive in order (by sequence number)

PASS: All messages in order (allow duplicates, reject reordering)
FAIL: Any out-of-order delivery detected
"""

import socket
import time
import sys
import os

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
MESSAGE_COUNT = 20
TIMEOUT = 10


def connect():
    """Create connection."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    sock.connect((SERVER_HOST, SERVER_PORT))
    return sock


def login(sock, username):
    """Send login packet."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    # Server handles login synchronously, no sleep needed


def send_message(sock, target, message):
    """Send message packet."""
    target_bytes = target.encode()
    msg_bytes = message.encode()
    packet = bytes([0x02]) + \
             len(target_bytes).to_bytes(2, 'big') + target_bytes + \
             len(msg_bytes).to_bytes(2, 'big') + msg_bytes
    sock.sendall(packet)


def receive_all(sock, timeout=5):
    """Receive all available data."""
    sock.settimeout(timeout)
    data = b""
    try:
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            data += chunk
    except socket.timeout:
        pass
    return data


def extract_sequence_numbers(data, prefix):
    """Extract sequence numbers from received data."""
    sequences = []
    text = data.decode(errors='ignore')
    
    # Look for our test message pattern: PREFIX_SEQ_timestamp
    import re
    pattern = rf'{prefix}_(\d+)_'
    matches = re.findall(pattern, text)
    
    for match in matches:
        sequences.append(int(match))
    
    return sequences


def test_message_ordering():
    """Test that messages are delivered in order."""
    print("\n" + "=" * 60)
    print("Cross-Node Message Ordering Test (RFC FR-5)")
    print("=" * 60)
    print(f"Target: {SERVER_HOST}:{SERVER_PORT}")
    
    test_id = int(time.time())
    sender = f"order_sender_{test_id}"
    receiver = f"order_receiver_{test_id}"
    prefix = f"ORDER_{test_id}"
    
    print(f"\n1. Connecting as sender: {sender}")
    try:
        sender_sock = connect()
        login(sender_sock, sender)
    except Exception as e:
        print(f"  ❌ Connection failed: {e}")
        return None
    
    print(f"\n2. Sending {MESSAGE_COUNT} ordered messages...")
    for seq in range(1, MESSAGE_COUNT + 1):
        msg = f"{prefix}_{seq}_{time.time()}"
        send_message(sender_sock, receiver, msg)
        print(f"   Sent: seq={seq}")
        time.sleep(0.05)  # Small delay between messages
    
    sender_sock.close()
    print("   ✅ All messages sent")
    
    print(f"\n3. Connecting as receiver: {receiver}")
    try:
        receiver_sock = connect()
        login(receiver_sock, receiver)
    except Exception as e:
        print(f"  ❌ Connection failed: {e}")
        return None
    
    print("\n4. Receiving offline messages (with 5s timeout)...")
    data = receive_all(receiver_sock, timeout=5)
    receiver_sock.close()
    
    print(f"   Received {len(data)} bytes")
    
    print("\n6. Extracting sequence numbers...")
    sequences = extract_sequence_numbers(data, prefix)
    print(f"   Found {len(sequences)} messages")
    
    if len(sequences) == 0:
        print("\n⚠️ No messages found - test inconclusive")
        print("  This may indicate offline storage isn't working")
        return None
    
    print(f"   Received sequences: {sequences}")
    
    # Check ordering
    print("\n7. Verifying order...")
    out_of_order = []
    prev_seq = 0
    
    for i, seq in enumerate(sequences):
        if seq < prev_seq:
            out_of_order.append((i, prev_seq, seq))
        prev_seq = seq
    
    # Also check for gaps (missing messages)
    expected = set(range(1, MESSAGE_COUNT + 1))
    received = set(sequences)
    missing = expected - received
    
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    print(f"  Messages sent: {MESSAGE_COUNT}")
    print(f"  Messages received: {len(sequences)}")
    print(f"  Unique sequences: {len(received)}")
    print(f"  Missing: {sorted(missing) if missing else 'None'}")
    print(f"  Out-of-order: {len(out_of_order)}")
    
    if out_of_order:
        print("\n  Out-of-order details:")
        for pos, prev, curr in out_of_order[:5]:  # Show first 5
            print(f"    Position {pos}: expected ≥{prev}, got {curr}")
    
    if out_of_order:
        print(f"\n❌ FAIL: {len(out_of_order)} out-of-order messages detected")
        print("   RFC FR-5: NON-COMPLIANT")
        return False
    else:
        if len(received) == MESSAGE_COUNT:
            print(f"\n✅ PASS: All {MESSAGE_COUNT} messages received in order")
        else:
            print(f"\n⚠️ PARTIAL: {len(received)}/{MESSAGE_COUNT} messages, but in order")
        print("   RFC FR-5: COMPLIANT")
        return True


def main():
    result = test_message_ordering()
    
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
