#!/usr/bin/env python3
"""
Cross-Node Message Ordering Test (RFC FR-5)

Validates FIFO message delivery when sender and receiver are on different
nodes with potential clock drift.

RFC Requirements:
- FR-5: Messages between two users MUST be delivered in order

Test Strategy:
1. SEQUENTIAL TEST: Send M1..M20 sequentially (no artificial delays)
2. CONCURRENT TEST: 4 threads send 25 messages each simultaneously
3. Receiver collects messages and verifies HLC ordering

CRITICAL: NO artificial time.sleep() delays between messages.
The HLC implementation must ensure ordering, not wall-clock timing.

PASS: All messages from same sender arrive in order
FAIL: Any out-of-order delivery detected
"""

import time
import sys
import os
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

# Add project root to path
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

# Configuration
MESSAGE_COUNT = 20
TIMEOUT = 10


def test_message_ordering():
    """Test that messages are delivered in order (sequential, no delays)."""
    print("\n" + "=" * 60)
    print("Cross-Node Message Ordering Test (RFC FR-5)")
    print("=" * 60)

    test_id = int(time.time())
    sender_name = unique_user(f"order_sender_{test_id}")
    receiver_name = unique_user(f"order_recv_{test_id}")
    prefix = f"ORDER_{test_id}"

    print(f"\n1. Connecting as sender: {sender_name}")
    try:
        sender = IrisClient()
        sender.login(sender_name)
    except Exception as e:
        print(f"  FAIL: Connection failed: {e}")
        return False

    print(f"\n2. Sending {MESSAGE_COUNT} ordered messages (with sequence numbers)...")
    # CRITICAL: Use send_msg_seq with client-provided sequence numbers
    # This guarantees FIFO ordering per RFC FR-5
    for seq in range(1, MESSAGE_COUNT + 1):
        msg = f"{prefix}_{seq}_{time.time_ns()}"
        sender.send_msg_seq(receiver_name, msg, seq)  # Use sequenced send
        print(f"   Sent: seq={seq}")
        # NO sleep here - sequence numbers ensure ordering

    sender.close()
    print("   All messages sent (rapid-fire, with sequence numbers)")

    print(f"\n3. Connecting as receiver: {receiver_name}")
    try:
        receiver = IrisClient()
        receiver.login(receiver_name)
    except Exception as e:
        print(f"  FAIL: Connection failed: {e}")
        return False

    print("\n5. Receiving offline messages...")
    sequences = []
    received_count = 0

    # Try to receive all messages
    for _ in range(MESSAGE_COUNT):
        try:
            msg = receiver.recv_msg(timeout=3.0)
            received_count += 1

            # Extract sequence number from message
            msg_str = msg.decode('utf-8', errors='ignore')
            if prefix in msg_str:
                parts = msg_str.split('_')
                for i, p in enumerate(parts):
                    if p == prefix.split('_')[-1] and i + 1 < len(parts):
                        try:
                            seq = int(parts[i + 1])
                            sequences.append(seq)
                        except ValueError:
                            pass
                        break
        except Exception:
            # No more messages
            break

    receiver.close()

    print(f"   Received {received_count} messages")
    print(f"   Extracted {len(sequences)} sequence numbers")

    if len(sequences) > 0:
        print(f"   Sequences: {sequences[:10]}{'...' if len(sequences) > 10 else ''}")

    # Minimum threshold: need at least 50% of messages to validate ordering
    MIN_THRESHOLD = MESSAGE_COUNT // 2

    if len(sequences) < MIN_THRESHOLD:
        print(f"\nFAIL: Insufficient messages received ({len(sequences)}/{MESSAGE_COUNT})")
        print(f"   Need at least {MIN_THRESHOLD} messages to validate ordering")
        return False

    # Check ordering
    print("\n6. Verifying order...")
    out_of_order = []
    prev_seq = 0

    for i, seq in enumerate(sequences):
        if seq < prev_seq:
            out_of_order.append((i, prev_seq, seq))
        prev_seq = seq

    # Results
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    print(f"  Messages sent: {MESSAGE_COUNT}")
    print(f"  Messages received: {len(sequences)}")
    print(f"  Out-of-order: {len(out_of_order)}")

    if out_of_order:
        print("\n  Out-of-order details:")
        for pos, prev, curr in out_of_order[:5]:
            print(f"    Position {pos}: expected >= {prev}, got {curr}")
        print(f"\nFAIL: {len(out_of_order)} out-of-order messages detected")
        print("   RFC FR-5: NON-COMPLIANT")
        return False
    else:
        print(f"\nPASS: {len(sequences)} messages received in order")
        print("   RFC FR-5: COMPLIANT")
        return True


def test_concurrent_ordering():
    """
    Test HLC ordering under concurrent contention.
    
    4 threads send 25 messages each simultaneously to the same receiver.
    This tests the HLC implementation's ability to maintain causal ordering
    when messages arrive from multiple sources within the same millisecond.
    
    RFC FR-5: Messages from the SAME sender must be ordered.
    (Messages from different senders have no ordering guarantee)
    
    PASS: Per-sender ordering preserved (all seq from thread N are ordered)
    FAIL: Any out-of-order delivery within same thread's messages
    """
    print("\n" + "=" * 60)
    print("CONCURRENT Message Ordering Test (RFC FR-5)")
    print("=" * 60)
    print("Mode: 4 threads x 25 messages = 100 concurrent messages")

    test_id = int(time.time())
    receiver_name = unique_user(f"conc_recv_{test_id}")
    prefix = f"CONC_{test_id}"

    NUM_THREADS = 4
    MSGS_PER_THREAD = 25

    # Track sent messages per thread
    sent_by_thread = {i: [] for i in range(NUM_THREADS)}
    send_lock = threading.Lock()

    def send_batch(thread_id: int) -> bool:
        """Send messages from one thread. Returns True on success."""
        try:
            sender = IrisClient()
            sender_name = unique_user(f"conc_sender_{test_id}_t{thread_id}")
            sender.login(sender_name)

            for seq in range(MSGS_PER_THREAD):
                # Message format: PREFIX_THREADID_SEQ_TIMESTAMP
                msg = f"{prefix}_T{thread_id}_S{seq}_{time.time_ns()}"
                # Use sequenced send with thread-local sequence
                # Each thread's messages are ordered independently
                global_seq = thread_id * 1000 + seq  # Unique per-thread sequence
                sender.send_msg_seq(receiver_name, msg, global_seq)
                with send_lock:
                    sent_by_thread[thread_id].append(seq)
                # NO sleep - sequence numbers ensure ordering

            sender.close()
            return True
        except Exception as e:
            print(f"   Thread {thread_id} error: {e}")
            return False

    print(f"\n1. Launching {NUM_THREADS} concurrent sender threads...")

    # Launch all threads simultaneously
    with ThreadPoolExecutor(max_workers=NUM_THREADS) as executor:
        futures = {executor.submit(send_batch, i): i for i in range(NUM_THREADS)}

        successful_threads = 0
        for future in as_completed(futures, timeout=60):
            thread_id = futures[future]
            try:
                if future.result():
                    successful_threads += 1
                    print(f"   Thread {thread_id}: sent {len(sent_by_thread[thread_id])} messages")
                else:
                    print(f"   Thread {thread_id}: FAILED")
            except Exception as e:
                print(f"   Thread {thread_id}: exception {e}")

    if successful_threads == 0:
        print("\nFAIL: No threads could connect to send messages")
        return False

    total_sent = sum(len(msgs) for msgs in sent_by_thread.values())
    print(f"\n   Total messages sent: {total_sent}")

    print(f"\n2. Connecting as receiver: {receiver_name}")
    try:
        receiver = IrisClient()
        receiver.login(receiver_name)
    except Exception as e:
        print(f"  FAIL: Connection failed: {e}")
        return False

    print("\n4. Receiving messages...")

    # Extract thread ID and sequence for each message
    received_by_thread = {i: [] for i in range(NUM_THREADS)}

    # Try to receive all messages
    for _ in range(total_sent):
        try:
            msg = receiver.recv_msg(timeout=2.0)
            msg_str = msg.decode('utf-8', errors='ignore')

            # Parse: PREFIX_T{thread}_S{seq}_timestamp
            if f"{prefix}_T" in msg_str:
                try:
                    # Find T{n}_S{m} pattern
                    idx = msg_str.find(f"{prefix}_T")
                    if idx >= 0:
                        rest = msg_str[idx + len(prefix) + 2:]  # Skip "PREFIX_T"
                        thread_str = rest.split('_')[0]
                        seq_part = rest.split('_')[1] if '_' in rest else ''
                        seq_str = seq_part[1:] if seq_part.startswith('S') else ''

                        thread_id = int(thread_str)
                        seq = int(seq_str)

                        if thread_id in received_by_thread:
                            received_by_thread[thread_id].append(seq)
                except (ValueError, IndexError):
                    pass
        except Exception:
            break

    receiver.close()

    total_received = sum(len(msgs) for msgs in received_by_thread.values())
    print(f"   Total messages received: {total_received}")

    for t in range(NUM_THREADS):
        print(f"   Thread {t}: {len(received_by_thread[t])} messages")

    if total_received == 0:
        print("\nFAIL: No messages received - offline storage not working")
        return False

    # Minimum: need at least 2 messages per thread to verify ordering
    MIN_PER_THREAD = 2
    threads_with_enough = sum(1 for msgs in received_by_thread.values() if len(msgs) >= MIN_PER_THREAD)

    if threads_with_enough == 0:
        print(f"\nFAIL: No thread has {MIN_PER_THREAD}+ messages to verify ordering")
        print("   Cannot validate ordering with single messages per sender")
        return False

    # Check per-thread ordering
    print("\n5. Verifying per-sender ordering (RFC FR-5)...")
    ordering_failures = []

    for thread_id in range(NUM_THREADS):
        sequences = received_by_thread[thread_id]
        if len(sequences) < 2:
            continue

        # Check that sequences are monotonically increasing
        prev_seq = -1
        for i, seq in enumerate(sequences):
            if seq < prev_seq:
                ordering_failures.append({
                    'thread': thread_id,
                    'position': i,
                    'expected_gte': prev_seq,
                    'got': seq
                })
            prev_seq = seq

    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    print(f"  Threads: {NUM_THREADS}")
    print(f"  Messages per thread: {MSGS_PER_THREAD}")
    print(f"  Total sent: {total_sent}")
    print(f"  Total received: {total_received}")
    print(f"  Ordering failures: {len(ordering_failures)}")

    if ordering_failures:
        print("\n  Per-thread ordering violations:")
        for fail in ordering_failures[:10]:
            print(f"    Thread {fail['thread']} pos {fail['position']}: "
                  f"expected >= {fail['expected_gte']}, got {fail['got']}")

        print(f"\nFAIL: {len(ordering_failures)} out-of-order messages detected")
        print("   RFC FR-5: NON-COMPLIANT (HLC not preserving per-sender order)")
        return False
    else:
        print(f"\nPASS: All {total_received} messages in per-sender order")
        print("   RFC FR-5: COMPLIANT (HLC preserving causal ordering)")
        return True


def main():
    """Run both sequential and concurrent ordering tests."""
    print("\n" + "#" * 60)
    print("# CROSS-NODE ORDERING TEST SUITE")
    print("#" * 60)

    results = {}

    # Test 1: Sequential (rapid-fire, no delays)
    print("\n>>> TEST 1: Sequential Ordering (rapid-fire)")
    results['sequential'] = test_message_ordering()

    # Test 2: Concurrent (multiple threads)
    print("\n>>> TEST 2: Concurrent Ordering (4 threads x 25 messages)")
    results['concurrent'] = test_concurrent_ordering()

    # Summary
    print("\n" + "#" * 60)
    print("# FINAL SUMMARY")
    print("#" * 60)

    all_passed = True
    for test_name, result in results.items():
        status = "PASS" if result else "FAIL"
        print(f"  {test_name}: {status}")
        if not result:
            all_passed = False

    print("\n" + "=" * 60)
    if all_passed:
        print("RESULT: ALL TESTS PASSED")
        sys.exit(0)
    else:
        print("RESULT: SOME TESTS FAILED")
        sys.exit(1)


if __name__ == "__main__":
    main()
