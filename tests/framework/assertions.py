"""
State-Based Assertions for Messaging Tests

Deterministic assertions that don't rely on timing.
Validates messaging semantics: delivery, ordering, no-loss, no-duplication.
"""

import time
from typing import Set, List, Dict, Callable, Any, Optional
from collections import defaultdict


class MessageTracker:
    """
    Tracks messages for correctness validation.
    
    Maintains:
    - Sent messages (with timestamps)
    - Received messages (with timestamps)
    - Duplicate detection
    - Order tracking
    """
    
    def __init__(self):
        self.sent: Dict[str, Dict[str, Any]] = {}  # msg_id -> {timestamp, target, payload}
        self.received: Dict[str, Dict[str, Any]] = {}  # msg_id -> {timestamp, count}
        self.receive_order: List[str] = []  # Order of received message IDs
        self._lock_sent = False
        self._lock_received = False
    
    def record_sent(
        self,
        message_id: str,
        target: str,
        payload: Any = None,
        sequence: Optional[int] = None
    ):
        """Record a sent message."""
        self.sent[message_id] = {
            "timestamp": time.monotonic_ns(),
            "target": target,
            "payload": payload,
            "sequence": sequence
        }
    
    def record_received(self, message_id: str):
        """Record a received message."""
        timestamp = time.monotonic_ns()
        
        if message_id in self.received:
            self.received[message_id]["count"] += 1
            self.received[message_id]["timestamps"].append(timestamp)
        else:
            self.received[message_id] = {
                "timestamp": timestamp,
                "timestamps": [timestamp],
                "count": 1
            }
        
        self.receive_order.append(message_id)
    
    def get_lost_messages(self) -> Set[str]:
        """Get message IDs that were sent but not received."""
        return set(self.sent.keys()) - set(self.received.keys())
    
    def get_duplicates(self) -> Dict[str, int]:
        """Get message IDs received more than once with counts."""
        return {
            msg_id: info["count"]
            for msg_id, info in self.received.items()
            if info["count"] > 1
        }
    
    def get_unexpected(self) -> Set[str]:
        """Get message IDs received but not sent."""
        return set(self.received.keys()) - set(self.sent.keys())
    
    def get_latencies_ms(self) -> List[float]:
        """Calculate latencies for delivered messages in milliseconds."""
        latencies = []
        for msg_id, recv_info in self.received.items():
            if msg_id in self.sent:
                send_ts = self.sent[msg_id]["timestamp"]
                recv_ts = recv_info["timestamp"]
                latency_ms = (recv_ts - send_ts) / 1_000_000
                latencies.append(latency_ms)
        return latencies
    
    def is_order_preserved(self) -> bool:
        """Check if receive order matches send sequence order."""
        # Get sequence numbers for received messages
        received_sequences = []
        for msg_id in self.receive_order:
            if msg_id in self.sent and self.sent[msg_id].get("sequence") is not None:
                received_sequences.append(self.sent[msg_id]["sequence"])
        
        # Check if monotonically increasing
        for i in range(1, len(received_sequences)):
            if received_sequences[i] < received_sequences[i-1]:
                return False
        return True
    
    def summary(self) -> Dict[str, Any]:
        """Get a summary of message tracking."""
        lost = self.get_lost_messages()
        dups = self.get_duplicates()
        latencies = self.get_latencies_ms()
        
        return {
            "sent": len(self.sent),
            "received": len(self.received),
            "delivered": len(self.sent) - len(lost),
            "lost": len(lost),
            "duplicates": len(dups),
            "unexpected": len(self.get_unexpected()),
            "order_preserved": self.is_order_preserved(),
            "latency_min_ms": min(latencies) if latencies else 0,
            "latency_max_ms": max(latencies) if latencies else 0,
            "latency_avg_ms": sum(latencies) / len(latencies) if latencies else 0
        }


def assert_message_delivered(
    tracker: MessageTracker,
    message_id: str,
    timeout_seconds: float = 5.0,
    poll_interval: float = 0.1
) -> bool:
    """
    Assert that a specific message was delivered (state-based, not timing-based).
    
    Polls for message receipt within timeout.
    """
    start = time.monotonic()
    while time.monotonic() - start < timeout_seconds:
        if message_id in tracker.received:
            return True
        time.sleep(poll_interval)
    
    raise AssertionError(f"Message {message_id} not delivered within {timeout_seconds}s")


def assert_no_message_loss(
    tracker: MessageTracker,
    allowed_loss_pct: float = 0.0
) -> bool:
    """
    Assert that no messages were lost (or within allowed threshold).
    """
    total_sent = len(tracker.sent)
    lost = tracker.get_lost_messages()
    lost_pct = (len(lost) / total_sent * 100) if total_sent > 0 else 0
    
    if lost_pct > allowed_loss_pct:
        raise AssertionError(
            f"Message loss {lost_pct:.2f}% exceeds threshold {allowed_loss_pct}%. "
            f"Lost {len(lost)}/{total_sent} messages: {list(lost)[:10]}..."
        )
    return True


def assert_no_duplicates(tracker: MessageTracker) -> bool:
    """
    Assert that no duplicate messages were received.
    """
    dups = tracker.get_duplicates()
    if dups:
        raise AssertionError(
            f"Duplicate messages detected: {dups}"
        )
    return True


def assert_ordering_preserved(tracker: MessageTracker) -> bool:
    """
    Assert that message ordering was preserved.
    """
    if not tracker.is_order_preserved():
        raise AssertionError("Message ordering was not preserved")
    return True


def assert_latency_under(
    tracker: MessageTracker,
    percentile: float,
    max_latency_ms: float
) -> bool:
    """
    Assert that a percentile of latencies is under the threshold.
    """
    latencies = sorted(tracker.get_latencies_ms())
    if not latencies:
        return True  # No messages to check
    
    idx = int(len(latencies) * percentile / 100)
    idx = min(idx, len(latencies) - 1)
    actual_latency = latencies[idx]
    
    if actual_latency > max_latency_ms:
        raise AssertionError(
            f"P{percentile} latency {actual_latency:.2f}ms exceeds threshold {max_latency_ms}ms"
        )
    return True


def wait_for_condition(
    condition: Callable[[], bool],
    timeout_seconds: float = 10.0,
    poll_interval: float = 0.1,
    description: str = "condition"
) -> bool:
    """
    Wait for a condition to become true (state-based waiting, not sleep).
    
    This is the correct way to wait for async operations in tests.
    """
    start = time.monotonic()
    while time.monotonic() - start < timeout_seconds:
        try:
            if condition():
                return True
        except Exception:
            pass
        time.sleep(poll_interval)
    
    raise TimeoutError(f"Condition '{description}' not met within {timeout_seconds}s")


def wait_for_count(
    tracker: MessageTracker,
    expected_count: int,
    timeout_seconds: float = 10.0
) -> bool:
    """
    Wait for a specific number of messages to be received.
    """
    return wait_for_condition(
        lambda: len(tracker.received) >= expected_count,
        timeout_seconds=timeout_seconds,
        description=f"receive {expected_count} messages"
    )


class AssertionReport:
    """
    Collects assertion results for reporting.
    """
    
    def __init__(self):
        self.assertions: List[Dict[str, Any]] = []
        self.passed = 0
        self.failed = 0
    
    def add(self, name: str, passed: bool, details: str = ""):
        """Add an assertion result."""
        self.assertions.append({
            "name": name,
            "passed": passed,
            "details": details
        })
        if passed:
            self.passed += 1
        else:
            self.failed += 1
    
    def run(self, name: str, assertion_fn: Callable[[], bool], **kwargs) -> bool:
        """Run an assertion and record the result."""
        try:
            result = assertion_fn(**kwargs)
            self.add(name, True)
            return True
        except AssertionError as e:
            self.add(name, False, str(e))
            return False
        except Exception as e:
            self.add(name, False, f"Unexpected error: {e}")
            return False
    
    def all_passed(self) -> bool:
        """Check if all assertions passed."""
        return self.failed == 0
    
    def summary(self) -> Dict[str, Any]:
        """Get assertion report summary."""
        return {
            "total": len(self.assertions),
            "passed": self.passed,
            "failed": self.failed,
            "assertions": self.assertions
        }
