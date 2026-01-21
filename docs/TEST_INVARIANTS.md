# Test Invariants Documentation

This document defines the critical system invariants that MUST be validated by the test suite. Every invariant has an associated test that MUST pass before release.

## Overview

| Invariant | RFC Reference | Test Location | Severity |
|-----------|---------------|---------------|----------|
| Message Ordering | FR-5 | `integration/test_ordering.py` | Critical |
| Delivery Guarantee | Section 5.1 | `chaos_dist/test_ack_durability.py` | Critical |
| Idempotency | Section 5.1 | `integration/test_deduplication.py` | Critical |
| Durability | NFR-6, NFR-8 | `chaos_dist/test_multimaster_durability.py` | Critical |
| Offline/Online | FR-2 | `integration/test_offline_storage.py` | High |
| Clock Skew | NFR-4 | `resilience/test_clock_skew.py` | Medium |

---

## 1. Message Ordering Invariant (RFC FR-5)

### Definition

> Messages from sender S to recipient R are delivered in FIFO order.

### Formal Statement

```
∀ m1, m2 ∈ Messages:
  (sender(m1) = sender(m2)) ∧ 
  (recipient(m1) = recipient(m2)) ∧
  (send_time(m1) < send_time(m2))
  ⟹ (deliver_time(m1) < deliver_time(m2))
```

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| Network reordering | TCP out-of-order delivery | Sequence gap in client |
| Parallel delivery | Multiple edges deliver | Duplicate sequence numbers |
| Retry race | Original + retry both deliver | Same message delivered twice |
| Offline race | Direct vs stored delivery | Old message after new |

### Test Strategy

```python
def test_ordering_invariant():
    # Send messages M1, M2, M3 with known sequence
    sender.send_msg(receiver, "MSG_1", seq=1)
    sender.send_msg(receiver, "MSG_2", seq=2)
    sender.send_msg(receiver, "MSG_3", seq=3)
    
    # Verify order preserved
    received = [receiver.recv_msg() for _ in range(3)]
    assert received == ["MSG_1", "MSG_2", "MSG_3"]
    
    # Repeat with delays, failures, reconnects
```

### Production Detection

- Client-side: Log if sequence N+1 received before sequence N
- Server-side: Audit log with timestamps per (sender, recipient) pair
- Metric: `iris_reorder_events_total{sender, recipient}`

### Test Files

- `tests/suites/integration/test_ordering.py` - Basic ordering
- `tests/suites/chaos_dist/test_ordering_under_failure.py` - Ordering during crashes

---

## 2. Delivery Guarantee Invariant (RFC Section 5.1)

### Definition

> Every acknowledged (ACKed) message is delivered at least once.

### Formal Statement

```
∀ m ∈ Messages:
  ACK_sent(m) ⟹ ◇ delivered(m)
```

(If ACK was sent for message m, eventually m will be delivered)

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| Pre-replication crash | ACK before sync_transaction | Message lost |
| Offline corruption | Mnesia data file corrupted | Message not in offline queue |
| Dedup false positive | Same ID marks as delivered | Never actually delivered |

### Test Strategy

```python
def test_delivery_guarantee():
    # Send message and wait for ACK
    msg_id = sender.send_msg(receiver_offline, "test")
    assert wait_for_ack(msg_id)
    
    # Crash the primary core node
    docker_kill("core-east-1")
    
    # Receiver comes online
    receiver.connect()
    
    # Message MUST be delivered
    msg = receiver.recv_msg(timeout=30)
    assert msg == "test"
```

### Production Detection

- Client reports "expected message never arrived"
- Reconciliation job: compare sent_log vs delivered_log
- Metric: `iris_undelivered_acked_messages_total`

### Test Files

- `tests/suites/chaos_dist/test_ack_durability.py` - Graceful crash
- `tests/suites/chaos_dist/test_multimaster_durability.py` - SIGKILL crash

---

## 3. Idempotency Invariant (RFC Section 5.1)

### Definition

> Duplicate messages (same message ID) are delivered at most once.

### Formal Statement

```
∀ m1, m2 ∈ Messages:
  (id(m1) = id(m2)) ∧ delivered(m1) ⟹ ¬delivered(m2)
```

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| Dedup window expiry | ID older than 7 days | Duplicate delivery |
| Cache eviction | Memory pressure | Duplicate delivery |
| Clock skew collision | Same ID from different senders | Wrong message deduplicated |

### Test Strategy

```python
def test_idempotency():
    msg_id = "TEST_MSG_12345"
    
    # Send same message twice
    sender.send_msg(receiver, "content", msg_id=msg_id)
    sender.send_msg(receiver, "content", msg_id=msg_id)
    
    # Should only receive once
    msg1 = receiver.recv_msg(timeout=5)
    msg2 = receiver.recv_msg(timeout=1)  # Should timeout
    
    assert msg1 == "content"
    assert msg2 is None  # Second should be deduplicated
```

### Production Detection

- Client-side: Track received message IDs, log duplicates
- Server-side: Dedup hit rate metric
- Metric: `iris_dedup_hits_total`, `iris_duplicate_deliveries_total`

### Test Files

- `tests/suites/integration/test_deduplication.py`

---

## 4. Durability Invariant (RFC NFR-6, NFR-8)

### Definition

> 99.999% of acknowledged messages are never lost. RPO=0 after ACK.

### Formal Statement

```
∀ m ∈ Messages:
  ACK_sent(m) ∧ crash(any_node) ⟹ persisted(m)
```

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| ACK before sync | ACK sent before Mnesia commit | Data loss on crash |
| Single-node Mnesia | No replication | Data loss on SIGKILL |
| Disk corruption | Hardware failure | Messages unreadable |

### Test Strategy

```python
def test_durability_multimaster():
    # Send message to offline user
    sender.send_msg("offline_user", "durable_test")
    assert wait_for_ack()
    
    # Wait for Mnesia replication
    time.sleep(2)
    
    # SIGKILL the PRIMARY core node
    docker_kill("core-east-1", signal="KILL")
    
    # Connect to SECONDARY and verify
    receiver = connect_to("core-east-2")
    receiver.login("offline_user")
    
    msg = receiver.recv_msg(timeout=5)
    assert msg == "durable_test"  # RPO=0 validated
```

### Production Detection

- Compare Mnesia table sizes across replicas
- Audit log verification
- Metric: `iris_messages_lost_total` (should be 0)

### Test Files

- `tests/suites/chaos_dist/test_ack_durability.py`
- `tests/suites/chaos_dist/test_multimaster_durability.py`

---

## 5. Offline/Online Transition Invariant (RFC FR-2)

### Definition

> Messages sent to an offline user are delivered when the user connects.

### Formal Statement

```
∀ m ∈ Messages, u ∈ Users:
  sent(m, u) ∧ offline(u) ∧ ◇online(u) ⟹ ◇delivered(m, u)
```

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| Race condition | Login before offline fetch | Messages missed |
| Wrong edge | Stored on different edge | Messages not found |
| Cleanup job | TTL expired | Messages deleted |

### Test Strategy

```python
def test_offline_delivery():
    # Receiver is offline (not connected)
    
    # Send messages while offline
    sender.send_msg("offline_user", "msg_1")
    sender.send_msg("offline_user", "msg_2")
    sender.send_msg("offline_user", "msg_3")
    
    # Wait for storage
    time.sleep(1)
    
    # Receiver comes online
    receiver = connect_and_login("offline_user")
    
    # All messages MUST be delivered immediately
    received = []
    for _ in range(3):
        msg = receiver.recv_msg(timeout=5)
        received.append(msg)
    
    assert "msg_1" in received
    assert "msg_2" in received
    assert "msg_3" in received
```

### Production Detection

- Client reports "no messages after reconnect"
- Server logs offline queue depth
- Metric: `iris_offline_messages_pending{user}`

### Test Files

- `tests/suites/integration/test_offline_storage.py`
- `tests/suites/e2e/test_offline_reconnect.py`

---

## 6. Clock Skew Tolerance Invariant

### Definition

> System functions correctly with clock skew up to 30 seconds between nodes.

### Formal Statement

```
∀ n1, n2 ∈ Nodes:
  |clock(n1) - clock(n2)| ≤ 30s ⟹ system_correct()
```

### How It Can Fail

| Failure Mode | Cause | Detection |
|--------------|-------|-----------|
| Message ID collision | Wall-clock based IDs | Dedup false positives |
| JWT expiry | Token rejected prematurely | Auth failures |
| Dedup window | Wrong expiry calculation | Duplicates or lost |

### Test Strategy

```python
def test_clock_skew():
    # Inject 25 seconds of clock skew on one node
    docker_exec("core-west-1", "date -s '+25 seconds'")
    
    # Verify message ordering still works
    sender_on_east.send_msg(receiver_on_west, "skew_test")
    
    msg = receiver_on_west.recv_msg()
    assert msg == "skew_test"
    
    # Verify dedup still works
    sender_on_east.send_msg(receiver_on_west, "skew_test", msg_id="SAME_ID")
    sender_on_west.send_msg(receiver_on_west, "skew_test", msg_id="SAME_ID")
    
    # Should only receive once
```

### Test Files

- `tests/suites/resilience/test_clock_skew.py` (to be implemented)

---

## 7. Fan-Out Invariant (Plan Section 4.2)

### Definition

> Single sender can deliver to N recipients in O(N) time with zero loss.

### Formal Statement

```
∀ s ∈ Senders, R ⊆ Recipients:
  fan_out(s, R, m) ⟹ 
    (∀ r ∈ R: ◇delivered(m, r)) ∧
    (time_to_deliver ∈ O(|R|))
```

### Thresholds

| Scale | Recipients | Max Latency |
|-------|------------|-------------|
| Small | 10 | 100ms |
| Medium | 100 | 200ms |
| Large | 1000 | 500ms |
| Burst | 100 @ 10K/sec | 500ms, 0 loss |

### Test Files

- `tests/suites/stress/test_fanout.py`

---

## 8. mTLS Enforcement Invariant (RFC NFR-15)

### Definition

> All inter-node communication uses mutual TLS.

### Formal Statement

```
∀ c ∈ Connections:
  internal(c) ⟹ mTLS_verified(c)
```

### Test Scenarios

| Test | Expected |
|------|----------|
| No client cert | Rejected |
| Expired cert | Rejected |
| Untrusted CA | Rejected |
| Valid cert | Accepted |

### Test Files

- `tests/suites/security/test_mtls_enforcement.py`

---

## Invariant Test Matrix

| Invariant | Unit | Integration | E2E | Chaos | Stress |
|-----------|------|-------------|-----|-------|--------|
| Ordering | ✓ | ✓ | ✓ | ✓ | |
| Delivery | | ✓ | ✓ | ✓ | |
| Idempotency | ✓ | ✓ | | | |
| Durability | | | | ✓ | |
| Offline | | ✓ | ✓ | | |
| Clock Skew | | | | ✓ | |
| Fan-Out | | | | | ✓ |
| mTLS | | ✓ | | | |

---

## Adding New Invariants

When adding a new invariant:

1. **Document** it in this file with:
   - Formal definition
   - Failure modes
   - Test strategy
   - Production detection

2. **Implement** tests at appropriate levels

3. **Add metrics** for production monitoring

4. **Update** the invariant test matrix

5. **Review** with team before merge

---

## Related Documents

- [RFC-001-SYSTEM-REQUIREMENTS.md](rfc/RFC-001-SYSTEM-REQUIREMENTS.md) - System requirements
- [RFC-001-TEST-DEVIATIONS.md](rfc/RFC-001-TEST-DEVIATIONS.md) - Known test deviations
- [TEST_STATUS.md](TEST_STATUS.md) - Current test status
