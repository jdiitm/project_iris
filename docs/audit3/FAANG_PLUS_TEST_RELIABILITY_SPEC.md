# Global Messaging Engine — FAANG-Plus Test, Reliability & Readiness Specification

This specification defines a test-first bar that exceeds typical FAANG internal standards. Every guarantee must be validated by deterministic, reproducible tests under current hardware constraints. If it cannot be tested, it is not production-ready.

## 1. Exhaustive Test Matrix (Primary Artifact)

Each test defines: purpose, preconditions, failure injected, expected outcome, risk mitigated.

### Unit Tests (Critical Invariants)
- **Protocol encode/decode**: full opcode coverage in `iris_proto.erl`.
  - Preconditions: valid and malformed frames.
  - Failure: truncated frame, invalid opcode.
  - Expected: deterministic rejection, no crash.
  - Risk: protocol corruption, parser crashes.
- **Session state machine**: `iris_session.erl` transitions.
  - Failure: out-of-order opcodes.
  - Expected: reject invalid transition.
  - Risk: undefined session behavior.
- **Ack tracking**: `iris_edge_conn.erl` pending ack map behavior.
  - Failure: duplicate ack, delayed ack.
  - Expected: idempotent behavior.
  - Risk: memory leaks, duplicate delivery.

### Property-Based Tests
- **Message ordering**:
  - Generate random interleavings for a conversation.
  - Expected: receiver order is a valid topological ordering with no gaps.
  - Risk: reordering under retry.
- **Deduplication keys**:
  - Random retries, reconnects, duplicate sends.
  - Expected: at-most-one delivery per message ID.
  - Risk: duplicate delivery and user confusion.

### Integration Tests
- **Edge -> Core -> Storage**:
  - Preconditions: two nodes, one edge, one core.
  - Failure: core restart mid-flight.
  - Expected: no loss for acked messages.
  - Risk: silent data loss.
- **Presence accuracy**:
  - Failure: edge restart.
  - Expected: presence reconciles within 10s.
  - Risk: stale presence and misrouting.

### End-to-End Tests
- **Online -> Offline -> Reconnect**:
  - Preconditions: sender online, receiver offline.
  - Failure: receiver reconnects after 5 min.
  - Expected: all messages delivered in order, no duplicates.
  - Risk: offline loss.
- **Multi-device consistency**:
  - Preconditions: same user on two devices.
  - Failure: one device reconnects after missing traffic.
  - Expected: devices converge to same message set.
  - Risk: data divergence across devices.

### Fault Injection Tests
- **Network partition (core split)**:
  - Failure: partition between core nodes.
  - Expected: no acknowledged message loss; eventual convergence with clear conflict strategy.
  - Risk: split-brain loss.
- **Clock skew**:
  - Failure: ±3s skew between nodes.
  - Expected: no ordering inversion beyond tolerance.
  - Risk: sequence break.
- **Partial persistence failure**:
  - Failure: offline storage write fails mid-transaction.
  - Expected: retry or fail-fast with no partial state.
  - Risk: corrupt storage.

### Load / Stress / Soak Tests
- **Burst**:
  - 10x baseline traffic for 60s.
  - Expected: backpressure engages, no crash, latency spike bounded.
- **Sustained**:
  - 2 hours at 70% max.
  - Expected: no steady growth in memory/queues.
- **Soak**:
  - 8 hours at 50% max.
  - Expected: resource usage stable within 5% drift.

### Chaos Tests (Hardware-Limited)
- **Kill router workers**:
  - Failure: random worker kills during load.
  - Expected: supervisor recovery; no loss beyond budget.
- **Edge overload + WAN loss**:
  - Failure: inject 1% loss, 200ms RTT.
  - Expected: delivery success >= 99.9%.

## 2. Test Coverage Bar (Non-Negotiable)

### Minimum coverage by subsystem
- **Protocol & session**: 100% opcode coverage.
- **Routing**: 100% decision paths under presence + offline cases.
- **Storage**: all offline store/write/read/delete paths.
- **Auth**: positive and negative paths, token expiration, invalid signature.

### Failure mode coverage
- Every listed failure mode must have at least one deterministic test.
- Release is blocked if any failure mode lacks coverage.

## 3. SLOs — Defined to Be Testable

Each SLO maps to one or more automated tests.

- **Send latency**:
  - p50 < 50ms, p95 < 150ms, p99 < 300ms (local).
  - Test: fixed-load latency test with deterministic baselines.
- **Delivery success**:
  - >= 99.99% under normal load.
  - Test: end-to-end delivery test with ack correlation.
- **Ordering correctness**:
  - <= 1 reorder per 10M messages.
  - Test: property-based ordering test + high-volume replay.
- **Duplicate rate**:
  - <= 1 per 1M messages.
  - Test: retry storm + dedup verification.
- **Availability**:
  - 99.99% for edge send API.
  - Test: chaos + uptime simulation.
- **Durability**:
  - 99.999% for acknowledged messages.
  - Test: storage failure injection with replay.

## 4. Error Budgets — Enforced by Tests

- **Allocation**:
  - Availability budget 0.01% monthly.
  - Durability budget 0.001%.
  - Latency budget 0.1% for >1s delivery.
- **Burn-rate thresholds**:
  - 5x burn for 30 minutes triggers automatic freeze.
  - 10x burn for 10 minutes triggers rollback.
- **Tests that detect burn**:
  - Automated latency and delivery success suites under load.

## 5. Cost Metrics — Validated Under Load Tests

- **Cost per message**: target <= $0.000001.
- **Cost per active user**: target <= $0.05/month.
- **Storage growth**: <= 0.1 KB/user/day for text.
- **Bandwidth growth**: < 30% of compute costs.
- **Load tests**:
  - Must report CPU, memory, and bandwidth per message.
  - Fail if costs exceed thresholds for 7 consecutive days in test runs.

## 6. Operational Test Requirements

- **Rollout/rollback**:
  - Test automated rollback within 5 minutes.
  - Failure: 5xx > 1% for 5 minutes triggers rollback.
- **Schema migrations**:
  - Test backward and forward compatibility with mixed versions.
  - Failure: any data corruption or read/write mismatch.
- **Disaster recovery (RPO/RTO)**:
  - RPO = 0 for acked messages.
  - RTO < 120s (test infra).
  - Failure: loss > budget or recovery > threshold.

## Constraints and Execution Notes

- Tests must be deterministic and runnable on current laptops and free-tier nodes.
- Scale down concurrency but preserve correctness assertions.
- No manual steps in pass/fail criteria.
