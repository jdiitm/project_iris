# Project Iris: 5B DAU Roadmap

**Last Updated**: 2026-01-29  
**Status**: Active Development  
**Target**: WhatsApp/Telegram-class scale (5 Billion Daily Active Users)

---

## Current State Assessment

**Readiness Level**: 3/10 (was 2/10 before forensic audit fixes)

| Capability | Status | Notes |
|------------|--------|-------|
| Core Messaging | ✅ Working | Happy path validated |
| Cluster Mode | ✅ Working | Multi-node tested |
| E2EE | ✅ Working | Signal Protocol implemented |
| Cross-Region | ⚠️ Partial | Requires manual setup |
| Scalability | ❌ Blocked | Architectural fixes in progress |
| Production Ready | ❌ No | Pre-alpha |

---

## Phase 1: Core Refactor (This Sprint) ✅ COMPLETE

**Goal**: Fix fundamental architectural blockers identified in forensic audit.

| Task | Status | Module |
|------|--------|--------|
| ETS presence backend default | ✅ Done | `iris_core.erl` |
| Async router HOL blocking fix | ✅ Done | `iris_async_router.erl` |
| Cluster self-healing manager | ✅ Done | `iris_cluster_manager.erl` |
| Validation tests | ✅ Done | `iris_forensic_audit_tests.erl` |

**Changes Made**:
- Changed `presence_backend` default from `mnesia` to `ets` (eliminates global lock)
- Refactored `route_to_remote/4` to spawn ephemeral tasks (eliminates HOL blocking)
- Created `iris_cluster_manager` to auto-wire replication on node join
- Added forensic audit validation test suite

---

## Phase 2: Durability & Distribution (Next Sprint)

**Goal**: Ensure data survives chaos.

| Task | Status | Blocked By |
|------|--------|------------|
| Persistent cross-region queue | ❌ Pending | RFC: Queue semantics |
| Mailbox overflow protection (AQM) | ❌ Pending | RFC: Drop policy |
| Cross-region Mnesia auto-setup | ⚠️ Partial | Docker volume config |

### RFC Required: Cross-Region Queue

**Questions to Answer**:
1. FIFO vs Priority ordering?
2. TTL for queued messages?
3. Overflow behavior (drop oldest vs reject new)?
4. Persistent storage backend (disk_log vs khepri)?

### RFC Required: Active Queue Management

**Questions to Answer**:
1. Drop policy: Tail Drop vs RED vs CoDel?
2. Backpressure signaling to senders?
3. Per-user vs per-shard limits?
4. Celebrity account handling?

---

## Phase 3: Validation at Scale (Future)

**Goal**: Prove 5B DAU readiness.

| Test | Status | Description |
|------|--------|-------------|
| "Messi Test" | ❌ Pending | 1M messages to single user |
| Network partition drill | ❌ Pending | 10-minute US-EU partition |
| 24h soak test | ⚠️ Partial | 100k concurrent at steady state |
| Cross-region latency | ❌ Pending | P99 < 50ms |

### Infrastructure Required

- [ ] `pumba` or `tc` integration for network chaos
- [ ] Prometheus/Grafana dashboards for soak tests
- [ ] Automated CI pipeline for multi-region testing

---

## Blockers

| Item | Blocker | Owner | ETA |
|------|---------|-------|-----|
| Cross-region queue | Design RFC | TBD | - |
| AQM/backpressure | Drop policy RFC | TBD | - |
| Partition drill | CI infra | TBD | - |
| "Messi Test" | AQM implementation | TBD | - |

---

## Architecture Decisions Pending

### 1. Cross-Region Message Queue

**Options**:
- `disk_log` (built-in, proven)
- `khepri` (RabbitMQ's new storage)
- External queue (Kafka, SQS)

**Recommendation**: `disk_log` for simplicity, with khepri as future upgrade path.

### 2. Mailbox Overflow Policy

**Options**:
- Tail Drop (simple, unfair to late messages)
- RED (Random Early Detection - probabilistic)
- CoDel (Controlled Delay - latency-focused)

**Recommendation**: CoDel for latency-sensitive messaging.

### 3. Worker Pool Strategy

**Current**: Ephemeral spawn + circuit breaker
**Alternative**: Bounded worker pool (`iris_router_pool`)

**Decision**: Keep ephemeral spawn. Bounded pool adds complexity without clear benefit given circuit breaker isolation.

---

## Success Criteria

### Phase 1 (Complete)
- [x] No HOL blocking in async router
- [x] ETS default for presence (no global lock)
- [x] Self-healing cluster topology

### Phase 2
- [ ] Messages survive cross-region link failure (queue durability)
- [ ] Celebrity accounts don't crash shards (AQM)
- [ ] Cross-region Mnesia auto-initializes

### Phase 3
- [ ] "Messi Test" passes: 1M msgs to single user, no crash
- [ ] 10-minute partition: 0 message loss, auto-recovery
- [ ] 24h soak: <1% memory growth, stable latency

---

## Related Documents

- [Architectural Decisions](DECISIONS.md)
- [Principal Audit Report](PRINCIPAL_AUDIT_REPORT.md)
- [Test Status](TEST_STATUS.md)
- [RFC-001 System Requirements](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
