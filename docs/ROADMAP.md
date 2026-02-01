# Project Iris: 5B DAU Roadmap

**Last Updated**: 2026-01-29  
**Status**: Active Development  
**Target**: WhatsApp/Telegram-class scale (5 Billion Daily Active Users)

---

## Current State Assessment

**Readiness Level**: 4/10 (improved from 3/10 after GA audit fixes)

| Capability | Status | Notes |
|------------|--------|-------|
| Core Messaging | ✅ Working | Happy path validated |
| Cluster Mode | ✅ Working | Multi-node tested |
| E2EE | ✅ Working | Signal Protocol implemented |
| Cross-Region | ✅ Hardened | Multi-node disc_copies for durability |
| Scalability (10K) | ✅ Validated | Local testing with metrics |
| Scalability (1M) | ⚠️ Designed | Architecture supports, not yet tested |
| Production Ready | ❌ No | Development/Pre-alpha |

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

## Phase 3: Validation at Scale

**Goal**: Validate scalability claims with measured data.

### Scalability Validation Milestones

| Scale | Status | Environment | Notes |
|-------|--------|-------------|-------|
| 100 connections | ✅ Done | Local dev | smoke profile |
| 10,000 connections | ✅ Done | Local dev | full profile |
| 100,000 connections | ❌ Pending | 8GB+ RAM | Requires staging infra |
| 1,000,000 connections | ❌ Pending | 64GB+ RAM | Requires production infra |

See [Scalability Analysis](SCALABILITY_ANALYSIS.md) for measured metrics and extrapolation methodology.

### Stress Test Status

| Test | Status | Description |
|------|--------|-------------|
| VIP Fan-in (100 senders) | ✅ Done | Batch message coalescing works |
| VIP Fan-in (10K senders) | ✅ Done | Linear scaling confirmed |
| "Messi Test" (1M senders) | ❌ Pending | Requires 64GB+ RAM infrastructure |
| Network partition drill | ❌ Pending | 10-minute US-EU partition |
| 24h soak test | ❌ Pending | 100k concurrent at steady state |

### Infrastructure Required

- [ ] Staging environment (8GB+ RAM) for 100K tests
- [ ] Production-scale infra (64GB+ RAM) for 1M tests
- [ ] `pumba` or `tc` integration for network chaos
- [ ] Prometheus/Grafana dashboards for soak tests

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
- [x] 10K connections validated locally
- [x] Linear scaling confirmed (smoke → full profile)
- [x] Per-connection overhead measured (~12KB)
- [ ] 100K connections validated (requires staging infra)
- [ ] "Messi Test" passes: 1M msgs to single user (requires 64GB+ RAM)
- [ ] 10-minute partition: 0 message loss, auto-recovery
- [ ] 24h soak: <1% memory growth, stable latency

---

## Related Documents

- [Scalability Analysis](SCALABILITY_ANALYSIS.md) - Measured metrics and extrapolation
- [Architecture Decisions](DECISIONS.md)
- [Testing Guide](TESTING.md)
- [Operations Guide](OPERATIONS.md)
- [RFC-001 System Requirements](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
