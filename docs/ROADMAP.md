# Project Iris: 5B DAU Roadmap

**Last Updated**: 2026-02-11  
**Status**: Active Development  
**Target**: WhatsApp/Telegram-class scale (5 Billion Daily Active Users)

---

## Current State Assessment

**Readiness Level**: 6/10 (improved from 5/10 after RFC v4.0 gap closures and test expansion)

| Capability | Status | Notes |
|------------|--------|-------|
| Core Messaging | ✅ Working | Happy path validated |
| Cluster Mode | ✅ Working | Multi-node tested with Docker |
| E2EE | ✅ Working | Signal Protocol implemented |
| Cross-Region | ✅ Hardened | Multi-node disc_copies, TLS enabled |
| **TLS Security** | ✅ **Enforced** | All client connections require TLS |
| **Test Suite** | ✅ **100% Pass** | 156 Python + 101 Erlang tests, all TLS-enabled |
| **RFC v4.0 Compliance** | ✅ **19 gaps closed** | See [RFC_COMPLIANCE.md](RFC_COMPLIANCE.md) |
| **Operational Limits** | ✅ **Enforced** | Inbox 10K, payload 64KB, session 100K |
| Scalability (10K) | ✅ Validated | Local testing with metrics |
| Scalability (1M) | ⚠️ Designed | Architecture supports, not yet tested |
| Production Ready | ❌ No | Development/Pre-alpha |

### Recent Improvements (2026-02-07 — 2026-02-08)
- Closed 19 RFC v4.0 compliance gaps via TDD
- Inbox 10K limit, payload 64KB limit, outbox 7-day TTL enforced
- JWT replay protection, key isolation, version negotiation
- Test suite expanded to 156 Python + 101 Erlang tests (from 115+), all passing
- Session cache bounded (100K), dedup Mnesia cross-check added
- Edge listener hardened, Docker image aligned to OTP 26

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

## Phase 2: Durability & Distribution ✅ MOSTLY COMPLETE

**Goal**: Ensure data survives chaos.

| Task | Status | Notes |
|------|--------|-------|
| Persistent cross-region queue | ✅ Done | `iris_region_bridge.erl` — FIFO, fsync, 7-day TTL |
| Inbox overflow protection | ✅ Done | 10K limit in `iris_core.erl` (GAP-6) |
| Outbox TTL enforcement | ✅ Done | 7-day cleanup in drain timer (GAP-1) |
| Queue depth alerting | ✅ Done | 50% metric (GAP-2) |
| Mailbox AQM (drop policy) | ✅ Done | CoDel implemented in `iris_mailbox_guard.erl` |
| Cross-region Mnesia auto-setup | ⚠️ Partial | Docker volume config |

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
| Network partition (iptables) | ✅ Done | `test_network_partition.py` validates split-brain handling |
| Cross-region durability | ✅ Done | `test_bridge_durability.py` validates RPO=0 |
| Multi-master failover | ✅ Done | `test_multimaster_durability.py` validates SIGKILL recovery |
| "Messi Test" (1M senders) | ❌ Pending | Requires 64GB+ RAM infrastructure |
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
| Partition drill | CI infra | TBD | - |
| "Messi Test" | Requires 64GB+ RAM infra | TBD | - |

---

## Architecture Decisions Pending

### 1. ~~Cross-Region Message Queue~~ — DECIDED

**Decision**: Mnesia `disc_copies` with `sync_transaction` — implemented in `iris_region_bridge.erl`.
- FIFO ordering via `cross_region_outbound` table
- 7-day TTL with automatic cleanup
- Overflow rejection at 10K queue depth

### 2. ~~Mailbox Overflow Policy~~ — DECIDED

**Decision**: CoDel (Controlled Delay) — implemented in `iris_mailbox_guard.erl` (2026-02-10).
- Exports `codel_new/0,1`, `codel_check/3` (pure functions for unit testing)
- Burst-tolerant: allows short bursts above target without dropping
- Latency-focused: drops only when sojourn time exceeds target for a full interval
- Tested in `iris_codel_tests.erl` (5 tests)

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

### Phase 1.5: Test Stabilization (Complete - 2026-02-03)
- [x] TLS enforced on all client connections (NFR-14)
- [x] 156 Python + 101 Erlang tests passing (100%)
- [x] All chaos_dist tests working with Docker cluster
- [x] Reliable message protocol (ACKs) properly implemented
- [x] Cross-region durability validated (RPO=0)

### Phase 1.75: RFC v4.0 Compliance (Complete - 2026-02-08)
- [x] 19 RFC v4.0 gaps closed via TDD
- [x] Inbox 10K, payload 64KB, outbox TTL enforced
- [x] JWT replay protection + key isolation
- [x] Span instrumentation + standard counters
- [x] Session cache bounded, dedup cross-check added

### Phase 2
- [x] Messages survive cross-region link failure (queue durability)
- [x] Inbox overflow protection (10K limit)
- [x] Celebrity accounts don't crash shards (AQM/CoDel in `iris_mailbox_guard`)
- [ ] Cross-region Mnesia auto-initializes

### Phase 3
- [x] 10K connections validated locally
- [x] Linear scaling confirmed (smoke → full profile)
- [x] Per-connection overhead measured (~12KB)
- [x] Network partition handling validated (iptables chaos)
- [ ] 100K connections validated (requires staging infra)
- [ ] "Messi Test" passes: 1M msgs to single user (requires 64GB+ RAM)
- [ ] 24h soak: <1% memory growth, stable latency

---

## Related Documents

- [Scalability Analysis](SCALABILITY_ANALYSIS.md) - Measured metrics and extrapolation
- [Architecture Decisions](DECISIONS.md)
- [Testing Guide](TESTING.md)
- [Operations Guide](OPERATIONS.md)
- [RFC-001 System Requirements](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
