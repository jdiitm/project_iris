# Project Iris: 5B DAU Roadmap

**Last Updated**: 2026-02-18  
**Status**: Release Candidate  
**Target**: WhatsApp/Telegram-class scale (5 Billion Daily Active Users)

---

## Current State Assessment

**Readiness Level**: 8/10 (improved from 6/10 after production hardening, CI optimization, and test cleanup)

| Capability | Status | Notes |
|------------|--------|-------|
| Core Messaging | ✅ Working | Happy path validated |
| Cluster Mode | ✅ Working | Multi-node tested with Docker |
| E2EE | ✅ Working | Signal Protocol implemented |
| Cross-Region | ✅ Hardened | Multi-node disc_copies, TLS enabled |
| **TLS Security** | ✅ **Enforced** | All client connections require TLS |
| **Test Suite** | ✅ **100% Pass** | All TLS-enabled. See [TESTING.md](TESTING.md) for counts |
| **RFC v4.0 Compliance** | ✅ **19 gaps closed** | See [RFC_COMPLIANCE.md](RFC_COMPLIANCE.md) |
| **Operational Limits** | ✅ **Enforced** | Inbox 10K, payload 64KB, session 100K |
| **CI Pipeline** | ✅ **Hardened** | Parallelized, security-gated, Dialyzer + Xref |
| Scalability (10K) | ✅ Validated | Local testing with metrics |
| Scalability (1M) | ⚠️ Designed | Architecture supports, not yet tested |
| Production Ready | ⚠️ Release Candidate | Pending staging/production-scale validation |

See [CHANGELOG.md](../CHANGELOG.md) for detailed change history.

---

## Phase 1: Core Refactor ✅ COMPLETE

ETS presence default, async router HOL blocking fix, cluster self-healing manager. All validated with forensic audit tests.

## Phase 2: Durability & Distribution ✅ MOSTLY COMPLETE

Persistent cross-region queue, inbox 10K limit, outbox 7-day TTL, queue depth alerting, CoDel AQM. One item remaining: cross-region Mnesia auto-setup (partial, blocked by Docker volume config).

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

## Blockers & Deferred Work

| Item | Effort | Blocker |
|------|--------|---------|
| Partition drill (pumba/tc in CI) | 1 week | Docker infra + CI pipeline changes |
| "Messi Test" (1M senders) | 1 day | Requires 64GB+ RAM infrastructure |
| Cross-region Mnesia auto-setup | 2-3 days | Docker volume config |
| `ra` library integration (full CP) | 1-2 days | External dependency |
| 5B user architecture | 2-3 months | Storage rewrite (Mnesia replacement) |

---

## Architecture Decisions (Resolved)

| Decision | Resolution |
|----------|------------|
| Cross-region message queue | Mnesia `disc_copies` in `iris_region_bridge.erl` (FIFO, 7-day TTL, 10K depth limit) |
| Mailbox overflow policy | CoDel AQM in `iris_mailbox_guard.erl` (burst-tolerant, latency-focused) |
| Worker pool strategy | Ephemeral spawn + circuit breaker; `iris_router_pool` removed as dead code |

See [DECISIONS.md](DECISIONS.md) for full rationale.

---

## Success Criteria

### Completed Phases
- **Phase 1** (Core Refactor): HOL blocking, ETS presence, cluster self-healing ✅
- **Phase 1.5** (Test Stabilization, 2026-02-03): TLS enforced, 100% tests passing, ACK protocol ✅
- **Phase 1.75** (RFC v4.0 Compliance, 2026-02-08): 19 gaps closed, limits enforced, JWT hardened ✅
- **Phase 2** (Durability): Queue durability, inbox protection, CoDel AQM ✅ (cross-region Mnesia auto-init pending)
- **Phase 2.5** (Production Hardening, 2026-02-18): CI parallelized, `time.sleep` cleanup (~130 removed), security gate, production config validation, documentation consolidated ✅

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
