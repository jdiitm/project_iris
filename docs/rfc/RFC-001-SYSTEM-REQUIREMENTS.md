# RFC-001: Project Iris — Global-Scale Messaging Platform

**Status**: Draft  
**Authors**: System Architecture Team  
**Created**: 2026-01-18  
**Last Updated**: 2026-01-18  
**Supersedes**: docs/audit1/, docs/audit4/, docs/audit5/

---

## Abstract

This RFC defines the requirements, guarantees, and constraints for Project Iris, a global-scale, WhatsApp-class messaging platform. It synthesizes findings from multiple architecture audits into a single authoritative reference.

---

## 1. System Goals

### 1.1 North Star

**Deliver a truly global-scale, FAANG-quality, WhatsApp-class communication platform** capable of:

- **5 Billion DAU** (Daily Active Users)
- **500 Million concurrent connections**
- **10 Million messages per second**
- **Sub-100ms P99 delivery latency** (same region)
- **99.999% message durability**
- **99.99% availability**

### 1.2 Design Principles

| Principle | Description |
|-----------|-------------|
| **Scale via Infrastructure** | Adding capacity = infrastructure change, NOT code change |
| **Hardware Efficiency** | Target 70-85% CPU, 80-90% RAM utilization |
| **Fault Tolerance** | Survive any single component failure |
| **Graceful Degradation** | Reduced functionality > total outage |
| **Mobile-First** | Optimize for unreliable networks, battery life |

---

## 2. Functional Requirements

### 2.1 Core Messaging [MUST]

| Requirement | Description | Guarantee |
|-------------|-------------|-----------|
| **FR-1** | 1:1 real-time messaging | Sub-second delivery when both online |
| **FR-2** | Offline message storage | 30-day retention, delivered on reconnect |
| **FR-3** | Delivery acknowledgment | Sender knows message was delivered |
| **FR-4** | Read receipts | Optional, user-controlled |
| **FR-5** | Message ordering | FIFO per conversation pair |

### 2.2 Presence [MUST]

| Requirement | Description | Guarantee |
|-------------|-------------|-----------|
| **FR-6** | Online/offline status | Visible to contacts within 30s |
| **FR-7** | Last seen timestamp | Updated on disconnect |
| **FR-8** | Typing indicators | Real-time, best-effort |

### 2.3 Authentication [MUST]

| Requirement | Description | Guarantee |
|-------------|-------------|-----------|
| **FR-9** | JWT-based auth | Stateless token validation |
| **FR-10** | Token expiry | Max 24h lifetime |
| **FR-11** | Token revocation | Immediate effect, cluster-wide |

### 2.4 Future Extensions [SHOULD]

| Requirement | Description | Status |
|-------------|-------------|--------|
| **FR-12** | Group messaging (1024 members) | NOT IMPLEMENTED |
| **FR-13** | Multi-device sync (4 devices) | NOT IMPLEMENTED |
| **FR-14** | Media messages | NOT IMPLEMENTED |
| **FR-15** | Voice/video calls | NOT IMPLEMENTED |
| **FR-16** | End-to-end encryption | NOT IMPLEMENTED |

---

## 3. Non-Functional Requirements

### 3.1 Performance [MUST]

| Metric | Target | Current Status |
|--------|--------|----------------|
| **NFR-1** Connection latency | <100ms to edge | ✅ MET |
| **NFR-2** Message delivery (same region) | <100ms P99 | ⚠️ DEGRADED (sync RPC) |
| **NFR-3** Message delivery (cross-region) | <500ms P99 | ⚠️ DEGRADED |
| **NFR-4** Reconnect storm handling | 100K/sec | ⚠️ PARTIAL |
| **NFR-5** Throughput per node | 100K msg/sec | ✅ MET |

### 3.2 Reliability [MUST]

| Metric | Target | Current Status |
|--------|--------|----------------|
| **NFR-6** Message durability | 99.999% | ❌ NOT MET (async writes) |
| **NFR-7** Availability | 99.99% | ⚠️ UNTESTED |
| **NFR-8** Data loss on crash | ZERO | ❌ NOT MET |
| **NFR-9** Failover time | <10s | ⚠️ MANUAL |

### 3.3 Scalability [MUST]

| Metric | Target | Current Status |
|--------|--------|----------------|
| **NFR-10** Connections per edge | 2M | ⚠️ 200K verified |
| **NFR-11** Horizontal scaling | Config-only | ✅ DESIGNED |
| **NFR-12** Regional expansion | Zero code changes | ✅ DESIGNED |
| **NFR-13** Shard count | 50,000 | CONFIG-DRIVEN |

### 3.4 Security [MUST]

| Metric | Target | Current Status |
|--------|--------|----------------|
| **NFR-14** TLS encryption | Mandatory | ⚠️ OPTIONAL |
| **NFR-15** JWT validation | Strict | ✅ IMPLEMENTED |
| **NFR-16** Rate limiting | Per-user | ✅ IMPLEMENTED |
| **NFR-17** Input validation | All inputs | ⚠️ PARTIAL |

### 3.5 Efficiency [SHOULD]

| Metric | Target | Current Status |
|--------|--------|----------------|
| **NFR-18** Memory per connection | <10KB | ✅ MET |
| **NFR-19** Cost per connection | <$0.0001/month | ✅ MET |
| **NFR-20** Cost per message | <$0.000001 | ✅ MET |

---

## 4. Architecture Overview

### 4.1 Component Hierarchy

```
┌─────────────────────────────────────────────────────────────────┐
│                     GLOBAL-SCALE ARCHITECTURE                    │
└─────────────────────────────────────────────────────────────────┘

Layer 1: GEO-ROUTING        DNS/Anycast → Nearest Region
                                   │
Layer 2: EDGE CLUSTER       Stateless connection handlers
         (Regional)         [100K connections per node]
                                   │
Layer 3: CORE SHARDS        User-partitioned state
         (Regional)         [hash(user_id) % num_shards]
                                   │
Layer 4: STORAGE            Mnesia (current) → Cassandra (future)
         (Replicated)       [disc_copies for durability]
```

### 4.2 Data Flow

```
SEND MESSAGE:
  Client → Edge → Router → Core (recipient shard) → Storage/Delivery

RECEIVE MESSAGE:
  Core → Edge (cached presence) → Client

OFFLINE FLOW:
  Store in Mnesia → Deliver on reconnect → Delete after ack
```

### 4.3 Current Node Configuration

| Node Type | Role | Capacity | Count |
|-----------|------|----------|-------|
| **Core Primary** | State + routing | 100K users | 1 |
| **Core Replica** | Hot standby | 100K users | 1 |
| **Edge Local** | Connections | 200K each | 2-3 |
| **Edge Remote** | Regional PoP | 10K each | 2 |

---

## 5. Delivery Guarantees

### 5.1 Message Semantics

| Guarantee | Definition | Implementation |
|-----------|------------|----------------|
| **At-Least-Once** | Every message delivered ≥1 time | Retry until ACK |
| **Exactly-Once Display** | User sees message once | Client-side dedup |
| **Ordering** | FIFO per conversation | Sequence numbers |

### 5.2 Durability Model

| Scenario | Current Behavior | Required Behavior |
|----------|------------------|-------------------|
| Clean shutdown | ✅ Messages preserved | ✅ Messages preserved |
| Kill -9 | ⚠️ May lose recent writes | ❌ ZERO loss |
| Power failure | ❌ Data loss likely | ❌ ZERO loss |
| Network partition | ⚠️ Undefined | Queued + delivered |

> **CRITICAL GAP**: Current `async_dirty` writes do NOT guarantee durability.
> **REQUIRED**: Switch to `sync_transaction` for offline message storage.

---

## 6. Security Model

### 6.1 Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│ UNTRUSTED: Internet, Client devices, User input            │
└─────────────────────────────────────────────────────────────┘
                              │
                         TLS Boundary
                              │
┌─────────────────────────────────────────────────────────────┐
│ EDGE: Connection handling, Protocol parsing, JWT validation │
└─────────────────────────────────────────────────────────────┘
                              │
                      Erlang Distribution
                              │
┌─────────────────────────────────────────────────────────────┐
│ CORE: State management, Routing decisions, Storage          │
└─────────────────────────────────────────────────────────────┘
```

### 6.2 Authentication Requirements

| Requirement | Status | Notes |
|-------------|--------|-------|
| JWT HMAC-SHA256 signature | ✅ Enforced | |
| Expiry validation | ✅ Implemented | |
| Revocation (cluster-wide) | ✅ Mnesia-backed | |
| Constant-time comparison | ✅ Implemented | |
| Algorithm "none" attack | ✅ Blocked | |

### 6.3 Known Security Gaps

1. **TLS Optional** - Must be mandatory in production
2. **Shared Erlang Cookie** - Single secret across cluster
3. **No E2E Encryption** - Messages readable by server

---

## 7. Operational Requirements

### 7.1 Observability

| Metric Category | Examples | Status |
|-----------------|----------|--------|
| Connection metrics | count, rate, errors | ✅ iris_metrics |
| Message metrics | sent, delivered, latency | ✅ iris_metrics |
| System metrics | CPU, memory, scheduler | ✅ BEAM telemetry |
| Alerting thresholds | configurable | ⚠️ NOT DEFINED |

### 7.2 Deployment

| Requirement | Status |
|-------------|--------|
| Zero-downtime deploys | ⚠️ UNTESTED |
| Rollback capability | ⚠️ MANUAL |
| Blue-green deployment | ⚠️ NOT IMPLEMENTED |
| Canary releases | ⚠️ NOT IMPLEMENTED |

### 7.3 Incident Response

| Playbook | Status |
|----------|--------|
| Node failure recovery | ✅ docs/runbooks/FAILOVER.md |
| Data corruption recovery | ✅ docs/runbooks/DATA_RECOVERY.md |
| Incident response | ✅ docs/runbooks/INCIDENT_RESPONSE.md |

---

## 8. Current Implementation Gaps

### 8.1 Critical (P0) — Blocks Production

| Gap | Risk | Remediation |
|-----|------|-------------|
| Async writes | Data loss on crash | Use `sync_transaction` |
| TLS optional | MITM attacks | Enforce TLS |
| Single-device only | UX limitation | Add device registry |

### 8.2 High (P1) — Blocks Scale

| Gap | Risk | Remediation |
|-----|------|-------------|
| Sync RPC on hot path | Latency ceiling | Async message queue |
| Mnesia as global store | Partition intolerance | Migrate to Cassandra |
| No group messaging | Missing feature | Fan-out architecture |

### 8.3 Medium (P2) — Technical Debt

| Gap | Risk | Remediation |
|-----|------|-------------|
| Unbounded queues | Memory exhaustion | Add limits |
| No protocol versioning | Evolution blocked | Add version negotiation |
| Clock skew untested | JWT edge cases | Add skew tolerance |

---

## 9. Capacity Model

### 9.1 Scaling Formula

```
Concurrent Users = DAU × 0.10 (10% online at peak)

Edge Nodes = Concurrent Users ÷ 100,000 connections/node
Core Shards = DAU ÷ 100,000 users/shard
Core Nodes = Shards × 2 (primary + replica)
Regions = Edge Nodes ÷ 50 nodes/region
```

### 9.2 Growth Projections

| Phase | DAU | Concurrent | Edges | Shards | Regions |
|-------|-----|------------|-------|--------|---------|
| Proof | 1M | 100K | 2 | 10 | 1 |
| Growth | 100M | 10M | 100 | 1,000 | 2 |
| Scale | 1B | 100M | 1,000 | 10,000 | 20 |
| Planet | 5B | 500M | 5,000 | 50,000 | 100 |

**Code changes required**: NONE (config-driven)

---

## 10. Testing Requirements

### 10.1 Required Test Categories

| Category | Purpose | Status |
|----------|---------|--------|
| Unit tests | Logic correctness | ✅ 58 tests |
| Integration tests | Component interaction | ✅ 10 tests |
| E2E tests | User flows | ✅ 2 tests |
| Security tests | Attack resistance | ✅ 3 tests |
| Resilience tests | Failure recovery | ✅ 2 tests |
| Chaos tests | System stability | ✅ 2 tests |
| Performance tests | Throughput/latency | ⚠️ Threshold issues |
| Durability tests | Data survival | ⚠️ Needs hardening |

### 10.2 Test Quality Requirements

1. **Strict Assertions**: Tests MUST fail on data loss
2. **Determinism**: No `time.sleep()` for synchronization
3. **CI-Compatible**: All tests runnable headlessly
4. **Realistic Conditions**: Test with latency/jitter injection

---

## 11. Compatibility

### 11.1 Protocol Versioning

| Version | Features | Status |
|---------|----------|--------|
| v1 (current) | Basic messaging, presence | ACTIVE |
| v2 (planned) | Batching, compression | NOT IMPLEMENTED |

### 11.2 Backward Compatibility Rules

1. New fields MUST be optional
2. Old clients MUST work with new servers
3. Deprecation requires 2 version lead time

---

## 12. Non-Requirements

The following are explicitly **NOT** requirements for the initial system:

1. **End-to-end encryption** — Server-side access acceptable initially
2. **Regulatory compliance** (GDPR/SOC2) — Future consideration
3. **Federation** — Single organization deployment only
4. **Rich media processing** — Store-and-forward only
5. **Full-text search** — Not a messaging primitive

---

## 13. Appendices

### A. Glossary

| Term | Definition |
|------|------------|
| **Edge** | Connection-handling node, stateless |
| **Core** | State-managing node, sharded |
| **Shard** | Partition of user data |
| **Presence** | Online/offline status |
| **DAU** | Daily Active Users |

### B. Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-18 | 1.0 | Initial RFC synthesized from audit documents |

---

**Approval**:

- [ ] Engineering Lead
- [ ] Security Review
- [ ] Operations Review
