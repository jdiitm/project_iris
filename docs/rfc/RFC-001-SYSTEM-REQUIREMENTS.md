# RFC-001: Project Iris — Global-Scale Messaging Platform

**Status**: Draft v2  
**Authors**: System Architecture Team  
**Created**: 2026-01-18  
**Last Updated**: 2026-01-18  
**Revision**: 2.0 (Post-Critique Resolution)

---

## Abstract

This RFC defines normative requirements for Project Iris, a global-scale messaging platform. All guarantees are defined in measurable terms with explicit testability criteria.

> **Scope**: This document contains requirements only. Implementation status is tracked separately.

---

## 1. System Goals

### 1.1 North Star

| Metric | Target | Measurement Definition |
|--------|--------|------------------------|
| Daily Active Users | 5 Billion | Unique users sending ≥1 message/day |
| Concurrent Connections | 500 Million | Active TCP connections at peak |
| Message Throughput | 10M msg/sec | Messages processed by Core layer |
| Delivery Latency (in-region) | ≤100ms P99 | Sender submit → Receiver client ACK |
| Delivery Latency (cross-region) | ≤500ms P99 | Including network transit time |
| Message Durability | 99.999% | No acknowledged message lost after server ACK |
| Availability | 99.99% | Measured as successful message deliveries / attempts |

### 1.2 Design Principles

| Principle | Definition | Testability |
|-----------|------------|-------------|
| Scale via Config | 10× capacity increase requires config-only changes | Add 10 nodes via config, measure throughput |
| Fault Tolerance | Survive any single component failure | Kill any node, verify zero message loss |
| Graceful Degradation | Under overload: reduce features, maintain durability | Inject 2× load, verify durability maintained |
| Resource Efficiency | 50% CPU nominal, 80% RAM, 70% storage | Measure under steady-state traffic |

---

## 2. Functional Requirements

### 2.1 Core Messaging [MUST]

| ID | Requirement | Definition | Test Criteria |
|----|-------------|------------|---------------|
| FR-1 | 1:1 messaging | Sender→Recipient text delivery | Send 1000 messages, verify 1000 received |
| FR-2 | Offline storage | Messages stored until recipient connects | Send to offline user, verify delivery on connect |
| FR-3 | Delivery ACK | Server ACKs to sender on durable write | Verify ACK only after persistence |
| FR-4 | Read receipts | Optional recipient→sender notification | Toggle setting, verify behavior |
| FR-5 | Message ordering | Sender-assigned sequence per conversation, FIFO delivery | Send M1,M2,M3; verify received in order |

### 2.2 Presence [MUST]

| ID | Requirement | Definition | Propagation SLA |
|----|-------------|------------|-----------------|
| FR-6 | Online status | User visible as online after connect | ≤30 seconds |
| FR-7 | Last seen | Timestamp updated on disconnect | ≤30 seconds |
| FR-8 | Typing indicator | Real-time, best-effort | ≤2 seconds |

### 2.3 Authentication [MUST]

| ID | Requirement | Definition | SLA |
|----|-------------|------------|-----|
| FR-9 | Token-based auth | JWT with HMAC-SHA256 signature | Validate on every request |
| FR-10 | Token expiry | Maximum 24-hour lifetime | Reject expired tokens |
| FR-11 | Token revocation | Propagate to all nodes | ≤60 seconds globally |

### 2.4 Future Scope (Deferred to RFC-002)

The following are explicitly **OUT OF SCOPE** for RFC-001:
- Group messaging
- Multi-device sync
- Media messages
- Voice/video calls
- End-to-end encryption

---

## 3. Non-Functional Requirements

### 3.1 Performance [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-1 | Connection latency | ≤100ms | TCP handshake + TLS + login |
| NFR-2 | Delivery latency (in-region) | ≤100ms P99 | End-to-end timer |
| NFR-3 | Delivery latency (cross-region) | ≤500ms P99 | End-to-end timer |
| NFR-4 | Reconnect storm handling | 100K/sec | Inject reconnects, measure success rate |
| NFR-5 | Throughput per node | 100K msg/sec | Sustained for 10 minutes |

### 3.2 Reliability [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-6 | Message durability | 99.999% | Acknowledged messages never lost |
| NFR-7 | Availability | 99.99% | Monthly error budget: 4.32 minutes |
| NFR-8 | Data loss on crash | ZERO | Kill -9 any node, verify all ACKed messages recovered |
| NFR-9 | Failover time | ≤30 seconds | Kill primary, measure recovery |

### 3.3 Scalability [MUST]

| ID | Metric | Target |
|----|--------|--------|
| NFR-10 | Connections per edge node | ≥100K (target 200K) |
| NFR-11 | Horizontal scaling | Config-only (no code changes) |
| NFR-12 | Regional expansion | Config-only (no code changes) |
| NFR-13 | User sharding | hash(user_id) % num_shards |

### 3.4 Security [MUST]

| ID | Metric | Requirement |
|----|--------|-------------|
| NFR-14 | TLS encryption | **MANDATORY** TLS 1.3 for all client connections |
| NFR-15 | Inter-node encryption | **MANDATORY** mTLS for all internal traffic |
| NFR-16 | JWT validation | Signature + expiry + revocation check on every request |
| NFR-17 | Rate limiting | Per-user limits enforced at Edge |
| NFR-18 | Input validation | All protocol fields validated before processing |

### 3.5 Efficiency [SHOULD]

| ID | Metric | Target |
|----|--------|--------|
| NFR-19 | Memory per connection | ≤10KB |
| NFR-20 | CPU utilization nominal | 50% (allows 2× headroom for bursts) |
| NFR-21 | Cost per connection | ≤$0.0001/month |

---

## 4. Architecture Constraints

### 4.1 Topology

| Constraint | Requirement | Rationale |
|------------|-------------|-----------|
| **AC-1** | Hub-and-Spoke topology | Erlang full mesh fails at >100 nodes |
| **AC-2** | Edge → Core via internal LB | No direct Edge-to-Edge routing |
| **AC-3** | No Distributed Erlang for data plane | Use explicit RPC/messaging |

### 4.2 Layer Requirements

| Layer | Statefulness | Horizontal Scaling |
|-------|--------------|-------------------|
| Edge | Stateless (session only) | Add nodes freely |
| Core | Stateful (sharded) | Reshard to scale |
| Storage | Durable, replicated | Per storage tier |

### 4.3 Storage Requirements (Technology-Agnostic)

| Requirement | Value |
|-------------|-------|
| Write durability | Acknowledged writes MUST survive node failure |
| Read latency | ≤10ms P99 for presence lookups |
| Write latency | ≤50ms P99 for message storage |
| Consistency | Eventual (30s max for presence), Strong (for messages) |

---

## 5. Delivery Guarantees

### 5.1 Message Semantics

| Guarantee | Definition | Implementation Requirement |
|-----------|------------|---------------------------|
| At-Least-Once | Every ACKed message delivered ≥1 time | Retry until client ACK |
| Idempotency | Duplicate detection for retries | Server-side message ID dedup |
| Ordering | FIFO per (sender, recipient) pair | Sender-assigned sequence numbers |

### 5.2 Message IDs

- **Format**: Globally unique, sortable (UUIDv7 or Snowflake)
- **Scope**: Unique across all time, all users
- **Dedup Window**: 7 days

### 5.3 Durability Contract

| Event | Required Behavior |
|-------|-------------------|
| Server ACKs message | Message MUST be durable (survives any single failure) |
| Node crash before ACK | Message MAY be lost (client retries) |
| Node crash after ACK | Message MUST NOT be lost |

---

## 6. Security Model

### 6.1 Threat Model

| Threat | Control |
|--------|---------|
| Eavesdropping | TLS 1.3 mandatory |
| MITM | Certificate validation |
| Replay attacks | Nonce + timestamp validation |
| Token theft | Short expiry (24h) + revocation |
| Internal lateral movement | mTLS between all nodes |

### 6.2 Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│ UNTRUSTED: Internet, Client devices, User input            │
└─────────────────────────────────────────────────────────────┘
                              │
                       TLS 1.3 (MANDATORY)
                              │
┌─────────────────────────────────────────────────────────────┐
│ EDGE: Connection handling, Protocol parsing, JWT validation │
└─────────────────────────────────────────────────────────────┘
                              │
                       mTLS (MANDATORY)
                              │
┌─────────────────────────────────────────────────────────────┐
│ CORE: State management, Routing decisions, Storage          │
└─────────────────────────────────────────────────────────────┘
```

### 6.3 Authentication Flow

1. Client opens TLS connection to Edge
2. Client sends JWT token
3. Edge validates: signature, expiry, revocation status
4. On failure: connection rejected with error code
5. On success: session established

---

## 7. Failure Semantics

### 7.1 Failure Modes and Behaviors

| Failure | Detection | Recovery | Data Impact |
|---------|-----------|----------|-------------|
| Edge node crash | Health check (5s) | LB removes; clients reconnect | Zero (stateless) |
| Core node crash | Health check (5s) | Replica promoted | Zero (replicated) |
| Storage node crash | Replication timeout | Read from replica | Zero (replicated) |
| Network partition | Timeout (30s) | Route to alternate path | Queued, delivered later |
| Full region outage | Monitoring alert | DNS failover | Queued in other regions |

### 7.2 Graceful Degradation Hierarchy

Under overload, disable in order:
1. Typing indicators (FR-8)
2. Presence updates (FR-6, FR-7)
3. Read receipts (FR-4)
4. **NEVER disable**: Message delivery (FR-1, FR-2, FR-3)

---

## 8. Abuse Prevention

### 8.1 Rate Limits

| Resource | Limit | Window |
|----------|-------|--------|
| Messages sent | 100/minute | Per user |
| Connections | 5/minute | Per IP |
| Failed logins | 10/hour | Per account |

### 8.2 Spam Controls

| Control | Implementation |
|---------|----------------|
| Message rate limiting | Token bucket per user |
| Connection rate limiting | Per-IP throttling at Edge |
| Reputation system | Deferred to RFC-003 |

---

## 9. Client Protocol

### 9.1 Version Negotiation

1. Client sends: `{version: [1, 2], capabilities: [...]}`
2. Server responds: `{version: 1, capabilities: [...]}`
3. Both use negotiated version for session

### 9.2 Sync Protocol

1. On connect: Client sends last-seen sequence number
2. Server sends all messages since that sequence
3. Client ACKs received messages
4. Server transitions to push mode

### 9.3 Wire Format (v1)

| Field | Size | Description |
|-------|------|-------------|
| Opcode | 1 byte | Message type |
| Length | 2 bytes | Payload length |
| Payload | Variable | Opcode-specific |

---

## 10. Capacity Model

### 10.1 Inputs (Configurable)

| Parameter | Default | Range |
|-----------|---------|-------|
| Concurrent ratio | 10% of DAU | 5-15% |
| Connections per Edge | 100K | 50K-200K |
| Users per Core shard | 100K | 50K-200K |
| Safety multiplier | 1.5× | 1.2-2.0× |

### 10.2 Formula

```
Edge Nodes = (DAU × concurrent_ratio) / conn_per_edge × safety_multiplier
Core Shards = DAU / users_per_shard × safety_multiplier
Core Nodes = Core Shards × 2 (primary + replica)
```

---

## 11. Compatibility

### 11.1 Version Support Matrix

| Server Version | Client v1 | Client v2 |
|----------------|-----------|-----------|
| Server v1 | ✅ | ✅ (fallback) |
| Server v2 | ✅ (compat) | ✅ |

### 11.2 Rules

1. New fields MUST be optional
2. Old clients MUST work with new servers for 2 versions
3. Breaking changes require version bump and migration period

---

## 12. Testing Requirements

### 12.1 Coverage Matrix

| Requirement | Test Type | Determinism |
|-------------|-----------|-------------|
| Every FR-* | Integration test | MUST be deterministic |
| Every NFR-* | Performance test | MUST have pass/fail threshold |
| Failure modes | Chaos test | MUST inject real failures |

### 12.2 Test Quality Rules

1. Tests MUST NOT use `time.sleep()` for synchronization
2. Tests MUST fail on any data loss
3. Tests MUST be runnable in CI without human intervention

---

## 13. Non-Requirements (Explicit Scope Exclusions)

| Item | Rationale | Future Consideration |
|------|-----------|---------------------|
| End-to-end encryption | Requires client changes | RFC-002 |
| GDPR compliance | Regional requirement | Post-launch |
| Federation | Single org only | Never for v1 |
| Rich media processing | Store-and-forward only | RFC-002 |

> **Note**: "Global-scale" refers to capacity, not security posture. E2E encryption is planned for v2.

---

## Appendix A: Glossary

| Term | Definition |
|------|------------|
| Edge | Connection-handling node, stateless |
| Core | State-managing node, sharded |
| Shard | Partition of user data |
| ACK | Acknowledgment from receiver |
| DAU | Daily Active Users |

## Appendix B: SLO/SLI Definitions

| SLI | Good Event | Total Events | Target SLO |
|-----|------------|--------------|------------|
| Availability | Successful message delivery | All delivery attempts | 99.99% |
| Latency | Delivery ≤100ms | In-region deliveries | 99% |
| Durability | Message not lost after ACK | All ACKed messages | 99.999% |

**Error Budget** (monthly):
- Availability: 4.32 minutes downtime
- Durability: 10 messages lost per 1M sent

## Appendix C: Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-18 | 1.0 | Initial RFC |
| 2026-01-18 | 2.0 | Post-critique revision: removed status, added sections |

---

**Approval**:

- [ ] Engineering Lead
- [ ] Security Review
- [ ] Operations Review

---

*This RFC supersedes all previous audit documents as the authoritative requirements reference.*
