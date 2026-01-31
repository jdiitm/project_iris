# RFC-001: Project Iris — Global-Scale Messaging Platform

**Status**: Draft v3  
**Authors**: System Architecture Team  
**Created**: 2026-01-18  
**Last Updated**: 2026-01-30  
**Revision**: 3.0 (Canonical Architecture Alignment)

---

## Abstract

This RFC defines normative requirements for Project Iris, a global-scale messaging platform targeting 5 Billion DAU. All guarantees are defined in measurable terms with explicit testability criteria.

> **Scope**: This document contains requirements only. Implementation status is tracked separately.

---

## 0. Design Philosophy

> **"Dumb Pipe, Smart Edge"** — The server is highly durable, available, ordered storage. The client is the source of truth for encryption and view state.

### 0.1 Core Tenets

| Tenet | Definition | Implication |
|-------|------------|-------------|
| **Server as Log** | The server is an append-only, ordered log per user | No server-side message processing beyond routing |
| **Client as Oracle** | Encryption, decryption, and view state live on client | Server sees only opaque binary blobs |
| **Sync over Push** | Synchronization is the primary primitive | Push notifications are optimizations, not guarantees |
| **Untrusted Storage** | Server cannot read message content | E2EE is mandatory for user-generated content |

### 0.2 Mental Model

```
┌─────────────────────────────────────────────────────────────────────┐
│                         CLIENT DEVICE                                │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                  │
│  │ Encryption  │  │ View State  │  │ Cursor      │                  │
│  │ (X3DH/DR)   │  │ (UI)        │  │ (Last Seen) │                  │
│  └─────────────┘  └─────────────┘  └─────────────┘                  │
└─────────────────────────────────────────────────────────────────────┘
                              │
                    Opaque Encrypted Blobs
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         SERVER (Iris)                                │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                    Inbox Log (per User)                      │    │
│  │  [Msg1] → [Msg2] → [Msg3] → [Msg4] → ... → [MsgN]           │    │
│  │   ↑                                          ↑               │    │
│  │ Offset 0                               Offset N              │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                      │
│  Operations: Append(UserID, Blob) → Offset                          │
│              Scan(UserID, AfterOffset, Limit) → [Blob]              │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 1. Fundamental Invariants

These invariants are **non-negotiable**. Any implementation violating them is incorrect.

### 1.1 The Inbox Invariant

Every user $U$ has a strictly ordered, append-only **Inbox Log** ($L_u$).

| Property | Requirement | Verification |
|----------|-------------|--------------|
| **Write** | Sending to $U$ = Append to $L_u$ | Trace message flow |
| **Read** | $U$ reads = Sequential consumption from $L_u$ | Verify FIFO delivery |
| **Ordering** | Messages have strictly monotonic IDs (HLC) | Assert `ID[n] < ID[n+1]` |
| **Serialization** | Writes to single Inbox are serialized | Concurrent write test |

### 1.2 The At-Least-Once Invariant

The network is adversarial. We guarantee **At-Least-Once** delivery.

| Responsibility | Requirement |
|----------------|-------------|
| **Client** | Generate unique `idempotency_key` (UUIDv7) for every action |
| **Client** | Retry indefinitely until durable ACK received |
| **Server** | Atomically deduplicate by `(user_id, idempotency_key)` |
| **Server** | Dedup window: 7 days minimum |

### 1.3 The End-to-End Invariant

The server is **Untrusted Storage**.

| Property | Requirement |
|----------|-------------|
| **Encryption** | All user content encrypted using Signal Protocol (X3DH + Double Ratchet) |
| **Transparency** | Server acts as switchboard for opaque binary blobs |
| **Metadata** | Server knows WHO talks to WHOM and WHEN, but never WHAT |

---

## 2. System Goals

### 2.1 North Star Metrics

| Metric | Target | Measurement Definition |
|--------|--------|------------------------|
| Daily Active Users | 5 Billion | Unique users sending ≥1 message/day |
| Concurrent Connections | 500 Million | Active TCP connections at peak |
| Message Throughput | 10M msg/sec | Messages processed by Core layer |
| Delivery Latency (in-region) | ≤100ms P99 | Sender submit → Receiver client ACK |
| Delivery Latency (cross-region) | ≤500ms P99 | Including network transit time |
| Message Durability | 99.999% | No acknowledged message lost after server ACK |
| Availability | 99.99% | Measured as successful message deliveries / attempts |

### 2.2 Design Principles

| Principle | Definition | Testability |
|-----------|------------|-------------|
| Scale via Config | 10× capacity increase requires config-only changes | Add 10 nodes via config, measure throughput |
| Fault Tolerance | Survive any single component failure | Kill any node, verify zero message loss |
| Graceful Degradation | Under overload: reduce features, maintain durability | Inject 2× load, verify durability maintained |
| Resource Efficiency | 50% CPU nominal, 80% RAM, 70% storage | Measure under steady-state traffic |

---

## 3. Functional Requirements

### 3.1 Core Messaging [MUST]

| ID | Requirement | Definition | Test Criteria |
|----|-------------|------------|---------------|
| FR-1 | 1:1 messaging | Sender→Recipient text delivery | Send 1000 messages, verify 1000 received |
| FR-2 | Offline storage | Messages appended to recipient's Inbox Log | Send to offline user, verify delivery on connect |
| FR-3 | Delivery ACK | Server ACKs to sender on durable write | Verify ACK only after persistence |
| FR-4 | Read receipts | Optional recipient→sender notification | Toggle setting, verify behavior |
| FR-5 | Message ordering | HLC-based ordering, FIFO per sender | Send M1,M2,M3; verify received in order |

### 3.2 Presence [MUST]

| ID | Requirement | Definition | Propagation SLA |
|----|-------------|------------|-----------------|
| FR-6 | Online status | User visible as online after connect | ≤30 seconds |
| FR-7 | Last seen | Timestamp updated on disconnect | ≤30 seconds |
| FR-8 | Typing indicator | Real-time, best-effort | ≤2 seconds |

> **Note**: Presence is **pull-on-demand** or **interest-based subscription** to avoid O(N²) fan-out at scale.

### 3.3 Authentication [MUST]

| ID | Requirement | Definition | SLA |
|----|-------------|------------|-----|
| FR-9 | Token-based auth | JWT with HMAC-SHA256 signature | Validate on every request |
| FR-10 | Token expiry | Maximum 24-hour lifetime | Reject expired tokens |
| FR-11 | Token revocation | Propagate to all nodes | ≤60 seconds globally |

### 3.4 Sync Protocol [MUST]

The Sync Protocol is the **primary** mechanism for message delivery.

| Step | Client Action | Server Action |
|------|---------------|---------------|
| 1. Connect | Open TLS connection, send JWT | Validate, establish session |
| 2. Hello | Send `(User_ID, Last_Known_Cursor)` | Query Inbox Log |
| 3. Catchup | - | Stream: `SELECT * FROM Inbox WHERE ID > Cursor` |
| 4. Paginate | ACK each page | Continue until caught up |
| 5. Ready | Persist new cursor locally | Enter "Push Mode" |

**Invariant**: If connection drops, client resumes from persisted `Last_Known_Cursor`. No messages lost.

### 3.5 Future Scope (See Amendment 001)

The following are covered in RFC-001-AMENDMENT-001:
- End-to-end encryption (FR-12 through FR-16)
- Group messaging (FR-17 through FR-23)
- Group E2EE with Sender Keys

---

## 4. Non-Functional Requirements

### 4.1 Performance [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-1 | Connection latency | ≤100ms | TCP handshake + TLS + login |
| NFR-2 | Delivery latency (in-region) | ≤100ms P99 | End-to-end timer |
| NFR-3 | Delivery latency (cross-region) | ≤500ms P99 | End-to-end timer |
| NFR-4 | Reconnect storm handling | 100K/sec | Inject reconnects, measure success rate |
| NFR-5 | Throughput per node | 100K msg/sec | Sustained for 10 minutes |

### 4.2 Reliability [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-6 | Message durability | 99.999% | Acknowledged messages never lost |
| NFR-7 | Availability | 99.99% | Monthly error budget: 4.32 minutes |
| NFR-8 | Data loss on crash | Zero acknowledged messages lost (RPO=0) | Kill -9 any node, verify all ACKed messages recovered |
| NFR-9 | Failover time | ≤30 seconds | Kill primary, measure recovery |

### 4.3 Scalability [MUST]

| ID | Metric | Target |
|----|--------|--------|
| NFR-10 | Connections per edge node | ≥100K (target 200K) |
| NFR-11 | Horizontal scaling | Config-only (no code changes) |
| NFR-12 | Regional expansion | Config-only (no code changes) |
| NFR-13 | User sharding | Consistent hash with vnodes (see Section 5.2) |

### 4.4 Security [MUST]

| ID | Metric | Requirement |
|----|--------|-------------|
| NFR-14 | TLS encryption | **MANDATORY** TLS 1.3 for all client connections |
| NFR-15 | Inter-node encryption | **MANDATORY** mTLS for all internal traffic |
| NFR-16 | JWT validation | Signature + expiry + revocation check on every request |
| NFR-17 | Rate limiting | Per-user limits enforced at Edge |
| NFR-18 | Input validation | All protocol fields validated before processing |

### 4.5 Efficiency [SHOULD]

| ID | Metric | Target |
|----|--------|--------|
| NFR-19 | Memory per connection | ≤10KB |
| NFR-20 | CPU utilization nominal | 50% (allows 2× headroom for bursts) |
| NFR-21 | Cost per connection | ≤$0.0001/month |

### 4.6 Observability [MUST]

| ID | Metric | Requirement |
|----|--------|-------------|
| NFR-30 | Distributed tracing | Every RPC MUST propagate `trace_id` |
| NFR-31 | Span timing | Every operation MUST emit `span_id` with duration |
| NFR-32 | Standard counters | MUST emit: `msg_in`, `msg_out`, `ack_sent`, `dedup_hit` |
| NFR-33 | Latency histograms | MUST emit: `e2e_latency`, `db_write_latency` P50/P90/P99 |

---

## 5. Architecture

### 5.1 Component Topology

| Component | Responsibility | Statefulness | Scalability |
|-----------|----------------|--------------|-------------|
| **Edge** | TLS termination, Auth, Connection hold | Stateless | Horizontal (Geo-DNS) |
| **Router** | Route between Edge and Core, Backpressure | Stateless | Horizontal |
| **Core** | Business logic, Fan-out, Inbox appending | Stateful (Sharded) | Partition by User ID |
| **Store** | Durable message persistence (Log/KV) | Stateful (Replicated) | Storage tiering |

```
┌─────────────────────────────────────────────────────────────────────┐
│                              CLIENTS                                 │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                            TLS 1.3 / Geo-DNS
                                   │
┌─────────────────────────────────────────────────────────────────────┐
│  EDGE LAYER (Stateless)                                              │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐                    │
│  │ Edge-1  │ │ Edge-2  │ │ Edge-3  │ │ Edge-N  │                    │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘                    │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                              Internal LB
                                   │
┌─────────────────────────────────────────────────────────────────────┐
│  CORE LAYER (Sharded by User ID)                                     │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐        │
│  │ Shard 0-999     │ │ Shard 1000-1999 │ │ Shard N...      │        │
│  │ (Primary+Replica)│ │ (Primary+Replica)│ │ (Primary+Replica)│       │
│  └─────────────────┘ └─────────────────┘ └─────────────────┘        │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                              Quorum Writes
                                   │
┌─────────────────────────────────────────────────────────────────────┐
│  STORAGE LAYER (Replicated)                                          │
│  Inbox Logs: Append(UserID, Blob) → Offset                          │
│              Scan(UserID, AfterOffset, Limit) → [Blob]              │
│  Durability: R + W > N (N=3, W=2, R=2)                              │
└─────────────────────────────────────────────────────────────────────┘
```

### 5.2 Partitioning Strategy

User data is partitioned by **User ID** (not conversation ID).

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| **Partition Key** | User ID | "Sync" is dominant operation; users verify their own messages |
| **Hash Function** | Jump Consistent Hash | Minimal data movement on scale (1/N+1 vs 100%) |
| **Virtual Nodes** | 256-1024 per physical | Smooth distribution, easy rebalancing |
| **Rebalancing** | Background, throttled | Never impact live traffic |

**Why User ID, not Conversation ID?**
- 90% of operations are "sync my inbox" (ListNewMessages)
- Partitioning by User ID makes this a single-shard query
- Cross-user messages fan-out to multiple shards (async, parallel)

### 5.3 Storage Semantics

The storage layer provides **log-centric** operations:

| Operation | Signature | Semantics |
|-----------|-----------|-----------|
| **Append** | `Append(LogID, Item) → Offset` | Add item, return monotonic offset |
| **Scan** | `Scan(LogID, AfterOffset, Limit) → [Item]` | Read items after offset |
| **Durability** | Quorum: R + W > N | N=3, W=2, R=2 typical |

### 5.4 Message IDs (Hybrid Logical Clock)

Message IDs use **Hybrid Logical Clocks (HLC)** for cross-region ordering without coordination.

```
┌────────────────────────────────────────────────────────────┐
│                    HLC Message ID (64 bits)                 │
├────────────────────┬──────────────┬────────────────────────┤
│ Physical Time (ms) │ Logical Ctr  │ Node ID                │
│     48 bits        │   8 bits     │    8 bits              │
└────────────────────┴──────────────┴────────────────────────┘
```

| Property | Guarantee |
|----------|-----------|
| **Total Order (same node)** | `HLC[t] < HLC[t+1]` always |
| **Causal Order (cross-node)** | If A→B (happens-before), then `HLC[A] < HLC[B]` |
| **No Coordinator** | Each node generates IDs independently |
| **Clock Skew Tolerance** | Logical counter handles skew up to 30s |

---

## 6. Delivery Guarantees

### 6.1 Message Semantics

| Guarantee | Definition | Implementation |
|-----------|------------|----------------|
| At-Least-Once | Every ACKed message delivered ≥1 time | Retry until client ACK |
| Idempotency | Duplicate detection for retries | Server-side dedup by `(user_id, idempotency_key)` |
| Ordering | FIFO per (sender, recipient) pair | HLC-based ordering |

### 6.2 Dedup Window

- **Hot Tier**: 5-minute ETS cache (instant lookup)
- **Warm Tier**: 7-day bloom filter (probabilistic, space-efficient)
- **Format**: `{user_id, idempotency_key}` → boolean

### 6.3 Durability Contract

| Event | Required Behavior |
|-------|-------------------|
| Server ACKs message | Message MUST be durable (survives any single failure) |
| Node crash before ACK | Message MAY be lost (client retries) |
| Node crash after ACK | Message MUST NOT be lost |

---

## 7. Failure Semantics

### 7.1 Failure Modes and Explicit Behaviors

| Failure | Detection | Behavior | Data Impact |
|---------|-----------|----------|-------------|
| **Edge crash** | Health check (5s) | LB removes; clients reconnect | Zero (stateless) |
| **Core crash** | Health check (5s) | Replica promoted | Zero (replicated) |
| **Storage crash** | Replication timeout | Read from replica | Zero (replicated) |
| **Network partition** | Timeout (30s) | Buffer in Outbox Queue | Queued, delivered on restore |
| **Region outage** | Monitoring alert | DNS failover | Queued in other regions |

### 7.2 Network Partition Behavior

When Region A cannot communicate with Region B:

```
┌─────────────────────────────────────────────────────────────────────┐
│                     PARTITION HANDLING                               │
│                                                                      │
│  User in Region A sends to User in Region B:                        │
│                                                                      │
│  1. Route attempt to Region B fails (timeout)                       │
│  2. Message buffered in "Outbox Queue" in Region A                  │
│  3. Queue properties:                                               │
│     - FIFO per destination user                                     │
│     - TTL: 7 days                                                   │
│     - Overflow: Reject new messages (backpressure)                  │
│  4. On link restore: Drain queue in order                           │
│  5. Dedup at destination handles any duplicates                     │
│                                                                      │
│  Invariant: AVAILABILITY over CONSISTENCY                            │
│  (Temporary divergence acceptable; eventual delivery guaranteed)     │
└─────────────────────────────────────────────────────────────────────┘
```

### 7.3 Catastrophic Failure (Region Loss)

| Attribute | Value |
|-----------|-------|
| **RPO** | Near-zero (async replication to backup region) |
| **Failover** | DNS points users to backup region |
| **Conflict Resolution** | Dedup handles re-sent messages |
| **Acceptable Loss** | Minimal duplication better than data loss |

### 7.4 Graceful Degradation Hierarchy

Under overload, disable in order:
1. Typing indicators (FR-8)
2. Presence updates (FR-6, FR-7)
3. Read receipts (FR-4)
4. **NEVER disable**: Message delivery (FR-1, FR-2, FR-3)

---

## 8. Operational Limits (Hard Constraints)

These limits are enforced to maintain system stability at scale.

| Metric | Hard Limit | Rationale |
|--------|------------|-----------|
| **Group Size (E2EE)** | 256 members | N² complexity for Sender Key distribution |
| **Group Size (Broadcast)** | 10,000 members | Server-side fan-out, no E2EE |
| **Payload Size** | 64 KB | Large media → Blob Store with URL + decrypt key |
| **Rate Limit (per user)** | 5 msg/sec sustained | Spam prevention |
| **Rate Limit (burst)** | 20 msg/sec for 10s | Allow typing bursts |
| **Fan-out Rate** | 1,000 Inboxes/sec/worker | Core worker throughput limit |
| **Inbox Size** | 10,000 messages | Oldest messages archived to cold storage |

---

## 9. Security Model

### 9.1 Threat Model

| Threat | Control |
|--------|---------|
| Eavesdropping | TLS 1.3 mandatory |
| MITM | Certificate validation |
| Replay attacks | Nonce + timestamp validation |
| Token theft | Short expiry (24h) + revocation |
| Internal lateral movement | mTLS between all nodes |
| Server reads messages | E2EE (server never sees plaintext) |

### 9.2 Trust Boundaries

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
│       (CANNOT decrypt user content)                         │
└─────────────────────────────────────────────────────────────┘
```

---

## 10. Abuse Prevention

### 10.1 Rate Limits

| Resource | Limit | Window |
|----------|-------|--------|
| Messages sent | 5/sec sustained, 100/minute burst | Per user |
| Connections | 5/minute | Per IP |
| Failed logins | 10/hour | Per account |

### 10.2 Spam Controls

| Control | Implementation |
|---------|----------------|
| Message rate limiting | Token bucket per user |
| Connection rate limiting | Per-IP throttling at Edge |
| Reputation system | Deferred to RFC-003 |

---

## 11. Client Protocol

### 11.1 Version Negotiation

1. Client sends: `{version: [1, 2], capabilities: [...]}`
2. Server responds: `{version: 1, capabilities: [...]}`
3. Both use negotiated version for session

### 11.2 Wire Format (v1)

| Field | Size | Description |
|-------|------|-------------|
| Opcode | 1 byte | Message type |
| Length | 2 bytes | Payload length |
| Payload | Variable | Opcode-specific |

---

## 12. Capacity Model

### 12.1 Inputs (Configurable)

| Parameter | Default | Range |
|-----------|---------|-------|
| Concurrent ratio | 10% of DAU | 5-15% |
| Connections per Edge | 100K | 50K-200K |
| Users per Core shard | 100K | 50K-200K |
| Safety multiplier | 1.5× | 1.2-2.0× |

### 12.2 Formula

```
Edge Nodes = (DAU × concurrent_ratio) / conn_per_edge × safety_multiplier
Core Shards = DAU / users_per_shard × safety_multiplier
Core Nodes = Core Shards × 2 (primary + replica)
```

---

## 13. Testing Requirements

### 13.1 Coverage Matrix

| Requirement | Test Type | Determinism |
|-------------|-----------|-------------|
| Every FR-* | Integration test | MUST be deterministic |
| Every NFR-* | Performance test | MUST have pass/fail threshold |
| Failure modes | Chaos test | MUST inject real failures |
| Invariants | Property-based test | MUST verify Section 1 invariants |

### 13.2 Test Quality Rules

1. Tests MUST NOT use `time.sleep()` for synchronization
2. Tests MUST fail on any data loss
3. Tests MUST be runnable in CI without human intervention
4. Tests MUST use seeded random for determinism

### 13.3 Chaos Testing Requirements (Jepsen-style)

| Injection | Verification |
|-----------|--------------|
| Network partition (netsplit) | Zero violated invariants after recovery |
| Clock skew (±30s) | HLC ordering maintained |
| Node kill (SIGKILL) | Zero ACKed message loss |
| Disk full | Graceful rejection, no corruption |

---

## 14. Compatibility

### 14.1 Version Support Matrix

| Server Version | Client v1 | Client v2 |
|----------------|-----------|-----------|
| Server v1 | ✅ | ✅ (fallback) |
| Server v2 | ✅ (compat) | ✅ |

### 14.2 Rules

1. New fields MUST be optional
2. Old clients MUST work with new servers for 2 versions
3. Breaking changes require version bump and migration period

---

## Appendix A: Glossary

| Term | Definition |
|------|------------|
| Edge | Connection-handling node, stateless |
| Core | State-managing node, sharded by User ID |
| Shard | Partition of user data |
| Inbox Log | Append-only message log per user |
| HLC | Hybrid Logical Clock for ordering |
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
| 2026-01-30 | 3.0 | Canonical architecture alignment: added Design Philosophy, Inbox Log invariants, HLC, explicit failure behaviors, operational limits, observability NFRs |

---

**Approval**:

- [ ] Engineering Lead
- [ ] Security Review
- [ ] Operations Review

---

*This RFC supersedes all previous versions as the authoritative requirements reference.*
