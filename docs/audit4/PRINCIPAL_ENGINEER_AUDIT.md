# Principal Engineer Audit — Global Messaging System

Brutal, production-focused review of the repository as it exists now, with WhatsApp/FAANG-scale expectations. Documentation is treated as claims; tests are treated as hypotheses.

## 1. Repository Understanding & Context

### Product purpose and intent (inferred from code and docs)
- **Purpose**: Real-time messaging engine with presence and offline delivery, aiming at WhatsApp-class reliability and scale.
- **Target users**: Consumer messaging users with mobile churn and unreliable networks.
- **Expected scale**: Implied global usage; code comments and test suites target very high concurrency and multi-region routing.
- **Critical paths**:
  - Client connection handling (`iris_edge_listener.erl`, `iris_edge_conn.erl`)
  - Routing (`iris_router.erl`, `iris_async_router.erl`, `iris_router_worker.erl`)
  - Offline persistence (`iris_offline_storage.erl`)
  - Authentication (`iris_auth.erl`)
- **Non-critical paths**:
  - Metrics and proof scripts under `tests/`
  - Deployment scripts and tuning scripts

### Architecture reconstructed from code
- **Edge nodes** accept TCP/SSL and run per-connection processes.
- **Routing** uses ETS presence and sharded workers; remote routes go through synchronous RPC to core.
- **Core nodes** manage user routing metadata and offline storage via Mnesia.
- **Persistence** uses Mnesia tables for offline messages and status batching.
- **Protocol** is custom binary framing with opcodes for login, send, ack, batch, status.

### Data flow (read/write)
- **Write path**: Client -> Edge -> Router -> Core RPC -> Mnesia.
- **Read path**: Offline fetch on login -> Edge -> Client.
- **Presence**: Edge-local ETS with optional core queries.

### Trust boundaries
- Client boundary at TCP/SSL socket.
- Auth boundary at JWT verification.
- Core boundary via Erlang distribution RPC.

### Dangerous or undocumented assumptions
- **Synchronous RPC** is assumed safe on hot path under WAN latency.
- **Mnesia** is assumed suitable for global, multi-region durability.
- **Shared secrets** (JWT HMAC, Erlang cookie) are assumed safe in hostile environments.
- **Per-connection process model** is assumed to scale without memory collapse.

## 2. Code-Level Review (Line-by-Line Where Critical)

### A. Correctness & Safety
- **Offline storage**: Mnesia `bag` semantics can duplicate; no strict idempotency enforced.
- **Schema safety**: Core init logic deletes schema on startup; restart risks data loss.
- **Ack handling**: Per-connection ack maps can grow unbounded if not configured.
- **Error handling**: RPC failures propagate as blocking calls; some error paths fall back silently.

### B. Readability & Maintainability
- **Responsibility overload**: `iris_edge_conn.erl` handles protocol, buffering, acks, and backpressure.
- **Hidden coupling**: Routing depends on local ETS plus core RPC with implicit state shape.
- **Artifact drift**: `ebin` and src may diverge; builds can run stale code.

### C. Performance & Efficiency
- **Hot path blocking**: `rpc:call` inside `iris_router_worker.erl` adds latency amplification.
- **Process-per-connection**: Memory overhead scales linearly; no multiplexing strategy.
- **ETS cache limits**: Local-only presence cache guarantees cross-node staleness.

### D. Concurrency & Asynchrony
- **Single-process bottlenecks**: Circuit breaker uses `gen_server:call`, serializing decisions.
- **Backpressure**: Partial; some queues are effectively unbounded.
- **Retry storms**: No centralized throttling; repeated RPC calls can cascade.

## 3. Architecture & System Design Review

### Separation of concerns
- Edge, routing, and core are separate modules but tightly coupled by synchronous RPC.
- Storage logic is embedded in core modules without clear interface boundaries.

### Scaling & resilience
- **Stateless edges**: Not truly stateless; presence ETS is local and not reconciled.
- **Idempotency**: No durable idempotency key path for message delivery.
- **Backpressure**: Inconsistent; missing end-to-end enforcement.
- **Circuit breakers**: Present but not designed for high concurrency or partition tolerance.

### Failure modes
- **Single points of failure**: Core state via Mnesia is a global dependency.
- **Blast radius**: Core RPC blocking can freeze edge routing globally under WAN issues.
- **Degradation**: Under partial failure, behavior is unpredictable and often silent.

## 4. Testing Strategy — What Exists vs What Is Missing

### Existing tests
- Unit tests for protocol/session in `test_utils` and `tests/suites/unit`.
- Integration tests for basic messaging and presence.
- Stress and chaos tests in `tests/suites/stress` and `tests/suites/chaos_controlled`.
- Proof-of-readiness suites with metrics output.

### Missing tests required for global standards
- **Ordering**: per-conversation ordering across retries and reconnects.
- **Deduplication**: end-to-end idempotency validation under retry storms.
- **Cross-region durability**: deterministic tests for partition recovery and convergence.
- **Multi-device consistency**: missing entirely.
- **Security negative tests**: token forgery, key rotation, TLS enforcement.
- **Migration safety**: no schema migration and rollback tests.

## 5. Security & Compliance Review

### AuthN/AuthZ
- JWT verification exists but test tooling issues tokens locally, masking failures.
- No multi-tenant isolation model or role-based boundaries.

### Secrets handling
- Shared HMAC secrets and Erlang cookie represent a catastrophic blast radius.

### Input validation
- Protocol parsing exists but downstream message payload validation is minimal.

### Compliance readiness
- No GDPR/SOC2/ISO27001 controls (data retention, audit logs, access control).

## 6. Observability & Operations

### Logging and metrics
- Logs exist but are not correlated by message IDs across nodes.
- Metrics are inconsistently validated; proof outputs contradict logs in places.

### On-call feasibility
- Recovery relies on manual scripts and kill-based operations.
- No runbooks for failover or data corruption scenarios.

## 7. Developer Experience & Repository Hygiene

- README and docs exist but conflict with runtime behavior.
- CI/CD is not clearly defined; linting/formatting enforcement is unclear.
- A new engineer would not be productive within a week without expert guidance.

## 8. Documentation Audit (Missing or Incomplete)

- **Architecture diagrams**: required for understanding edge/core/state flows.
- **ADRs**: no record of key design tradeoffs (Mnesia, RPC, protocol design).
- **API contracts**: protocol contracts and guarantees are under-defined.
- **Data schemas**: Mnesia table definitions and migration plans are not documented.
- **Operational playbooks**: no documented incident response.

## 9. Engineering Maturity Scoring (0–10)

- **Code quality**: 4 — core logic exists but lacks safety and clear boundaries.
- **Architecture**: 3 — global state and synchronous RPC are hard blockers.
- **Testing**: 3 — test breadth exists but lacks correctness assertions.
- **Security**: 2 — shared secrets and optional TLS are unacceptable.
- **Scalability**: 2 — per-connection processes and global RPC will collapse.
- **Reliability**: 2 — persistence and failover are not safe.
- **Developer experience**: 4 — documentation exists but does not align with runtime.
- **Production readiness**: 1 — cannot support global production use.

## 10. Actionable Refactor & Upgrade Plan

### P0 — Immediate fixes
- **Block schema deletion on boot**
  - Why: prevents catastrophic data loss.
  - Risk if ignored: total offline message loss on restart.
  - Effort: 1 day.
  - Impact: prevents data loss incidents.
- **Enforce TLS and server-issued auth**
  - Why: current auth model is invalid.
  - Risk if ignored: account compromise at scale.
  - Effort: 3–5 days.
  - Impact: restores trust boundary.
- **Bound all queues and ack maps**
  - Why: memory exhaustion risk.
  - Risk if ignored: edge crashes under abuse.
  - Effort: 2–3 days.
  - Impact: stabilizes under churn.

### P1 — Short-term improvements
- **Replace synchronous RPC on hot path**
  - Why: latency amplification under WAN conditions.
  - Risk if ignored: global stalls during spikes.
  - Effort: 2–4 weeks.
  - Impact: removes primary throughput ceiling.
- **Add deterministic ordering + dedup tests**
  - Why: guarantees must be testable.
  - Risk if ignored: silent correctness degradation.
  - Effort: 1–2 weeks.
  - Impact: validates critical messaging semantics.
- **Create runbooks and recovery tests**
  - Why: incident response is undefined.
  - Risk if ignored: extended downtime and data loss.
  - Effort: 1–2 weeks.
  - Impact: operational readiness.

### P2 — Long-term architectural changes
- **Move off Mnesia for global state**
  - Why: Mnesia is not a planetary-scale, partition-tolerant store.
  - Risk if ignored: permanent correctness ceiling.
  - Effort: 2–3 months.
  - Impact: enables true multi-region correctness.
- **Introduce durable idempotency layer**
  - Why: retries and duplicate suppression require durable keys.
  - Risk if ignored: persistent duplicates and ordering breaks.
  - Effort: 1–2 months.
  - Impact: aligns with global messaging guarantees.

## Final Verdict

This system is **not** globally production-ready. It has structural correctness and reliability issues that cannot be mitigated with tuning. The architecture must be hardened with deterministic tests, strong auth boundaries, and a durable global storage model before any credible claim of WhatsApp-scale readiness can be made.
