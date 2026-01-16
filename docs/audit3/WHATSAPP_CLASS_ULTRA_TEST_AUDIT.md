# WhatsApp-Class System Audit with Ultra-High Test Coverage Mandate (Codex 5.2)

This audit evaluates the repository as it exists now. Documentation is treated as claims; tests are treated as hypotheses. The system is judged by testability first: if a guarantee cannot be validated with deterministic, reproducible tests under current hardware limits, it is not production-ready.

## 1. System Understanding Summary

### Runtime components and responsibilities
- **Edge ingress**: `iris_edge_listener.erl` accepts TCP/SSL; `iris_edge_conn.erl` manages per-connection protocol handling, buffering, and pending acks; `iris_ws_lite.erl` handles WebSocket connections.
- **Routing layer**: `iris_router.erl` dispatches to `iris_async_router.erl` workers; `iris_router_worker.erl` performs presence lookup and RPC to core.
- **Core services**: `iris_core.erl` manages routing metadata and offline storage access; `iris_core_registry.erl` maps users to core shards via `pg`.
- **Storage**: `iris_offline_storage.erl` stores offline messages in Mnesia tables; status batching in `iris_status_batcher.erl`.
- **Protocol**: `iris_proto.erl` defines custom framing/opcodes; `iris_session.erl` handles session state transitions.
- **Auth**: `iris_auth.erl` performs HMAC JWT verify; client utilities mint JWTs locally (`iris_client.py`, `tests/utilities/iris_client.py`).

### Message lifecycle (derived from code)
1. **Client ingress**: TCP accept -> connection process spawn.
2. **Authentication**: login opcode decoded; JWT verified if present.
3. **Routing**: edge route -> async router -> router worker -> presence lookup -> core RPC.
4. **Persistence**: offline messages stored in Mnesia; delivery on reconnect.
5. **Delivery**: direct send for online; offline fetch on login.
6. **Acknowledgment**: pending ack map per connection; ack opcode clears.
7. **Retry & recovery**: no durable retry queue; recovery relies on Mnesia and client reconnect behavior.

### State transitions and failure boundaries (test-critical)
- **Connection lifecycle**: connected -> authenticated -> sending -> acked -> disconnected.
- **Presence state**: local ETS entry set/unset; no global reconciliation.
- **Offline store**: insert -> fetch -> delete; duplication possible under `bag` semantics.
- **Core RPC boundary**: synchronous call path; failover depends on circuit breaker.

## 2. Goal-to-Testability Mapping

Each goal is invalid unless backed by deterministic tests.

### Explicit/implicit goals (from repo)
- **WhatsApp-class reliability**: low latency, low loss, global scale.
- **Durable offline delivery**: messages persisted and delivered after reconnect.
- **High concurrency**: large numbers of concurrent connections.
- **Security**: authenticated messaging, TLS optional.

### Testability status
- **Low latency**: partially testable via existing throughput tests, but no deterministic latency baselines under controlled load.
  - **Needed**: fixed-load latency suite with reproducible baselines.
- **Durable offline delivery**: partially tested in `tests/suites/integration/test_offline_storage.py` but not validated across failover or Mnesia partition.
  - **Needed**: deterministic failover + persistence validation.
- **High concurrency**: stress tests exist but often do not assert protocol correctness.
  - **Needed**: framed protocol assertions and message integrity checks under load.
- **Security**: JWT tests exist but are invalidated by shared client secrets.
  - **Needed**: server-issued tokens, negative auth tests, TLS enforcement tests.

## 3. Current Test Coverage Assessment

### Tested today
- **Unit**: protocol parsing and session tests (`tests/suites/unit/`, `test_utils/*_tests.erl`).
- **Integration**: online/offline, presence, hotkey bucketing, basic security.
- **Stress/Chaos**: churn, fan-in, geo-scale, resilience suites.
- **Tokyo assurance**: manual proof suites with CSV outputs.

### Partially tested
- **Ordering**: inferred from delivery counts, but not validated per-conversation sequence.
- **Deduplication**: not asserted end-to-end; duplicates are tolerated in docs.
- **Failover**: some scripts exercise failover but do not validate data correctness.

### Untested or untestable today
- **Multi-device consistency**: no model or tests for multi-device sync.
- **Cross-region durability**: no deterministic validation of ordering or loss under partitions.
- **Recovery correctness**: no authoritative replay or idempotency tests.
- **Security boundaries**: TLS off by default in tests; client-side JWT issuance hides server auth failures.

### Coverage illusions
- **Protocol correctness**: several stress tests parse raw TCP payloads without framing, which can pass under corruption.
- **Metrics integrity**: CSV outputs show PASS despite zero or inconsistent traffic in proof outputs.
- **Auth tests**: local token issuance bypasses real auth boundaries.

## 4. FAANG-Plus Gap Analysis (Test-Focused)

### Messaging semantics (test-first)
- **Ordering**: no test enforces per-conversation ordering; FAANG-grade systems require deterministic sequence checks.
- **Delivery guarantees**: no end-to-end proof of “no loss after ack” under failover.
- **Deduplication**: no idempotency key tests across retries and reconnect.
- **Offline behavior**: tested only for single-node happy path.
- **Multi-device**: absent, untestable.

### Failure-driven testing
- **Partitions**: chaos tools exist but lack deterministic assertions.
- **Clock skew**: no test coverage.
- **Delayed acks**: no controlled tests of ack timeouts or resend behavior.

### Scalability under current hardware
- **Concurrency bottlenecks**: synchronous RPC on hot path is not isolated in tests.
- **Resource leaks**: long-running soak tests exist but lack clear invariants and pass/fail thresholds.

### Operational readiness for testability
- **Observability gaps**: no consistent message IDs across logs; testing assertions are not correlated.
- **Reproducibility**: manual steps in tests reduce determinism.

## 5. Final Verdict: Test Readiness for Global Production

**No-Go.** The current system cannot be validated to FAANG-plus standards with the existing tests. It lacks deterministic coverage for ordering, durability under failover, security enforcement, and multi-region correctness. Any claim of production readiness is invalid until those tests exist and pass consistently.
