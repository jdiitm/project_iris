# Cold-Start System Re-Audit (Codex 5.2)

This audit treats the repository as if seen for the first time. Tests are treated as hypotheses, documentation as claims. The target objective is a WhatsApp-class system at global scale (billions of users), with low latency, strong reliability, and robust security under adversarial conditions.

Note on scope gaps: Some artifacts are binary or ignored by `.gitignore` and cannot be meaningfully reviewed (`config/tls/iris.crt`, `config/tls/iris.key`, `check_sim.beam`, `offline_msgs.dets`, embedded binaries inside `rebar3`). Their presence is treated as operational risk because their behavior cannot be verified from source.

## Executive Verdict

No. The system, as written, does not meet its stated objectives, and it will fail long before WhatsApp-scale conditions. The design couples global state to synchronous RPC, relies on Mnesia as a multi-region truth source, and lacks the security, identity, and operational controls required for hostile environments. The most immediate failure is data loss on restart and failover.

**What breaks first if deployed today**
- **Data loss on core restart**: `iris_core:init_db/0` deletes local schema (`mnesia:delete_schema`) and `auto_init_db` is enabled in the core app env (`ebin/iris_core.app`). Any restart or mis-sequenced boot can wipe local state. The system has no guardrails to prevent this.

**Top failure vectors at scale**
- **Synchronous core RPC on the hot path** (`iris_router_worker.erl`, `iris_circuit_breaker.erl`) turns latency and failure into system-wide stalls.
- **Global state on Mnesia** (`iris_core.erl`, `iris_offline_storage.erl`) guarantees contention, replication lag, and split-brain data drift under real-world partitions.
- **Security model is non-viable**: shared Erlang cookie, client-issued JWTs, optional TLS, no E2EE, and no abuse containment (`iris_auth.erl`, configs, test clients).

## Failure Map

Ordered by likelihood and blast radius.

1. **Core restart wipes user state**
   - Condition: any core restart or mis-sequenced boot.
   - Failure: Mnesia schema deletion and re-init destroy local state; message loss becomes permanent.
   - Evidence: `iris_core.erl`, `ebin/iris_core.app` (`auto_init_db` true).

2. **Latency collapse under any WAN jitter**
   - Condition: inter-region RTT spikes or transient packet loss.
   - Failure: synchronous `rpc:call` stack multiplies latency; router workers block; cascading backlog.
   - Evidence: `iris_router_worker.erl`, `iris_circuit_breaker.erl`, `docs/PROOF_OF_READINESS.md` claims contradict test artifacts.

3. **Split-brain divergence and message loss**
   - Condition: core partition or core failover.
   - Failure: Mnesia replication lag and divergent state; offline storage and presence drift.
   - Evidence: `iris_core.erl`, `iris_offline_storage.erl`, `tests/suites/resilience/test_resilience.py`, `failover_proof_evidence.csv`.

4. **Security compromise through shared secrets**
   - Condition: attacker obtains Erlang cookie (`iris_secret`) or JWT HMAC secret.
   - Failure: full remote code execution via distributed Erlang; identity forgery.
   - Evidence: hard-coded cookie usage in scripts/tests; JWT secrets in clients (`iris_client.py`, `messi_client.py`, `tests/utilities/iris_client.py`).

5. **Unbounded connection or ACK state growth**
   - Condition: high fan-in or abusive clients.
   - Failure: per-connection process overhead + `pending_acks` growth exhaust memory; node crash.
   - Evidence: `iris_edge_conn.erl`, `tests/suites/stress/test_churn.py`, `tests/suites/stress/stress_hotspot.py`.

6. **Metrics and test artifacts falsely indicate success**
   - Condition: any attempt to validate system behavior using the included proof scripts.
   - Failure: zero-traffic or malformed metrics still marked PASS; operational decisions made on false signals.
   - Evidence: `tests/suites/tokyo_assurance/proof_outputs/*.csv`, `TOKYO_PROOF_REPORT.md`, `tokyo_scalability_metrics.csv`.

## Critical Weaknesses

### Objective–Reality Mismatch
- **E2EE is absent**: There is no key exchange, no ratchet, no client-side cryptography. This violates the baseline security requirements. (`README.md`, `docs/GLOBAL_SCALE_ARCHITECTURE.md` vs codebase)
- **Media delivery and multi-device sync are unimplemented**: Protocol and storage model handle simple text payloads only. No blob store, no chunking, no device fan-out. (`iris_proto.erl`, `iris_session.erl`)
- **Global routing is not region-aware**: Routing uses `phash2` and local ETS without durable global location mapping. This cannot support billions of users with locality. (`iris_core_registry.erl`, `iris_async_router.erl`)

### Architectural Soundness
- **Synchronous RPC dependency chain**: Edge routing calls core RPC synchronously, then core does Mnesia reads/writes. Every hop is blocking, making latency and failure multiplicative. (`iris_router_worker.erl`, `iris_core.erl`)
- **Mnesia as global state**: Mnesia is not a planetary-scale, multi-region, partition-tolerant store. The design cannot survive WAN partitions or high write volume. (`iris_core.erl`, `iris_offline_storage.erl`)
- **Presence model is local and inconsistent**: ETS caches are local to each edge node and never globally reconciled; presence queries return stale or incorrect results under churn. (`iris_edge_sup.erl`, `iris_async_router.erl`, `iris_router_worker.erl`)

### Code & Design Integrity
- **Schema deletion on startup**: `mnesia:delete_schema/1` in `iris_core:init_db/0` destroys state on boot. This is catastrophic for any production system.
- **Circuit breaker as a serialized bottleneck**: `gen_server:call` in `iris_circuit_breaker` gates every RPC decision on a single process. Under load, it becomes the bottleneck.
- **Stale or inconsistent build artifacts**: `ebin/iris_core.app` references `iris_rocksdb` (missing in `src/`), and `ebin/iris_edge.app` omits modules present in source. Runtime behavior is undefined or stale.
- **Protocol framing mismatch in tests**: Multiple tests parse raw TCP as strings or count substrings instead of decoding frames, creating false confidence. (`tests/suites/stress/stress_geo_scale.py`, `stress_global_fan_in.py`)

### Scalability & Resource Behavior
- **One process per connection**: At 100M+ connections, per-connection Erlang processes alone exhaust memory and schedulers; no pooling or connection multiplexing exists. (`iris_edge_conn.erl`)
- **Backpressure is partial**: `pending_acks` is bounded only if config is set; default behavior allows growth, and message queues are unbounded elsewhere.
- **Global coordination overhead**: `pg` and `rpc` become coordination hotspots at high node counts. (`iris_core_registry.erl`, `iris_router_worker.erl`)

### Data Correctness & State Drift
- **Offline storage correctness is fragile**: Mnesia bag semantics can generate duplicates; ordering is not guaranteed. (`iris_offline_storage.erl`, `COMPREHENSIVE_REPORT.md`)
- **Bucket reassignment risk**: `phash2` with configurable bucket counts can re-home users without migration, producing "lost" messages. (`iris_core_registry.erl`, stress tests)
- **Split-brain writes are not reconciled**: There is no conflict resolution for concurrent writes during partition; data can silently diverge.

### Failure & Degradation Modes
- **Failover is lossy by design**: In-flight messages during core loss are dropped or duplicated; client-side dedup is not enforced. (`iris_router_worker.erl`, `iris_session.erl`)
- **Recovery depends on manual operator steps**: Scripts require manual intervention and unsafe `killall beam.smp` behavior. (`scripts/start_cluster.sh`, `tests/suites/tokyo_assurance/*`)

### Security, Abuse & Trust Boundaries
- **Erlang cookie is shared and public**: This is effectively a shared root key that enables full remote execution. (`scripts/*.sh`, test suites, docs)
- **JWT secret is shared and client-generated**: Clients mint their own tokens using a default secret, defeating authentication. (`iris_auth.erl`, `iris_client.py`, `messi_client.py`)
- **TLS is optional and disabled in test config**: Plaintext traffic remains the default. (`config/test.config`, `config/tokyo.config`)
- **No abuse controls**: No rate limits, quota enforcement, or anomaly detection at edge.

### Testing Illusions
- **Proof outputs contradict metrics**: Tokyo proof claims 10k stable users while scalability CSV shows zero traffic at that load. (`tests/suites/tokyo_assurance/proof_outputs/*`)
- **Metrics are frequently zeros**: Resource and latency CSVs show 0 for CPU/memory or sent/recv under "PASS." (`tokyo_resource_metrics.csv`, `tokyo_scalability_metrics.csv`)
- **Tests are not protocol-accurate**: Many tests count byte patterns without framing, allowing silent corruption or loss. (`stress_geo_scale.py`, `stress_global_fan_in.py`)

### Operational Reality
- **No safe recovery plan**: Core schema deletion, manual kill-based scripts, and no automated rollback guarantee data loss during routine ops.
- **Observability is insufficient**: Logs exist but are not correlated; no reliable tracing or error budget enforcement. (`tests/framework/logger.py`, `docs/PROOF_OF_READINESS.md`)

## Non-Obvious Risks

- **Stale compiled artifacts embedded in repo**: `ebin/*.app` contents do not match current source, implying deployments may run outdated code without obvious failures.
- **Embedded dependency blobs**: `rebar3` is a bundled escript with embedded binaries; it cannot be fully audited here and can mask dependency risk.
- **Token issuance on clients**: Tests and tools issue JWTs using the same secret as the server, hiding auth failures in production.
- **False success signals**: CSVs and reports frequently show PASS when the underlying telemetry is missing or zero, encouraging production decisions on invalid evidence.
- **Inconsistent ports and protocols**: Test suites hardcode mismatched ports (8080 vs 8085) and legacy opcodes, making results non-reproducible.

## Closing Judgment

This system is **architecturally fragile** and **operationally unsafe** for its stated goals. It might function as a learning prototype on a single machine with manual supervision, but it is not a credible foundation for WhatsApp-class scale. The primary failure modes are not edge cases; they are guaranteed under normal operations (restarts, partitions, latency spikes, and abuse). In its current form, it does not deserve production deployment.
