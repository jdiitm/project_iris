# RFC-001 Compliance Status

**Status**: 169 Python + 185 Erlang tests pass | **TLS Enforced** | **Last Updated**: 2026-02-15

## Verification Status Legend

| Status | Meaning |
|--------|---------|
| **VERIFIED** | Requirement tested with production-realistic conditions |
| **PARTIAL** | Requirement tested with simulation or simplified conditions |
| **TRANSPORT-ONLY** | Protocol layer tested, cryptographic operations not validated |

## Implemented Requirements

| RFC | Requirement | Implementation | Test | Verification |
|-----|-------------|----------------|------|--------------|
| NFR-1 | Connection latency ≤ 100ms | `iris_edge_listener.erl` TLS accept | `measure_dials.py`, `benchmark_unit_cost.py` | **PARTIAL** (dev-scale; production SLA requires load testing) |
| NFR-2 | Delivery latency ≤ 100ms P99 (in-region) | `iris_router.erl` local-first routing | `benchmark_throughput.py`, `benchmark_e2ee_latency.py` | **PARTIAL** (measured at dev scale; P99 at production load unproven) |
| NFR-3 | Cross-region P99 ≤ 500ms | `iris_region_bridge.erl` | `test_cross_region_latency.py` | **VERIFIED** |
| NFR-5 | 100K msg/sec per node | `iris_async_router.erl` worker pool, lock-free ETS | `benchmark_throughput.py` | **PARTIAL** (8K msg/s measured locally; 100K requires production hardware) |
| NFR-6 | 99.999% durability | `store_durable/3` with sync_transaction | `test_ack_durability.py` | **VERIFIED** (SIGKILL) |
| NFR-7 | 99.99% availability | Supervisor trees, circuit breaker, failover | `test_slo_tracking.py`, `error_budget.py` | **PARTIAL** (SLI/SLO metrics implemented; 99.99% requires extended soak validation) |
| NFR-8 | Zero data loss (RPO=0) | ACK after Mnesia commit | `test_ack_durability.py`, `test_multimaster_durability.py` | **VERIFIED** (SIGKILL, multi-node) |
| NFR-9 | Failover ≤ 30s | pg discovery + circuit breaker | `test_failover_time.py` | **VERIFIED** |
| **NFR-14** | **TLS mandatory** | **TLS enforced on all connections** | `test_tls_mandatory.py`, all tests | **VERIFIED** |
| NFR-16 | Clock skew tolerance (30s) | HLC-based ordering | `test_clock_skew.py`, `iris_hlc_tests.erl` | **VERIFIED** (unit) / **PARTIAL** (integration) |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` + `pg` gossip | `test_rate_limiting.py`, `test_distributed_rate_limit.py` | **VERIFIED** |
| NFR-18 | Input validation | E2EE header (`ik`, `ek`), payload 64KB, CBOR schema | `test_cbor_schema_validation.py` | **VERIFIED** |
| NFR-22 | E2EE key durability | Quorum writes | `test_key_bundle_durability.py` | **VERIFIED** |
| NFR-24 | OPK pool alerts | Low OPK + SPK fallback | `test_opk_exhaustion.py` | **VERIFIED** |
| NFR-30 | Distributed tracing | `traced_rpc/4` propagation | `iris_trace_tests.erl` | **VERIFIED** |
| NFR-31 | Span instrumentation | 7 key session operations | `iris_trace_tests.erl` | **IMPLEMENTED** |
| NFR-32 | Standard counters | `msg_in`/`msg_out`/`ack_sent` in session handlers | `iris_metrics_callsite_tests.erl` | **IMPLEMENTED** |
| FR-5 | Message ordering | HLC + `send_msg_seq` (opcode 0x07) | `test_cross_node_ordering.py` | **VERIFIED** |
| FR-20 | Group E2EE | Sender Keys protocol | `test_group_e2ee.py` | **VERIFIED** (crypto: AES-256-GCM + HKDF + X25519) |
| Section 8 | Inbox 10K limit | Guard in `iris_core:store_offline_durable/2` | `iris_inbox_limit_tests.erl` | **IMPLEMENTED** |
| Section 8 | Payload 64KB limit | `iris_limits:validate_payload/1` on E2EE/Group paths | `iris_rfc_v4_constants_tests.erl` | **IMPLEMENTED** |
| Section 7.2 | Outbox 7-day TTL | `cleanup_expired_outbox/0` in drain timer | `iris_region_bridge.erl` | **IMPLEMENTED** |
| Section 7.2 | 50% queue depth alert | `iris_outbox_queue_warning` metric | `iris_metrics.erl` | **IMPLEMENTED** |
| Section 9.1 | JWT replay protection | `jti` nonce tracking in ETS with TTL | `iris_auth.erl` | **IMPLEMENTED** |
| Section 9.1 | JWT key isolation | `auth_mode` signer/verifier | `iris_auth_key_isolation_tests.erl` | **IMPLEMENTED** |
| Section 11.1 | Version negotiation | Opcode 0x0C | `iris_proto_tests.erl` | **IMPLEMENTED** |

### Verification Notes

**NFR-8 (RPO=0 Durability)**:
- `test_ack_durability.py`: SIGKILL simulates hard crash (power loss)
- `test_multimaster_durability.py`: Multi-node replication survives SIGKILL
- Single-node: `sync_transaction` flushes before ACK
- Multi-node (recommended): replication to surviving nodes

**NFR-16 (Clock Skew / HLC)**:
- **Erlang Unit Tests (VERIFIED)**: `iris_hlc_tests.erl` — drift handling, concurrent sends, causal ordering, `MAX_DRIFT_MS = 30000`
- **Python (PARTIAL)**: `test_clock_skew.py` — protocol-level simulation; real NTP injection requires `libfaketime`

**FR-20 (Group E2EE / Double Ratchet)**:
- **Erlang (VERIFIED)**: `iris_ratchet_tests.erl` — forward secrecy, attack resistance, out-of-order delivery, bidirectional DH ratchet
- **Python (VERIFIED crypto)**: `test_group_e2ee.py` — real AES-256-GCM encryption/decryption, HKDF Sender Key chain derivation, X25519 key exchange; `test_post_compromise.py` — 100+ ratchet advances

---

## RFC v4.0 Gap Closures

All gap closures from 2026-02-07 and 2026-02-08 in a single table.

| Gap | RFC Section | Problem | Fix | Test |
|-----|-------------|---------|-----|------|
| P1-7 | Section 1.2 | No UUIDv7 idempotency key validation | `iris_uuid.erl` + CBOR path in `iris_session.erl` | `iris_uuid_tests.erl` (14 tests) |
| — | Section 11.1 | No version/capability negotiation | Opcode 0x0C in `iris_proto.erl` | `iris_proto_tests.erl` (3 tests) |
| — | NFR-30 | trace_id not propagated across RPCs | `traced_rpc/4` in `iris_session.erl` | `iris_trace_tests.erl` (2 tests) |
| — | Section 9.1 | JWT private key on all nodes | `auth_mode` signer/verifier | `iris_auth_key_isolation_tests.erl` (8 tests) |
| — | Section 5.4 | No 64-bit HLC backward compat | `from_binary/1` accepts 8-byte | `iris_hlc_tests.erl` (4 tests) |
| — | Section 6.2 | Bloom-only dedup (no Mnesia cross-check) | Mnesia `dedup_log` cross-check on bloom positive | `iris_dedup_sole_drop_tests.erl` (4 tests) |
| — | Section 3.4 | Session cache unbounded | 100K hard limit, LRU eviction | `iris_session_cache_bound_tests.erl` (5 tests) |
| — | NFR-18 | E2EE header not validated | `validate_e2ee_header/1` requires `ik`, `ek` | `test_cbor_schema_validation.py` (4 tests) |
| — | Section 5.4 | No mixed 64/80-bit HLC test | Structural + live ordering test | `test_hlc_mixed_version.py` (2 tests) |
| — | Amendment 6.3 | No sender key rotation race test | Server resilience during rotation | `test_sender_key_rotation_window.py` (2 tests) |
| — | NFR-19 | No hard fail in memory benchmark | ≤10KB/conn gate at full scale | `benchmark_memory.py` |
| — | NFR-4 | No reconnect rate measurement | Rate calculation in storm test | `test_reconnect_storm.py` |
| GAP-6 | Section 8 | Inbox 10K limit not enforced | Guard in `iris_core:store_offline_durable/2` | `iris_inbox_limit_tests.erl` (3 tests) |
| GAP-7 | Section 8 | 64KB payload limit not enforced on E2EE/Group | `iris_limits:validate_payload/1` in handlers | `iris_rfc_v4_constants_tests.erl` |
| GAP-5 | NFR-32 | `msg_in`/`msg_out`/`ack_sent` not called | 6 counter calls in `iris_session.erl` | `iris_metrics_callsite_tests.erl` (5 tests) |
| GAP-4 | NFR-31 | No span instrumentation | `new_span`/`end_span` on 7 operations | Span metrics via `iris_trace` |
| GAP-1 | Section 7.2 | Outbox TTL not enforced | `cleanup_expired_outbox/0` in drain timer | `OUTBOX_TTL_MS = 604800000` |
| GAP-2 | Section 7.2 | No 50% queue depth alert | `iris_outbox_queue_warning` metric | `iris_metrics.erl` |
| GAP-13 | Amendment 5.3.2 | No key change notification | IK change detection + Mnesia contact tracking + online/offline delivery via opcode 0x1A | `iris_key_change_notify_tests.erl`, `iris_key_change_delivery_tests.erl` |
| GAP-15 | Section 9.1 | No JWT replay protection | `jti` ETS table with TTL cleanup | `iris_auth.erl` |
| GAP-3 | NFR-17 | Distributed rate limiting | Already via `pg` gossip | `test_distributed_rate_limit.py` |

---

## Previously Deferred (Now Resolved)

| Item | Status | Notes |
|------|--------|-------|
| NFR-15 mTLS (inter-node) | **Enforced in production** | `check_mtls_enforcement` defaults `enforce_mtls=true` when `env=production`; `iris_cluster_manager` blocks replication without SSL dist |
| Amendment 5.3.2 Key Change Notification | **IMPLEMENTED** | Detection + contact tracking (Mnesia) + online/offline delivery via opcode 0x1A; tested in `iris_key_change_delivery_tests.erl` |

---

## RFC v4.0 Forensic Audit Fixes (2026-02-09 — 2026-02-10)

| Finding | RFC/NFR | Problem | Fix | Test |
|---------|---------|---------|-----|------|
| F1 | Section 7.1.1 | Split-brain blind overwrite | LWW for `group_member`, union merge for `bag` tables in `iris_core:merge_table_batch/3` | `iris_reconcile_conflict_tests.erl` |
| F2 | NFR-11 | `spawn` breaks FIFO for sequenced messages | Synchronous inline processing in `iris_async_router.erl` | `iris_sequenced_fifo_tests.erl` |
| F3 | NFR-1 | WAL on tmpfs allows RPO=0 violation | Production crashes if WAL is on tmpfs | `iris_wal_tmpfs_enforcement_tests.erl` |
| G1 | NFR-15 | mTLS defaults to `false` | `enforce_mtls=true` in production; cluster manager pre-check | `iris_mtls_production_tests.erl`, `iris_cluster_mtls_tests.erl` |
| A2 | NFR-27 | Group size hardcoded 1000 vs iris_limits 10000 | Use `iris_limits` as single source of truth | `iris_group_size_limits_tests.erl` |
| GAP-1 | Amendment 5.3.1 | Safety number modulo bias | Uniform byte-pair extraction | `iris_safety_number_bias_tests.erl` |
| GAP-2 | Amendment 6.3 | No sender key rotation on member removal | Invalidate all sender keys for remaining members | `iris_group_sender_key_rotation_tests.erl` |
| GAP-3 | Amendment 5.3.2 | Key contacts in RAM-only ETS | Mnesia-persisted `iris_key_contacts` table | `iris_key_contacts_persistence_tests.erl` |
| AQM | Roadmap Phase 2 | No CoDel/RED drop policy | CoDel algorithm in `iris_mailbox_guard.erl` | `iris_codel_tests.erl` |

---

## Security Hardening

| Issue | Fix |
|-------|-----|
| C1: ACK-before-durability | Pending ACKs saved on disconnect |
| C3: Dedup window | 3-tier: ETS 5-min → Bloom → Mnesia 7-day |
| C4: JWT secret | Minimum 32-byte length enforcement |
| H1: Partition guard | Warning on missing `expected_cluster_nodes` |
| H2: Token revocation | Synchronous via `pg` push |
| H3: Region router | Health probing |

---

See [TESTING.md](TESTING.md) for full test suite details.
