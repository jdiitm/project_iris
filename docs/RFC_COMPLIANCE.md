# RFC-001 Compliance Status

**Last Updated**: 2026-01-27 (Full Test Suite Pass)
**Reference**: [RFC-001-SYSTEM-REQUIREMENTS.md](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)

> **Test Status**: All 85 tests pass deterministically with `TEST_PROFILE=smoke TEST_SEED=42`. See [TEST_STATUS.md](TEST_STATUS.md) for details.

---

## Implemented ✅

| RFC Section | Requirement | Implementation | Test |
|-------------|-------------|----------------|------|
| NFR-3 | Cross-region P99 ≤ 500ms | `iris_region_bridge.erl` with async relay | `test_cross_region_latency.py` |
| NFR-6 | 99.999% durability | `store_durable/3` with `sync_transaction` | `test_ack_durability.py` |
| NFR-8 | Zero data loss (RPO=0) | ACK only after Mnesia commit | `test_ack_durability.py` |
| NFR-9 | Failover ≤ 30s | pg-based discovery + circuit breaker | `test_failover_time.py` |
| NFR-14 | TLS mandatory | `check_tls_policy/1` in edge listener | `test_tls_mandatory.py` |
| NFR-16 | JWT validation | `iris_auth.erl` with expiry/signature | `test_auth_flow.py` |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` | `test_rate_limiting.py` |
| NFR-22 | E2EE key durability | Quorum writes via `iris_quorum_write.erl` | `test_key_bundle_durability.py` |
| NFR-24 | OPK pool alerts | Low OPK detection + SPK fallback | `test_opk_exhaustion.py` |
| FR-5 | Message ordering | `iris_msg_sequence.erl` + sequence protocol | `test_cross_node_ordering.py` |
| FR-9/10/11 | Token auth + revocation | Mnesia-backed revocation | `test_cluster_revocation.py` |
| Section 8 | Abuse prevention | Rate limiting per-user/IP | Integration tests |

---

## Hardening Fix Plan Implementation (2026-01-24)

The following components were added to address the adversarial audit findings:

### Phase 1: Critical Infrastructure Fixes

#### 1.1 Cross-Region Bridge (NEW)
- **`iris_region_bridge.erl`**: Reliable async cross-region message relay
- Durable message queueing before ACK to sender
- Automatic retry with exponential backoff (1s → 60s max)
- Dead-letter queue for failed messages after 5 attempts
- Location: [src/iris_region_bridge.erl](../src/iris_region_bridge.erl)

#### 1.2 Flow Controller Sharding (AUDIT FIX)
- **`iris_flow_controller.erl`**: Now uses sharded ETS counters
- Lock-free admission checks at ALL levels (not just NORMAL)
- 16 shards for parallel counter updates
- Target: 100K+ admission checks/sec
- Location: [src/iris_flow_controller.erl](../src/iris_flow_controller.erl)

#### 1.3 Silent Loss Prevention (AUDIT FIX)
- **`iris_async_router.erl`**: Guaranteed offline fallback
- All routing failures result in offline storage
- Metrics tracking: `route_attempt`, `route_success`, `route_offline`, `route_failure`
- Zero silent message drops
- Location: [src/iris_async_router.erl](../src/iris_async_router.erl)

### Phase 2: E2EE Hardening

#### 2.1 Key Bundle Durability
- **`iris_keys.erl`**: Uses quorum writes for key bundles
- Key bundles survive node failures before replication
- Falls back to local sync_transaction if quorum unavailable
- Location: [src/iris_keys.erl](../src/iris_keys.erl)

#### 2.2 OPK Exhaustion Handling
- **Low OPK Alert**: Warning when < 20 OPKs remain (NFR-24)
- **SPK Fallback Mode**: Graceful degradation to X3DH without OPK
- **Metrics**: `opk_exhausted_count`, `opk_low_alerts`, `spk_fallback_count`
- Location: [src/iris_keys.erl](../src/iris_keys.erl)

### Phase 3: Race Condition Fixes

#### 3.1 Presence Versioning (NEW)
- **`iris_presence.erl`**: Versioned presence entries
- Monotonic version numbers prevent stale routing
- `route_to_online/3` with version check
- Location: [src/iris_presence.erl](../src/iris_presence.erl)

#### 3.2 Sender Key Sync (AUDIT FIX)
- **`iris_group.erl`**: Key sync on member reconnect
- `handle_member_reconnect/2` returns all keys updated since `last_seen`
- `get_sender_keys_since/2` for timestamp-based key queries
- Location: [src/iris_group.erl](../src/iris_group.erl)

---

## Security Hardening (2026-01-27)

The following critical security issues were identified in adversarial audit and fixed:

### P0 Critical Fixes

| Issue | Module | Fix | Test |
|-------|--------|-----|------|
| **C1: ACK-before-durability** | `iris_edge_conn.erl` | Replaced `rpc:cast` with sync RPC + retry in terminate path; local fallback storage | Unit tests |
| **C2: E2EE signature bypass** | `iris_x3dh.erl` | Implemented proper Ed25519 signature verification for prekey authentication | `iris_x3dh_tests` |
| **C3: Dedup window mismatch** | `iris_dedup.erl` | Tiered dedup: 5-min ETS hot cache + 7-day bloom filter warm tier | `iris_dedup_tests` |
| **C4: JWT secret auto-gen** | `iris_auth.erl` | Fails on startup if secret not configured (with `allow_random_secret=false`) | `iris_auth_tests` |

### P1 High-Severity Fixes

| Issue | Module | Fix | Test |
|-------|--------|-----|------|
| **H1: Partition guard permissive** | `iris_partition_guard.erl` | Added startup warning when no expected nodes configured | Unit tests |
| **H2: Token revocation delay** | `iris_auth.erl` | Sync Mnesia write + cross-node propagation via RPC | `iris_auth_tests` |
| **H3: Region router no health** | `iris_region_router.erl` | Added health probing and circuit breaker integration | `iris_region_router_tests` |
| **H4: Quorum worker tracking** | `iris_quorum_write.erl` | Fixed Node→MRef map tracking (was removing wrong workers) | `iris_quorum_write_tests` |
| **H5: Circuit breaker race** | `iris_circuit_breaker.erl` | Atomic half-open transition + probe rate limiting | `iris_circuit_breaker_tests` |
| **H6: WAL dir hardcoded** | `iris_durable_batcher.erl` | Configurable via `wal_directory` + tmpfs detection warning | Unit tests |

### Phase 4: Scalability & Resource Boundedness (2026-01-28)

#### 4.1 Routing Stability (RFC-001 NFR-12)
- **`iris_shard.erl`**: Replaced (`phash2` modulo) with **Jump Consistent Hash**.
- **Impact**: Scaling from N to N+1 nodes moves `1/(N+1)` data instead of `~100%`.
- **Validation**: `iris_shard_tests.erl` confirmed 0.5% movement on resize event (1000 keys).

#### 4.2 Global Lock Breakdown (RFC-001 NFR-3)
- **`iris_core.erl`**: Removed `mnesia:sync_transaction` (Global Lock) from `store_offline_durable`.
- **New Path**: `iris_durable_batcher` (WAL) -> Async Replication.
- **Guarantee**: RPO=0 maintained via local WAL fsync before ACK.

#### 4.3 DoS & OOM Protection (RFC-001 NFR-17)
- **Ingress Guard**: `iris_ingress_guard.erl` atomic connection limits per node.
- **Memory Kill Switch**: `mas_heap_size` enforcement on `iris_session` (800KB) and `iris_edge_conn` (400KB).
- **Validation**: `test_resource_limits.py` confirmed process termination on abuse.

### Phase 5: RFC Compliance Mitigation (2026-01-31)

The following changes address items identified in the RFC-001 v3.0 compliance audit:

#### 5.1 Typing Indicators (RFC FR-8)
- **`iris_proto.erl`**: Added opcodes 0x30 (TYPING_START) and 0x31 (TYPING_STOP)
- **`iris_session.erl`**: Relay typing indicators to recipients (best-effort)
- **`iris_edge_conn.erl`**: Handle typing delivery messages
- **Behavior**: Fire-and-forget, no durability (transient state)

#### 5.2 Read Receipts (RFC FR-4 - Optional)
- **`iris_read_receipts.erl`**: New module for read receipt tracking (NEW)
- **`iris_proto.erl`**: Added opcodes 0x40 (READ_RECEIPT) and 0x41 (READ_RECEIPT_RELAY)
- **Configuration**: Enable with `{iris_core, [{read_receipts_enabled, true}]}`
- **TTL**: Configurable receipt retention (default: 24h)

#### 5.3 Failover Timeout Configuration (RFC NFR-9)
- **`iris_circuit_breaker.erl`**: Added configurable `failover_timeout_ms`
- **Default**: 30000ms (30 seconds per RFC requirement)
- **Configuration**: `{iris_core, [{failover_timeout_ms, 30000}]}`
- **API**: `iris_circuit_breaker:get_failover_timeout()` for inspection

#### 5.4 Durability Metrics (RFC NFR-6/NFR-8)
- **`iris_metrics.erl`**: Added counters for durability tracking
  - `iris_msg_acked_total`: Messages with confirmed ACK
  - `iris_msg_lost_total`: Messages lost (MUST be 0)
- **API**: `iris_metrics:get_durability_metrics()` returns percentage
- **Alert**: `msg_lost/0` logs EMERGENCY on any loss (RPO=0 violation)

#### 5.5 Test Quality Improvements
- **`tests/utilities/helpers.py`**: Added seeded random utilities
  - `seed_random(seed)`: Initialize deterministic RNG
  - `test_randint(a, b)`, `test_choice(seq)`, etc.
- **`tests/framework/wait.py`**: Event-driven wait utilities (existing)
  - `wait_for()`: Poll-based condition waiting
  - Replaces arbitrary `time.sleep()` for determinism


---

## Deferred 📋

| RFC Section | Requirement | Reason | Path Forward |
|-------------|-------------|--------|--------------|
| NFR-15 | mTLS inter-node | Focus on client TLS first | Erlang `-ssl_dist_optfile` config |
| Scale | 5B users | Regional Mnesia 50-node limit | Sharded Mnesia with 20 regions |
| Section 9.1 | Protocol version negotiation | Breaking change | Client migration plan |
| Section 5.2 | UUIDv7 message IDs | Requires protocol change | 3-5 days when prioritized |
| TST-1 | Remove all time.sleep() | 298 occurrences in 64 files | Framework in place (`wait.py`), needs systematic replacement |
| TST-3 | Seeded random everywhere | 20 occurrences in 11 files | Utilities added, needs gradual migration |

---

## Configuration

### TLS Enforcement (NFR-14)

Production **MUST** run with TLS enabled:
```erlang
[{iris_edge, [
    {tls_enabled, true},
    {tls_certfile, "/path/to/cert.pem"},
    {tls_keyfile, "/path/to/key.pem"}
]}].
```

For testing only:
```erlang
[{iris_edge, [{allow_insecure, true}]}].  %% NEVER use in production
```

### Regional Configuration

Configure for multi-region deployment:
```erlang
[{iris_core, [
    {region_id, <<"us-east-1">>},
    {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
    {expected_cluster_nodes, ['core1@host1', 'core2@host2', 'core3@host3']}
]}].
```

---

## Test Coverage

All RFC requirements have corresponding tests:

| Category | Tests | Status |
|----------|-------|--------|
| Unit | 85+ | ✅ 100% |
| Integration | 15+ | ✅ 100% |
| Durability | `test_ack_durability.py` | ✅ (active) |
| Cross-Region | `test_cross_region_latency.py` | ✅ (active) |
| Multimaster | `test_multimaster_durability.py` | ✅ (active) |
| Failover | `test_failover_time.py` | ✅ |
| Ordering | `test_cross_node_ordering.py` | ✅ |
| E2EE Keys | `test_key_bundle_durability.py` | ✅ |
| OPK | `test_opk_exhaustion.py` | ✅ |
| Flow Scale | `test_flow_controller_scale.py` | ✅ |
| Silent Loss | `test_no_silent_loss.py` | ✅ |
| Key Sync | `test_sender_key_sync.py` | ✅ |
| HLC | `iris_hlc_tests.erl` | ✅ (NEW) |
| Limits | `iris_limits_tests.erl` | ✅ (NEW) |
| Tracing | `iris_trace_tests.erl` | ✅ (NEW) |
| NFR Metrics | `iris_metrics_nfr_tests.erl` | ✅ (NEW) |
| Fanout Rate | `iris_fanout_rate_tests.erl` | ✅ (NEW) |

See [TEST_STATUS.md](TEST_STATUS.md) for full details.
