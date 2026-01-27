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

## Deferred 📋

| RFC Section | Requirement | Reason | Path Forward |
|-------------|-------------|--------|--------------|
| NFR-15 | mTLS inter-node | Focus on client TLS first | Erlang `-ssl_dist_optfile` config |
| Scale | 5B users | Regional Mnesia 50-node limit | Sharded Mnesia with 20 regions |
| Section 9.1 | Protocol version negotiation | Breaking change | Client migration plan |
| Section 5.2 | UUIDv7 message IDs | Requires protocol change | 3-5 days when prioritized |

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
| Unit | 60+ | ✅ 100% |
| Integration | 15+ | ✅ 100% |
| Durability | `test_ack_durability.py` | ✅ (active) |
| Cross-Region | `test_cross_region_latency.py` | ✅ (active) |
| Multimaster | `test_multimaster_durability.py` | ✅ (active) |
| Failover | `test_failover_time.py` | ✅ |
| Ordering | `test_cross_node_ordering.py` | ✅ |
| E2EE Keys | `test_key_bundle_durability.py` | ✅ (NEW) |
| OPK | `test_opk_exhaustion.py` | ✅ (NEW) |
| Flow Scale | `test_flow_controller_scale.py` | ✅ (NEW) |
| Silent Loss | `test_no_silent_loss.py` | ✅ (NEW) |
| Key Sync | `test_sender_key_sync.py` | ✅ (NEW) |

See [TEST_STATUS.md](TEST_STATUS.md) for full details.
