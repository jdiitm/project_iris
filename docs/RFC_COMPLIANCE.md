# RFC-001 Compliance Status

**Last Updated**: 2026-01-19 (Audit Synthesis)
**Reference**: [RFC-001-SYSTEM-REQUIREMENTS.md](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)

---

## Implemented ✅

| RFC Section | Requirement | Implementation | Test |
|-------------|-------------|----------------|------|
| NFR-3 | Cross-region P99 ≤ 500ms | Network-aware routing | `test_cross_region_latency.py` |
| NFR-6 | 99.999% durability | `store_durable/3` with `sync_transaction` | `test_ack_durability.py` |
| NFR-8 | Zero data loss (RPO=0) | ACK only after Mnesia commit | `test_ack_durability.py` |
| NFR-9 | Failover ≤ 30s | pg-based discovery + circuit breaker | `test_failover_time.py` |
| NFR-14 | TLS mandatory | `check_tls_policy/1` in edge listener | `test_tls_mandatory.py` |
| NFR-16 | JWT validation | `iris_auth.erl` with expiry/signature | `test_auth_flow.py` |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` | `test_rate_limiting.py` |
| FR-5 | Message ordering | `iris_msg_sequence.erl` + sequence protocol | `test_cross_node_ordering.py` |
| FR-9/10/11 | Token auth + revocation | Mnesia-backed revocation | `test_cluster_revocation.py` |
| Section 8 | Abuse prevention | Rate limiting per-user/IP | Integration tests |

---

## Audit Synthesis Implementation (2026-01-19)

The following components were added to address Audit1 and Audit2 findings:

### Durability Hardening (Audit2 P0-1)
- **`store_durable/3`**: Guarantees ACK only after `sync_transaction` completes
- **`store_offline_durable/2`**: API for durability-critical paths
- Location: [iris_offline_storage.erl](../src/iris_offline_storage.erl)

### Split-Brain Detection (Audit1)
- **`iris_partition_guard.erl`**: Monitors cluster membership
- Safe mode: Rejects writes when quorum is lost
- Location: [iris_partition_guard.erl](../src/iris_partition_guard.erl)

### Message Ordering (Audit2)
- **`iris_msg_sequence.erl`**: Tracks sequence per sender/recipient
- **Protocol opcode 0x07**: Messages with 64-bit sequence numbers
- Location: [iris_msg_sequence.erl](../src/iris_msg_sequence.erl)

### Global Cluster Simulation
- **Docker Compose**: 5-region, 20-container topology
- **Pumba chaos**: 100ms latency injection to Sydney/São Paulo
- Location: [docker/global-cluster/](../docker/global-cluster/)

---

## Deferred 📋

| RFC Section | Requirement | Reason | Path Forward |
|-------------|-------------|--------|--------------|
| NFR-15 | mTLS inter-node | Focus on client TLS first | Erlang `-ssl_dist_optfile` config |
| Scale | 5B users | Mnesia 50-node limit | Sharded Mnesia or storage replacement |
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

### Partition Detection

Configure expected cluster nodes for quorum calculation:
```erlang
[{iris_core, [
    {expected_cluster_nodes, ['core1@host1', 'core2@host2', 'core3@host3']}
]}].
```

---

## Test Coverage

All RFC requirements have corresponding tests:

| Category | Tests | Status |
|----------|-------|--------|
| Unit | 58 | ✅ 100% |
| Integration | 11 | ✅ 100% |
| Durability | `test_ack_durability.py` | ✅ |
| Latency | `test_cross_region_latency.py` | ✅ |
| Failover | `test_failover_time.py` | ✅ |
| Ordering | `test_cross_node_ordering.py` | ✅ |
| Security | `test_tls_mandatory.py` | ✅ |

See [TEST_STATUS.md](TEST_STATUS.md) for full details.
