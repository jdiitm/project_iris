# RFC-001 Compliance Status

**Status**: 75 tests pass (100%) | **TLS Enforced** | **Last Updated**: 2026-02-04

## Verification Status Legend

| Status | Meaning |
|--------|---------|
| **VERIFIED** | Requirement tested with production-realistic conditions |
| **PARTIAL** | Requirement tested with simulation or simplified conditions |
| **TRANSPORT-ONLY** | Protocol layer tested, cryptographic operations not validated |

## Implemented Requirements

| RFC | Requirement | Implementation | Test | Verification |
|-----|-------------|----------------|------|--------------|
| NFR-3 | Cross-region P99 ≤ 500ms | `iris_region_bridge.erl` | `test_cross_region_latency.py` | **VERIFIED** |
| NFR-6 | 99.999% durability | `store_durable/3` with sync_transaction | `test_ack_durability.py` | **VERIFIED** (SIGKILL) |
| NFR-8 | Zero data loss (RPO=0) | ACK after Mnesia commit | `test_ack_durability.py`, `test_multimaster_durability.py` | **VERIFIED** (SIGKILL, multi-node) |
| NFR-9 | Failover ≤ 30s | pg discovery + circuit breaker | `test_failover_time.py` | **VERIFIED** |
| **NFR-14** | **TLS mandatory** | **TLS enforced on all connections** | `test_tls_mandatory.py`, all tests | **VERIFIED** |
| NFR-16 | Clock skew tolerance (30s) | HLC-based ordering | `test_clock_skew.py` | **PARTIAL** (simulation only) |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` | `test_rate_limiting.py` | **VERIFIED** |
| NFR-22 | E2EE key durability | Quorum writes | `test_key_bundle_durability.py` | **VERIFIED** |
| NFR-24 | OPK pool alerts | Low OPK + SPK fallback | `test_opk_exhaustion.py` | **VERIFIED** |
| FR-5 | Message ordering | `iris_msg_sequence.erl` | `test_cross_node_ordering.py` | **VERIFIED** |
| FR-20 | Group E2EE | Sender Keys protocol | `test_group_e2ee.py` | **TRANSPORT-ONLY** |

### Verification Notes

**NFR-8 (RPO=0 Durability)**:
- `test_ack_durability.py`: Uses SIGKILL to simulate hard crash (power loss)
- `test_multimaster_durability.py`: Validates multi-node replication survives SIGKILL
- Single-node durability relies on sync_transaction flushing before ACK
- Multi-node durability (recommended) relies on replication to surviving nodes

**NFR-16 (Clock Skew)**:
- Tests validate message ordering and reconnect behavior
- Real clock manipulation requires `libfaketime` in Docker containers
- Simulation mode verifies protocol correctness, not actual clock drift handling

**FR-20 (Group E2EE)**:
- Tests validate protocol opcodes (0x30-0x36) and message routing
- Cryptographic operations (Signal Protocol) validated at unit level only
- Full crypto integration test requires `cryptography` library validation

> **TLS Enforcement (2026-02-03)**: All client connections require TLS. Server uses `config/test_tls.config`.
> All Python test clients use `ssl.SSLContext` with CA verification via `certs/ca.pem`.

## Key Components

| Module | Purpose |
|--------|---------|
| `iris_region_bridge.erl` | Cross-region relay with durability |
| `iris_flow_controller.erl` | Sharded ETS counters (100K+ checks/sec) |
| `iris_async_router.erl` | Zero silent drops, offline fallback |
| `iris_presence.erl` | Versioned presence (race fix) |
| `iris_hlc.erl` | Hybrid Logical Clocks |
| `iris_limits.erl` | Configurable limits |
| `iris_trace.erl` | Distributed tracing |

## Security Hardening

| Issue | Fix |
|-------|-----|
| C1: ACK-before-durability | Pending ACKs saved on disconnect |
| C3: Dedup window | 7-day bloom filter |
| C4: JWT secret | Minimum length enforcement |
| H1: Partition guard | Warning on missing config |
| H2: Token revocation | Synchronous |
| H3: Region router | Health probing |

## Deferred / Partial

| Item | Status | Notes |
|------|--------|-------|
| NFR-15 mTLS (inter-node) | Deferred | Requires PKI infrastructure |
| NFR-16 Clock Skew | Partial | Simulation only; real injection requires libfaketime |
| Section 9.1 Versioning | Deferred | Phase 2 (post-launch) |
| E2EE Crypto Validation | Partial | Protocol tested; crypto ops at unit level |

## Test Coverage

| Suite | Tests | Pass | Rate | Notes |
|-------|-------|------|------|-------|
| unit | 2 | 2 | 100% | Property-based tests |
| integration | 22 | 22 | 100% | Core message flow |
| stress | 14 | 14 | 100% | Load testing |
| chaos_dist | 12 | 12 | 100% | Docker required, SIGKILL durability |
| security | 7 | 7 | 100% | TLS, auth, rate limiting |
| performance_light | 6 | 6 | 100% | Benchmarks |
| e2e | 5 | 5 | 100% | End-to-end scenarios |
| resilience | 3 | 3 | 100% | Fault tolerance |
| chaos_controlled | 2 | 2 | 100% | Controlled chaos |
| contract | 1 | 1 | 100% | Edge-core contract |
| compatibility | 1 | 1 | 100% | Protocol versions |
| **TOTAL** | **75** | **75** | **100%** | All TLS-enabled |

See [TESTING.md](TESTING.md) for details.
