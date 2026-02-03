# RFC-001 Compliance Status

**Status**: All 115+ tests pass (100%) | **TLS Enforced** | **Last Updated**: 2026-02-03

## Implemented Requirements

| RFC | Requirement | Implementation | Test |
|-----|-------------|----------------|------|
| NFR-3 | Cross-region P99 ≤ 500ms | `iris_region_bridge.erl` | `test_cross_region_latency.py` |
| NFR-6 | 99.999% durability | `store_durable/3` with sync_transaction | `test_ack_durability.py` |
| NFR-8 | Zero data loss (RPO=0) | ACK after Mnesia commit | `test_ack_durability.py` |
| NFR-9 | Failover ≤ 30s | pg discovery + circuit breaker | `test_failover_time.py` |
| **NFR-14** | **TLS mandatory** | **TLS enforced on all connections** | `test_tls_mandatory.py`, all tests |
| NFR-16 | JWT validation | `iris_auth.erl` | `test_auth_flow.py` |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` | `test_rate_limiting.py` |
| NFR-22 | E2EE key durability | Quorum writes | `test_key_bundle_durability.py` |
| NFR-24 | OPK pool alerts | Low OPK + SPK fallback | `test_opk_exhaustion.py` |
| FR-5 | Message ordering | `iris_msg_sequence.erl` | `test_cross_node_ordering.py` |

> **TLS Enforcement (2026-02-03)**: All client connections now require TLS. Server uses `config/test_tls.config`.
> All Python test clients updated to use `ssl.SSLContext` with CA verification via `certs/ca.pem`.

## Key Components

| Module | Purpose |
|--------|---------|
| `iris_region_bridge.erl` | Cross-region relay with durability |
| `iris_flow_controller.erl` | Sharded ETS counters (100K+ checks/sec) |
| `iris_async_router.erl` | Zero silent drops, offline fallback |
| `iris_presence.erl` | Versioned presence (race fix) |
| `iris_hlc.erl` | Hybrid Logical Clocks (NFR-27) |
| `iris_limits.erl` | Configurable limits (NFR-29) |
| `iris_trace.erl` | Distributed tracing (NFR-30/31) |

## Security Hardening

| Issue | Fix |
|-------|-----|
| C1: ACK-before-durability | Pending ACKs saved on disconnect |
| C3: Dedup window | 7-day bloom filter |
| C4: JWT secret | Minimum length enforcement |
| H1: Partition guard | Warning on missing config |
| H2: Token revocation | Synchronous |
| H3: Region router | Health probing |

## Deferred

| Item | Reason |
|------|--------|
| NFR-15 mTLS (inter-node) | Requires PKI infrastructure for node certificates |
| Section 9.1 Versioning | Phase 2 (post-launch) |

## Test Coverage

| Suite | Tests | Pass | Rate | Notes |
|-------|-------|------|------|-------|
| unit | 2 | 2 | 100% | Property-based tests |
| integration | 22 | 22 | 100% | Core message flow |
| stress | 9 | 9 | 100% | Load testing |
| chaos_dist | 12 | 12 | 100% | Docker required, TLS |
| security | 7 | 7 | 100% | TLS, auth, rate limiting |
| performance_light | 1 | 1 | 100% | CPU utilization |
| e2e | 5 | 5 | 100% | End-to-end scenarios |
| resilience | 3 | 3 | 100% | Fault tolerance |
| contract | 1 | 1 | 100% | Edge-core contract |
| compatibility | 6 | 6 | 100% | Protocol versions |
| **TOTAL** | **115+** | **115+** | **100%** | All TLS-enabled |

See [TESTING.md](TESTING.md) for details.
