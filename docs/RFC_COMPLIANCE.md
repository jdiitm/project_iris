# RFC-001 Compliance Status

**Status**: All 113 tests pass (100%) | **Last Updated**: 2026-02-01

## Implemented Requirements

| RFC | Requirement | Implementation | Test |
|-----|-------------|----------------|------|
| NFR-3 | Cross-region P99 ≤ 500ms | `iris_region_bridge.erl` | `test_cross_region_latency.py` |
| NFR-6 | 99.999% durability | `store_durable/3` with sync_transaction | `test_ack_durability.py` |
| NFR-8 | Zero data loss (RPO=0) | ACK after Mnesia commit | `test_ack_durability.py` |
| NFR-9 | Failover ≤ 30s | pg discovery + circuit breaker | `test_failover_time.py` |
| NFR-14 | TLS mandatory | `check_tls_policy/1` | `test_tls_mandatory.py` |
| NFR-16 | JWT validation | `iris_auth.erl` | `test_auth_flow.py` |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` | `test_rate_limiting.py` |
| NFR-22 | E2EE key durability | Quorum writes | `test_key_bundle_durability.py` |
| NFR-24 | OPK pool alerts | Low OPK + SPK fallback | `test_opk_exhaustion.py` |
| FR-5 | Message ordering | `iris_msg_sequence.erl` | `test_cross_node_ordering.py` |

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
| NFR-15 mTLS | Requires PKI infrastructure |
| Section 9.1 Versioning | Phase 2 (post-launch) |

## Test Coverage

| Suite | Tests | Pass | Rate |
|-------|-------|------|------|
| unit | 41 | 41 | 100% |
| integration | 22 | 22 | 100% |
| stress | 14 | 14 | 100% |
| chaos_dist | 11 | 11 | 100% |
| security | 7 | 7 | 100% |
| performance_light | 6 | 6 | 100% |
| e2e | 5 | 5 | 100% |
| resilience | 3 | 3 | 100% |
| chaos_controlled | 2 | 2 | 100% |
| contract | 1 | 1 | 100% |
| compatibility | 1 | 1 | 100% |
| **TOTAL** | **113** | **113** | **100%** |

See [TESTING.md](TESTING.md) for details.
