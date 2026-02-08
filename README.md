# Project Iris: WhatsApp-Class Messaging Engine

[![Tests](https://img.shields.io/badge/tests-120%2B%20passing-brightgreen)](tests/run_all_tests.sh)
[![TLS](https://img.shields.io/badge/TLS-enforced-green)](docs/DEPLOYMENT.md)
[![Erlang](https://img.shields.io/badge/Erlang-OTP%2026%2B-blue)](https://www.erlang.org/)

> **Status**: Development / Pre-alpha. Tested at **10K concurrent connections** locally.
> Full test suite (120+ tests) passing with TLS enforced. Last verified: 2026-02-08.
> Architecture designed for 1M+ users per region. See [Scalability Analysis](docs/SCALABILITY_ANALYSIS.md).

## What This Is

Distributed messaging system in **Erlang/OTP 26**, targeting WhatsApp-class reliability. Two node types: stateless **Edge** (TLS, auth, connection hold) and stateful **Core** (Mnesia, user sharding, offline storage). E2EE via Signal Protocol. Multi-region via async bridge.

See [DEPLOYMENT.md](docs/DEPLOYMENT.md) for architecture diagrams and setup.

### Measured Performance (local, 24-core)

| Metric | Tested | Designed For |
|--------|--------|--------------|
| Concurrent Users | 10K | 1M+ per region |
| Throughput | 8K msg/s | 100K+ msg/s |
| Memory per Connection | ~12 KB | ≤10 KB (NFR-19) |
| P99 Latency | < 25ms | < 50ms cross-region |
| Message Durability | Zero loss (guaranteed mode) | 99.999% (NFR-6) |

### Non-Goals (Deferred)

- Multi-device sync (RFC-002)
- Media messages, voice/video (RFC-002)
- Message edit/delete (opcodes 0x40-0x43, post-launch)
- Sealed sender, key backup, MFA, OAuth 2.0

### Known Limitations

- **Mnesia RAM**: `disc_copies` loads all data into RAM at startup. >32 GB data requires multi-region sharding.
- **Test sleeps**: ~500 `time.sleep()` calls remain in tests (RFC Section 13.2 violation). See [audit](docs/audit/time_sleep_audit.md).
- **mTLS inter-node**: Configurable but not enforced (NFR-15 deferred pending PKI infrastructure).
- **Key change notification**: Identity key change detected + metric emitted, but peer notification protocol is PENDING_DESIGN.

---

## Modules (57 total)

### Edge Layer

| Module | Purpose |
|--------|---------|
| `iris_edge_listener` | TLS accept loop, per-IP connection rate limiting |
| `iris_edge_conn` | Per-connection process, WebSocket upgrade |
| `iris_session` | Packet dispatch, auth, rate checks, span instrumentation |
| `iris_proto` | Wire protocol codec (v1.1, 30+ opcodes) |
| `iris_ws_lite` | Lightweight WebSocket framing |
| `iris_compression` | Zstd/zlib payload compression |
| `iris_auth` | JWT validation (EdDSA primary, HMAC legacy), refresh tokens, revocation |
| `iris_rate_limiter` | Per-user token bucket + distributed gossip via `pg` |

### Core Layer

| Module | Purpose |
|--------|---------|
| `iris_core` | Application entry, Mnesia init, user registry, offline storage |
| `iris_shard` | Jump consistent hash, vnode assignment, rebalancing |
| `iris_async_router` | Auto-tuned worker pool, cross-core user lookup |
| `iris_router` | Route-to-user dispatch (local / remote / offline) |
| `iris_region_router` | Consistent-hash region assignment for 2B+ users |
| `iris_region_bridge` | Cross-region outbox queue (7-day TTL, fsync, FIFO drain) |
| `iris_presence` | Versioned presence with privacy controls |
| `iris_group` | Group CRUD, membership (≤256 E2EE, ≤10K broadcast) |
| `iris_group_fanout` | Parallel group message delivery |

### Storage & Durability

| Module | Purpose |
|--------|---------|
| `iris_store` | Unified storage API (`guaranteed` / `quorum` / `best_effort`) |
| `iris_quorum_write` | Majority-ACK writes (RF=3) |
| `iris_durable_batcher` | WAL + batched `sync_transaction` |
| `iris_dedup` | 3-tier dedup: ETS hot (5 min) → Bloom warm → Mnesia cold (7 day) |
| `iris_session_cache` | Session resume cache (100K cap, LRU eviction, 5-min TTL) |
| `iris_hlc` | 80-bit Hybrid Logical Clocks for cross-region ordering |

### Security & E2EE (Signal Protocol)

| Module | Purpose |
|--------|---------|
| `iris_x3dh` | X3DH key agreement (Curve25519) |
| `iris_ratchet` | Double Ratchet (AES-256-GCM, HKDF-SHA256) |
| `iris_keys` | Key bundle storage, identity key change detection |
| `iris_sender_keys` | Sender Keys for group E2EE |

### Resilience & Observability

| Module | Purpose |
|--------|---------|
| `iris_partition_guard` | Split-brain detection, write blocking on minority side |
| `iris_circuit_breaker` | Adaptive timeout, fallback routing |
| `iris_flow_controller` | Sharded ETS counters, multi-level backpressure |
| `iris_metrics` | Counters (msg_in/out, ack_sent, dedup_hit), SLI/SLO computation |
| `iris_trace` | Distributed tracing (trace_id/span_id propagation across RPCs) |
| `iris_limits` | Hard operational limits (RFC Section 8) |
| `iris_cluster_manager` | Auto-wire replication on node join/leave |

---

## Quick Start

### Prerequisites

- **Erlang/OTP 26+**
- **Python 3.11+** (tests only)
- **Docker** (optional, for chaos tests)

### Build & Run

```bash
make clean && make          # Compile (auto-tunes VM flags)
make start                  # Start local core + edge (TLS on port 8085)
./tests/run_all_tests.sh    # Run ALL tests
```

### Docker Cluster

```bash
cd docker/global-cluster
./cluster.sh up             # 5-region cluster (6 cores, 11 edges)
./run_chaos_tests.sh        # Fresh cluster per test
./cluster.sh down           # Teardown
```

---

## Testing

**120+ tests** across 13 suites. See [TESTING.md](docs/TESTING.md) for full details.

```bash
./tests/run_all_tests.sh              # Full suite
./tests/run_all_tests.sh --quick      # Non-Docker only (~30 min)
./tests/run_all_tests.sh --docker-only # Docker chaos only (~2 hr)
```

---

## Security

| Feature | Implementation | Status |
|---------|---------------|--------|
| TLS 1.2/1.3 | Enforced on all client connections | Verified |
| mTLS (inter-node) | Configurable via `docker-compose.mtls.yml` | Deferred (NFR-15) |
| JWT Authentication | EdDSA (Ed25519) primary; HMAC-SHA256 legacy | Verified |
| JWT Replay Protection | `jti` nonce tracking in ETS with TTL cleanup | Implemented |
| Rate Limiting | Per-user token bucket + distributed `pg` gossip | Verified |
| Connection Rate Limit | Per-IP throttling at edge (5/min default, RFC 10.1) | Verified |
| E2EE | Signal Protocol (X3DH + Double Ratchet + Sender Keys) | Verified |
| Input Validation | E2EE header fields, CBOR schema, payload 64KB limit | Verified |

See [DEPLOYMENT.md](docs/DEPLOYMENT.md) for TLS and certificate setup.

---

## Documentation

| Guide | Scope |
|-------|-------|
| [DEPLOYMENT.md](docs/DEPLOYMENT.md) | Architecture, setup, configuration, TLS, Docker cluster |
| [OPERATIONS.md](docs/OPERATIONS.md) | Incident response, failover, scaling, monitoring |
| [TESTING.md](docs/TESTING.md) | Test suites, CI pipeline, test contract |
| [DECISIONS.md](docs/DECISIONS.md) | Architecture decisions and trade-offs |
| [RFC_COMPLIANCE.md](docs/RFC_COMPLIANCE.md) | RFC v4.0 requirement verification status |
| [PROTOCOL_V1_FREEZE.md](docs/PROTOCOL_V1_FREEZE.md) | Canonical wire protocol specification |
| [RFC-001](docs/rfc/RFC-001-SYSTEM-REQUIREMENTS.md) | System requirements (v4.0) |
| [Amendment 001](docs/rfc/RFC-001-AMENDMENT-001.md) | E2EE and group messaging requirements |
| [ROADMAP.md](docs/ROADMAP.md) | 5B DAU scaling roadmap |
| [SCALABILITY_ANALYSIS.md](docs/SCALABILITY_ANALYSIS.md) | Measured performance data and extrapolation |

## Project Structure

```
project_iris/
├── src/                    # 57 Erlang source modules (20K lines)
├── test_utils/             # 70 Erlang EUnit test modules (326+ tests)
├── tests/
│   ├── run_all_tests.sh    # Authoritative test runner
│   ├── suites/             # 13 test categories
│   │   ├── unit/           # Property-based (Hypothesis)
│   │   ├── integration/    # Core message flow (37 tests)
│   │   ├── e2e/            # End-to-end scenarios (11 tests)
│   │   ├── security/       # TLS, auth, fuzz, rate limiting (23 tests)
│   │   ├── resilience/     # Fault tolerance (7 tests)
│   │   ├── stress/         # Load testing (18 tests)
│   │   ├── performance_light/ # Benchmarks, NFR gates (8 tests)
│   │   ├── chaos_dist/     # Docker chaos (25 tests, fresh cluster each)
│   │   ├── chaos_controlled/ # Combined chaos (2 tests)
│   │   ├── compatibility/  # Protocol versions, HLC migration (7 tests)
│   │   ├── contract/       # Edge-core + RFC contracts (3 tests)
│   │   └── conformance/    # WebSocket RFC 6455 (1 test)
│   ├── framework/          # ClusterManager, assertions, wait utilities
│   └── utilities/          # IrisClient (TLS-enabled), TLS helpers
├── config/                 # Erlang app configs (test, test_tls, docker, mTLS)
├── certs/                  # TLS certificates + generate_certs.sh
├── docker/
│   └── global-cluster/     # 5-region Docker cluster
├── scripts/                # auto_tune.sh, rfc_watchdog.py
├── docs/                   # All documentation
└── Makefile                # Build, test, cluster targets
```

---

**License**: MIT
