# Testing Guide

**Status**: 156 Python + 101 Erlang tests passing | **Last Verified**: 2026-02-11

## Quick Start

```bash
# Run ALL tests (recommended)
./tests/run_all_tests.sh

# Run non-Docker tests only (faster iteration, ~30 min)
./tests/run_all_tests.sh --quick

# Run Docker chaos tests only (~2 hr)
./tests/run_all_tests.sh --docker-only

# Show help
./tests/run_all_tests.sh --help
```

## Test Infrastructure

| Script | Purpose |
|--------|---------|
| `tests/run_all_tests.sh` | **Authoritative test runner** — single entry point |
| `docker/global-cluster/cluster.sh` | Docker cluster up/down |
| `docker/global-cluster/init_cluster.sh` | Mnesia cluster initialization |
| `docker/global-cluster/run_chaos_tests.sh` | Chaos tests with fresh cluster per test |

### Single Docker Test

```bash
cd docker/global-cluster
./cluster.sh down && ./cluster.sh up && python3 ../../tests/suites/chaos_dist/test_network_partition.py
```

---

## Test Modes

| Mode | Command | Description |
|------|---------|-------------|
| **Full** | `./tests/run_all_tests.sh` | All tests (unit → Docker chaos) |
| **Quick** | `./tests/run_all_tests.sh --quick` | Non-Docker tests only (CI Tier 0) |
| **Docker Only** | `./tests/run_all_tests.sh --docker-only` | Docker chaos tests only (CI Tier 1) |

---

## Phase-Based Execution

Tests run in **phases** based on infrastructure requirements:

| Phase | Description | Infrastructure |
|-------|-------------|----------------|
| **Phase 1** | Unit tests (property-based) | No server |
| **Phase 2** | Standalone tests (integration, e2e, security, etc.) | Local TLS server |
| **Phase 3** | ClusterManager tests (chaos_controlled) | Self-managed per test |
| **Phase 4** | Docker chaos tests (chaos_dist) | Fresh cluster per test |

### Why Fresh Cluster Per Test?

Docker chaos tests (`chaos_dist/`) are **destructive** — they kill containers, partition networks, and corrupt state. Each test gets a **fresh cluster** via `cluster.sh up` to ensure isolation.

---

## Test Suites

### Python Test Suites (156 files)

| Suite | Files | Description |
|-------|-------|-------------|
| unit | 4 | Property-based tests (Hypothesis) |
| integration | 40 | Core message flow, dedup, metrics, presence |
| e2e | 11 | End-to-end scenarios (conversation, key verification, ratchet) |
| security | 23 | TLS, JWT, fuzz, CBOR, rate limiting, sender keys |
| resilience | 8 | Fault tolerance, connection resume, clock skew |
| performance_light | 8 | Benchmarks, CPU, memory (NFR-19 hard gate) |
| stress | 18 | Load testing, fan-out, soak, reconnect storm |
| chaos_dist | 27 | Docker-based chaos (SIGKILL, partition, disk full) |
| chaos_controlled | 2 | Combined chaos (self-managed cluster) |
| compatibility | 8 | Protocol versions, HLC migration, compression |
| contract | 6 | Edge-core contract, rate limit constants, RFC v4 |
| conformance | 1 | WebSocket RFC 6455 compliance |

### Erlang Test Suites (101 test modules + 6 support modules)

| Category | Modules | Examples |
|----------|---------|----------|
| Core protocol | 5 | `iris_proto_tests`, `iris_session_tests`, `iris_cbor_tests` |
| Storage & dedup | 8 | `iris_dedup_tests`, `iris_store_tests`, `iris_durable_batcher_tests` |
| Auth & security | 7 | `iris_auth_tests`, `iris_auth_eddsa_tests`, `iris_auth_key_isolation_tests` |
| E2EE | 5 | `iris_x3dh_tests`, `iris_ratchet_tests`, `iris_keys_tests`, `iris_sender_keys_tests` |
| Routing & sharding | 6 | `iris_shard_tests`, `iris_router_pool_tests`, `iris_region_bridge_tests` |
| Resilience | 5 | `iris_circuit_breaker_tests`, `iris_partition_guard_tests`, `iris_flow_controller_tests` |
| Observability | 3 | `iris_metrics_nfr_tests`, `iris_metrics_slo_tests`, `iris_trace_tests` |
| RFC v4 gaps | 8 | `iris_inbox_limit_tests`, `iris_rfc_v4_constants_tests`, `iris_key_change_notify_tests` |
| Other | 23 | Presence, typing, group, ingress guard, concurrency torture, etc. |

---

## CI Pipeline

| Tier | Trigger | Scope | Timeout |
|------|---------|-------|---------|
| **Tier 0** | Every commit + PR | `--quick` (non-Docker) | 35 min |
| **Tier 1** | Nightly + main push | `--docker-only` (chaos) | 2 hr |
| **Tier 2** | Manual dispatch | Full suite | 24 hr |

See [`.github/workflows/ci.yml`](../.github/workflows/ci.yml) for full configuration.

---

## Test Contract

### Exit Codes

| Code | Meaning |
|------|---------|
| `0` | PASS |
| `1` | FAIL |
| `2` | SKIP (with reason) |

### Required Patterns

```python
# Explicit skip with reason
if not infrastructure_available():
    print("SKIP:DOCKER - Container not running")
    sys.exit(2)

# Seed randomness for reproducibility
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))
random.seed(TEST_SEED)
```

### Prohibited Patterns

```python
# CI-conditional pass
if os.environ.get("CI"): sys.exit(0)

# Swallow exceptions
except: pass

# Arbitrary sleeps instead of proper waits
time.sleep(60)  # Hope it works
```

---

## Troubleshooting

### Server Not Available

```bash
CONFIG=config/test_tls make start
```

### Mnesia Errors

```bash
rm -rf Mnesia.* MnesiaCore.* /tmp/Mnesia.*
```

### Docker Cluster Issues

```bash
cd docker/global-cluster
./cluster.sh down
./cluster.sh up
```

### Test Hangs

```bash
pkill -9 -f "beam.smp.*iris_"
```

---

## Directory Structure

```
tests/
├── run_all_tests.sh     # Authoritative test runner
├── conftest.py          # Seeded randomness, deterministic IDs
├── suites/              # Test suites by category (12 suites)
├── framework/           # ClusterManager, assertions, wait utilities
├── utilities/           # IrisClient (TLS-enabled), TLS helpers
└── artifacts/           # Test outputs (gitignored)

test_utils/              # 101 Erlang EUnit test modules + 6 support modules
docker/global-cluster/   # Docker cluster scripts
```
