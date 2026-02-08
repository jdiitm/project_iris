# Testing Guide

**Status**: 87+ tests | **Last Verified**: 2026-02-08

## Quick Start

```bash
# Run ALL tests (recommended)
./tests/run_all_tests.sh

# Run non-Docker tests only (faster iteration)
./tests/run_all_tests.sh --quick

# Run Docker chaos tests only
./tests/run_all_tests.sh --docker-only

# Show help
./tests/run_all_tests.sh --help
```

## Proven Scripts

The test infrastructure uses these **proven, verified scripts**:

| Script | Purpose |
|--------|---------|
| `tests/run_all_tests.sh` | **Main test runner** - single entry point for all tests |
| `docker/global-cluster/cluster.sh` | Docker cluster management (up/down) |
| `docker/global-cluster/init_cluster.sh` | Mnesia cluster initialization |
| `docker/global-cluster/run_chaos_tests.sh` | Runs chaos tests with fresh cluster per test |

### Single Test Execution (Docker)

```bash
# Start cluster and run one test
cd docker/global-cluster
./cluster.sh down && ./cluster.sh up && python3 ../../tests/suites/chaos_dist/test_network_partition.py
```

---

## Test Modes

| Mode | Command | Description |
|------|---------|-------------|
| **Full** | `./tests/run_all_tests.sh` | All tests (unit → Docker chaos) |
| **Quick** | `./tests/run_all_tests.sh --quick` | Non-Docker tests only |
| **Docker Only** | `./tests/run_all_tests.sh --docker-only` | Docker chaos tests only |

---

## Phase-Based Execution

Tests run in **phases** based on infrastructure requirements:

| Phase | Description | Infrastructure |
|-------|-------------|----------------|
| **Phase 1** | Unit tests | No server |
| **Phase 2** | Standalone tests | Local TLS server |
| **Phase 3** | ClusterManager tests | Self-managed |
| **Phase 4** | Docker chaos tests | Fresh cluster per test |

### Why Fresh Cluster Per Test?

Docker chaos tests (`chaos_dist/`) are **destructive** - they kill containers, partition networks, and corrupt state. Each test gets a **fresh cluster** via `cluster.sh up` to ensure isolation.

---

## Test Suites

| Suite | Tests | Description |
|-------|-------|-------------|
| unit | 2 | Property-based tests |
| integration | 22+ | Core message flow |
| e2e | 5+ | End-to-end scenarios |
| security | 9+ | TLS, auth, rate limiting, CBOR validation, sender key rotation |
| resilience | 3 | Fault tolerance |
| performance_light | 6 | Benchmarks (NFR-19 hard gate) |
| stress | 14 | Load testing (NFR-4 rate metric) |
| chaos_dist | 18 | Docker-based chaos tests |
| chaos_controlled | 2 | Controlled chaos |
| contract | 1 | Edge-core contract |
| compatibility | 2 | Protocol versions, mixed HLC ordering |
| erlang unit (new) | 9 | Dedup bloom cross-check (4), session cache bound (5) |

---

## CI Pipeline

The CI pipeline uses the same proven scripts:

```yaml
# Tier 0 - Every commit (fast)
./tests/run_all_tests.sh --quick

# Tier 2 - Nightly (full Docker chaos)
./tests/run_all_tests.sh --docker-only
```

See `.github/workflows/ci.yml` for full configuration.

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
# ✅ Explicit skip with reason
if not infrastructure_available():
    print("SKIP:DOCKER - Container not running")
    sys.exit(2)

# ✅ Seed randomness for reproducibility
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))
random.seed(TEST_SEED)
```

### Prohibited Patterns

```python
# ❌ CI-conditional pass
if os.environ.get("CI"): sys.exit(0)

# ❌ Swallow exceptions
except: pass

# ❌ Arbitrary sleeps instead of proper waits
time.sleep(60)  # Hope it works
```

---

## Troubleshooting

### Server Not Available

```bash
# For standalone tests, start server manually
CONFIG=config/test_tls make start
```

### Mnesia Errors

```bash
rm -rf Mnesia.* MnesiaCore.* /tmp/Mnesia.*
```

### Docker Cluster Issues

```bash
# Use proven cluster.sh script
cd docker/global-cluster
./cluster.sh down
./cluster.sh up
```

### Test Hangs

```bash
pkill -9 -f beam.smp
```

---

## Directory Structure

```
tests/
├── run_all_tests.sh     # Main test runner (PROVEN)
├── suites/              # Test suites by category
│   ├── unit/            # Property-based tests
│   ├── integration/     # Core message delivery
│   ├── e2e/             # End-to-end scenarios
│   ├── security/        # Security validation
│   ├── resilience/      # Fault tolerance
│   ├── stress/          # Load testing
│   ├── performance_light/   # Benchmarks
│   ├── chaos_dist/      # Docker chaos tests (PROVEN)
│   ├── chaos_controlled/    # Controlled chaos
│   ├── compatibility/   # Protocol versions
│   └── contract/        # Edge-core contract
├── framework/           # ClusterManager, assertions
├── utilities/           # IrisClient (TLS-enabled)
└── artifacts/           # Test outputs (gitignored)

docker/global-cluster/
├── cluster.sh           # Cluster management (PROVEN)
├── init_cluster.sh      # Mnesia initialization (PROVEN)
├── run_chaos_tests.sh   # Chaos test runner (PROVEN)
└── docker-compose.yml   # Cluster definition
```
