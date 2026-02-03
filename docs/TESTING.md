# Testing Guide

**Status**: 75 tests pass (100%) | **Last Verified**: 2026-02-03

## Quick Start

```bash
# Run all tests (server lifecycle managed automatically)
python3 tests/run_tests.py --all

# Run all tests, skip Docker (faster)
python3 tests/run_tests.py --all --skip-docker

# CI Tiers (independent, no overlap)
python3 tests/run_tests.py --tier 0   # unit, integration
python3 tests/run_tests.py --tier 1   # e2e, security, resilience
python3 tests/run_tests.py --tier 2   # performance, stress

# Run specific suite
python3 tests/run_tests.py --suite integration

# List all tests
python3 tests/run_tests.py --list

# Kill all processes
python3 tests/run_tests.py --nuke
```

## Phase-Based Execution

The test runner organizes tests into **phases** based on infrastructure requirements:

| Phase | Description | Server Management |
|-------|-------------|-------------------|
| **Phase 1** | Unit tests (2) | No server needed |
| **Phase 2** | Standalone tests (45+) | Shared TLS server started once |
| **Phase 3** | ClusterManager tests (14) | Self-managed per test |
| **Phase 4** | Docker chaos tests (12) | Docker global cluster |

### Why Phases?

1. **Efficiency**: Phase 2 starts the server once and runs all standalone tests
2. **Isolation**: Phase 3 tests that use `ClusterManager` get a fresh cluster each
3. **Docker separation**: Phase 4 tests require Docker and run separately

### Test Categorization

Tests are categorized by their infrastructure needs:

**Standalone tests** (Phase 2): Expect a pre-started TLS server
- All integration, e2e, contract, compatibility, security tests
- `benchmark_e2ee_latency`, `benchmark_throughput`, `benchmark_unit_cost`
- `test_clock_skew`, `test_hard_kill`
- `stress_offline_delete`, `test_flow_controller_scale`, `test_group_fanout`, `test_soak_memory`

**ClusterManager tests** (Phase 3): Use `with ClusterManager(...)` to self-manage
- `test_resilience`, `benchmark_memory`, `measure_dials`, `test_cpu_utilization`
- `stress_geo_scale`, `stress_global_fan_in`, `stress_hotspot`, `stress_presence`
- `test_backpressure_collapse`, `test_churn`, `test_connection_scale`, `test_fanout`
- `test_hot_shard`, `test_limits`, `chaos_combined`, `ultimate_chaos`

**Docker tests** (Phase 4): Require Docker global cluster
- All tests in `chaos_dist/`

---

## Test Results

| Suite | Tests | Notes |
|-------|-------|-------|
| unit | 2 | Property-based tests |
| integration | 22 | Core message flow |
| stress | 14 | Load testing (4 standalone, 10 ClusterManager) |
| performance_light | 6 | Benchmarks (3 standalone, 3 ClusterManager) |
| chaos_dist | 12 | Docker required |
| security | 7 | TLS, auth, rate limiting |
| e2e | 5 | End-to-end scenarios |
| resilience | 3 | Fault tolerance (2 standalone, 1 ClusterManager) |
| chaos_controlled | 2 | Controlled chaos |
| contract | 1 | Edge-core contract |
| compatibility | 1 | Protocol versions |
| **TOTAL** | **75** | |

---

## CI Tiers

Each tier runs **only** its own suites (no duplicate test runs):

| Tier | Suites | Trigger | Approx Time |
|------|--------|---------|-------------|
| 0 | unit, integration | Every commit | ~3 min |
| 1 | e2e, contract, compatibility, security, resilience | Every PR | ~5 min |
| 2 | performance_light, stress, chaos_controlled | Nightly | ~15 min |

Docker chaos tests (`chaos_dist`) run in a separate CI job.

### CI Workflow

```yaml
# Tier 0 - Every commit
python3 tests/run_tests.py --tier 0

# Tier 1 - Every PR (only after Tier 0 passes)
python3 tests/run_tests.py --tier 1

# Tier 2 - Nightly (skip Docker for faster CI)
python3 tests/run_tests.py --tier 2 --skip-docker
```

---

## Test Contract

### Exit Codes (Mandatory)

| Code | Meaning | When |
|------|---------|------|
| `0` | PASS | All assertions passed |
| `1` | FAIL | Assertion failed or unexpected error |
| `2` | SKIP | Missing prerequisites (with reason) |

### Skip Reasons

| Code | Use |
|------|-----|
| `SKIP:DOCKER` | Container not available |
| `SKIP:CLUSTER` | Cluster not configured |
| `SKIP:TLS` | TLS not configured |
| `SKIP:INFRA` | Infrastructure limitation |

### Prohibited Patterns

```python
# ❌ CI-conditional pass
if os.environ.get("CI"): sys.exit(0)

# ❌ Return None as skip  
if not ready(): return None

# ❌ Swallow exceptions
except: pass
```

### Required Patterns

```python
# ✅ Explicit skip with reason
if not infrastructure_available():
    print("SKIP:DOCKER - Container not running")
    sys.exit(2)

# ✅ Seed randomness
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))
random.seed(TEST_SEED)
```

---

## Determinism Standards

### Principles

Tests MUST:
1. Produce identical results with same seed
2. Never depend on wall-clock time (only timeouts)
3. Clean up all state before/after execution
4. Run in any order without affecting others

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TEST_SEED` | 42 | Master random seed |
| `TEST_PROFILE` | smoke | Intensity (smoke/full) |
| `IRIS_TEST_RUNNER` | 1 | Set by runner, signals managed lifecycle |

---

## Recent Changes (Feb 2026)

### Test Runner Refactor (2026-02-03)

- Refactored `run_tests.py` to phase-based execution (1860→660 lines)
- CI tiers now independent (no duplicate test runs)
- Server lifecycle managed per phase, not per suite
- Added `--skip-docker` flag for faster runs

### TLS Stabilization (2026-02-03)

- TLS enforced on all client connections
- All Python test clients use TLS via `ssl.SSLContext`
- Certificates in `certs/` directory

---

## Troubleshooting

### Common Issues

**Server not available**: Test runner manages server automatically. If running manually:
```bash
CONFIG=config/test_tls make start
```

**Mnesia errors**: 
```bash
rm -rf /tmp/Mnesia.* /tmp/mnesia* Mnesia.*
```

**Test hangs**: 
```bash
python3 tests/run_tests.py --nuke
# or
pkill -9 -f beam.smp
```

**Docker issues**:
```bash
docker compose -f docker/global-cluster/docker-compose.yml down -v
```

### Reproducing Failures

1. Get seed from failing run: `TEST_SEED: 12345`
2. Reproduce: `TEST_SEED=12345 python3 tests/run_tests.py --suite <suite>`

---

## Directory Structure

```
tests/
├── run_tests.py        # Unified test runner (phase-based)
├── run_all_tests.sh    # Shell script alternative
├── framework/          # ClusterManager, assertions
├── suites/             # Test suites by category
│   ├── unit/           # Property-based tests
│   ├── integration/    # Core message delivery
│   ├── e2e/            # End-to-end scenarios
│   ├── security/       # TLS, auth, rate limiting
│   ├── resilience/     # Fault tolerance
│   ├── stress/         # Load testing
│   ├── performance_light/  # Benchmarks
│   ├── chaos_dist/     # Docker-dependent chaos
│   ├── chaos_controlled/   # Controlled chaos
│   ├── compatibility/  # Protocol versions
│   └── contract/       # Edge-core contract
├── utilities/          # IrisClient, TLS helpers
└── artifacts/          # Test outputs (gitignored)
```

## Test Client TLS Configuration

All test clients use TLS by default:

```python
from tests.utilities.iris_client import IrisClient

# TLS enabled by default
client = IrisClient(host='localhost', port=8085)

# Explicit TLS control
client = IrisClient(host='localhost', port=8085, use_tls=True)
client = IrisClient(host='localhost', port=8085, use_tls=False)  # For plaintext rejection tests
```
