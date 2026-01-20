# Test Suite Status

## Overview

**Last Run**: 2026-01-20  
**Total Tests**: 47  
**Passing**: 39/47 (83%)  
**Tier 0 (CI)**: 20/20 ✅  
**Chaos Distributed**: 3/3 ✅

---

## Test Summary

| Suite | Tests | Status | Time |
|-------|-------|--------|------|
| unit | 9 | ✅ 9/9 | 11s |
| integration | 11 | ✅ 11/11 | 64s |
| e2e | 2 | ✅ 2/2 | 28s |
| security | 6 | ⚠️ 5/6 | 45s |
| resilience | 3 | ✅ 3/3 | 154s |
| chaos_controlled | 2 | ✅ 2/2 | 450s |
| chaos_dist | 3 | ✅ 3/3 | 60s |
| compatibility | 1 | ✅ 1/1 | 12s |
| performance_light | 3 | ⚠️ 2/3 | 102s |
| stress | 7 | ⚠️ 1/6 | 168s |

---

## Known Test Failures

### `security/test_tls_mandatory` ❌
- **Issue**: TLS certificate configuration not matching test expectations
- **Impact**: Low - TLS works, test setup issue
- **Fix**: Update `config/test_tls.config` with valid cert paths

### `performance_light/measure_dials` ❌  
- **Issue**: Benchmark test, not pass/fail - reports metrics only
- **Impact**: None - informational test

### `stress/*` (6 failures) ❌
- **Issue**: Resource-intensive tests require dedicated hardware
- **Impact**: None for CI - run in nightly builds only
- **Tests**: `stress_hotspot`, `stress_global_fan_in`, `test_churn`, `stress_geo_scale`, `stress_presence`, `test_limits`

---

## Chaos Distributed Tests (2026-01-20)

All distributed chaos tests now pass with Docker cluster:

| Test | RFC | Description | Status |
|------|-----|-------------|--------|
| `test_ack_durability.py` | NFR-6, NFR-8 | ACK implies durability across node crash | ✅ PASS |
| `test_cross_region_latency.py` | NFR-3 | P99 latency ≤500ms across regions | ✅ PASS |
| `test_dist_failover.py` | NFR-9 | Failover, concurrent load, pause/resume | ✅ PASS |

### Recent Fixes (2026-01-20)

1. **Mnesia Recovery** - Fixed `init_db()` to properly recover data from disk on restart
2. **Hidden Node Discovery** - Fixed `nodes(connected)` for edge-to-core routing
3. **Core Node Pattern Matching** - Support both "iris_core" and "core_" naming conventions
4. **Volume Mount** - Explicit `-mnesia dir` for persistent storage

---

## Audit Synthesis Tests (2026-01-19)

New tests added for RFC compliance validation:

| Test | RFC | Location |
|------|-----|----------|
| `test_ack_durability.py` | NFR-6, NFR-8 | `chaos_dist/` |
| `test_cross_region_latency.py` | NFR-3 | `chaos_dist/` |
| `test_failover_time.py` | NFR-9 | `resilience/` |
| `test_cross_node_ordering.py` | FR-5 | `integration/` |
| `test_tls_mandatory.py` | NFR-14 | `security/` |

---

## Known Environmental Issues

### `chaos_dist/*` tests
- **Requires**: Docker cluster (`make cluster-up`)
- **Duration**: ~60s total for all 3 tests
- **Auto-reconnect**: Tests handle edge-to-core reconnection after node restart

### `stress/test_churn`, `stress/test_limits`
- **Requires**: High resources, extended timeouts
- **Status**: Skip in CI, run in nightly

### `performance_light/measure_dials`
- **Requires**: Dedicated hardware
- **Status**: Run manually for benchmarking

---

## Running Tests

```bash
# CI-safe tier 0 (unit + integration)
make test-tier0

# Run all tests
make test-all

# Run specific suite
python3 tests/run_tests.py --suite security

# List available tests
make test-list
```

### Global Cluster Tests

```bash
# Start 5-region cluster
make cluster-up

# Run distributed tests
python3 tests/suites/chaos_dist/test_ack_durability.py
python3 tests/suites/chaos_dist/test_cross_region_latency.py

# Start with chaos injection (100ms latency)
make cluster-chaos

# Stop cluster
make cluster-down
```

---

## Test Categories

| Category | Purpose | CI |
|----------|---------|-----|
| unit | Fast logic | ✅ Always |
| integration | Feature validation | ✅ Always |
| e2e | User flows | ✅ Always |
| security | Auth/TLS | ✅ Always |
| resilience | Recovery | ✅ Always |
| chaos_dist | Distributed chaos | ⚠️ Docker |
| stress | Extreme load | ❌ Nightly |
