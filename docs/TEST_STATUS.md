# Test Suite Status

## Overview

**Last Run**: 2026-01-19  
**Total Tests**: 45+  
**Tier 0 (CI)**: 20/20 ✅  
**Unit**: 58/58 ✅

---

## Test Summary

| Suite | Tests | Status | Time |
|-------|-------|--------|------|
| unit | 9 | ✅ 100% | 11s |
| integration | 11 | ✅ 100% | 59s |
| e2e | 2 | ✅ 100% | ~10s |
| security | 3 | ✅ 100% | ~5s |
| resilience | 2 | ✅ 100% | ~30s |
| chaos_controlled | 2 | ✅ 100% | ~20s |

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

### `chaos_dist/test_dist_failover`
- **Requires**: Multi-node distributed setup or Docker cluster
- **Fix**: `make cluster-up` then run test

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
