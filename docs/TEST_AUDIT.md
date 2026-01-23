# Test Suite Audit Report

**Date**: 2026-01-23  
**Auditor**: Automated Analysis  
**Scope**: All 53 test files in `tests/suites/` and `test_utils/`

---

## Executive Summary

| Category | Count | Severity |
|----------|-------|----------|
| CI-mode graceful skipping | 7 files | CRITICAL |
| Timing-based sleeps | 214 calls | HIGH |
| Implicit skip on failure | 10 files | HIGH |
| Inconsistent return codes | 12 files | MEDIUM |
| Dynamic scale reduction | 6 files | MEDIUM |

---

## Category A: CI-Mode Graceful Skipping (CRITICAL)

Tests that return exit code 0 (pass) when they should fail.

### A1. test_cross_region_latency.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/chaos_dist/test_cross_region_latency.py` |
| **Lines** | 45-46, 395-399 |
| **Trick** | `IS_CI` check returns `sys.exit(0)` when no messages delivered |
| **Root Cause** | Mnesia `presence` table not replicated across regions |
| **Required Fix** | Implement cross-region replication, remove IS_CI branch |

```python
# PROBLEMATIC CODE (lines 395-399):
if IS_CI:
    print("\n[CI MODE] SKIP: Cross-region Mnesia replication not configured")
    sys.exit(0)  # <-- Returns PASS when test actually FAILED
```

---

### A2. test_multimaster_durability.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/chaos_dist/test_multimaster_durability.py` |
| **Lines** | 47-48, 374-384 |
| **Trick** | Returns `0` (skip) when test fails in CI mode |
| **Root Cause** | Multi-master Mnesia not configured in Docker cluster |
| **Required Fix** | Configure proper multi-master replication |

```python
# PROBLEMATIC CODE (lines 374-384):
if IS_CI:
    print("RESULT: SKIPPED (CI) - Multi-master infrastructure not fully configured")
    return 0  # Graceful skip in CI  <-- WRONG: should return 2 or fail
```

---

### A3. test_failover_time.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/resilience/test_failover_time.py` |
| **Lines** | 34-35, 219-222 |
| **Trick** | Returns `None` when Docker container unavailable |
| **Root Cause** | Docker cluster not guaranteed to be running |
| **Required Fix** | Ensure cluster is running or explicitly skip with exit(2) |

```python
# PROBLEMATIC CODE (lines 219-222):
if IS_CI:
    print("\n[CI MODE] SKIP: Docker container not available")
    return None  # Graceful skip  <-- Treated as PASS by runner
```

---

### A4. test_churn.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/stress/test_churn.py` |
| **Lines** | 117-137 |
| **Trick** | Reduces scale from 500,000 to 100 users (99.98% reduction) |
| **Root Cause** | Test designed for production-scale infra, not CI |
| **Required Fix** | Create separate smoke profile with explicit targets |

```python
# PROBLEMATIC CODE (lines 119-128):
if IS_CI:
    DEFAULT_BASE = 100      # vs 500000 (0.02% of intended)
    DEFAULT_CHURN = 50      # vs 50000 (0.1% of intended)
    DEFAULT_CYCLES = 2      # vs 3
```

---

### A5. test_limits.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/stress/test_limits.py` |
| **Lines** | 132-159 |
| **Trick** | Reduces scale AND dynamically adjusts RAM limits |
| **Root Cause** | Test designed for production-scale infra |
| **Required Fix** | Fixed targets per profile, no dynamic adjustment |

```python
# PROBLEMATIC CODE (lines 158-159):
if IS_CI:
    # CI mode: base 1GB + 50KB/user (very generous for stability)
    ram_limit_mb = 1024 + (args.users * 50 // 1024)  # <-- Moves goalpost
```

---

### A6. stress_global_fan_in.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/stress/stress_global_fan_in.py` |
| **Lines** | 19, 23-35 |
| **Trick** | Reduces from 5 regions/250 threads to 2 regions/20 threads |
| **Root Cause** | Test requires multi-region Docker cluster |
| **Required Fix** | Separate smoke vs full profiles |

```python
# PROBLEMATIC CODE (lines 23-35):
if IS_CI:
    VIP_BUCKET_COUNT = 50   # vs 200
    NUM_REGIONS = 2         # vs 5
    SENDERS_PER_REGION = 10 # vs 50
    DURATION = 30           # vs 60
```

---

### A7. test_fanout.py

| Attribute | Value |
|-----------|-------|
| **File** | `tests/suites/stress/test_fanout.py` |
| **Lines** | 50-57 |
| **Trick** | Reduces recipients from 1000 to 50, adds timeout multiplier |
| **Root Cause** | Test requires significant connection capacity |
| **Required Fix** | Separate smoke vs full profiles |

```python
# PROBLEMATIC CODE (lines 54-57):
if IS_CI:
    RECIPIENTS_SMALL = 5      # vs 10
    RECIPIENTS_MEDIUM = 20    # vs 100
    RECIPIENTS_LARGE = 50     # vs 1000
```

---

## Category B: Timing-Based Sleeps

### B1. Docker Compose Startup Sleeps

| File | Line | Sleep | Purpose | Fix |
|------|------|-------|---------|-----|
| `docker-compose.yml` | 68 | `timer:sleep(2000)` | Wait for core-east-1 | Readiness check |
| `docker-compose.yml` | 110 | `timer:sleep(3000)` | Wait for core-west-1 | Readiness check |
| `docker-compose.yml` | 125 | `timer:sleep(4000)` | Wait for core-west-2 | Readiness check |
| `docker-compose.yml` | 167 | `timer:sleep(5000)` | Wait for core-eu-1 | Readiness check |
| `docker-compose.yml` | 182 | `timer:sleep(6000)` | Wait for core-eu-2 | Readiness check |

### B2. Test File Sleep Counts

| File | Sleep Calls | Worst Offender |
|------|-------------|----------------|
| `test_ordering_under_failure.py` | 15 | `time.sleep(1)` after each message |
| `test_resilience.py` | 12 | `time.sleep(2)` in monitoring loop |
| `test_ack_durability.py` | 10 | `time.sleep(2)` waiting for replication |
| `test_dist_failover.py` | 11 | `time.sleep(5)` after container operations |
| `stress_global_fan_in.py` | 7 | `time.sleep(1)` in monitor loop |
| `test_presence.py` | 6 | `time.sleep(0.5)` for presence propagation |
| `test_compatibility.py` | 7 | `time.sleep(0.1)` between protocol versions |

---

## Category C: Implicit Skip on Failure

Tests that return `None` or `exit(0)` instead of failing.

| File | Pattern | Line | Correct Behavior |
|------|---------|------|------------------|
| `test_failover_time.py` | `return None` | 182, 187, 211, 222 | `sys.exit(2)` |
| `test_multimaster_durability.py` | `return None` | 224, 229, 234, 255, 262 | `sys.exit(2)` |
| `test_ack_durability.py` | `return None` | 174, 187, 283, 297, 304 | `sys.exit(2)` |
| `test_split_brain.py` | `return None` | 230, 234 | `sys.exit(2)` |
| `test_tls_mandatory.py` | `sys.exit(0)` | 214, 227 | `sys.exit(2)` |
| `test_cross_node_ordering.py` | `return None` | 106, 127 | `sys.exit(2)` |
| `test_edge_core_contract.py` | `return None` | 452, 512 | `sys.exit(2)` |

---

## Category D: Inconsistent Return Code Semantics

| File | True | False | None | exit(0) | exit(1) |
|------|------|-------|------|---------|---------|
| `test_failover_time.py` | Pass | Fail | Skip | Pass | Fail |
| `test_multimaster_durability.py` | Pass | Fail | Skip | Skip/CI | Fail |
| `test_cross_region_latency.py` | N/A | N/A | N/A | Pass/Skip | Fail |
| `test_ack_durability.py` | Pass | Fail | Skip | Pass | Fail |
| `test_split_brain.py` | Pass | Fail | Skip | Pass | Fail |
| `test_tls_mandatory.py` | N/A | N/A | N/A | Pass/Skip | Fail |

**Standard Required**:
- `exit(0)` = PASS
- `exit(1)` = FAIL
- `exit(2)` = SKIP (with documented reason)

---

## Category E: Infrastructure Gaps

### E1. Mnesia Replication Not Initialized

**Problem**: `init_cross_region_replication()` exists in `iris_core.erl` but is NEVER called.

**Evidence**:
```bash
$ grep -r "init_cross_region_replication" docker/
# No results - function is never invoked during cluster startup
```

**Affected Tests**:
- `test_cross_region_latency.py`
- `test_multimaster_durability.py`
- `test_ack_durability.py`

### E2. Edge Nodes Hardcoded to Single Core

**Problem**: Each edge only pings ONE core, cannot route to users on other cores.

**Evidence** (from `docker-compose.yml`):
```yaml
edge-west-1:
  command: "... net_adm:ping(core_west_1@corewest1)."  # Only pings core_west_1
```

**Required Fix**: Edges must discover all cores via pg registry.

### E3. No Readiness Checks

**Problem**: Cluster startup uses `timer:sleep()` instead of actual readiness verification.

**Required Fix**: Add health endpoint that returns `ready` only when:
- Mnesia tables are loaded
- Cross-region replication is complete
- All expected nodes are connected

---

## Category F: Test Discovery Analysis

### Current Discovery

| Source | Files | Method |
|--------|-------|--------|
| `tests/suites/*/` | 46 Python files | Glob `*.py` excluding `__init__.py` |
| `test_utils/` | 7 Erlang files | Glob `*_tests.erl` (unit suite only) |
| **Total** | 53 | |

### Files Not Matching `test_*.py` Pattern

These files ARE discovered (glob is `*.py`), but naming is inconsistent:

| File | Current Name | Should Be |
|------|--------------|-----------|
| `stress_geo_scale.py` | Non-standard | `test_geo_scale.py` |
| `stress_global_fan_in.py` | Non-standard | `test_global_fan_in.py` |
| `stress_hotspot.py` | Non-standard | `test_hotspot.py` |
| `stress_offline_delete.py` | Non-standard | `test_offline_delete.py` |
| `stress_presence.py` | Non-standard | `test_presence_stress.py` |
| `chaos_combined.py` | Non-standard | `test_chaos_combined.py` |
| `ultimate_chaos.py` | Non-standard | `test_ultimate_chaos.py` |
| `benchmark_throughput.py` | Non-standard | `test_benchmark_throughput.py` |
| `benchmark_unit_cost.py` | Non-standard | `test_benchmark_unit_cost.py` |
| `measure_dials.py` | Non-standard | `test_measure_dials.py` |

---

## Remediation Priority

### P0 - Must Fix Before Any Test Can Be Trusted

1. Fix Mnesia cross-region replication (infra)
2. Remove all `IS_CI` branches that change pass/fail
3. Standardize return codes (0/1/2)

### P1 - Required for Determinism

4. Replace `timer:sleep()` with readiness checks in Docker
5. Replace `time.sleep()` with polling in tests
6. Create explicit smoke vs full profiles

### P2 - Code Quality

7. Rename files to `test_*.py` convention
8. Add test documentation
9. Create test dependency graph

---

## Appendix: Full Sleep Inventory

```
tests/suites/stress/test_limits.py:3
tests/suites/stress/test_churn.py:5
tests/suites/resilience/test_failover_time.py:4
tests/suites/stress/stress_global_fan_in.py:7
tests/suites/chaos_dist/test_multimaster_durability.py:5
tests/run_tests.py:5
tests/suites/chaos_dist/test_cross_region_latency.py:5
tests/suites/stress/test_fanout.py:4
tests/suites/performance_light/benchmark_throughput.py:1
tests/suites/contract/test_edge_core_contract.py:1
tests/suites/resilience/test_clock_skew.py:4
tests/suites/chaos_dist/test_ordering_under_failure.py:15
tests/suites/unit/test_proto_properties.py:1
tests/suites/security/test_protocol_fuzz.py:15
tests/suites/security/test_mtls_enforcement.py:1
tests/suites/performance_light/benchmark_unit_cost.py:1
tests/suites/integration/test_presence.py:6
tests/suites/resilience/test_hard_kill.py:4
tests/suites/integration/test_hotkey_bucketing.py:2
tests/suites/integration/test_offline_storage.py:5
tests/suites/integration/test_rate_limiting.py:2
tests/suites/integration/test_durability.py:3
tests/suites/integration/test_backpressure.py:1
tests/perf/collect_metrics.py:2
tests/suites/compatibility/test_protocol_versions.py:7
tests/suites/chaos_dist/test_split_brain.py:2
tests/suites/chaos_dist/test_ack_durability.py:10
tests/suites/performance_light/measure_dials.py:2
tests/suites/security/test_tls_mandatory.py:2
tests/suites/chaos_dist/test_dist_failover.py:11
tests/suites/integration/test_cross_node_ordering.py:4
tests/suites/e2e/test_full_conversation.py:5
tests/suites/e2e/test_offline_reconnect.py:7
tests/suites/security/test_security_basics.py:5
tests/suites/integration/test_deduplication.py:6
tests/suites/integration/test_auth_flow.py:1
tests/suites/integration/test_message_ordering.py:2
tests/suites/chaos_controlled/chaos_combined.py:3
tests/suites/resilience/test_resilience.py:12
tests/framework/cluster.py:6
tests/suites/stress/stress_hotspot.py:7
tests/suites/stress/stress_geo_scale.py:1
tests/suites/stress/stress_offline_delete.py:1
tests/suites/chaos_controlled/ultimate_chaos.py:5
```

**Total: 214 sleep calls across 52 files**
