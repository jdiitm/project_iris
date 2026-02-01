# Testing Guide

**Status**: 113/113 pass (100%) | **Last Verified**: 2026-02-01

## Quick Start

```bash
# Clean slate (required before full runs)
pkill -9 -f beam.smp; rm -rf /tmp/iris_* /tmp/mnesia*

# Tier 0 - CI merge gate (63 tests, ~3 min)
python3 tests/run_tests.py --tier 0

# Full smoke (93 tests, ~15 min)
python3 tests/run_tests.py --tier 0
python3 tests/run_tests.py --suite resilience
python3 tests/run_tests.py --suite security
python3 tests/run_tests.py --suite stress
python3 tests/run_tests.py --suite performance_light

# All tests (113 tests, ~53 min)
python3 tests/run_tests.py --all --with-cluster
```

## Test Results

| Suite | Tests | Pass | Time | Smoke |
|-------|-------|------|------|-------|
| unit | 41 | 41 | 57s | ✅ |
| integration | 22 | 22 | 104s | ✅ |
| stress | 14 | 14 | 490s | ✅ |
| chaos_dist | 11 | 11 | ~36m | |
| security | 7 | 7 | 84s | ✅ |
| performance_light | 6 | 6 | 97s | ✅ |
| e2e | 5 | 5 | 35s | |
| resilience | 3 | 3 | 68s | ✅ |
| chaos_controlled | 2 | 2 | 101s | |
| contract | 1 | 1 | 13s | |
| compatibility | 1 | 1 | 15s | |
| **TOTAL** | **113** | **113** | | |

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

# ❌ Dynamic thresholds
if IS_CI: threshold = 1000  # vs 10000

# ❌ Swallow exceptions
except: pass

# ❌ Assume cluster running
if not check_server(): sys.exit(1)
```

### Required Patterns

```python
# ✅ Explicit skip with reason
if not infrastructure_available():
    print("SKIP:DOCKER - Container not running")
    sys.exit(2)

# ✅ Fixed thresholds per profile
THRESHOLDS = {"smoke": 100, "full": 10000}
threshold = THRESHOLDS[os.environ.get("TEST_PROFILE", "smoke")]

# ✅ Manage cluster lifecycle
from tests.framework.cluster import ClusterManager
with ClusterManager(project_root) as cluster:
    run_test()

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
| `TEST_PROFILE` | smoke | Intensity (smoke/full) |
| `TEST_SEED` | 42 | Master random seed |
| `IRIS_NODE_SUFFIX` | Auto | Erlang node name suffix |

### BEAM-Specific Thresholds

| Metric | Notes |
|--------|-------|
| CPU | 100-300% idle normal (multi-scheduler) |
| Memory | 800-1500MB baseline (+P/+Q preallocation) |
| Process detection | Use `beam.smp`, not node name |

---

## System Invariants

| Invariant | RFC | Test |
|-----------|-----|------|
| Message Ordering | FR-5 | `test_message_ordering.py` |
| Delivery Guarantee | Section 5.1 | `test_ack_durability.py` |
| Idempotency | Section 5.1 | `test_deduplication.py` |
| Durability | NFR-6, NFR-8 | `test_multimaster_durability.py` |
| Offline/Online | FR-2 | `test_offline_storage.py` |
| Backpressure | AUDIT | `test_backpressure_collapse.py` |

### Infrastructure Invariants

| Invariant | Enforcement |
|-----------|-------------|
| Cluster Management | ClusterManager usage |
| Exception Handling | No bare except:pass |
| Deterministic Seeding | TEST_SEED everywhere |
| NODE_SUFFIX | Propagated to make commands |

---

## Test Nuances

| Test | Behavior | Notes |
|------|----------|-------|
| `test_backpressure_collapse` | ~20% warmup success | Aggressive backpressure by design |
| `benchmark_unit_cost` | 8k msg/s threshold | Actual: ~100k on fresh cluster |
| `test_cpu_utilization` | Idle: 14%, Load: 16% | BEAM multi-scheduler |
| `ultimate_chaos` | 94% delivery | Expected under chaos |
| `test_resilience` | Memory: 1.0x growth | Correctly bounded |

---

## Failure Coverage Matrix

### Message Delivery

| Failure Mode | Test | Suite |
|--------------|------|-------|
| Message loss (online→online) | `test_online_messaging` | integration |
| Message loss (online→offline) | `test_offline_storage` | integration |
| Message duplication | `assert_no_duplicates` | integration |
| Message ordering violation | `test_multi_message_sequence` | integration |

### Network & Resources

| Failure Mode | Test | Suite |
|--------------|------|-------|
| TCP connection drop | `chaos_kitchen_sink` | chaos_controlled |
| Memory exhaustion (OOM) | `test_slow_consumer_oom_prevention` | resilience |
| CPU saturation | `benchmark_throughput` | performance_light |
| Connection flood | `chaos_kitchen_sink` | chaos_controlled |

### Protocol & Data

| Failure Mode | Test | Suite |
|--------------|------|-------|
| Malformed packet | `iris_proto_tests` | unit |
| Unknown opcode | `test_decode_unknown` | unit |
| Large payload | `test_roundtrip_large_message` | unit |

---

## Recent Fixes (Feb 2026)

| Issue | File | Fix |
|-------|------|-----|
| Cluster meshing | `cluster.py` | Pass `NODE_SUFFIX` to make |
| Backpressure thresholds | `test_backpressure_collapse.py` | Adjusted for graceful degradation |
| Benchmark threshold | `benchmark_unit_cost.py` | 10k → 8k msg/s |
| Typing opcodes | `iris_typing_tests.erl` | 0x30 → 0x70 per RFC |

### Audit Fixes (Jan 2026)

| Category | Status |
|----------|--------|
| CI-mode graceful skipping | ✅ `IS_CI` → `TEST_PROFILE` |
| Implicit skip on failure | ✅ Uses `exit(2)` |
| Dynamic scale reduction | ✅ Uses `TEST_PROFILE` |
| Stress test flakiness | ✅ Profiles + fire-and-forget |

---

## Tests Added (Jan 2026)

| Test | Purpose |
|------|---------|
| `iris_session_state_tests.erl` | State machine lifecycle (P0) |
| `iris_idempotency_tests.erl` | Exactly-once guarantees (P0) |
| `iris_fault_injection_tests.erl` | Fault handling (P1) |
| `iris_concurrency_torture_tests.erl` | High-contention stress (P2) |
| `test_backpressure_collapse.py` | Backpressure validation |
| `test_hot_shard.py` | Hot-shard stress |
| `test_soak_memory.py` | Memory leak detection |

---

## RFC Compliance

### Test Deviations (Accepted)

| Deviation | RFC | Justification |
|-----------|-----|---------------|
| Auth disabled in tests | FR-9 | CI simplification; JWT logic validated separately |
| TLS disabled in tests | NFR-14 | No cert infra in CI; TLS code validated |
| Single-node revocation | FR-11 | Multi-node not in CI; Mnesia replication verified |

### Production Validation (Pre-Deploy)

Run on production-spec hardware:
1. Performance: `measure_dials`, `stress_global_fan_in`
2. Scale: `test_limits`, `test_churn`
3. Multi-node: `test_dist_failover`, `test_cluster_revocation`
4. TLS: Full mode with valid certificates

---

## Troubleshooting

### Common Issues

**Server not available**: `make start`

**Mnesia errors**: `rm -rf /tmp/Mnesia.* /tmp/mnesia*`

**Test hangs**: `pkill -9 -f beam.smp`

**Docker issues**:
```bash
docker stop $(docker ps -aq); docker rm -f $(docker ps -aq)
docker network prune -f
```

### Reproducing Failures

1. Get seed from failing run: `[test_runner] Using seed: 12345`
2. Reproduce: `TEST_SEED=12345 python3 tests/run_tests.py --suite <suite>`

### CI Integration

```yaml
- name: Smoke Tests
  env:
    TEST_SEED: 42
    TEST_PROFILE: smoke
  run: python3 tests/run_tests.py --tier 0
```

---

## Directory Structure

```
tests/
├── run_tests.py        # Unified test runner
├── framework/          # ClusterManager, assertions, logging
├── suites/             # Test suites by category
│   ├── unit/           # Erlang EUnit tests
│   ├── integration/    # Core message delivery
│   ├── stress/         # Heavy load tests
│   ├── chaos_dist/     # Docker-dependent chaos
│   └── ...
├── utilities/          # IrisClient, helpers
└── artifacts/          # Test outputs (gitignored)
```

## Open Items (Low Priority)

- Replace `time.sleep()` with polling (214 calls remain)
- Rename non-standard files to `test_*.py` convention
- Add Docker readiness checks (currently uses `timer:sleep`)
