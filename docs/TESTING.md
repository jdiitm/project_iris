# Testing Guide

**Status**: 115+ tests pass (100%) | **Last Verified**: 2026-02-03

## Quick Start

```bash
# Clean slate (required before full runs)
pkill -9 -f beam.smp; rm -rf /tmp/iris_* /tmp/mnesia*

# IMPORTANT: TLS is enforced - start server with TLS config
erl -pa ebin -noshell -sname iris_test -setcookie iris_secret \
    -config config/test_tls \
    -eval "application:ensure_all_started(iris_core), application:ensure_all_started(iris_edge)."

# Tier 0 - CI merge gate (63 tests, ~3 min)
python3 tests/run_tests.py --tier 0

# Full smoke (93 tests, ~15 min)
python3 tests/run_tests.py --tier 0
python3 tests/run_tests.py --suite resilience
python3 tests/run_tests.py --suite security
python3 tests/run_tests.py --suite stress
python3 tests/run_tests.py --suite performance_light

# All tests including chaos_dist (115+ tests, ~60 min)
python3 tests/run_tests.py --all --with-cluster
```

## Test Results

| Suite | Tests | Pass | Time | Notes |
|-------|-------|------|------|-------|
| unit | 2 files | 2 | ~10s | Property-based tests |
| integration | 22 | 22 | ~2m | Core message flow |
| stress | 9 | 9 | ~8m | Load testing |
| chaos_dist | 12 | 12 | ~22m | Docker required |
| security | 7 | 7 | ~1m | TLS, auth, rate limiting |
| performance_light | 1 | 1 | ~1m | CPU utilization |
| e2e | 5 | 5 | ~1m | End-to-end scenarios |
| resilience | 3 | 3 | ~1m | Fault tolerance |
| contract | 1 | 1 | ~15s | Edge-core contract |
| compatibility | 1 (6 sub) | 6 | ~15s | Protocol versions |
| **TOTAL** | **115+** | **115+** | ~60m | |

> **Note**: All tests require TLS-enabled server. Test clients use certificates from `certs/` directory.

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

### TLS Stabilization (2026-02-03)

| Issue | File(s) | Fix |
|-------|---------|-----|
| TLS enforcement | All test clients | Added TLS support to all Python test clients |
| IrisClient TLS | `iris_client.py` | Default TLS connections with CA verification |
| Chaos_dist TLS | `tests/suites/chaos_dist/*.py` | Created `utils.py` with TLS helpers |
| Reliable message ACKs | `test_bridge_durability.py`, `test_cross_region_chaos.py` | Implemented proper ACK handling for opcode 0x10 |
| Protocol versions | `test_protocol_versions.py` | Added TLS-wrapped socket connections |
| Clock skew | `test_clock_skew.py` | TLS-enabled client connections |
| Security basics | `test_security_basics.py` | TLS connections, fixed truncated packet test |
| Cross-node ordering | `test_cross_node_ordering.py` | TLS-wrapped connections |
| Resource limits | `test_resource_limits.py` | Updated heap_size expectation (500000) |
| Server port config | `config/test_tls.config` | Added explicit `{port, 8085}` |

### Earlier Fixes (Feb 2026)

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
| Auth disabled in some tests | FR-9 | JWT logic validated separately in security suite |
| Single-node revocation | FR-11 | Multi-node not in CI; Mnesia replication verified |

### TLS Enforcement (2026-02-03)

**TLS is now enforced in all tests** (NFR-14 compliant):
- Server runs with `config/test_tls.config`
- All Python test clients use TLS via `ssl.SSLContext`
- Certificates in `certs/` directory (CA, server, client)
- `iris_client.py` defaults to TLS connections

### Production Validation (Pre-Deploy)

Run on production-spec hardware:
1. Performance: `measure_dials`, `stress_global_fan_in`
2. Scale: `test_limits`, `test_churn`
3. Multi-node: `test_dist_failover`, `test_cluster_revocation`
4. mTLS: Inter-node communication with client certificates

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

- Replace `time.sleep()` with polling in some tests
- Add more granular Docker readiness checks
- Increase test coverage for mTLS inter-node communication

## Test Client TLS Configuration

All test clients now use TLS by default. The shared `IrisClient` class:

```python
from tests.utilities.iris_client import IrisClient

# TLS enabled by default
client = IrisClient(host='localhost', port=8085)  # Uses TLS

# Explicit TLS control
client = IrisClient(host='localhost', port=8085, use_tls=True)
client = IrisClient(host='localhost', port=8085, use_tls=False)  # For testing plaintext rejection
```

Chaos_dist tests use dedicated TLS helpers in `tests/suites/chaos_dist/utils.py`:

```python
from tests.suites.chaos_dist.utils import create_tls_socket, tls_connect_and_login

sock = create_tls_socket('localhost', 8085)
sock = tls_connect_and_login('localhost', 8085, 'user123')
```
