# Test Suite Status

## Overview

**Last Run**: 2026-01-24  
**Total Tests**: 58 (including 5 new tests from hardening plan)  
**Passing**: 58 (100%)  
**Failed**: 0  
**Skipped**: 0  
**Deferred**: 0 (all previously deferred tests re-enabled)

## Full Test Results (Jan 24, 2026)

| Tier | Suite | Tests | Passed | Failed | Skipped | Status |
|------|-------|-------|--------|--------|---------|--------|
| 0 | unit | 8 | 8 | 0 | 0 | ✅ ALL PASS |
| 0 | integration | 13 | 13 | 0 | 0 | ✅ ALL PASS |
| 1 | e2e | 4 | 4 | 0 | 0 | ✅ ALL PASS |
| 1 | security | 7 | 7 | 0 | 0 | ✅ ALL PASS |
| 1 | resilience | 4 | 4 | 0 | 0 | ✅ ALL PASS |
| 1 | compatibility | 1 | 1 | 0 | 0 | ✅ ALL PASS |
| 1 | contract | 1 | 1 | 0 | 0 | ✅ ALL PASS |
| 2 | performance_light | 3 | 3 | 0 | 0 | ✅ ALL PASS |
| 2 | chaos_controlled | 2 | 2 | 0 | 0 | ✅ ALL PASS |
| 2 | chaos_dist | 7 | 7 | 0 | 0 | ✅ ALL PASS |
| 3 | stress | 9 | 9 | 0 | 0 | ✅ ALL PASS |
| **TOTAL** | | **58** | **58** | **0** | **0** | **100% pass** |

---

## New Tests Added (Jan 24, 2026)

| Test | Suite | Purpose | Verifies |
|------|-------|---------|----------|
| `test_key_bundle_durability.py` | chaos_dist | E2EE key survival after node kill | NFR-22 |
| `test_opk_exhaustion.py` | e2e | OPK pool depletion handling | NFR-24 |
| `test_no_silent_loss.py` | integration | Zero untracked message drops | AUDIT FIX |
| `test_flow_controller_scale.py` | stress | 100K+ admission checks/sec | AUDIT FIX |
| `test_sender_key_sync.py` | e2e | Offline member key sync | AUDIT FIX |

---

## Re-Enabled Tests (Jan 24, 2026)

The following tests were previously deferred and have been re-enabled:

| Test | Location | Status |
|------|----------|--------|
| `test_ack_durability.py` | chaos_dist | ✅ Active |
| `test_cross_region_latency.py` | chaos_dist | ✅ Active |
| `test_multimaster_durability.py` | chaos_dist | ✅ Active |

These tests now use the new `iris_region_bridge.erl` for reliable cross-region messaging.

---

## Test Profiles

Tests support explicit profiles:

| Profile | Purpose | Command |
|---------|---------|---------|
| `smoke` | Quick validation (default) | `TEST_PROFILE=smoke python3 tests/run_tests.py --all` |
| `full` | Production-scale testing | `TEST_PROFILE=full python3 tests/run_tests.py --all` |

---

## Infrastructure Components

### New Modules for Testing

| Module | Purpose |
|--------|---------|
| `iris_region_bridge.erl` | Cross-region message relay with durability |
| `iris_presence.erl` | Versioned presence for race condition prevention |
| `tests/framework/wait.py` | Polling utilities replacing time.sleep() |
| `scripts/verify_cluster_ready.py` | Cluster readiness verification |
| `docker/global-cluster/init_cluster.sh` | Mnesia replication initialization |

### Cluster Test Infrastructure

| Component | File | Purpose |
|-----------|------|---------|
| Cluster startup | `cluster.sh up` | Start Docker containers |
| Replication init | `init_cluster.sh` | Configure Mnesia multi-master |
| Verification | `verify_cluster_ready.py` | Validate cluster health |
| Test target | `make test-cluster-dist` | Run all cluster tests |
| Test flag | `--with-cluster` | Enable Docker in test runner |

### Test Contract

All tests follow the contract defined in [TEST_CONTRACT.md](TEST_CONTRACT.md):

- `exit(0)` = PASS
- `exit(1)` = FAIL  
- `exit(2)` = SKIP (with documented reason)

---

## Running Tests

### Quick Start

```bash
# Run all local tests (no Docker required)
python3 tests/run_tests.py --all

# Run all tests INCLUDING cross-region (auto-starts Docker)
python3 tests/run_tests.py --all --with-cluster

# Run specific suite
python3 tests/run_tests.py --suite integration
```

### Test Tiers

| Tier | Suites | Command |
|------|--------|---------|
| 0 | unit, integration | `make test-tier0` |
| 1 | resilience, performance_light, chaos_controlled | `make test-tier1` |
| 2 | chaos_dist, stress | `make test-cluster-dist` |

### Cross-Region Tests (Docker Required)

The `chaos_dist` suite requires a Docker global cluster with multi-region Mnesia replication.

**Option 1: Automated (Recommended)**
```bash
# Runs all cluster-dependent tests with automatic lifecycle management
make test-cluster-dist
```

**Option 2: Manual Cluster Management**
```bash
# Start cluster with replication
./docker/global-cluster/cluster.sh up

# Verify cluster is ready
python3 scripts/verify_cluster_ready.py

# Run specific cross-region tests
python3 tests/suites/chaos_dist/test_cross_region_latency.py
python3 tests/suites/chaos_dist/test_multimaster_durability.py

# Stop cluster
make cluster-down
```

**Option 3: Test Runner with Cluster**
```bash
# Run all tests with Docker cluster support
python3 tests/run_tests.py --all --with-cluster

# Run specific suite with Docker cluster
python3 tests/run_tests.py --suite chaos_dist
```

### Cluster Verification

Before running cross-region tests, verify the cluster is ready:

```bash
# Full verification (including cross-region delivery test)
python3 scripts/verify_cluster_ready.py

# Quick verification (skip delivery test)
python3 scripts/verify_cluster_ready.py --quick

# Via Makefile
make cluster-verify
make cluster-verify-quick
```

The verification script checks:
1. All core containers are running
2. Mnesia cluster has formed with all nodes
3. Key tables have >= 2 replicas (replication working)
4. Cross-region message delivery works (West → Sydney)

---

## Hardening Changes Summary (Jan 24, 2026)

### Phase 1: Critical Infrastructure
- ✅ Cross-region bridge (`iris_region_bridge.erl`)
- ✅ Flow controller sharding (lockfree ETS)
- ✅ Silent loss prevention (guaranteed offline fallback)

### Phase 2: E2EE Hardening
- ✅ Key bundle durability (quorum writes)
- ✅ OPK exhaustion handling (SPK fallback)

### Phase 3: Race Condition Fixes
- ✅ Presence versioning (`iris_presence.erl`)
- ✅ Sender key sync on reconnect

### Phase 4: Testing Infrastructure
- ✅ Re-enabled 3 deferred tests
- ✅ Added 5 new chaos/E2EE tests
- ✅ Created polling utility (`tests/framework/wait.py`)

### Phase 5: Documentation
- ✅ Updated RFC_COMPLIANCE.md
- ✅ Updated TEST_STATUS.md
- ✅ Created CROSS_REGION.md runbook

---

## Troubleshooting

### "Server not available" errors

Start the server with `make start` or `./docker/global-cluster/cluster.sh up`.

### Cross-region tests fail

Initialize replication after cluster start:
```bash
./docker/global-cluster/cluster.sh setup-replication
```

### Mnesia table errors

Clear Mnesia data and restart:
```bash
./docker/global-cluster/cluster.sh down
rm -rf /tmp/Mnesia.*
./docker/global-cluster/cluster.sh up
```

---

## See Also

- [TEST_CONTRACT.md](TEST_CONTRACT.md) - Test exit code contract
- [TEST_DETERMINISM.md](TEST_DETERMINISM.md) - Deterministic test standards
- [RFC_COMPLIANCE.md](RFC_COMPLIANCE.md) - RFC compliance status
- [runbooks/CROSS_REGION.md](runbooks/CROSS_REGION.md) - Cross-region operations
