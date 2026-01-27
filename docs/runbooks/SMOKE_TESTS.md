# Smoke Test Runbook

This runbook documents the procedure for running the complete Project Iris test suite from a clean slate to ensure deterministic, reproducible results.

## Overview

| Metric | Value |
|--------|-------|
| Total Tests | 86 |
| Suites | 11 |
| Estimated Duration | 50-90 minutes |
| Docker Required | Yes (for chaos_dist suite) |

## Prerequisites

### Required Software

- **Erlang/OTP 26+**: `erl -version`
- **Python 3.8+**: `python3 --version`
- **Docker**: `docker --version`
- **Docker Compose**: `docker compose version`

### Python Dependencies

```bash
pip install -r requirements-test.txt
```

### Verify Environment

```bash
# Check Erlang
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'

# Check Docker
docker info > /dev/null 2>&1 && echo "Docker OK" || echo "Docker NOT running"
```

---

## Clean Slate Procedure

**CRITICAL**: Always start from a clean slate to ensure deterministic results.

### Why Clean Slate is Necessary

1. **Zombie Erlang Processes**: Previous test runs may leave beam.smp processes
2. **Stale Mnesia Data**: `/tmp/mnesia*` and `/tmp/iris_*` contain test state
3. **Docker Containers**: Previous cluster containers may be in bad state
4. **Test Artifacts**: Old test results can confuse the runner

### Clean Slate Command

Run this command before every full test run:

```bash
# Kill all Erlang processes
pkill -9 -f beam.smp

# Stop and remove all Docker containers
docker stop $(docker ps -aq) 2>/dev/null
docker rm -f $(docker ps -aq) 2>/dev/null

# Clean temp files and test artifacts
rm -rf /tmp/iris_* /tmp/mnesia* tests/artifacts/runs/*

echo "Clean slate ready"
```

### One-Liner Version

```bash
pkill -9 -f beam.smp; docker stop $(docker ps -aq) 2>/dev/null; docker rm -f $(docker ps -aq) 2>/dev/null; rm -rf /tmp/iris_* /tmp/mnesia* tests/artifacts/runs/*; echo "Clean slate ready"
```

---

## Test Execution

### Quick Smoke (Tier 0 Only)

For fast CI validation (unit + integration):

```bash
TEST_SEED=42 TEST_PROFILE=smoke python3 tests/run_tests.py --tier 0
```

- **Tests**: 39 (21 unit + 18 integration)
- **Duration**: ~2 minutes
- **Docker**: Not required

### Full Smoke (All 86 Tests)

Complete test suite including Docker-dependent tests:

```bash
TEST_SEED=42 TEST_PROFILE=smoke python3 tests/run_tests.py --all --with-cluster
```

- **Tests**: 86
- **Duration**: 50-90 minutes
- **Docker**: Required (auto-starts)

### Tier-by-Tier

| Tier | Command | Tests | Duration |
|------|---------|-------|----------|
| 0 | `--tier 0` | 39 | ~2 min |
| 1 | `--tier 1` | 11 | ~4 min |
| All | `--all --with-cluster` | 86 | ~60 min |

### Individual Suites

```bash
# Run specific suite
python3 tests/run_tests.py --suite unit
python3 tests/run_tests.py --suite integration
python3 tests/run_tests.py --suite security
python3 tests/run_tests.py --suite chaos_dist  # Auto-starts Docker
```

---

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TEST_SEED` | 42 | Master seed for deterministic random |
| `TEST_PROFILE` | smoke | Test profile (smoke/full) |
| `IRIS_HOST` | localhost | Target host for tests |
| `IRIS_PORT` | 8085 | Target port for tests |
| `CI` | (unset) | Enables CI-specific behaviors |

### Deterministic Execution

For reproducible results, always set:

```bash
TEST_SEED=42 TEST_PROFILE=smoke python3 tests/run_tests.py --all
```

---

## Expected Results

### Test Counts by Suite

| Suite | Tests | Docker |
|-------|-------|--------|
| unit | 21 | No |
| integration | 18 | No |
| e2e | 5 | No |
| security | 7 | No |
| resilience | 3 | No |
| compatibility | 1 | No |
| contract | 1 | No |
| performance_light | 6 | No |
| chaos_controlled | 2 | No |
| chaos_dist | 9 | Yes |
| stress | 13 | No |
| **Total** | **86** | |

### Pass/Fail Criteria

- **PASS**: Exit code 0, all tests pass
- **FAIL**: Exit code 1, one or more tests failed
- **SKIP**: Exit code 2, test skipped with documented reason

### Known Flaky Tests

The following tests may occasionally fail due to Docker cluster state:

| Test | Suite | Issue |
|------|-------|-------|
| `test_cross_region_latency` | chaos_dist | Requires manual cluster setup |
| `test_ack_durability` | chaos_dist | Transient cluster state |

These tests pass when the Docker cluster is properly initialized with Mnesia replication.

---

## Troubleshooting

### "Server not available" errors

```bash
# Start local cluster
make start

# Or start Docker cluster
./docker/global-cluster/cluster.sh up
```

### Docker cluster won't start

```bash
# Full Docker cleanup
docker stop $(docker ps -aq)
docker rm -f $(docker ps -aq)
docker network prune -f
docker volume prune -f

# Restart
./docker/global-cluster/cluster.sh up
```

### Mnesia table errors

```bash
# Clear Mnesia data
rm -rf /tmp/Mnesia.*
rm -rf /tmp/mnesia*

# Restart cluster
./docker/global-cluster/cluster.sh down
./docker/global-cluster/cluster.sh up
```

### Cross-region tests fail

```bash
# Initialize replication
./docker/global-cluster/cluster.sh setup-replication

# Verify cluster
python3 scripts/verify_cluster_ready.py
```

### Test hangs

```bash
# Kill all Erlang processes
pkill -9 -f beam.smp

# Check for zombie containers
docker ps -a

# Full cleanup and retry
pkill -9 -f beam.smp; docker stop $(docker ps -aq) 2>/dev/null; docker rm -f $(docker ps -aq) 2>/dev/null
```

### Re-run a single failed test

```bash
# Run specific test file
python3 tests/suites/chaos_dist/test_ack_durability.py

# Run with verbose output
python3 -u tests/suites/chaos_dist/test_ack_durability.py 2>&1 | tee test.log
```

---

## CI Integration

### GitHub Actions

```yaml
- name: Run Smoke Tests
  env:
    TEST_SEED: 42
    TEST_PROFILE: smoke
  run: |
    python3 tests/run_tests.py --tier 0
```

### Full CI with Docker

```yaml
- name: Run Full Suite
  env:
    TEST_SEED: 42
    TEST_PROFILE: smoke
  run: |
    python3 tests/run_tests.py --all --with-cluster
```

---

## See Also

- [TEST_STATUS.md](../TEST_STATUS.md) - Current test status and results
- [TEST_CONTRACT.md](../TEST_CONTRACT.md) - Test exit code contract
- [TEST_DETERMINISM.md](../TEST_DETERMINISM.md) - Deterministic test standards
- [CROSS_REGION.md](CROSS_REGION.md) - Cross-region cluster operations
