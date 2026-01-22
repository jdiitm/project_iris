# Test Determinism Standards

This document defines the requirements, constraints, and standards for maintaining deterministic, reproducible test execution in Project Iris.

## Table of Contents

1. [Principles](#principles)
2. [Environment Requirements](#environment-requirements)
3. [Test Runner Configuration](#test-runner-configuration)
4. [Randomness Control](#randomness-control)
5. [State Isolation](#state-isolation)
6. [Timing and Concurrency](#timing-and-concurrency)
7. [Docker Execution](#docker-execution)
8. [Standards for New Tests](#standards-for-new-tests)
9. [Known Exceptions](#known-exceptions)
10. [Troubleshooting Flakiness](#troubleshooting-flakiness)

---

## Principles

All tests in this repository MUST:

1. **Produce identical results** when run multiple times with the same seed
2. **Never depend on wall-clock time** for correctness (only for timeouts)
3. **Clean up all state** before and after execution
4. **Run in any order** without affecting other tests
5. **Work identically** across x86 and ARM architectures
6. **Execute within Docker** as the canonical environment

---

## Environment Requirements

### Required Software

| Component | Version | Purpose |
|-----------|---------|---------|
| Docker | 24.0+ | Hermetic execution environment |
| Docker Compose | 2.20+ | Multi-container orchestration |
| Python | 3.11+ | Test runner and test scripts |
| Erlang/OTP | 25+ | Application runtime |

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `TEST_SEED` | `42` | Master seed for all random operations |
| `TEST_RUN_ID` | Auto-generated | Unique identifier for test run (derived from seed) |
| `PYTHONUNBUFFERED` | `1` | Force unbuffered Python output |
| `CI` | unset | When `true`, reduces resource-intensive test parameters |
| `IRIS_NODE_SUFFIX` | Auto-generated | Suffix for Erlang node names to prevent collisions |
| `IRIS_COOKIE` | `iris_secret` | Erlang distribution cookie |

### Resource Constraints

For consistent results across environments:

| Resource | Minimum | Recommended |
|----------|---------|-------------|
| CPU Cores | 2 | 4+ |
| RAM | 4 GB | 8 GB |
| Disk | 10 GB | 20 GB |
| File Descriptors | 65536 | 1048576 |

---

## Test Runner Configuration

### Execution Command

**Canonical command (Docker):**
```bash
make test-docker
```

**Local development (less deterministic):**
```bash
python3 tests/run_tests.py --all
```

### Test Discovery and Ordering

Tests are discovered and executed in **lexicographic order** by:
1. Suite name (alphabetical)
2. Test file name (alphabetical within suite)
3. Test function name (alphabetical within file)

This ordering is enforced by the test runner and MUST NOT be overridden.

### Timeout Configuration

| Test Tier | Per-Test Timeout | Suite Timeout |
|-----------|------------------|---------------|
| Tier 0 (unit, integration) | 60 seconds | 10 minutes |
| Tier 1 (e2e, security, resilience) | 120 seconds | 15 minutes |
| Tier 2 (chaos, performance) | 300 seconds | 2 hours |
| Tier 3 (stress, soak) | 600 seconds | 24 hours |

---

## Randomness Control

### Seeding Requirements

All random operations MUST use the seeded random instance from `tests/conftest.py`:

```python
# CORRECT: Use seeded random
from tests.conftest import get_seeded_random
rng = get_seeded_random()
username = ''.join(rng.choices(string.ascii_lowercase, k=8))

# INCORRECT: Using unseeded random
import random
username = ''.join(random.choices(string.ascii_lowercase, k=8))
```

### Deterministic Identifiers

For unique identifiers, use the deterministic ID generator:

```python
# CORRECT: Deterministic ID
from tests.conftest import get_test_id
msg_id = get_test_id("latency_test")  # Returns "latency_test_00001"

# INCORRECT: Time-based or UUID
msg_id = f"msg_{int(time.time())}"
msg_id = f"msg_{uuid.uuid4().hex}"
```

### UUID Replacement

If UUIDs are required for protocol compliance, use the deterministic UUID generator:

```python
from tests.conftest import deterministic_uuid
uuid_val = deterministic_uuid()  # Returns predictable UUID format
```

---

## State Isolation

### Pre-Test Cleanup

Before each test suite, the runner performs:

1. Kill all `beam.smp` processes
2. Kill `epmd` (Erlang Port Mapper Daemon)
3. Remove all `Mnesia.*` directories
4. Remove all `*.log` files in project root
5. Wait 2 seconds for port release

### Post-Test Cleanup

After each test suite:

1. Graceful cluster shutdown via `make stop`
2. Force kill any remaining Erlang processes
3. Docker container cleanup (if applicable)

### Mnesia State

Mnesia directories MUST be ephemeral:
- Local tests: `/tmp/Mnesia.{node_name}`
- Docker tests: Named volumes, recreated per run

### File System State

Tests MUST NOT:
- Write to the source tree (except `tests/artifacts/`)
- Depend on files created by previous tests
- Leave files that affect subsequent runs

---

## Timing and Concurrency

### Prohibited Patterns

```python
# PROHIBITED: Relying on wall-clock for correctness
if time.time() > start_time + 5:
    assert condition  # May pass/fail based on system load

# PROHIBITED: Fixed sleep without retry
time.sleep(5)
assert server_ready()  # May fail if server slow

# PROHIBITED: Tight timing assertions
assert latency < 1.0  # May fail on slow CI runners
```

### Approved Patterns

```python
# APPROVED: Polling with timeout
def wait_for_condition(predicate, timeout=30, interval=0.5):
    deadline = time.time() + timeout
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(interval)
    return False

assert wait_for_condition(server_ready)

# APPROVED: Relative timing with generous bounds
latencies = measure_latencies(100)
p99 = percentile(latencies, 99)
assert p99 < 500  # 500ms is generous, catches regressions
```

### Concurrency Rules

1. Tests MUST NOT run in parallel (sequential execution only)
2. Tests MUST NOT spawn background threads that outlive the test
3. Tests using threads MUST join all threads before completion

---

## Docker Execution

### Canonical Environment

The Docker test environment provides:
- Identical OS (Alpine Linux)
- Identical Erlang version
- Identical Python version
- Pinned Python dependencies
- Isolated network
- Consistent resource limits

### Docker Compose Structure

```
docker/test/
├── Dockerfile           # Test runner image
├── docker-compose.test.yml  # Full test cluster
└── .env.test            # Default environment variables
```

### Running Tests in Docker

```bash
# Full test suite (deterministic)
make test-docker

# With custom seed (for reproduction)
TEST_SEED=12345 make test-docker

# Specific suite
TEST_SUITE=unit make test-docker
```

---

## Standards for New Tests

### Checklist for New Tests

- [ ] Uses seeded random from `tests/conftest.py`
- [ ] Uses deterministic IDs, not timestamps or UUIDs
- [ ] Cleans up any created state (files, processes, connections)
- [ ] Uses polling/retry for async operations, not fixed sleeps
- [ ] Has generous timeouts that work on slow CI runners
- [ ] Works when run in isolation AND as part of full suite
- [ ] Documented in test docstring with purpose and requirements

### Test File Template

```python
#!/usr/bin/env python3
"""
Test Name: test_example.py
Suite: integration
Tier: 0

Purpose:
    Validates [specific functionality].

Requirements:
    - Running Iris cluster (core + edge)
    - No special configuration

Determinism:
    - Uses seeded random for usernames
    - No wall-clock dependencies
"""

import sys
import os

# Ensure project root is in path
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.conftest import get_seeded_random, get_test_id

def test_example():
    rng = get_seeded_random()
    test_id = get_test_id("example")
    
    # Test implementation...
    pass

if __name__ == "__main__":
    test_example()
```

---

## Known Exceptions

The following tests are **known to fail** and are excluded from determinism requirements until fixed:

| Test | Suite | Reason | Tracking |
|------|-------|--------|----------|
| `test_cross_region_latency` | chaos_dist | Requires multi-region Mnesia replication | Phase 2 |
| `test_churn` | stress | Requires `iris_extreme_gen.erl` implementation | Phase 2 |
| `test_limits` | stress | Requires `iris_extreme_gen.erl` implementation | Phase 2 |

These tests are marked with `@pytest.mark.known_failure` and skipped in CI.

---

## Troubleshooting Flakiness

### Reproduction Steps

1. Get the seed from the failing CI run:
   ```
   [test_runner] Using seed: 12345
   ```

2. Reproduce locally:
   ```bash
   TEST_SEED=12345 make test-docker
   ```

3. If it passes locally, the issue is environment-specific. Check:
   - Resource constraints (CI may have less RAM/CPU)
   - Network conditions (CI may have higher latency)
   - Timing assumptions (CI may be slower)

### Common Causes

| Symptom | Likely Cause | Fix |
|---------|--------------|-----|
| "Connection refused" | Server not ready | Use `wait_for_port()` |
| "Timeout" on CI only | Timing too tight | Increase timeout 5x for CI |
| Different results each run | Unseeded random | Use `get_seeded_random()` |
| "Address in use" | Previous test leaked | Add cleanup in `teardown` |
| Works alone, fails in suite | State pollution | Check for global state |

### Debugging Tools

```bash
# Run single test with verbose output
python3 tests/run_tests.py --suite unit --verbose

# Run with debug logging
DEBUG=1 python3 tests/run_tests.py --suite integration

# Check for leaked processes
ps aux | grep beam

# Check for leaked ports
lsof -i :8085
```

---

## Changelog

| Date | Change | Author |
|------|--------|--------|
| 2026-01-22 | Initial determinism standards | AI Assistant |

---

## References

- [Test Status](TEST_STATUS.md) - Current test results
- [Test Invariants](TEST_INVARIANTS.md) - Formal test invariants
- [RFC-001](rfc/RFC-001-SYSTEM-REQUIREMENTS.md) - System requirements
