# Test Suite

**Status**: 75 tests pass (100%) | **TLS Enforced** | [Full Documentation](../docs/TESTING.md)

## Quick Start

```bash
# Run all tests (server lifecycle managed automatically)
python3 tests/run_tests.py --all

# Run all tests, skip Docker (faster)
python3 tests/run_tests.py --all --skip-docker

# CI Tiers (independent, no overlap between tiers)
python3 tests/run_tests.py --tier 0   # unit, integration
python3 tests/run_tests.py --tier 1   # e2e, security, resilience
python3 tests/run_tests.py --tier 2   # performance, stress, chaos_controlled

# Run specific suite
python3 tests/run_tests.py --suite integration

# List all tests
python3 tests/run_tests.py --list

# Kill all processes (cleanup)
python3 tests/run_tests.py --nuke
```

## Phase-Based Execution

The test runner organizes tests into phases based on infrastructure requirements:

| Phase | Tests | Server Management |
|-------|-------|-------------------|
| Phase 1 | Unit tests (2) | No server needed |
| Phase 2 | Integration, E2E, Security, etc. | Shared TLS server |
| Phase 3 | ClusterManager tests (14) | Self-managed per test |
| Phase 4 | Docker chaos_dist (12) | Docker global cluster |

This eliminates redundant server restarts and ensures proper test isolation.

## Structure

```
tests/
├── run_tests.py        # Unified test runner (phase-based)
├── run_all_tests.sh    # Shell script alternative
├── framework/          # ClusterManager, assertions
├── suites/
│   ├── unit/           # Property-based tests (2 files)
│   ├── integration/    # Core message flow (22 tests)
│   ├── e2e/            # End-to-end scenarios (5 tests)
│   ├── security/       # TLS, auth, rate limiting (7 tests)
│   ├── resilience/     # Fault tolerance (3 tests)
│   ├── stress/         # Load testing (14 tests)
│   ├── performance_light/  # Benchmarks (6 tests)
│   ├── chaos_dist/     # Docker-based chaos (12 tests)
│   ├── chaos_controlled/   # Controlled chaos (2 tests)
│   ├── compatibility/  # Protocol versions (1 test)
│   └── contract/       # Edge-core contract (1 test)
└── utilities/
    ├── iris_client.py  # TLS-enabled client (default)
    └── tls_connection.py  # TLS helpers
```

## CI Tiers

Each tier runs **only** its own suites (no overlap):

| Tier | Suites | Trigger |
|------|--------|---------|
| 0 | unit, integration | Every commit |
| 1 | e2e, contract, compatibility, security, resilience | Every PR |
| 2 | performance_light, stress, chaos_controlled | Nightly |

## Writing Tests

```python
from tests.utilities.iris_client import IrisClient
import sys

def main():
    # IrisClient uses TLS by default
    client = IrisClient(host='localhost', port=8085)
    client.connect()
    client.login('test_user')
    # Test logic
    client.close()
    sys.exit(0)  # PASS=0, FAIL=1, SKIP=2

if __name__ == "__main__":
    main()
```

**Rules**: 
- Use TLS-enabled `IrisClient` (default)
- Seed randomness with `TEST_SEED`
- No bare `except: pass`
- Exit codes: 0=PASS, 1=FAIL, 2=SKIP

See [TESTING.md](../docs/TESTING.md) for complete guide.
