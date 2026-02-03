# Test Suite

**Status**: 115+ tests pass (100%) | **TLS Enforced** | [Full Documentation](../docs/TESTING.md)

## Quick Start

```bash
# Clean slate
pkill -9 -f beam.smp; rm -rf /tmp/iris_* /tmp/mnesia*

# Start TLS-enabled server (REQUIRED)
erl -pa ebin -noshell -sname iris_test -setcookie iris_secret \
    -config config/test_tls \
    -eval "application:ensure_all_started(iris_core), application:ensure_all_started(iris_edge)."

# Tier 0 - CI gate (~3 min)
python3 tests/run_tests.py --tier 0

# Full smoke (~15 min)
python3 tests/run_tests.py --tier 0 && \
python3 tests/run_tests.py --suite resilience && \
python3 tests/run_tests.py --suite security && \
python3 tests/run_tests.py --suite stress && \
python3 tests/run_tests.py --suite performance_light

# All tests including chaos_dist (~60 min, Docker required)
python3 tests/run_tests.py --all --with-cluster

# List tests
python3 tests/run_tests.py --list
```

## Structure

```
tests/
├── run_tests.py        # Unified test runner
├── framework/          # ClusterManager, assertions
├── suites/
│   ├── unit/           # Property-based tests (2 files)
│   ├── integration/    # Core message flow (22 tests)
│   ├── e2e/            # End-to-end scenarios (5 tests)
│   ├── security/       # TLS, auth, rate limiting (7 tests)
│   ├── resilience/     # Fault tolerance (3 tests)
│   ├── stress/         # Load testing (9 tests)
│   ├── chaos_dist/     # Docker-based chaos (12 tests)
│   ├── compatibility/  # Protocol versions (6 sub-tests)
│   ├── contract/       # Edge-core contract (1 test)
│   └── performance_light/  # CPU utilization (1 test)
└── utilities/
    ├── iris_client.py  # TLS-enabled client (default)
    └── tls_connection.py  # TLS helpers
```

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
