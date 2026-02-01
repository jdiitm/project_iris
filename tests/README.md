# Test Suite

**Status**: 113/113 pass (100%) | [Full Documentation](../docs/TESTING.md)

## Quick Start

```bash
# Clean slate
pkill -9 -f beam.smp; rm -rf /tmp/iris_* /tmp/mnesia*

# Tier 0 - CI gate (63 tests, ~3 min)
python3 tests/run_tests.py --tier 0

# Full smoke (93 tests, ~15 min)
python3 tests/run_tests.py --tier 0 && \
python3 tests/run_tests.py --suite resilience && \
python3 tests/run_tests.py --suite security && \
python3 tests/run_tests.py --suite stress && \
python3 tests/run_tests.py --suite performance_light

# All tests (113 tests, ~53 min)
python3 tests/run_tests.py --all --with-cluster

# List tests
python3 tests/run_tests.py --list
```

## Structure

```
tests/
├── run_tests.py     # Test runner
├── framework/       # ClusterManager, assertions
├── suites/          # Test suites
└── utilities/       # IrisClient, helpers
```

## Writing Tests

```python
from tests.framework.cluster import ClusterManager
import sys

def main():
    with ClusterManager(project_root) as cluster:
        # Test logic
        pass
    sys.exit(0)  # PASS=0, FAIL=1, SKIP=2

if __name__ == "__main__":
    main()
```

**Rules**: Use `ClusterManager`, seed randomness with `TEST_SEED`, no bare `except: pass`

See [TESTING.md](../docs/TESTING.md) for complete guide.
