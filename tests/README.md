# Test Suite

**Status**: 75+ tests | **TLS Enforced** | [Full Documentation](../docs/TESTING.md)

## Quick Start

```bash
# Run ALL tests (recommended)
./tests/run_all_tests.sh

# Run non-Docker tests only (faster)
./tests/run_all_tests.sh --quick

# Run Docker chaos tests only
./tests/run_all_tests.sh --docker-only

# Show help
./tests/run_all_tests.sh --help
```

## Proven Scripts

| Script | Purpose |
|--------|---------|
| `tests/run_all_tests.sh` | Main test runner |
| `docker/global-cluster/cluster.sh` | Cluster up/down |
| `docker/global-cluster/init_cluster.sh` | Mnesia initialization |
| `docker/global-cluster/run_chaos_tests.sh` | Batch chaos runner |

## Single Docker Test

```bash
cd docker/global-cluster
./cluster.sh down && ./cluster.sh up && python3 ../../tests/suites/chaos_dist/<test>.py
```

## Phase-Based Execution

| Phase | Tests | Server Management |
|-------|-------|-------------------|
| Phase 1 | Unit tests | No server needed |
| Phase 2 | Integration, E2E, Security, etc. | Shared TLS server |
| Phase 3 | ClusterManager tests | Self-managed per test |
| Phase 4 | Docker chaos_dist | Fresh cluster per test |

## Structure

```
tests/
├── run_all_tests.sh    # Main test runner (proven)
├── framework/          # ClusterManager, assertions
├── suites/
│   ├── unit/           # Property-based tests
│   ├── integration/    # Core message flow
│   ├── e2e/            # End-to-end scenarios
│   ├── security/       # TLS, auth, rate limiting
│   ├── resilience/     # Fault tolerance
│   ├── stress/         # Load testing
│   ├── performance_light/  # Benchmarks
│   ├── chaos_dist/     # Docker-based chaos
│   ├── chaos_controlled/   # Controlled chaos
│   ├── compatibility/  # Protocol versions
│   └── contract/       # Edge-core contract
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
