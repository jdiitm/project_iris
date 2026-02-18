# Test Suite

All tests passing with TLS enforced. See [docs/TESTING.md](../docs/TESTING.md) for authoritative counts, suite details, CI pipeline, and test contract.

```bash
./tests/run_all_tests.sh              # Full suite
./tests/run_all_tests.sh --quick      # Non-Docker only
./tests/run_all_tests.sh --docker-only # Docker chaos only
```
