# Test Suite

**156 Python + 102 Erlang tests** | **TLS Enforced** | **Last Verified**: 2026-02-11

Full documentation: [docs/TESTING.md](../docs/TESTING.md)

## Quick Start

```bash
./tests/run_all_tests.sh              # Full suite
./tests/run_all_tests.sh --quick      # Non-Docker only
./tests/run_all_tests.sh --docker-only # Docker chaos only
./tests/run_all_tests.sh --help       # Show all options
```

## Test Contract

- Exit codes: `0` = PASS, `1` = FAIL, `2` = SKIP (with reason)
- All clients use TLS by default (`IrisClient`)
- Seed randomness with `TEST_SEED` for reproducibility
- No bare `except: pass`, no `time.sleep()` for sync (RFC 13.2)

See [docs/TESTING.md](../docs/TESTING.md) for suite details, CI pipeline, and writing tests.
