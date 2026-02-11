# Rigorous Testing Skill for Project Iris

## Skill Name: `rigorous-testing`

## Purpose
Zero-tolerance testing methodology with TLS enforcement, comprehensive log analysis, real-time failure detection, and immediate fix iteration. No errors swallowed, no tests skipped without explicit RFC-compliant reason, all failures analyzed line-by-line.

---

## Key Principles (MANDATORY)

1. **TLS is ENFORCED** - All tests use `config/test_tls.config`
2. **No errors swallowed** - Every error pattern in logs must be addressed
3. **No silent skips** - Every skip must have SKIP:REASON documented
4. **Line-by-line analysis** - Read every line of every log
5. **Iterate until clean** - Test → Fix → Test until 100% pass with 0 errors

---

## Proven Test Scripts

| Script | Purpose |
|--------|---------|
| `tests/run_all_tests.sh` | **Main test runner** - single entry point |
| `docker/global-cluster/cluster.sh` | Docker cluster management (up/down) |
| `docker/global-cluster/init_cluster.sh` | Mnesia cluster initialization |
| `docker/global-cluster/run_chaos_tests.sh` | Chaos tests with fresh cluster per test |

---

## Quick Reference Commands

```bash
# Navigate to project
cd /home/j/.gemini/antigravity/scratch/project_iris

# STEP 1: Run ALL tests (handles cleanup automatically)
./tests/run_all_tests.sh

# STEP 2: Run quick tests only (no Docker)
./tests/run_all_tests.sh --quick

# STEP 3: Run Docker chaos tests only
./tests/run_all_tests.sh --docker-only

# Single Docker test
cd docker/global-cluster
./cluster.sh down && ./cluster.sh up && python3 ../../tests/suites/chaos_dist/test_network_partition.py
```

---

## Test Modes

| Mode | Command | Description |
|------|---------|-------------|
| **Full** | `./tests/run_all_tests.sh` | All tests (unit → Docker chaos) |
| **Quick** | `./tests/run_all_tests.sh --quick` | Non-Docker tests only |
| **Docker Only** | `./tests/run_all_tests.sh --docker-only` | Docker chaos tests only |

---

## Test Suites (156 Python + 101 Erlang tests)

| Suite | Tests | Requires | TLS |
|-------|-------|----------|-----|
| `unit` | 4 | Local compile | N/A |
| `integration` | 40 | TLS server | ✅ |
| `e2e` | 11 | TLS server | ✅ |
| `security` | 23 | TLS server | ✅ |
| `resilience` | 8 | TLS server | ✅ |
| `stress` | 18 | TLS server | ✅ |
| `chaos_dist` | 27 | Docker cluster | ✅ |
| `chaos_controlled` | 2 | Self-managed | ✅ |
| `performance_light` | 8 | TLS server | ✅ |
| `compatibility` | 8 | TLS server | ✅ |
| `contract` | 6 | TLS server | ✅ |
| `conformance` | 1 | TLS server | ✅ |

---

## Error Patterns to ALWAYS Flag

### Critical Errors (Must Fix)
```
FAIL
ERROR
Exception
Traceback
crash
timeout / TIMEOUT
badrpc / noproc / badarg
Connection refused
BrokenPipe
SSLError / ssl.SSLError
ConnectionResetError
0.0% delivery
0/X messages
Message NOT found
```

### Warnings (Must Investigate)
```
SKIP
⚠ / WARN / Warning
deprecated
not found / missing
timed out
```

### Success Validation
```
✓ PASS / PASS: / [PASS]
ALL TESTS PASSED
100% delivery
```

---

## Docker Cluster Management (PROVEN)

### Start Fresh Cluster
```bash
cd docker/global-cluster
./cluster.sh down   # Stop and clean up
./cluster.sh up     # Start and initialize Mnesia
```

### Verify Cluster Health
```bash
docker ps --format '{{.Names}}' | grep -E '^(core|edge)-' | wc -l
# Should show 15+ containers
```

### Run Chaos Tests (Fresh Cluster Per Test)
```bash
cd docker/global-cluster
./run_chaos_tests.sh
```

---

## Prohibited Actions

1. **NEVER** skip tests without SKIP:DOCKER or SKIP:INFRA reason
2. **NEVER** use `continue-on-error` without immediate analysis
3. **NEVER** claim "tests passed" without showing log evidence
4. **NEVER** truncate logs - read every line
5. **NEVER** ignore "0 messages" or "0% delivery" in output
6. **NEVER** mask errors with try/except pass
7. **NEVER** use plaintext TCP for TLS-enabled server

---

## Activation Triggers

This skill activates when user mentions:
- "run all tests"
- "run tests" / "test suite"
- "verify no regressions"
- "full test run"
- "chaos tests"
- "TLS tests"
- "fix tests"
- "test stabilization"

---

## Files Reference

| File | Purpose |
|------|---------|
| `tests/run_all_tests.sh` | Main test runner (PROVEN) |
| `docker/global-cluster/cluster.sh` | Cluster management (PROVEN) |
| `docker/global-cluster/init_cluster.sh` | Mnesia initialization (PROVEN) |
| `tests/utilities/iris_client.py` | TLS-enabled Python client |
| `tests/suites/chaos_dist/utils.py` | TLS helpers for chaos tests |
| `config/test_tls.config` | TLS server configuration |
| `certs/ca.pem` | Certificate Authority for verification |
