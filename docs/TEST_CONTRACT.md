# Test Return Code Contract

**Effective Date**: 2026-01-23  
**Last Updated**: 2026-01-27  
**Status**: MANDATORY for all tests - **FULLY IMPLEMENTED**

---

## Exit Code Definitions

All test scripts MUST use these exit codes consistently:

| Exit Code | Meaning | When to Use |
|-----------|---------|-------------|
| `0` | **PASS** | Test executed successfully and all assertions passed |
| `1` | **FAIL** | Test executed but assertions failed OR unexpected error |
| `2` | **SKIP** | Test could not execute due to missing prerequisites |

---

## Code Examples

### Python Tests

```python
import sys

def main():
    # Check prerequisites FIRST
    if not check_prerequisites():
        print("SKIP: Prerequisites not met - <specific reason>")
        sys.exit(2)  # SKIP - not PASS
    
    try:
        result = run_test()
        if result:
            print("PASS: <description>")
            sys.exit(0)  # PASS
        else:
            print("FAIL: <description>")
            sys.exit(1)  # FAIL
    except Exception as e:
        print(f"FAIL: Unexpected error - {e}")
        sys.exit(1)  # FAIL on exception
```

### Erlang Tests (EUnit)

```erlang
%% EUnit uses exceptions for failure
%% Return values don't affect pass/fail

my_test() ->
    %% Prerequisites check
    case check_prerequisites() of
        false ->
            {skip, "Prerequisites not met"};
        true ->
            ?assertEqual(expected, actual)  %% Fails via exception
    end.
```

---

## Prohibited Patterns

### DO NOT use environment checks to change pass/fail

```python
# WRONG - This is a trick that hides failures
if os.environ.get("CI"):
    sys.exit(0)  # Pretends to pass in CI

# CORRECT - Use exit(2) for skip
if not infrastructure_available():
    print("SKIP: Docker cluster not running")
    sys.exit(2)
```

### DO NOT return None to avoid failure

```python
# WRONG - None gets treated as pass by some runners
def test_something():
    if not ready():
        return None  # Ambiguous

# CORRECT - Explicit exit codes
def test_something():
    if not ready():
        print("SKIP: Not ready")
        sys.exit(2)
```

### DO NOT dynamically adjust thresholds

```python
# WRONG - Moving the goalpost
if os.environ.get("CI"):
    threshold = 1000  # Lowered from 10000
    
# CORRECT - Fixed thresholds per profile
THRESHOLDS = {
    "smoke": {"connections": 100, "duration": 30},
    "full": {"connections": 10000, "duration": 300}
}
profile = os.environ.get("TEST_PROFILE", "smoke")
threshold = THRESHOLDS[profile]["connections"]
```

### DO NOT swallow exceptions (Added 2026-01-27)

```python
# WRONG - Creates false positives
try:
    send_message(target, payload)
except:
    pass  # Hides ALL errors, test appears to pass

# WRONG - Ignores specific errors
try:
    result = recv_message(timeout=5)
except socket.timeout:
    pass  # Test passes with 0 messages received

# CORRECT - Log, count, and assert on errors
errors = 0
try:
    send_message(target, payload)
except socket.timeout as e:
    logging.warning(f"Timeout: {e}")
    errors += 1
except socket.error as e:
    logging.error(f"Socket error: {e}")
    errors += 1

# Fail if too many errors
assert errors < max_errors, f"Too many errors: {errors}"
```

### DO NOT assume cluster is running (Added 2026-01-27)

```python
# WRONG - Depends on previous test's cluster state
def main():
    if not check_server():
        print("Start cluster with: make start")
        sys.exit(1)  # Fails if previous test stopped cluster
    run_test()

# CORRECT - Manage own cluster lifecycle
from tests.framework.cluster import ClusterManager

def main():
    with ClusterManager(project_root=project_root) as cluster:
        run_test()  # Cluster guaranteed to be running
    # Cluster automatically stopped
```

---

## Skip Reasons (Required Documentation)

When using `exit(2)`, you MUST print one of these standardized reasons:

| Reason Code | Description | Example |
|-------------|-------------|---------|
| `SKIP:DOCKER` | Docker/container not available | "SKIP:DOCKER - Container core-east-1 not running" |
| `SKIP:CLUSTER` | Cluster not in required state | "SKIP:CLUSTER - Mnesia replication not configured" |
| `SKIP:TLS` | TLS/mTLS not configured | "SKIP:TLS - Server not running with TLS config" |
| `SKIP:RESOURCE` | Insufficient resources | "SKIP:RESOURCE - Requires 8GB RAM, only 4GB available" |
| `SKIP:DEPS` | Missing dependencies | "SKIP:DEPS - hypothesis package not installed" |

---

## Test Runner Contract

The test runner (`tests/run_tests.py`) MUST:

1. **Count exit codes correctly**:
   - `exit(0)` → Increment `passed`
   - `exit(1)` → Increment `failed`
   - `exit(2)` → Increment `skipped`

2. **Report skips explicitly**:
   ```
   Suite: chaos_dist
     PASS: test_split_brain (45.2s)
     FAIL: test_ack_durability (12.1s)
     SKIP: test_cross_region_latency (0.5s) - SKIP:CLUSTER
   ```

3. **Fail the run if any test fails**:
   - Skips do NOT cause run failure
   - But skips MUST be reported prominently

---

## Migration Checklist

For each test file, verify:

- [x] No `IS_CI` or `CI` environment checks that change pass/fail **✅ DONE (2026-01-25)** - Now uses `TEST_PROFILE`
- [x] No `return None` that gets treated as pass - **✅ DONE (2026-01-25)** - Changed to `sys.exit(2)`
- [x] All prerequisite failures use `exit(2)` with reason - **✅ DONE (2026-01-25)**
- [x] All assertion failures use `exit(1)` - **✅ DONE**
- [x] Success path uses `exit(0)` - **✅ DONE**
- [x] No bare `except: pass` blocks - **✅ DONE (2026-01-27)** - All replaced with explicit error handling
- [x] Uses `ClusterManager` for cluster lifecycle - **✅ DONE (2026-01-27)** - All performance_light tests updated
- [x] Uses `TEST_SEED` for randomness - **✅ DONE (2026-01-27)** - 14+ files seeded

### Verification Status (2026-01-27)

All 61 tests in core suites follow the contract:
- **Unit tests**: 21 tests ✅
- **Integration tests**: 17 tests ✅
- **E2E tests**: 5 tests ✅
- **Contract tests**: 1 test ✅
- **Compatibility tests**: 1 test ✅
- **Security tests**: 7 tests ✅
- **Resilience tests**: 3 tests ✅
- **Performance_light tests**: 6 tests ✅

Total smoke duration: **~6 minutes** for core suites (unit through performance_light)

---

## Enforcement

Starting from this date, any PR that introduces:

1. CI-conditional pass/fail logic
2. `return None` in test functions
3. Dynamic threshold adjustment
4. `exit(0)` without actual test passing
5. Bare `except: pass` blocks (added 2026-01-27)
6. Tests that don't use `ClusterManager` when requiring a cluster (added 2026-01-27)
7. Unseeded random operations (added 2026-01-27)

Will be **automatically rejected** by CI checks.

### Pre-Commit Checks

Run these commands before submitting PRs:

```bash
# Check for bare exception handlers
grep -rn "except:$" tests/
grep -rn "except Exception:$" tests/

# Check for unseeded random (should use TEST_SEED)
grep -rn "random\." tests/ | grep -v "random.seed" | grep -v "random.Random"

# Verify all tests pass deterministically
TEST_SEED=42 python3 tests/run_tests.py --suite unit --suite integration --suite e2e
```

---

## See Also

- [TEST_INVARIANTS.md](TEST_INVARIANTS.md) - System and test infrastructure invariants
- [TEST_DETERMINISM.md](TEST_DETERMINISM.md) - Determinism standards and BEAM-specific guidelines
- [TEST_STATUS.md](TEST_STATUS.md) - Current test results
