# Test Return Code Contract

**Effective Date**: 2026-01-23  
**Last Updated**: 2026-01-25  
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

### Verification Status (2026-01-25)

All 60+ tests now follow the contract:
- **Unit tests**: 4 tests ✅
- **Integration tests**: 17 tests ✅
- **E2E tests**: 5 tests ✅
- **Contract tests**: 1 test ✅
- **Compatibility tests**: 1 test ✅
- **Security tests**: 7 tests ✅
- **Stress tests**: 13 tests ✅
- **Resilience tests**: 3 tests ✅
- **Performance tests**: 6 tests ✅
- **Chaos tests**: 2 tests ✅

Total smoke duration: **~8 minutes** for all suites

---

## Enforcement

Starting from this date, any PR that introduces:

1. CI-conditional pass/fail logic
2. `return None` in test functions
3. Dynamic threshold adjustment
4. `exit(0)` without actual test passing

Will be **automatically rejected** by CI checks.
