#!/usr/bin/env python3
"""
AUDIT P2-3: Unit Tests for Polling Helper Functions

Tests for wait_until, wait_until_or_fail, and wait_for_value helpers
from tests/utilities/helpers.py. These helpers replace time.sleep() calls
to make tests deterministic and bounded.

Tests verify:
1. wait_until returns True when predicate succeeds before timeout
2. wait_until returns False when predicate never succeeds
3. wait_until handles exceptions in predicate gracefully
4. wait_until_or_fail raises AssertionError on timeout
5. wait_until_or_fail succeeds when predicate is true
6. wait_for_value returns True when supplier returns expected value
7. wait_for_value returns False when supplier never returns expected value
8. Polling interval is respected (not busy-spinning)
9. Timeout is bounded (doesn't wait forever)

Tier: 0 (Required on every merge — no server needed)
"""

import sys
import os
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# Deterministic seeding
TEST_SEED = int(os.environ.get("TEST_SEED", 42))

from tests.utilities.helpers import wait_until, wait_until_or_fail, wait_for_value

passed = 0
failed = 0


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def check(name, condition, detail=""):
    global passed, failed
    if condition:
        log(f"  PASS: {name}")
        passed += 1
    else:
        log(f"  FAIL: {name}")
        if detail:
            log(f"        {detail}")
        failed += 1


# =============================================================================
# Test 1: wait_until returns True when predicate succeeds
# =============================================================================

def test_wait_until_immediate_true():
    """Predicate that immediately returns True should succeed."""
    log("\n=== Test: wait_until immediate True ===")
    result = wait_until(lambda: True, timeout=1.0, interval=0.01, description="always true")
    check("wait_until returns True for always-true predicate", result is True)
    return result is True


# =============================================================================
# Test 2: wait_until returns False on timeout
# =============================================================================

def test_wait_until_timeout_false():
    """Predicate that never returns True should return False after timeout."""
    log("\n=== Test: wait_until timeout False ===")
    start = time.time()
    result = wait_until(lambda: False, timeout=0.3, interval=0.05, description="never true")
    elapsed = time.time() - start
    check("wait_until returns False for never-true predicate", result is False)
    check("wait_until respects timeout bound", 0.25 < elapsed < 0.8,
          f"elapsed={elapsed:.3f}s, expected ~0.3s")
    return result is False


# =============================================================================
# Test 3: wait_until handles exceptions in predicate
# =============================================================================

def test_wait_until_exception_handling():
    """Predicate that raises should be caught, not propagated."""
    log("\n=== Test: wait_until exception handling ===")
    call_count = [0]

    def flaky_predicate():
        call_count[0] += 1
        if call_count[0] < 3:
            raise RuntimeError("not ready yet")
        return True

    result = wait_until(flaky_predicate, timeout=2.0, interval=0.05, description="flaky")
    check("wait_until succeeds after predicate raises then succeeds", result is True)
    check("predicate was called multiple times", call_count[0] >= 3,
          f"call_count={call_count[0]}")
    return result is True


# =============================================================================
# Test 4: wait_until_or_fail raises AssertionError on timeout
# =============================================================================

def test_wait_until_or_fail_raises():
    """Should raise AssertionError when predicate never succeeds."""
    log("\n=== Test: wait_until_or_fail raises ===")
    raised = False
    try:
        wait_until_or_fail(lambda: False, timeout=0.2, interval=0.05, description="never true")
    except AssertionError as e:
        raised = True
        check("AssertionError message contains description",
              "never true" in str(e), f"message: {e}")
    check("wait_until_or_fail raised AssertionError", raised)
    return raised


# =============================================================================
# Test 5: wait_until_or_fail succeeds when predicate is true
# =============================================================================

def test_wait_until_or_fail_succeeds():
    """Should not raise when predicate succeeds."""
    log("\n=== Test: wait_until_or_fail succeeds ===")
    try:
        wait_until_or_fail(lambda: True, timeout=1.0, interval=0.01, description="always true")
        check("wait_until_or_fail did not raise for true predicate", True)
        return True
    except AssertionError:
        check("wait_until_or_fail did not raise for true predicate", False)
        return False


# =============================================================================
# Test 6: wait_for_value returns True when value matches
# =============================================================================

def test_wait_for_value_match():
    """Supplier returning expected value immediately should succeed."""
    log("\n=== Test: wait_for_value match ===")
    result = wait_for_value(lambda: 42, 42, timeout=1.0, interval=0.01, description="answer")
    check("wait_for_value returns True when supplier matches expected", result is True)
    return result is True


# =============================================================================
# Test 7: wait_for_value returns False when value never matches
# =============================================================================

def test_wait_for_value_no_match():
    """Supplier never returning expected value should return False."""
    log("\n=== Test: wait_for_value no match ===")
    result = wait_for_value(lambda: "wrong", "right", timeout=0.3, interval=0.05, description="match")
    check("wait_for_value returns False when values don't match", result is False)
    return result is False


# =============================================================================
# Test 8: wait_for_value with delayed match
# =============================================================================

def test_wait_for_value_delayed_match():
    """Supplier that eventually returns expected value should succeed."""
    log("\n=== Test: wait_for_value delayed match ===")
    state = {"value": "loading"}

    def delayed_supplier():
        return state["value"]

    # Schedule the value change
    import threading
    def set_value():
        time.sleep(0.15)
        state["value"] = "ready"

    t = threading.Thread(target=set_value)
    t.start()

    result = wait_for_value(delayed_supplier, "ready", timeout=2.0, interval=0.05, description="ready state")
    t.join()

    check("wait_for_value detects delayed value match", result is True)
    return result is True


# =============================================================================
# Test 9: wait_until with delayed predicate
# =============================================================================

def test_wait_until_delayed_predicate():
    """Predicate that eventually becomes True should succeed."""
    log("\n=== Test: wait_until delayed predicate ===")
    import threading

    ready = [False]

    def delayed_set():
        time.sleep(0.15)
        ready[0] = True

    t = threading.Thread(target=delayed_set)
    t.start()

    start = time.time()
    result = wait_until(lambda: ready[0], timeout=2.0, interval=0.05, description="delayed ready")
    elapsed = time.time() - start
    t.join()

    check("wait_until returns True for delayed predicate", result is True)
    check("waited reasonable time (not full timeout)", elapsed < 1.0,
          f"elapsed={elapsed:.3f}s")
    return result is True


# =============================================================================
# Test 10: Polling is bounded (not busy-spinning)
# =============================================================================

def test_polling_not_busy():
    """Verify that polling doesn't call predicate excessively."""
    log("\n=== Test: polling is bounded ===")
    call_count = [0]

    def counting_predicate():
        call_count[0] += 1
        return False

    wait_until(counting_predicate, timeout=0.5, interval=0.1, description="count calls")
    # With 0.5s timeout and 0.1s interval, expect ~5 calls (not 1000+)
    check("poll count is bounded by interval",
          call_count[0] <= 15,
          f"call_count={call_count[0]}, expected <= 15")
    check("poll count is at least 3 (actually polling)",
          call_count[0] >= 3,
          f"call_count={call_count[0]}")
    return call_count[0] <= 15


# =============================================================================
# Main
# =============================================================================

def main():
    log("=" * 60)
    log("AUDIT P2-3: Polling Helper Unit Tests")
    log("=" * 60)

    tests = [
        test_wait_until_immediate_true,
        test_wait_until_timeout_false,
        test_wait_until_exception_handling,
        test_wait_until_or_fail_raises,
        test_wait_until_or_fail_succeeds,
        test_wait_for_value_match,
        test_wait_for_value_no_match,
        test_wait_for_value_delayed_match,
        test_wait_until_delayed_predicate,
        test_polling_not_busy,
    ]

    for test_fn in tests:
        try:
            test_fn()
        except Exception as e:
            log(f"  EXCEPTION in {test_fn.__name__}: {e}")
            global failed
            failed += 1

    log("\n" + "=" * 60)
    log(f"Results: {passed} passed, {failed} failed")
    log("=" * 60)

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
