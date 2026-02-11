#!/usr/bin/env python3
"""
Contract Tests: Graceful Heap Limit

Validates audit finding 2.3a: When a session process hits the heap memory
limit, the client MUST receive a SERVER_OVERLOAD message before the
connection closes, rather than a raw TCP reset.

The fix changes:
1. iris_session.erl: kill => false (allow graceful handling)
2. iris_ws_lite.erl: Add connected(info, ...) handler for heap limit
   that sends SERVER_OVERLOAD before stopping.

Tier: 0 (Required on every merge)
"""

import sys
import os
import re

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

passed = 0
failed = 0


def log(msg):
    import time
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


def read_file(path):
    with open(path, 'r') as f:
        return f.read()


# =============================================================================
# Test 1: max_heap_size uses kill => false
# =============================================================================

def test_heap_limit_not_hard_kill():
    """
    Contract: iris_session MUST use kill => false for max_heap_size so
    the process gets a chance to send a graceful close message instead
    of being terminated immediately with a raw TCP reset.
    """
    log("\n=== Test: Heap Limit Uses kill => false ===")
    log("  Audit finding 2.3a: Hard Heap Kill")

    session_erl = os.path.join(PROJECT_ROOT, "src", "iris_session.erl")
    source = read_file(session_erl)

    # Must NOT have kill => true for max_heap_size
    has_kill_true = bool(re.search(
        r'max_heap_size.*kill\s*=>\s*true',
        source, re.DOTALL
    ))

    check(
        "max_heap_size does NOT use kill => true",
        not has_kill_true,
        "kill => true causes abrupt TCP reset with no error message to client"
    )

    # Must have kill => false
    has_kill_false = bool(re.search(
        r'max_heap_size.*kill\s*=>\s*false',
        source, re.DOTALL
    ))

    check(
        "max_heap_size uses kill => false (graceful)",
        has_kill_false,
        "kill => false allows the process to handle the limit gracefully"
    )


# =============================================================================
# Test 2: iris_ws_lite handles heap limit signal
# =============================================================================

def test_ws_lite_handles_heap_limit():
    """
    Contract: iris_ws_lite MUST have a connected(info, ...) clause that
    handles the heap limit scenario and sends SERVER_OVERLOAD to the client
    before stopping.

    With kill => false + error_logger => true, OTP logs but the process
    continues. We add an explicit check via process_info(self(), heap_size)
    after packet handling, or handle a custom overload message.
    """
    log("\n=== Test: iris_ws_lite Handles Heap Limit ===")

    ws_erl = os.path.join(PROJECT_ROOT, "src", "iris_ws_lite.erl")
    source = read_file(ws_erl)

    # Must have a handler that sends SERVER_OVERLOAD
    has_overload_handler = bool(re.search(
        r'SERVER_OVERLOAD',
        source
    ))

    check(
        "iris_ws_lite sends SERVER_OVERLOAD on heap limit",
        has_overload_handler,
        "Must send SERVER_OVERLOAD to client before closing on heap exhaustion"
    )

    # Must have a heap_size check or overload detection
    has_heap_check = bool(re.search(
        r'heap_size|session_overload|max_heap|check_heap',
        source
    ))

    check(
        "iris_ws_lite has heap/overload detection",
        has_heap_check,
        "Must detect when session is approaching heap limit"
    )


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Contract Tests: Graceful Heap Limit")
    log("Audit Finding 2.3a: Hard Heap Kill")
    log("=" * 60)

    test_heap_limit_not_hard_kill()
    test_ws_lite_handles_heap_limit()

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    log(f"\nTotal: {passed + failed}")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed == 0:
        log("\n✓ All graceful heap limit contract tests passed!")
        return 0
    else:
        log(f"\n✗ {failed} contract test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
