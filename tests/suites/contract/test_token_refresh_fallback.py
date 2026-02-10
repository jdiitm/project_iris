#!/usr/bin/env python3
"""
Contract Tests: Token Refresh Fallback

Validates audit finding 2.3b: token_refresh MUST use call_with_fallback
so that when the primary Core is down, alternate Core nodes are tried.

Current code uses iris_circuit_breaker:call/4 (no fallback).
Fix must use iris_circuit_breaker:call_with_fallback/5.

Also validates that iris_core_registry exports get_fallback_cores/1.

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
# Test 1: token_refresh uses call_with_fallback, not call/4
# =============================================================================

def test_token_refresh_uses_fallback():
    """
    Contract: handle_packet({token_refresh, ...}) in iris_session.erl
    MUST use iris_circuit_breaker:call_with_fallback/5, not call/4,
    for the validate_and_rotate_refresh RPC.
    """
    log("\n=== Test: Token Refresh Uses Fallback ===")
    log("  Audit finding 2.3b: Sync Core Dependency (no fallback)")

    session_erl = os.path.join(PROJECT_ROOT, "src", "iris_session.erl")
    source = read_file(session_erl)

    # Find the token_refresh handler and check for call_with_fallback
    # The pattern should be: iris_circuit_breaker:call_with_fallback(..., validate_and_rotate_refresh, ...)
    has_fallback_call = bool(re.search(
        r'iris_circuit_breaker:call_with_fallback\(.*validate_and_rotate_refresh',
        source, re.DOTALL
    ))

    check(
        "token_refresh uses call_with_fallback for validate_and_rotate_refresh",
        has_fallback_call,
        "Must use iris_circuit_breaker:call_with_fallback/5 to try alternate cores on failure"
    )

    # Must NOT use plain call/4 for validate_and_rotate_refresh
    # (check that there's no call/4 pattern without fallback)
    has_plain_call = bool(re.search(
        r'iris_circuit_breaker:call\(CoreNode,\s*iris_auth,\s*validate_and_rotate_refresh',
        source
    ))

    check(
        "token_refresh does NOT use plain call/4 (without fallback)",
        not has_plain_call,
        "call/4 has no fallback -- circuit_open means total failure"
    )


# =============================================================================
# Test 2: iris_core_registry exports get_fallback_cores/1
# =============================================================================

def test_registry_has_fallback_function():
    """
    Contract: iris_core_registry MUST export get_fallback_cores/1
    which returns all core nodes except the specified one.
    """
    log("\n=== Test: Registry Has Fallback Function ===")

    registry_erl = os.path.join(PROJECT_ROOT, "src", "iris_core_registry.erl")
    source = read_file(registry_erl)

    # Must export get_fallback_cores/1
    has_export = bool(re.search(
        r'-export\(\[.*get_fallback_cores/1',
        source, re.DOTALL
    ))

    check(
        "iris_core_registry exports get_fallback_cores/1",
        has_export,
        "Must export get_fallback_cores/1 for token_refresh fallback routing"
    )

    # Must have the function implementation
    has_function = bool(re.search(
        r'^get_fallback_cores\(',
        source,
        re.MULTILINE
    ))

    check(
        "get_fallback_cores/1 function is implemented",
        has_function,
        "Must implement get_fallback_cores(ExcludeNode) -> [node()]"
    )

    # Must use get_all_cores internally
    has_all_cores_ref = bool(re.search(
        r'get_fallback_cores.*get_all_cores',
        source,
        re.DOTALL
    ))

    check(
        "get_fallback_cores uses get_all_cores internally",
        has_all_cores_ref,
        "Should filter get_all_cores() to exclude the primary node"
    )


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Contract Tests: Token Refresh Fallback")
    log("Audit Finding 2.3b: Sync Core Dependency")
    log("=" * 60)

    test_token_refresh_uses_fallback()
    test_registry_has_fallback_function()

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    log(f"\nTotal: {passed + failed}")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed == 0:
        log("\n✓ All token refresh fallback contract tests passed!")
        return 0
    else:
        log(f"\n✗ {failed} contract test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
