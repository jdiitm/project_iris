#!/usr/bin/env python3
"""
Contract Tests: Revocation Propagation Hardening

Validates audit finding 2.1b: Revocation propagation must NOT be
fire-and-forget. Failures must be logged, not silently swallowed.

The current code uses:
    spawn(fun() -> rpc:cast(Node, ...) catch _:_ -> ok end)

This is double-fire-and-forget (spawn + rpc:cast) with all errors silenced.
The fix must use rpc:call with a timeout and log failures at warning level.

This test reads the Erlang source to validate the code pattern (Tier 0).

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


def extract_function(source, func_name, arity):
    """Extract an Erlang function body from source by name/arity."""
    # Find function start
    pattern = rf'^{func_name}\('
    lines = source.split('\n')
    start_idx = None
    for i, line in enumerate(lines):
        if re.match(pattern, line):
            start_idx = i
            break
    if start_idx is None:
        return ""
    # Collect lines until we hit the next function or end
    end_idx = start_idx + 1
    for i in range(start_idx + 1, len(lines)):
        line = lines[i]
        # End of function: line starts with a new function def or module attribute
        if re.match(r'^[a-z]\w*\(', line) or re.match(r'^-', line):
            end_idx = i
            break
        end_idx = i + 1
    return '\n'.join(lines[start_idx:end_idx])


# =============================================================================
# Test 1: propagate_revocation must use rpc:call, not rpc:cast
# =============================================================================

def test_revocation_uses_rpc_call_not_cast():
    """
    Contract: propagate_revocation/2 MUST use rpc:call (synchronous with
    timeout) instead of rpc:cast (fire-and-forget) for cross-node revocation.

    rpc:call returns {badrpc, Reason} on failure, allowing error logging.
    rpc:cast returns 'true' regardless of success or failure.
    """
    log("\n=== Test: Revocation Uses rpc:call Not rpc:cast ===")
    log("  Audit finding 2.1b: Fire-and-forget revocation")

    auth_erl = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")
    source = read_file(auth_erl)
    func_body = extract_function(source, "propagate_revocation", 2)

    # Must NOT contain rpc:cast for receive_revocation
    has_rpc_cast = bool(re.search(r'rpc:cast\(.*receive_revocation', func_body))
    check(
        "propagate_revocation does NOT use rpc:cast",
        not has_rpc_cast,
        "rpc:cast is fire-and-forget -- failures are invisible. Use rpc:call with timeout."
    )

    # Must contain rpc:call for receive_revocation
    has_rpc_call = bool(re.search(r'rpc:call\(.*receive_revocation', func_body))
    check(
        "propagate_revocation uses rpc:call with timeout",
        has_rpc_call,
        "Must use rpc:call(Node, ?MODULE, receive_revocation, [TokenId, Timestamp], Timeout)"
    )


# =============================================================================
# Test 2: Failures must be logged, not swallowed
# =============================================================================

def test_revocation_logs_failures():
    """
    Contract: When revocation propagation to a node fails, a warning-level
    log MUST be emitted. The current code has `catch _:_ -> ok` which
    silently swallows all errors.
    """
    log("\n=== Test: Revocation Logs Failures ===")

    auth_erl = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")
    source = read_file(auth_erl)
    func_body = extract_function(source, "propagate_revocation", 2)

    # Must NOT contain catch _:_ -> ok (silent error swallow)
    has_silent_catch = bool(re.search(r'catch\s+_:_\s*->\s*ok', func_body))
    check(
        "propagate_revocation does NOT silently catch errors",
        not has_silent_catch,
        "catch _:_ -> ok swallows all errors. Failures must be logged."
    )

    # Must contain logger:warning for propagation failure
    has_failure_log = bool(re.search(
        r'logger:warning\(.*[Rr]evocation propagation',
        func_body
    ))
    check(
        "propagate_revocation logs failures at warning level",
        has_failure_log,
        "Must log warning when rpc:call to a node fails (badrpc)"
    )


# =============================================================================
# Test 3: spawn wrapper is kept (non-blocking)
# =============================================================================

def test_revocation_still_non_blocking():
    """
    Contract: propagate_revocation/2 MUST remain non-blocking for the
    gen_server caller. The spawn wrapper should be kept -- only the
    inner rpc:cast should change to rpc:call.
    """
    log("\n=== Test: Revocation Remains Non-blocking ===")

    auth_erl = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")
    source = read_file(auth_erl)
    func_body = extract_function(source, "propagate_revocation", 2)

    # Must still use spawn to avoid blocking the gen_server
    has_spawn = bool(re.search(r'spawn\(fun\(\)', func_body))
    check(
        "propagate_revocation still uses spawn (non-blocking)",
        has_spawn,
        "The spawn wrapper must remain to avoid blocking the gen_server caller"
    )


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Contract Tests: Revocation Propagation Hardening")
    log("Audit Finding 2.1b: Fire-and-Forget Revocation")
    log("=" * 60)

    test_revocation_uses_rpc_call_not_cast()
    test_revocation_logs_failures()
    test_revocation_still_non_blocking()

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    log(f"\nTotal: {passed + failed}")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed == 0:
        log("\n✓ All revocation propagation contract tests passed!")
        return 0
    else:
        log(f"\n✗ {failed} contract test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
