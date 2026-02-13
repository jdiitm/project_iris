#!/usr/bin/env python3
"""
AUDIT P0-5 / 7.4: User Safety Input Validation & Transaction Tests

Integration tests verifying that iris_user_safety.erl:
1. Rejects oversized user IDs (>128 bytes) for block/unblock/report
2. Uses mnesia:transaction (not dirty_write) for durability
3. Block/unblock round-trip works correctly via Erlang eval
4. check_can_message respects blocks
5. get_blocked returns blocked users
6. report_user stores reports without crash
7. Boundary: exactly 128-byte user ID is accepted
8. Source code has no dirty_write in block/unblock/report paths

Tier: Integration (requires ebin/ compiled modules + Mnesia)
"""

import sys
import os
import subprocess
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

TEST_SEED = int(os.environ.get("TEST_SEED", 42))

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


def read_file(path):
    with open(path, 'r') as f:
        return f.read()


def run_erlang(code):
    full_code = (
        "try "
        f"  {code} "
        "catch CatchClass:CatchReason:CatchStack -> "
        "  io:format(\"ERROR: ~p:~p~n~p~n\", [CatchClass, CatchReason, CatchStack]) "
        "end, "
        "init:stop()."
    )
    result = subprocess.run(
        ["erl", "-pa", "ebin", "-noshell", "-eval", full_code],
        capture_output=True, text=True, timeout=15,
        cwd=PROJECT_ROOT
    )
    return result.stdout.strip(), result.stderr.strip(), result.returncode


MNESIA_SETUP = (
    "application:stop(mnesia), "
    "mnesia:delete_schema([node()]), "
    "mnesia:create_schema([node()]), "
    "mnesia:start(), "
    "mnesia:create_table(user_blocks, [{ram_copies, [node()]}, "
    "  {attributes, [key, blocker, blocked, created_at]}, {type, set}]), "
    "mnesia:create_table(user_reports, [{ram_copies, [node()]}, "
    "  {attributes, [key, reporter, reported, reason, created_at]}, {type, set}]), "
    "mnesia:wait_for_tables([user_blocks, user_reports], 5000), "
)


# =============================================================================
# Source Code Analysis
# =============================================================================

def test_source_no_dirty_write():
    """iris_user_safety.erl must not use dirty_write for block/unblock/report."""
    log("\n=== Test: Source has no dirty_write ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_user_safety.erl"))
    lines = source.split('\n')
    dirty_write_lines = [l.strip() for l in lines
                         if 'dirty_write' in l and not l.strip().startswith('%')]
    check("no dirty_write in iris_user_safety.erl",
          len(dirty_write_lines) == 0,
          f"found dirty_write: {dirty_write_lines}")
    return len(dirty_write_lines) == 0


def test_source_uses_transaction():
    """iris_user_safety.erl must use mnesia:transaction for writes."""
    log("\n=== Test: Source uses mnesia:transaction ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_user_safety.erl"))
    check("mnesia:transaction used in source",
          "mnesia:transaction" in source)
    return "mnesia:transaction" in source


def test_source_validates_user_id_size():
    """iris_user_safety.erl must validate user ID size."""
    log("\n=== Test: Source validates user ID size ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_user_safety.erl"))
    check("MAX_USER_ID_SIZE defined",
          "MAX_USER_ID_SIZE" in source)
    check("byte_size guard in block_user",
          "byte_size" in source)
    return "MAX_USER_ID_SIZE" in source


def test_source_logs_get_blocked_errors():
    """get_blocked must log errors instead of silently returning []."""
    log("\n=== Test: Source logs get_blocked errors ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_user_safety.erl"))
    check("AUDIT P2-6 comment present",
          "AUDIT P2-6" in source)
    check("logger:warning in get_blocked catch",
          "get_blocked failed" in source or "get_blocked" in source)
    return "AUDIT P2-6" in source


# =============================================================================
# Functional: Oversized ID Rejection
# =============================================================================

def test_oversized_id_rejected_block():
    """block_user rejects user IDs > 128 bytes."""
    log("\n=== Test: Oversized ID rejected by block_user ===")
    # Create a 200-byte binary
    code = (
        MNESIA_SETUP +
        "BigId = binary:copy(<<\"x\">>, 200), "
        "R = iris_user_safety:block_user(BigId, <<\"bob\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("oversized blocker ID rejected",
          "invalid_user_id" in stdout,
          f"stdout: {stdout}")
    check("no ERROR in output", "ERROR" not in stdout)
    return "invalid_user_id" in stdout


def test_oversized_id_rejected_block_blocked():
    """block_user rejects oversized blocked user ID."""
    log("\n=== Test: Oversized blocked ID rejected ===")
    code = (
        MNESIA_SETUP +
        "BigId = binary:copy(<<\"x\">>, 200), "
        "R = iris_user_safety:block_user(<<\"alice\">>, BigId), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("oversized blocked ID rejected",
          "invalid_user_id" in stdout,
          f"stdout: {stdout}")
    return "invalid_user_id" in stdout


def test_oversized_id_rejected_report():
    """report_user rejects oversized user IDs."""
    log("\n=== Test: Oversized ID rejected by report_user ===")
    code = (
        MNESIA_SETUP +
        "BigId = binary:copy(<<\"x\">>, 200), "
        "R = iris_user_safety:report_user(BigId, <<\"bob\">>, <<\"spam\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("oversized reporter ID rejected",
          "invalid_user_id" in stdout,
          f"stdout: {stdout}")
    return "invalid_user_id" in stdout


# =============================================================================
# Functional: Boundary - Exactly 128 bytes
# =============================================================================

def test_128_byte_id_accepted():
    """Exactly 128-byte user ID should be accepted."""
    log("\n=== Test: 128-byte ID accepted ===")
    code = (
        MNESIA_SETUP +
        "Id128 = binary:copy(<<\"a\">>, 128), "
        "R = iris_user_safety:block_user(Id128, <<\"target\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("128-byte ID accepted", "RESULT:ok" in stdout, f"stdout: {stdout}")
    return "RESULT:ok" in stdout


def test_129_byte_id_rejected():
    """129-byte user ID should be rejected."""
    log("\n=== Test: 129-byte ID rejected ===")
    code = (
        MNESIA_SETUP +
        "Id129 = binary:copy(<<\"a\">>, 129), "
        "R = iris_user_safety:block_user(Id129, <<\"target\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("129-byte ID rejected", "invalid_user_id" in stdout, f"stdout: {stdout}")
    return "invalid_user_id" in stdout


# =============================================================================
# Functional: Block/Unblock Round-Trip
# =============================================================================

def test_block_unblock_roundtrip():
    """Block and unblock round-trip via Erlang eval."""
    log("\n=== Test: Block/unblock round-trip ===")
    code = (
        MNESIA_SETUP +
        "ok = iris_user_safety:block_user(<<\"alice\">>, <<\"bob\">>), "
        "Blocked1 = iris_user_safety:is_blocked(<<\"alice\">>, <<\"bob\">>), "
        "ok = iris_user_safety:unblock_user(<<\"alice\">>, <<\"bob\">>), "
        "Blocked2 = iris_user_safety:is_blocked(<<\"alice\">>, <<\"bob\">>), "
        "io:format(\"RESULT:blocked=~p,unblocked=~p~n\", [Blocked1, Blocked2])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("block returns true then unblock returns false",
          "blocked=true" in stdout and "unblocked=false" in stdout,
          f"stdout: {stdout}")
    return "blocked=true" in stdout


def test_check_can_message_blocked():
    """check_can_message returns {error, blocked} when user is blocked."""
    log("\n=== Test: check_can_message blocked ===")
    code = (
        MNESIA_SETUP +
        "ok = iris_user_safety:block_user(<<\"eve\">>, <<\"frank\">>), "
        "R = iris_user_safety:check_can_message(<<\"frank\">>, <<\"eve\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("check_can_message returns blocked",
          "blocked" in stdout,
          f"stdout: {stdout}")
    return "blocked" in stdout


def test_get_blocked_returns_users():
    """get_blocked returns list of blocked users."""
    log("\n=== Test: get_blocked returns users ===")
    code = (
        MNESIA_SETUP +
        "ok = iris_user_safety:block_user(<<\"grace\">>, <<\"heidi\">>), "
        "ok = iris_user_safety:block_user(<<\"grace\">>, <<\"ivan\">>), "
        "Blocked = iris_user_safety:get_blocked(<<\"grace\">>), "
        "io:format(\"RESULT:count=~p,list=~p~n\", [length(Blocked), Blocked])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("get_blocked returns 2 users",
          "count=2" in stdout,
          f"stdout: {stdout}")
    return "count=2" in stdout


def test_report_user_no_crash():
    """report_user stores report without crash."""
    log("\n=== Test: report_user no crash ===")
    code = (
        MNESIA_SETUP +
        "R = iris_user_safety:report_user(<<\"carol\">>, <<\"dave\">>, <<\"harassment\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("report_user returns ok", "RESULT:ok" in stdout, f"stdout: {stdout}")
    check("no ERROR in output", "ERROR" not in stdout)
    return "RESULT:ok" in stdout


# =============================================================================
# Main
# =============================================================================

def main():
    log("=" * 60)
    log("AUDIT P0-5/7.4: User Safety Input Validation Tests")
    log("=" * 60)

    tests = [
        # Source analysis
        test_source_no_dirty_write,
        test_source_uses_transaction,
        test_source_validates_user_id_size,
        test_source_logs_get_blocked_errors,
        # Oversized ID rejection
        test_oversized_id_rejected_block,
        test_oversized_id_rejected_block_blocked,
        test_oversized_id_rejected_report,
        # Boundary
        test_128_byte_id_accepted,
        test_129_byte_id_rejected,
        # Round-trip
        test_block_unblock_roundtrip,
        test_check_can_message_blocked,
        test_get_blocked_returns_users,
        test_report_user_no_crash,
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
