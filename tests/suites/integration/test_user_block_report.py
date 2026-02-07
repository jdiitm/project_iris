#!/usr/bin/env python3
"""
P1-9 (RL-3): User Block/Report Integration Tests

RFC-001 v4.0 FR-8b: Users can block/unblock and report other users.

Tests verify:
1. Blocked user message rejected via Erlang API
2. Report user via Erlang API handled gracefully

Pattern: follows test_presence_privacy.py using run_erlang helper.
"""

import sys
import os
import subprocess
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


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


def test_blocked_user_message_rejected():
    """
    Block user via Erlang, check messaging is blocked.
    """
    log("=" * 60)
    log("TEST: Blocked user message rejected")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(user_blocks, [{ram_copies, [node()]}, {attributes, [key, blocker, blocked, created_at]}, {type, set}]), "
        "mnesia:create_table(user_reports, [{ram_copies, [node()]}, {attributes, [key, reporter, reported, reason, created_at]}, {type, set}]), "
        "mnesia:wait_for_tables([user_blocks, user_reports], 5000), "
        "ok = iris_user_safety:block_user(<<\"alice\">>, <<\"bob\">>), "
        "R = iris_user_safety:check_can_message(<<\"bob\">>, <<\"alice\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "blocked" in stdout, f"Expected blocked, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def test_report_user_via_api():
    """
    Report user via Erlang API, verify no crash.
    """
    log("=" * 60)
    log("TEST: Report user via API")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(user_blocks, [{ram_copies, [node()]}, {attributes, [key, blocker, blocked, created_at]}, {type, set}]), "
        "mnesia:create_table(user_reports, [{ram_copies, [node()]}, {attributes, [key, reporter, reported, reason, created_at]}, {type, set}]), "
        "mnesia:wait_for_tables([user_blocks, user_reports], 5000), "
        "R = iris_user_safety:report_user(<<\"alice\">>, <<\"bob\">>, <<\"spam\">>), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "RESULT:ok" in stdout, f"Expected ok, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def main():
    log("User Block/Report Integration Tests (P1-9 / RL-3)")
    log("")

    tests = [
        ("blocked_message_rejected", test_blocked_user_message_rejected),
        ("report_user_api", test_report_user_via_api),
    ]

    passed = 0
    failed = 0

    for name, test_fn in tests:
        try:
            result = test_fn()
            if result:
                passed += 1
            else:
                failed += 1
                log(f"  FAIL: {name}")
        except Exception as e:
            failed += 1
            log(f"  FAIL: {name} raised {type(e).__name__}: {e}")

    log("")
    log("=" * 60)
    log(f"Results: {passed} passed, {failed} failed out of {len(tests)}")
    log("=" * 60)

    if failed > 0:
        sys.exit(1)
    sys.exit(0)


if __name__ == "__main__":
    main()
