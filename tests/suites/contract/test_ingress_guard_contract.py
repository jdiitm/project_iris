#!/usr/bin/env python3
"""
AUDIT P0-3: Ingress Guard Contract Tests

Contract tests verifying the fail-closed/fail-open behavior of
iris_ingress_guard.erl via source analysis and Erlang eval.

Tests verify:
1. Source: check/0 reads deployment_mode from application env
2. Source: production mode returns {deny, guard_not_ready} when guard not started
3. Source: non-production mode returns allow when guard not started
4. Functional: production mode check returns deny when guard not started
5. Functional: development mode check returns allow when guard not started
6. Functional: unset mode defaults to development (allow)
7. Source: get_active_count/0 returns 0 when guard not started
8. Source: close/0 is safe when guard not started

Tier: 0 (Contract — no running server needed)
"""

import sys
import os
import subprocess
import time
import re

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


# =============================================================================
# Source Code Analysis
# =============================================================================

def test_source_reads_deployment_mode():
    """check/0 must read deployment_mode from application env."""
    log("\n=== Test: Source reads deployment_mode ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_ingress_guard.erl"))
    check("check/0 reads deployment_mode", "deployment_mode" in source)
    return "deployment_mode" in source


def test_source_denies_in_production():
    """Source must contain {deny, guard_not_ready} for production mode."""
    log("\n=== Test: Source denies in production ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_ingress_guard.erl"))
    check("deny guard_not_ready in source",
          "deny" in source and "guard_not_ready" in source)
    return "deny" in source and "guard_not_ready" in source


def test_source_allows_in_development():
    """Source must allow in non-production mode when guard not started."""
    log("\n=== Test: Source allows in development ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_ingress_guard.erl"))
    # The development/default branch returns allow
    has_allow = bool(re.search(r'_\s*->\s*allow', source))
    check("development mode returns allow", has_allow)
    return has_allow


def test_source_audit_comment():
    """Source must have AUDIT P0-3 comment documenting the fix."""
    log("\n=== Test: Source has AUDIT P0-3 comment ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_ingress_guard.erl"))
    check("AUDIT P0-3 comment present", "AUDIT P0-3" in source)
    return "AUDIT P0-3" in source


# =============================================================================
# Functional Tests (via Erlang eval)
# =============================================================================

def test_production_mode_deny():
    """In production mode with guard not started, check/0 returns {deny, guard_not_ready}."""
    log("\n=== Test: Production mode deny ===")
    code = (
        "try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end, "
        "application:set_env(iris_edge, deployment_mode, production), "
        "Result = iris_ingress_guard:check(), "
        "io:format(\"RESULT:~p~n\", [Result]), "
        "application:unset_env(iris_edge, deployment_mode)"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("production returns {deny,guard_not_ready}",
          "deny" in stdout and "guard_not_ready" in stdout,
          f"stdout: {stdout}")
    check("no ERROR in output", "ERROR" not in stdout, f"stdout: {stdout}")
    return "deny" in stdout and "guard_not_ready" in stdout


def test_development_mode_allow():
    """In development mode with guard not started, check/0 returns allow."""
    log("\n=== Test: Development mode allow ===")
    code = (
        "try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end, "
        "application:set_env(iris_edge, deployment_mode, development), "
        "Result = iris_ingress_guard:check(), "
        "io:format(\"RESULT:~p~n\", [Result]), "
        "application:unset_env(iris_edge, deployment_mode)"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("development returns allow",
          "RESULT:allow" in stdout,
          f"stdout: {stdout}")
    return "RESULT:allow" in stdout


def test_default_mode_allow():
    """With no deployment_mode set (defaults to development), check/0 returns allow."""
    log("\n=== Test: Default mode allow ===")
    code = (
        "try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end, "
        "application:unset_env(iris_edge, deployment_mode), "
        "Result = iris_ingress_guard:check(), "
        "io:format(\"RESULT:~p~n\", [Result])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("default mode returns allow",
          "RESULT:allow" in stdout,
          f"stdout: {stdout}")
    return "RESULT:allow" in stdout


def test_active_count_zero_when_not_started():
    """get_active_count/0 returns 0 when guard not started."""
    log("\n=== Test: Active count 0 when not started ===")
    code = (
        "try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end, "
        "Count = iris_ingress_guard:get_active_count(), "
        "io:format(\"RESULT:~p~n\", [Count])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("active count is 0 when not started",
          "RESULT:0" in stdout,
          f"stdout: {stdout}")
    return "RESULT:0" in stdout


def test_close_safe_when_not_started():
    """close/0 should not crash when guard not started."""
    log("\n=== Test: close/0 safe when not started ===")
    code = (
        "try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end, "
        "Result = iris_ingress_guard:close(), "
        "io:format(\"RESULT:~p~n\", [Result])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")
    check("close/0 returns ok when not started",
          "RESULT:ok" in stdout,
          f"stdout: {stdout}")
    check("no ERROR in output", "ERROR" not in stdout, f"stdout: {stdout}")
    return "RESULT:ok" in stdout


# =============================================================================
# Main
# =============================================================================

def main():
    log("=" * 60)
    log("AUDIT P0-3: Ingress Guard Contract Tests")
    log("=" * 60)

    tests = [
        # Source analysis
        test_source_reads_deployment_mode,
        test_source_denies_in_production,
        test_source_allows_in_development,
        test_source_audit_comment,
        # Functional
        test_production_mode_deny,
        test_development_mode_allow,
        test_default_mode_allow,
        test_active_count_zero_when_not_started,
        test_close_safe_when_not_started,
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
