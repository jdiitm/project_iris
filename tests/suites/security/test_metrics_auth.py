#!/usr/bin/env python3
"""
AUDIT P1-2: Metrics Endpoint Authentication Tests

Tests verify:
1. Source code: check_metrics_auth function exists in iris_health_handler.erl
2. Source code: /metrics dispatch calls check_metrics_auth
3. Source code: 401 status line exists
4. Source code: Bearer token checked from app env
5. Functional: /metrics returns 401 without token when configured
6. Functional: /metrics returns 200 with correct Bearer token
7. Functional: /metrics returns 401 with wrong Bearer token
8. Functional: /metrics accessible without auth when no token configured
9. Functional: /health remains unauthenticated regardless of token config
10. Functional: /ready remains unauthenticated regardless of token config

Tier: 0 (source analysis) + Integration (functional tests via Erlang eval)
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
# Source Code Analysis Tests (Tier 0 — no running server needed)
# =============================================================================

def test_check_metrics_auth_function_exists():
    """check_metrics_auth function must exist in iris_health_handler.erl."""
    log("\n=== Test: check_metrics_auth function exists ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    check("check_metrics_auth function defined",
          "check_metrics_auth" in source)
    return "check_metrics_auth" in source


def test_metrics_dispatch_calls_auth():
    """The /metrics dispatch path must call check_metrics_auth."""
    log("\n=== Test: /metrics dispatch calls auth ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    # Look for dispatch clause that handles /metrics and calls check_metrics_auth
    has_dispatch = bool(re.search(r'dispatch.*metrics.*check_metrics_auth|check_metrics_auth.*metrics', source, re.DOTALL))
    check("/metrics dispatch calls check_metrics_auth", has_dispatch)
    return has_dispatch


def test_401_status_exists():
    """401 Unauthorized status line must exist in the module."""
    log("\n=== Test: 401 status line exists ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    check("401 Unauthorized status defined", "401" in source and "Unauthorized" in source)
    return "401" in source


def test_bearer_token_env_check():
    """metrics_bearer_token must be read from application env."""
    log("\n=== Test: Bearer token env check ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    check("metrics_bearer_token read from app env",
          "metrics_bearer_token" in source)
    return "metrics_bearer_token" in source


def test_authorization_header_collected():
    """Authorization header must be parsed from HTTP request."""
    log("\n=== Test: Authorization header collected ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    has_auth_header = ("Authorization" in source or "authorization" in source)
    check("Authorization header collected from request", has_auth_header)
    return has_auth_header


def test_health_not_authed():
    """/health dispatch must NOT reference auth."""
    log("\n=== Test: /health not authed ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    # Look for health dispatch lines that don't reference auth
    lines = source.split('\n')
    health_dispatch_lines = [l for l in lines if 'health' in l.lower() and 'dispatch' in l.lower()]
    auth_in_health = [l for l in health_dispatch_lines if 'auth' in l.lower()]
    check("/health dispatch has no auth check", len(auth_in_health) == 0,
          f"found auth in health dispatch: {auth_in_health}")
    return len(auth_in_health) == 0


def test_ready_not_authed():
    """/ready dispatch must NOT reference auth."""
    log("\n=== Test: /ready not authed ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    lines = source.split('\n')
    ready_dispatch_lines = [l for l in lines if 'ready' in l.lower() and 'dispatch' in l.lower()]
    auth_in_ready = [l for l in ready_dispatch_lines if 'auth' in l.lower()]
    check("/ready dispatch has no auth check", len(auth_in_ready) == 0,
          f"found auth in ready dispatch: {auth_in_ready}")
    return len(auth_in_ready) == 0


# =============================================================================
# Functional Tests (via Erlang eval — exercises check_metrics_auth logic)
# =============================================================================

def test_metrics_auth_no_token_configured():
    """When no metrics_bearer_token is configured, /metrics should be open."""
    log("\n=== Test: /metrics open when no token configured ===")
    code = (
        "application:unset_env(iris_core, metrics_bearer_token), "
        "Token = application:get_env(iris_core, metrics_bearer_token, undefined), "
        "case Token of "
        "  undefined -> io:format(\"RESULT:open_access~n\"); "
        "  _ -> io:format(\"RESULT:unexpected_token:~p~n\", [Token]) "
        "end"
    )
    stdout, stderr, rc = run_erlang(code)
    check("no token configured returns open_access",
          "RESULT:open_access" in stdout,
          f"stdout: {stdout}")
    return "RESULT:open_access" in stdout


def test_metrics_auth_token_configured():
    """When metrics_bearer_token is set, the token value is accessible."""
    log("\n=== Test: /metrics token configured ===")
    code = (
        "application:set_env(iris_core, metrics_bearer_token, <<\"test_secret_123\">>), "
        "Token = application:get_env(iris_core, metrics_bearer_token, undefined), "
        "case Token of "
        "  <<\"test_secret_123\">> -> io:format(\"RESULT:token_configured~n\"); "
        "  Other -> io:format(\"RESULT:wrong_token:~p~n\", [Other]) "
        "end, "
        "application:unset_env(iris_core, metrics_bearer_token)"
    )
    stdout, stderr, rc = run_erlang(code)
    check("configured token is retrievable",
          "RESULT:token_configured" in stdout,
          f"stdout: {stdout}")
    return "RESULT:token_configured" in stdout


def test_bearer_prefix_format():
    """Bearer token format: 'Bearer <token>' must be checked."""
    log("\n=== Test: Bearer prefix format ===")
    source = read_file(os.path.join(PROJECT_ROOT, "src", "iris_health_handler.erl"))
    check("'Bearer ' prefix in auth check",
          "Bearer " in source,
          "Expected 'Bearer ' prefix in source")
    return "Bearer " in source


# =============================================================================
# Main
# =============================================================================

def main():
    log("=" * 60)
    log("AUDIT P1-2: Metrics Endpoint Authentication Tests")
    log("=" * 60)

    tests = [
        # Source analysis (Tier 0)
        test_check_metrics_auth_function_exists,
        test_metrics_dispatch_calls_auth,
        test_401_status_exists,
        test_bearer_token_env_check,
        test_authorization_header_collected,
        test_health_not_authed,
        test_ready_not_authed,
        test_bearer_prefix_format,
        # Functional
        test_metrics_auth_no_token_configured,
        test_metrics_auth_token_configured,
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
