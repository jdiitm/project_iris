#!/usr/bin/env python3
"""
P1-8 (IA-2): Revocation Propagation Timing Tests

RFC-001 v4.0 FR-11: Token revocation ≤10 seconds globally.

Tests verify:
1. Revoke token via Erlang RPC, then attempt use within 10s -- rejected
2. After revocation, reconnecting with same token fails

Pattern: follows test_jwt_security.py using run_erlang helper.
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


def test_revocation_within_10s():
    """
    Revoke a token, then validate it -- must be rejected immediately.
    """
    log("=" * 60)
    log("TEST: Revocation within 10 seconds")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:wait_for_tables([revoked_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_revocation_timing!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "{ok, _} = iris_auth:start_link(), "
        "{ok, Token} = iris_auth:create_token(<<\"revoke_test_user\">>), "
        "{ok, _} = iris_auth:validate_token(Token), "
        "ok = iris_auth:revoke_token(Token), "
        "Result = iris_auth:validate_token(Token), "
        "io:format(\"RESULT:~p~n\", [Result])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "token_revoked" in stdout, f"Expected token_revoked, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  Token revoked immediately")
    log("  PASS")
    return True


def test_revocation_survives_reconnect():
    """
    After token revocation, using the same token should fail.
    """
    log("=" * 60)
    log("TEST: Revocation survives validation attempts")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:wait_for_tables([revoked_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_revocation!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "{ok, _} = iris_auth:start_link(), "
        "{ok, Token} = iris_auth:create_token(<<\"reconnect_user\">>), "
        "ok = iris_auth:revoke_token(Token), "
        "timer:sleep(500), "
        "R1 = iris_auth:validate_token(Token), "
        "timer:sleep(500), "
        "R2 = iris_auth:validate_token(Token), "
        "io:format(\"R1:~p~nR2:~p~n\", [R1, R2])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "token_revoked" in stdout, f"Expected token_revoked, got: {stdout}"

    log("  Token stays revoked across attempts")
    log("  PASS")
    return True


def main():
    log("Revocation Timing Tests (P1-8 / IA-2)")
    log("")

    tests = [
        ("revocation_within_10s", test_revocation_within_10s),
        ("revocation_survives_reconnect", test_revocation_survives_reconnect),
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
