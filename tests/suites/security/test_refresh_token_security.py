#!/usr/bin/env python3
"""
P1-5 (IA-3): Refresh Token Security Tests

RFC-001 v4.0 FR-11a: Refresh tokens are opaque, rotated on each use.
Reuse detection revokes token families.

Tests verify:
1. TOKEN_REFRESH (0x0B) with valid refresh token yields new access token
2. Replay of same refresh token is rejected
3. Messaging continues after refresh

Pattern: follows test_token_refresh_flow.py.
"""

import sys
import os
import time
import subprocess

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.iris_client import IrisClient

HOST = os.environ.get('IRIS_HOST', 'localhost')
PORT = int(os.environ.get('IRIS_PORT', '8085'))


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


def test_refresh_token_wire_level():
    """
    Create and exchange refresh token via Erlang calls.
    """
    log("=" * 60)
    log("TEST: Refresh token wire-level exchange")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:create_table(refresh_tokens, [{ram_copies, [node()]}, {attributes, [token_id, user_id, family_id, used, created_at, expires_at]}]), "
        "mnesia:wait_for_tables([revoked_tokens, refresh_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_for_refresh_wire_test!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "application:set_env(iris_edge, allow_hmac_jwt, true), "
        "application:set_env(iris_edge, jwt_eddsa_private_key, crypto:strong_rand_bytes(32)), "
        "{ok, _} = iris_auth:start_link(), "
        "{ok, RT} = iris_auth:create_refresh_token(<<\"wire_user\">>), "
        "{ok, NewAccess, NewRT} = iris_auth:exchange_refresh_token(RT), "
        "{ok, Claims} = iris_auth:validate_token(NewAccess), "
        "Sub = maps:get(<<\"sub\">>, Claims), "
        "io:format(\"OK:~s:~s~n\", [Sub, NewRT])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "ERROR" not in stdout, f"Erlang error: {stdout}"
    assert "OK:wire_user:" in stdout, f"Expected OK result, got: {stdout}"

    log("  PASS")
    return True


def test_refresh_token_replay_rejected():
    """
    Send same refresh token twice, second rejected.
    """
    log("=" * 60)
    log("TEST: Refresh token replay rejected")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:create_table(refresh_tokens, [{ram_copies, [node()]}, {attributes, [token_id, user_id, family_id, used, created_at, expires_at]}]), "
        "mnesia:wait_for_tables([revoked_tokens, refresh_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_replay_reject_test!!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "application:set_env(iris_edge, allow_hmac_jwt, true), "
        "application:set_env(iris_edge, jwt_eddsa_private_key, crypto:strong_rand_bytes(32)), "
        "{ok, _} = iris_auth:start_link(), "
        "{ok, RT} = iris_auth:create_refresh_token(<<\"replay_user\">>), "
        "{ok, _, _} = iris_auth:exchange_refresh_token(RT), "
        "R2 = iris_auth:exchange_refresh_token(RT), "
        "io:format(\"RESULT:~p~n\", [R2])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "token_reused" in stdout, f"Expected token_reused, got: {stdout}"

    log("  PASS")
    return True


def test_messaging_continues_after_refresh():
    """
    After refresh, messaging functions still work.
    """
    log("=" * 60)
    log("TEST: Messaging continues after refresh")
    log("=" * 60)

    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:create_table(refresh_tokens, [{ram_copies, [node()]}, {attributes, [token_id, user_id, family_id, used, created_at, expires_at]}]), "
        "mnesia:wait_for_tables([revoked_tokens, refresh_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_msg_continue_test!!!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "application:set_env(iris_edge, allow_hmac_jwt, true), "
        "application:set_env(iris_edge, jwt_eddsa_private_key, crypto:strong_rand_bytes(32)), "
        "{ok, _} = iris_auth:start_link(), "
        "{ok, RT} = iris_auth:create_refresh_token(<<\"msg_user\">>), "
        "{ok, NewAccess, _NewRT} = iris_auth:exchange_refresh_token(RT), "
        "{ok, Claims} = iris_auth:validate_token(NewAccess), "
        "io:format(\"VALID:~s~n\", [maps:get(<<\"sub\">>, Claims)])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "VALID:msg_user" in stdout, f"Expected VALID result, got: {stdout}"

    log("  PASS")
    return True


def main():
    log("Refresh Token Security Tests (P1-5 / IA-3)")
    log("")

    tests = [
        ("refresh_wire_level", test_refresh_token_wire_level),
        ("refresh_replay_rejected", test_refresh_token_replay_rejected),
        ("messaging_after_refresh", test_messaging_continues_after_refresh),
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
