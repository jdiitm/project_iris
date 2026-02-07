#!/usr/bin/env python3
"""
P1-7 (IA-1 residual): HMAC Deprecation Enforcement Tests

RFC-001 v4.0 FR-9 mandates EdDSA (Ed25519). HMAC-SHA256 path should
be disableable for production deployment.

Tests verify:
1. Server accepts HMAC login when allowed (default)
2. EdDSA mode only when HMAC disabled (config via Erlang)

Pattern: follows test_jwt_security.py using IrisClient.
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


def unique_user(prefix):
    import uuid
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def test_login_with_hmac_when_allowed():
    """
    Server accepts HMAC-based login by default.
    """
    log("=" * 60)
    log("TEST: HMAC login accepted when allowed (default)")
    log("=" * 60)

    client = None
    try:
        client = IrisClient(HOST, PORT)
        user = unique_user("hmac_allowed")
        client.login(user)
        log(f"  Logged in as {user}")
        log("  PASS")
        return True
    finally:
        if client:
            client.close()


def test_messaging_with_eddsa_only_mode():
    """
    When HMAC disabled (via Erlang config), EdDSA clients can authenticate.
    This test verifies the config mechanism exists by checking the Erlang
    function directly.
    """
    log("=" * 60)
    log("TEST: EdDSA-only mode config exists")
    log("=" * 60)

    # Verify via Erlang that the config flag works
    # Must set up Mnesia + iris_auth gen_server in the subprocess
    code = (
        "application:stop(mnesia), "
        "mnesia:delete_schema([node()]), "
        "mnesia:create_schema([node()]), "
        "mnesia:start(), "
        "mnesia:create_table(revoked_tokens, [{ram_copies, [node()]}, {attributes, [token_id, timestamp]}]), "
        "mnesia:wait_for_tables([revoked_tokens], 5000), "
        "application:set_env(iris_edge, jwt_secret, <<\"test_secret_hmac_deprecation_key!\">>), "
        "application:set_env(iris_edge, auth_enabled, true), "
        "{ok, _} = iris_auth:start_link(), "
        "application:set_env(iris_edge, allow_hmac_jwt, false), "
        "{ok, T} = iris_auth:create_token(<<\"test\">>), "
        "R = iris_auth:validate_token(T), "
        "application:unset_env(iris_edge, allow_hmac_jwt), "
        "io:format(\"RESULT:~p~n\", [R])"
    )
    result = subprocess.run(
        ["erl", "-pa", "ebin", "-noshell", "-eval",
         f"try {code} catch C:R:S -> io:format(\"ERROR:~p:~p~n\", [C,R]) end, init:stop()."],
        capture_output=True, text=True, timeout=15,
        cwd=PROJECT_ROOT
    )
    stdout = result.stdout.strip()
    log(f"  Erlang result: {stdout}")

    assert "hmac_deprecated" in stdout, f"Expected hmac_deprecated error, got: {stdout}"

    log("  HMAC rejection confirmed when disabled")
    log("  PASS")
    return True


def main():
    log("HMAC Deprecation Tests (P1-7 / IA-1)")
    log(f"Server: {HOST}:{PORT}")
    log("")

    tests = [
        ("hmac_login_allowed", test_login_with_hmac_when_allowed),
        ("eddsa_only_mode", test_messaging_with_eddsa_only_mode),
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
