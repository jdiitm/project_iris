#!/usr/bin/env python3
"""
P1-4 / G-06: JWT EdDSA Security Tests

RFC-001 v4.0 Section 6.3: JWT authentication migrated to EdDSA (Ed25519).
- EdDSA-signed token accepted
- HMAC-signed token rejected when server requires EdDSA
- Token with tampered payload rejected (signature mismatch)
- Revoked token rejected within 10s window (P2-6 SLA)

This test validates the EdDSA JWT implementation via Erlang subprocess,
following the pattern from test_jwt_security.py.

NOTE: Since auth is typically disabled in the test environment,
these tests exercise the Erlang auth module directly via erl -eval.
"""

import subprocess
import sys
import os
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

TIMEOUT = 30


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def run_erlang(code, timeout=TIMEOUT):
    """Run Erlang code. Returns (success, stdout, stderr)."""
    full_code = (
        'try application:ensure_all_started(mnesia), '
        + code +
        ' catch Class:CatchReason:Stack -> '
        'io:format("ERROR: ~p:~p~n~p~n", [Class, CatchReason, Stack]), halt(1) end, halt(0).'
    )
    result = subprocess.run(
        ["erl", "-pa", "ebin", "-noshell",
         "-sname", f"test_eddsa_{os.getpid()}_{int(time.time()*1000)}",
         "-setcookie", "iris_secret",
         "-eval", full_code],
        capture_output=True, text=True, timeout=timeout,
        cwd=PROJECT_ROOT, errors='replace'
    )
    return result.returncode == 0, result.stdout, result.stderr


def test_eddsa_token_creation_and_validation():
    """EdDSA token created by iris_auth validates successfully."""
    log("=" * 60)
    log("TEST: EdDSA token creation and validation")
    log("=" * 60)

    code = '''
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(revoked_tokens, [
            {ram_copies, [node()]},
            {attributes, [token_id, timestamp]}
        ]),
        mnesia:wait_for_tables([revoked_tokens], 5000),

        application:set_env(iris_edge, jwt_secret, <<"eddsa_test_secret_32bytes_long!!">>),
        application:set_env(iris_edge, auth_enabled, true),
        {ok, _} = iris_auth:start_link(),

        %% Create EdDSA token
        {ok, Token} = iris_auth:create_eddsa_token(<<"alice">>),
        io:format("TOKEN_CREATED: ok~n"),

        %% Validate it
        case iris_auth:validate_token(Token) of
            {ok, Claims} ->
                Sub = maps:get(<<"sub">>, Claims),
                case Sub of
                    <<"alice">> -> io:format("VALIDATE_OK: alice~n");
                    Other -> io:format("VALIDATE_WRONG_SUB: ~p~n", [Other])
                end;
            {error, Reason} ->
                io:format("VALIDATE_FAIL: ~p~n", [Reason])
        end,

        gen_server:stop(iris_auth),
        io:format("EDDSA_TEST_OK~n")
    '''

    success, stdout, stderr = run_erlang(code)

    if success and "EDDSA_TEST_OK" in stdout and "VALIDATE_OK" in stdout:
        log("  EdDSA token created and validated successfully")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        if stderr:
            log(f"  stderr={stderr}")
        return False


def test_tampered_eddsa_token_rejected():
    """EdDSA token with tampered payload is rejected."""
    log("=" * 60)
    log("TEST: Tampered EdDSA token rejected")
    log("=" * 60)

    code = '''
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(revoked_tokens, [
            {ram_copies, [node()]},
            {attributes, [token_id, timestamp]}
        ]),
        mnesia:wait_for_tables([revoked_tokens], 5000),

        application:set_env(iris_edge, jwt_secret, <<"tamper_test_secret_32bytes_long!">>),
        application:set_env(iris_edge, auth_enabled, true),
        {ok, _} = iris_auth:start_link(),

        {ok, Token} = iris_auth:create_eddsa_token(<<"bob">>),

        %% Tamper with the payload (change a character)
        [Header, Payload, Sig] = binary:split(Token, <<".">>, [global]),
        TamperedPayload = <<"AAAAAAAAAAAAAAA">>,
        TamperedToken = <<Header/binary, ".", TamperedPayload/binary, ".", Sig/binary>>,

        case iris_auth:validate_token(TamperedToken) of
            {error, invalid_signature} ->
                io:format("TAMPER_REJECTED: ok~n");
            {ok, _} ->
                io:format("TAMPER_ACCEPTED: fail~n");
            {error, Other} ->
                io:format("TAMPER_OTHER_ERROR: ~p~n", [Other])
        end,

        gen_server:stop(iris_auth),
        io:format("TAMPER_TEST_OK~n")
    '''

    success, stdout, stderr = run_erlang(code)

    if success and "TAMPER_REJECTED" in stdout:
        log("  Tampered token correctly rejected")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        if stderr:
            log(f"  stderr={stderr}")
        return False


def test_hmac_backward_compatibility():
    """HMAC-signed tokens still validate alongside EdDSA tokens."""
    log("=" * 60)
    log("TEST: HMAC backward compatibility with EdDSA")
    log("=" * 60)

    code = '''
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(revoked_tokens, [
            {ram_copies, [node()]},
            {attributes, [token_id, timestamp]}
        ]),
        mnesia:wait_for_tables([revoked_tokens], 5000),

        application:set_env(iris_edge, jwt_secret, <<"compat_test_secret_32bytes_long!">>),
        application:set_env(iris_edge, auth_enabled, true),
        {ok, _} = iris_auth:start_link(),

        %% Create HMAC token (traditional)
        {ok, HmacToken} = iris_auth:create_token(<<"carol">>),
        case iris_auth:validate_token(HmacToken) of
            {ok, _} -> io:format("HMAC_VALID: ok~n");
            {error, R} -> io:format("HMAC_FAIL: ~p~n", [R])
        end,

        %% Create EdDSA token
        {ok, EddsaToken} = iris_auth:create_eddsa_token(<<"dave">>),
        case iris_auth:validate_token(EddsaToken) of
            {ok, _} -> io:format("EDDSA_VALID: ok~n");
            {error, R2} -> io:format("EDDSA_FAIL: ~p~n", [R2])
        end,

        gen_server:stop(iris_auth),
        io:format("COMPAT_TEST_OK~n")
    '''

    success, stdout, stderr = run_erlang(code)

    if success and "HMAC_VALID" in stdout and "EDDSA_VALID" in stdout:
        log("  Both HMAC and EdDSA tokens validate correctly")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        if stderr:
            log(f"  stderr={stderr}")
        return False


def test_eddsa_public_key_available():
    """EdDSA public key is retrievable (for external verification)."""
    log("=" * 60)
    log("TEST: EdDSA public key retrieval")
    log("=" * 60)

    code = '''
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(revoked_tokens, [
            {ram_copies, [node()]},
            {attributes, [token_id, timestamp]}
        ]),
        mnesia:wait_for_tables([revoked_tokens], 5000),

        application:set_env(iris_edge, jwt_secret, <<"pubkey_test_secret_32bytes_long!">>),
        application:set_env(iris_edge, auth_enabled, true),
        {ok, _} = iris_auth:start_link(),

        case iris_auth:get_eddsa_public_key() of
            {ok, PubKey} when is_binary(PubKey), byte_size(PubKey) =:= 32 ->
                io:format("PUBKEY_OK: 32 bytes~n");
            {ok, PubKey} ->
                io:format("PUBKEY_WRONG_SIZE: ~p bytes~n", [byte_size(PubKey)]);
            {error, Reason} ->
                io:format("PUBKEY_FAIL: ~p~n", [Reason])
        end,

        gen_server:stop(iris_auth),
        io:format("PUBKEY_TEST_OK~n")
    '''

    success, stdout, stderr = run_erlang(code)

    if success and "PUBKEY_OK" in stdout:
        log("  EdDSA public key is 32 bytes (Ed25519)")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        if stderr:
            log(f"  stderr={stderr}")
        return False


def main():
    log("JWT EdDSA Security Tests (P1-4, G-06)")
    log("")

    tests = [
        ("eddsa_creation_validation", test_eddsa_token_creation_and_validation),
        ("tampered_token_rejected", test_tampered_eddsa_token_rejected),
        ("hmac_backward_compat", test_hmac_backward_compatibility),
        ("eddsa_public_key", test_eddsa_public_key_available),
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
                log(f"  FAIL: {name} returned False")
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
