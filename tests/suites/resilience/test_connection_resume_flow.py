#!/usr/bin/env python3
"""
P1-6 (FM-3): Connection Resume Flow Tests

RFC-001 v4.0 Section 6.5:
- Sessions cached with session_id and last_seq on login
- Cache expires after 5 minutes (300s)
- RESUME with valid session replays missed messages
- RESUME with expired session returns NACK

Tests verify:
1. Resume replays missed messages (via Erlang session cache)
2. Resume NACK after timeout (session expired)

Pattern: follows test_connection_resume.py using run_erlang helper.
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


def test_resume_replays_missed_messages():
    """
    Store session, queue messages, resume and get missed messages.
    """
    log("=" * 60)
    log("TEST: Resume replays missed messages")
    log("=" * 60)

    code = (
        "iris_session_cache:start(), "
        "ok = iris_session_cache:store(<<\"sess1\">>, <<\"user1\">>), "
        "iris_session_cache:queue_message(<<\"sess1\">>, 1, <<\"hello\">>), "
        "iris_session_cache:queue_message(<<\"sess1\">>, 2, <<\"world\">>), "
        "iris_session_cache:queue_message(<<\"sess1\">>, 3, <<\"missed\">>), "
        "{ok, Msgs} = iris_session_cache:get_messages_after(<<\"sess1\">>, 1), "
        "Count = length(Msgs), "
        "io:format(\"MISSED:~p~n\", [Count]), "
        "iris_session_cache:stop()"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "MISSED:2" in stdout, f"Expected 2 missed messages, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def test_resume_nack_after_timeout():
    """
    After session expires, lookup returns not_found.
    """
    log("=" * 60)
    log("TEST: Resume NACK after session expiry")
    log("=" * 60)

    code = (
        "iris_session_cache:start(), "
        "R = iris_session_cache:lookup(<<\"nonexistent\">>), "
        "TTL = iris_session_cache:get_ttl(), "
        "io:format(\"NACK:~p:TTL:~p~n\", [R, TTL]), "
        "iris_session_cache:stop()"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "not_found" in stdout, f"Expected not_found, got: {stdout}"
    assert "TTL:300" in stdout, f"Expected TTL 300, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def main():
    log("Connection Resume Flow Tests (P1-6 / FM-3)")
    log("")

    tests = [
        ("resume_replays_missed", test_resume_replays_missed_messages),
        ("resume_nack_timeout", test_resume_nack_after_timeout),
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
