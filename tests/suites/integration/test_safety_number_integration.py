#!/usr/bin/env python3
"""
P0-1 (EK-1): Safety Number Integration Tests

RFC-001-AMENDMENT-001 v1.3 Section 5.3.1:
Safety Number = SHA-256(sort(IK_A, IK_B))[:30] displayed as 12 groups of 5 digits.

Tests verify:
1. compute_safety_number/2 is callable via Erlang and returns correct format
2. Changing identity key produces a different safety number

Pattern: follows test_key_verification.py using run_erlang helper.
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
    """Execute Erlang expression via erl -noshell and return stdout."""
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


def test_safety_number_via_erlang():
    """
    Call iris_keys:compute_safety_number/2 through Erlang and verify format.
    """
    log("=" * 60)
    log("TEST: Safety number via Erlang RPC - format verification")
    log("=" * 60)

    code = (
        "IK_A = crypto:strong_rand_bytes(32), "
        "IK_B = crypto:strong_rand_bytes(32), "
        "{ok, SN} = iris_keys:compute_safety_number(IK_A, IK_B), "
        "io:format(\"SN:~s~n\", [SN])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    # Extract safety number from output
    lines = stdout.strip().split("\n")
    sn_line = [l for l in lines if l.startswith("SN:")]
    assert len(sn_line) == 1, f"Expected SN: line, got: {lines}"
    sn = sn_line[0][3:]  # strip "SN:" prefix

    # Verify format: 12 groups of 5 digits separated by spaces
    groups = sn.split(" ")
    assert len(groups) == 12, f"Expected 12 groups, got {len(groups)}: {sn}"
    for g in groups:
        assert len(g) == 5, f"Expected 5-digit group, got '{g}'"
        assert g.isdigit(), f"Non-digit group: '{g}'"

    log(f"  Safety number: {sn}")
    log("  PASS")
    return True


def test_key_change_produces_different_safety_number():
    """
    Changing one identity key produces a different safety number.
    """
    log("=" * 60)
    log("TEST: Key change produces different safety number")
    log("=" * 60)

    code = (
        "IK_A = crypto:strong_rand_bytes(32), "
        "IK_B = crypto:strong_rand_bytes(32), "
        "IK_C = crypto:strong_rand_bytes(32), "
        "{ok, SN1} = iris_keys:compute_safety_number(IK_A, IK_B), "
        "{ok, SN2} = iris_keys:compute_safety_number(IK_A, IK_C), "
        "io:format(\"SN1:~s~nSN2:~s~nSAME:~p~n\", [SN1, SN2, SN1 =:= SN2])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "ERROR" not in stdout, f"Erlang error: {stdout}"
    assert "SAME:false" in stdout, f"Expected different safety numbers, got: {stdout}"

    log("  Safety numbers differ when keys change")
    log("  PASS")
    return True


def main():
    log("Safety Number Integration Tests (P0-1 / EK-1)")
    log("")

    tests = [
        ("safety_number_via_erlang", test_safety_number_via_erlang),
        ("key_change_different_sn", test_key_change_produces_different_safety_number),
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
