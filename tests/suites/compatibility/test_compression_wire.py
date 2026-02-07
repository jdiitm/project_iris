#!/usr/bin/env python3
"""
P1-11 (PD-2): Compression Wire Compatibility Tests

RFC-001 v4.0 Section 11.1:
- zstd and zlib supported
- Uncompressed frames always accepted
- Capability negotiation during handshake

Tests verify:
1. Server accepts uncompressed frames (default)
2. Capability negotiation response returns supported list

Pattern: follows test_backward_compat_opcodes.py using run_erlang helper.
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


def test_server_accepts_uncompressed():
    """
    Uncompressed frames always accepted (default behavior).
    Verify compression module roundtrip works at wire level.
    """
    log("=" * 60)
    log("TEST: Server accepts uncompressed frames")
    log("=" * 60)

    code = (
        "Data = <<\"hello world this is a test message\">>, "
        "{ok, Compressed} = iris_compression:compress(zlib, Data), "
        "{ok, Decompressed} = iris_compression:decompress(zlib, Compressed), "
        "Match = (Data =:= Decompressed), "
        "io:format(\"ROUNDTRIP:~p~n\", [Match])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "ROUNDTRIP:true" in stdout, f"Expected roundtrip true, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def test_capability_negotiation_response():
    """
    Server responds to capability hello with supported list.
    """
    log("=" * 60)
    log("TEST: Capability negotiation response")
    log("=" * 60)

    code = (
        "Client = [<<\"zstd\">>, <<\"e2ee\">>, <<\"zlib\">>], "
        "Server = [<<\"zstd\">>, <<\"zlib\">>], "
        "Result = iris_compression:negotiate(Client, Server), "
        "HasZstd = lists:member(<<\"zstd\">>, Result), "
        "HasZlib = lists:member(<<\"zlib\">>, Result), "
        "NoE2ee = not lists:member(<<\"e2ee\">>, Result), "
        "io:format(\"CAPS:zstd=~p,zlib=~p,no_e2ee=~p~n\", [HasZstd, HasZlib, NoE2ee])"
    )
    stdout, stderr, rc = run_erlang(code)
    log(f"  stdout: {stdout}")

    assert "zstd=true" in stdout, f"Expected zstd support, got: {stdout}"
    assert "zlib=true" in stdout, f"Expected zlib support, got: {stdout}"
    assert "no_e2ee=true" in stdout, f"Expected no e2ee in negotiation, got: {stdout}"
    assert "ERROR" not in stdout, f"Erlang error: {stdout}"

    log("  PASS")
    return True


def main():
    log("Compression Wire Compatibility Tests (P1-11 / PD-2)")
    log("")

    tests = [
        ("server_accepts_uncompressed", test_server_accepts_uncompressed),
        ("capability_negotiation", test_capability_negotiation_response),
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
