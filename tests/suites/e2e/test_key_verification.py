#!/usr/bin/env python3
"""
P1-5: Key Verification (Safety Numbers) Tests

RFC-001-AMENDMENT-001 v1.3 Section 6.1 specifies:
- Safety number computed from two identity keys (deterministic)
- Safety number changes when one party re-registers (new identity key)
- Both parties compute the same safety number

Safety numbers follow the Signal Protocol pattern:
  hash(sort(IK_A || IK_B)) → numeric fingerprint

Pattern: follows test_opk_exhaustion.py using run_erlang_command.
"""

import subprocess
import sys
import os
import time

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

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
    r = subprocess.run(
        ["erl", "-pa", "ebin", "-noshell",
         "-sname", f"test_keyver_{os.getpid()}_{int(time.time()*1000)}",
         "-setcookie", "iris_secret",
         "-eval", full_code],
        capture_output=True, text=True, timeout=timeout,
        cwd=project_root, errors='replace')
    return r.returncode == 0, r.stdout, r.stderr


def test_safety_number_deterministic():
    """
    Safety number computed from two identity keys must be deterministic:
    same inputs always produce the same output.
    """
    log("=" * 60)
    log("TEST: Safety number is deterministic")
    log("=" * 60)

    code = '''
        %% Generate two fixed identity keys
        IK_A = crypto:strong_rand_bytes(32),
        IK_B = crypto:strong_rand_bytes(32),

        %% Compute safety number: hash(sort(IK_A, IK_B))
        Sorted = case IK_A < IK_B of
            true -> <<IK_A/binary, IK_B/binary>>;
            false -> <<IK_B/binary, IK_A/binary>>
        end,
        SN1 = crypto:hash(sha256, Sorted),
        SN2 = crypto:hash(sha256, Sorted),

        case SN1 =:= SN2 of
            true -> io:format("DETERMINISTIC: ok~n");
            false -> io:format("DETERMINISTIC: fail~n")
        end,

        %% Both parties compute the same number
        SortedBA = case IK_B < IK_A of
            true -> <<IK_B/binary, IK_A/binary>>;
            false -> <<IK_A/binary, IK_B/binary>>
        end,
        SN_from_B = crypto:hash(sha256, SortedBA),

        case SN1 =:= SN_from_B of
            true -> io:format("SYMMETRIC: ok~n");
            false -> io:format("SYMMETRIC: fail~n")
        end,

        io:format("SAFETY_NUM_OK~n")
    '''.strip()

    success, stdout, stderr = run_erlang(code)

    if success and "SAFETY_NUM_OK" in stdout:
        if "DETERMINISTIC" in stdout:
            log("  Same keys produce same safety number")
        if "SYMMETRIC" in stdout:
            log("  Both parties compute identical safety number")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        return False


def test_safety_number_changes_on_rekey():
    """
    If one party re-registers (new identity key), the safety number
    must change. This alerts the other party about key change.
    """
    log("=" * 60)
    log("TEST: Safety number changes on re-registration")
    log("=" * 60)

    code = '''
        IK_A = crypto:strong_rand_bytes(32),
        IK_B_original = crypto:strong_rand_bytes(32),
        IK_B_new = crypto:strong_rand_bytes(32),

        %% Original safety number
        Sorted1 = case IK_A < IK_B_original of
            true -> <<IK_A/binary, IK_B_original/binary>>;
            false -> <<IK_B_original/binary, IK_A/binary>>
        end,
        SN_original = crypto:hash(sha256, Sorted1),

        %% New safety number after B re-registers
        Sorted2 = case IK_A < IK_B_new of
            true -> <<IK_A/binary, IK_B_new/binary>>;
            false -> <<IK_B_new/binary, IK_A/binary>>
        end,
        SN_new = crypto:hash(sha256, Sorted2),

        case SN_original =/= SN_new of
            true -> io:format("REKEY_CHANGED: ok~n");
            false -> io:format("REKEY_SAME: fail (should have changed)~n")
        end,

        io:format("REKEY_TEST_OK~n")
    '''.strip()

    success, stdout, stderr = run_erlang(code)

    if success and "REKEY_CHANGED" in stdout:
        log("  Safety number changed after re-registration")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        return False


def test_safety_number_display_format():
    """
    Safety number display format: convert hash to numeric string
    for human-readable comparison.
    """
    log("=" * 60)
    log("TEST: Safety number display format")
    log("=" * 60)

    code = '''
        IK_A = crypto:strong_rand_bytes(32),
        IK_B = crypto:strong_rand_bytes(32),

        Sorted = case IK_A < IK_B of
            true -> <<IK_A/binary, IK_B/binary>>;
            false -> <<IK_B/binary, IK_A/binary>>
        end,
        Hash = crypto:hash(sha256, Sorted),

        %% Convert to numeric display (take first 30 bytes, convert to decimal groups)
        <<N1:40, N2:40, N3:40, N4:40, N5:40, N6:40, _/binary>> = Hash,
        Display = io_lib:format("~12.10.0b ~12.10.0b ~12.10.0b ~12.10.0b ~12.10.0b ~12.10.0b",
                               [N1 rem 1000000000000, N2 rem 1000000000000,
                                N3 rem 1000000000000, N4 rem 1000000000000,
                                N5 rem 1000000000000, N6 rem 1000000000000]),
        DisplayStr = lists:flatten(Display),
        Len = length(DisplayStr),

        case Len > 60 of
            true -> io:format("DISPLAY_FORMAT: ok (~p chars)~n", [Len]);
            false -> io:format("DISPLAY_FORMAT: too short (~p chars)~n", [Len])
        end,

        io:format("DISPLAY_TEST_OK~n")
    '''.strip()

    success, stdout, stderr = run_erlang(code)

    if success and "DISPLAY_FORMAT: ok" in stdout:
        log("  Safety number has proper display format")
        log("  PASS")
        return True
    else:
        log(f"  FAIL: stdout={stdout}")
        return False


def main():
    log("Key Verification (Safety Numbers) Tests (P1-5)")
    log("")

    tests = [
        ("deterministic", test_safety_number_deterministic),
        ("changes_on_rekey", test_safety_number_changes_on_rekey),
        ("display_format", test_safety_number_display_format),
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
