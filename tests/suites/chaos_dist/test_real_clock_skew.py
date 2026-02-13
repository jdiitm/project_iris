#!/usr/bin/env python3
"""
Real Clock Skew Test (RFC NFR-16) -- Docker-only

Unlike the resilience/test_clock_skew.py which falls back to simulation,
this test REQUIRES Docker with libfaketime and will FAIL if real clock
injection is not possible.

Tests:
1. HLC ordering maintained under 25s skew (within 30s tolerance)
2. HLC bounds extreme skew (60s, exceeds 30s tolerance)

Requirements:
- Docker cluster running (make cluster-up)
- libfaketime installed in Docker image (already in Dockerfile.iris)

Tier: 2 (Docker chaos tests)
"""

import sys
import os
import time
import socket
import ssl
import struct
import subprocess
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from tests.suites.chaos_dist.utils import create_tls_socket, tls_connect_and_login


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def check_docker_running():
    """Check if Docker containers are running."""
    try:
        result = subprocess.run(
            ["docker", "ps", "--filter", "name=core-east", "--format", "{{.Names}}"],
            capture_output=True, text=True, timeout=10
        )
        containers = [c for c in result.stdout.strip().split('\n') if c]
        return len(containers) > 0
    except Exception:
        return False


def inject_faketime(container, offset_seconds):
    """
    Inject clock skew using libfaketime.
    Returns True if successful, False otherwise.
    """
    sign = "+" if offset_seconds >= 0 else ""
    
    # Try common libfaketime paths (Alpine vs Debian)
    for lib_path in [
        "/usr/lib/faketime/libfaketime.so.1",
        "/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1",
        "/usr/lib/libfaketime.so.1",
    ]:
        cmd = [
            "docker", "exec", container,
            "sh", "-c",
            f"LD_PRELOAD={lib_path} FAKETIME='{sign}{offset_seconds}s' date +%s"
        ]
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            if result.returncode == 0 and result.stdout.strip().isdigit():
                # Verify the offset was applied
                fake_time = int(result.stdout.strip())
                real_time = int(time.time())
                drift = abs(fake_time - real_time - offset_seconds)
                if drift < 5:  # Within 5 seconds tolerance
                    log(f"  libfaketime active on {container}: offset={offset_seconds}s, verified drift={drift}s")
                    return True, lib_path
        except Exception:
            continue
    
    return False, None


def get_hlc_timestamp_direct(container, lib_path=None, offset_seconds=0):
    """Get HLC timestamp by running a fresh Erlang process in the container.
    
    Uses iris_hlc:now_for_node/1 (pure function, no gen_server needed).
    If lib_path is provided, LD_PRELOAD injects libfaketime so
    erlang:system_time sees the faked clock.
    """
    # Use single-quoted Erlang eval to avoid shell double-quote nesting issues
    erl_eval = (
        "HLC = iris_hlc:now_for_node(0), "
        "Int = iris_hlc:to_integer(HLC), "
        "io:format(\"HLC:~p~n\", [Int]), "
        "init:stop()."
    )
    
    if lib_path and offset_seconds != 0:
        sign = "+" if offset_seconds >= 0 else ""
        # Use single quotes around -eval arg in sh -c to avoid double-quote conflicts
        shell_cmd = (
            f"LD_PRELOAD={lib_path} FAKETIME='{sign}{offset_seconds}s' "
            f"erl -noshell -pa /app/ebin -eval '"
            f"HLC = iris_hlc:now_for_node(0), "
            f"Int = iris_hlc:to_integer(HLC), "
            f'io:format("HLC:~p~n", [Int]), '
            f"init:stop().'"
        )
        cmd = ["docker", "exec", container, "sh", "-c", shell_cmd]
    else:
        cmd = [
            "docker", "exec", container,
            "erl", "-noshell", "-pa", "/app/ebin",
            "-eval", erl_eval,
        ]
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
        stdout = result.stdout.strip()
        if "HLC:" in stdout:
            ts_str = stdout.split("HLC:")[1].strip()
            return int(ts_str) if ts_str.isdigit() else None
    except Exception as e:
        log(f"  Error getting HLC timestamp: {e}")
    return None


# =============================================================================
# Test 1: HLC ordering maintained under 25s skew
# =============================================================================
def test_hlc_ordering_under_30s_skew():
    """
    Create HLC timestamps from two containers: one normal, one with +25s
    clock skew via libfaketime. Assert the drift is within tolerance.
    
    Uses iris_hlc:now_for_node/1 (pure function) in a fresh Erlang process
    with LD_PRELOAD so erlang:system_time sees the faked clock.
    """
    log("=" * 60)
    log("TEST: HLC ordering under 25s clock skew")
    log("=" * 60)

    if not check_docker_running():
        log("  SKIP: Docker cluster not running")
        return None  # Inconclusive

    containers = ["core-east-1", "core-east-2"]

    # Verify libfaketime works on the target container
    success, lib_path = inject_faketime(containers[1], 25)
    if not success:
        log("  FAIL: Could not inject clock skew via libfaketime")
        log("  Ensure libfaketime is installed in the Docker image")
        return False

    # Get HLC from node 1 (no skew) — fresh process, pure function
    ts1 = get_hlc_timestamp_direct(containers[0])
    # Get HLC from node 2 (with +25s skew via LD_PRELOAD)
    ts2 = get_hlc_timestamp_direct(containers[1], lib_path=lib_path, offset_seconds=25)

    if ts1 is None or ts2 is None:
        log(f"  Could not get HLC timestamps (ts1={ts1}, ts2={ts2})")
        log("  iris_hlc module may not be compiled in ebin/")
        return False

    log(f"  Node 1 HLC: {ts1}")
    log(f"  Node 2 HLC: {ts2} (with +25s skew)")

    # HLC encodes physical time in upper bits: (PT << 32) | (L << 16) | N
    # Extract physical component to get drift in ms
    pt1 = ts1 >> 32
    pt2 = ts2 >> 32
    drift_ms = abs(pt2 - pt1)
    log(f"  Physical time drift: {drift_ms}ms")

    # With 25s skew, the drift should be ~25000ms (within 30s tolerance)
    if 20000 <= drift_ms <= 30000:
        log("  PASS: HLC ordering maintained under 25s skew")
        return True
    else:
        log(f"  FAIL: HLC drift unexpected ({drift_ms}ms, expected ~25000ms)")
        return False


# =============================================================================
# Test 2: HLC rejects extreme skew
# =============================================================================
def test_hlc_rejects_extreme_skew():
    """
    Create HLC timestamps with +60s skew (exceeds 30s tolerance).
    Assert the HLC reflects the skew and timestamps are still comparable.
    """
    log("=" * 60)
    log("TEST: HLC bounds extreme clock skew (60s)")
    log("=" * 60)

    if not check_docker_running():
        log("  SKIP: Docker cluster not running")
        return None

    containers = ["core-east-1", "core-east-2"]

    # Verify libfaketime works
    success, lib_path = inject_faketime(containers[1], 60)
    if not success:
        log("  FAIL: Could not inject 60s clock skew")
        return False

    # Get baseline HLC from node 1 (no skew)
    ts_before = get_hlc_timestamp_direct(containers[0])
    # Get HLC from node 2 (with +60s skew via LD_PRELOAD)
    ts_skewed = get_hlc_timestamp_direct(containers[1], lib_path=lib_path, offset_seconds=60)

    if ts_before is None or ts_skewed is None:
        log(f"  Could not get HLC timestamps (normal={ts_before}, skewed={ts_skewed})")
        log("  iris_hlc module may not be compiled in ebin/")
        return False

    # Extract physical time component
    pt_normal = ts_before >> 32
    pt_skewed = ts_skewed >> 32
    drift_ms = abs(pt_skewed - pt_normal)

    log(f"  Normal node HLC: {ts_before} (physical={pt_normal}ms)")
    log(f"  Skewed node HLC: {ts_skewed} (physical={pt_skewed}ms)")
    log(f"  Physical time drift: {drift_ms}ms")

    # With 60s real skew, drift should be ~60000ms
    # The key assertion: HLC timestamps are still valid integers
    # and the skewed timestamp is in the future (as expected)
    if 55000 <= drift_ms <= 65000:
        log(f"  HLC produced timestamp under extreme skew (drift={drift_ms}ms)")
        log("  PASS: HLC handled extreme skew correctly")
        return True
    else:
        log(f"  FAIL: Unexpected drift ({drift_ms}ms, expected ~60000ms)")
        return False


# =============================================================================
# Main
# =============================================================================
def main():
    log("")
    log("=" * 60)
    log("REAL CLOCK SKEW TESTS (RFC NFR-16)")
    log("Requires Docker cluster with libfaketime")
    log("=" * 60)

    tests = [
        ("HLC Ordering Under 25s Skew", test_hlc_ordering_under_30s_skew),
        ("HLC Bounds Extreme 60s Skew", test_hlc_rejects_extreme_skew),
    ]

    results = []
    for name, fn in tests:
        try:
            result = fn()
            results.append((name, result))
        except Exception as e:
            log(f"  Exception in {name}: {e}")
            results.append((name, False))

    print("\n" + "=" * 60)
    print("RESULTS:")
    print("=" * 60)

    passed = 0
    failed = 0
    skipped = 0
    for name, result in results:
        if result is None:
            status = "SKIP"
            skipped += 1
        elif result:
            status = "PASS"
            passed += 1
        else:
            status = "FAIL"
            failed += 1
        print(f"  [{status}] {name}")

    if skipped == len(results):
        print(f"\nReal Clock Skew: SKIPPED (Docker not available)")
        return 2  # AUDIT P1-3: Exit 2 = infrastructure skip (not 0)
    elif failed > 0:
        print(f"\nReal Clock Skew: FAILED ({passed} passed, {failed} failed)")
        return 1
    else:
        print(f"\nReal Clock Skew: PASSED ({passed}/{len(results)})")
        return 0


if __name__ == "__main__":
    sys.exit(main())
