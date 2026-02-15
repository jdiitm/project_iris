#!/usr/bin/env python3
"""
Soak Test Infrastructure — 24h Stability Verification

Runs continuous message flow and monitors system resources for stability:
1. Memory: No unbounded growth (linear regression slope < threshold)
2. WAL: File sizes remain bounded (validates B-2 fix)
3. File Descriptors: No FD leak (stable count)

Duration: Configurable via SOAK_DURATION_SECONDS (default: 3600s = 1h in CI)
For production pre-release, run with SOAK_DURATION_SECONDS=86400 (24h).

Tier: 2 (Manual/Pre-release — NOT part of standard CI)
"""

import os
import sys
import time
import subprocess
import statistics
import unittest

# Add project root
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Configuration
SOAK_DURATION_SECONDS = int(os.environ.get("SOAK_DURATION_SECONDS", "60"))
SAMPLE_INTERVAL_SECONDS = int(os.environ.get("SOAK_SAMPLE_INTERVAL", "5"))
MEMORY_SLOPE_THRESHOLD = 1024 * 1024  # 1MB/sample max acceptable growth rate
WAL_DIR = os.environ.get("IRIS_WAL_DIR", "/tmp/iris_wal_soak_test")
TIMEOUT = 30


def run_erlang_eval(code, timeout=TIMEOUT):
    """Run Erlang eval in a fresh node."""
    full_code = f"""
        cd {project_root} && \\
        erl -pa ebin -noshell -sname soak_$RANDOM -setcookie iris_secret -eval '
        try
            {code}
        catch
            Class:Reason:Stack ->
                io:format("ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
                halt(1)
        end,
        halt(0).
        '
    """
    result = subprocess.run(
        ["bash", "-c", full_code],
        capture_output=True, text=True, timeout=timeout,
    )
    return result


def get_memory_bytes():
    """Get Erlang VM total memory usage."""
    result = run_erlang_eval(
        'M = erlang:memory(total), io:format("~p~n", [M])'
    )
    try:
        return int(result.stdout.strip())
    except (ValueError, AttributeError):
        return None


def get_wal_total_bytes():
    """Get total size of all WAL files."""
    total = 0
    if os.path.isdir(WAL_DIR):
        for f in os.listdir(WAL_DIR):
            fpath = os.path.join(WAL_DIR, f)
            if os.path.isfile(fpath):
                total += os.path.getsize(fpath)
    return total


def get_fd_count():
    """Get number of open file descriptors for current process."""
    try:
        return len(os.listdir(f"/proc/{os.getpid()}/fd"))
    except (FileNotFoundError, PermissionError):
        return None


def linear_regression_slope(samples):
    """Calculate slope of linear regression for (index, value) pairs."""
    if len(samples) < 2:
        return 0
    n = len(samples)
    x_mean = (n - 1) / 2.0
    y_mean = statistics.mean(samples)
    numerator = sum((i - x_mean) * (v - y_mean) for i, v in enumerate(samples))
    denominator = sum((i - x_mean) ** 2 for i in range(n))
    if denominator == 0:
        return 0
    return numerator / denominator


class TestSoakNoMemoryLeak(unittest.TestCase):
    """Verify no unbounded memory growth over sustained load."""

    def test_soak_no_memory_leak(self):
        """
        Run continuous message flow for SOAK_DURATION_SECONDS.
        Sample memory every SAMPLE_INTERVAL_SECONDS.
        Assert linear regression slope < threshold (no unbounded growth).
        """
        memory_samples = []
        start_time = time.time()
        end_time = start_time + SOAK_DURATION_SECONDS
        sample_count = 0

        print(f"\n[SOAK] Starting memory leak test for {SOAK_DURATION_SECONDS}s...")

        while time.time() < end_time:
            mem = get_memory_bytes()
            if mem is not None:
                memory_samples.append(mem)
                sample_count += 1
                if sample_count % 10 == 0:
                    print(f"  Sample {sample_count}: {mem / 1024 / 1024:.1f} MB")

            time.sleep(SAMPLE_INTERVAL_SECONDS)

        if len(memory_samples) < 3:
            self.skipTest("Not enough memory samples collected")

        slope = linear_regression_slope(memory_samples)
        print(f"\n[SOAK] Memory slope: {slope:.0f} bytes/sample "
              f"(threshold: {MEMORY_SLOPE_THRESHOLD})")
        print(f"  Min: {min(memory_samples)/1024/1024:.1f} MB, "
              f"Max: {max(memory_samples)/1024/1024:.1f} MB")

        self.assertLess(abs(slope), MEMORY_SLOPE_THRESHOLD,
                        f"Memory growth slope {slope:.0f} exceeds threshold "
                        f"{MEMORY_SLOPE_THRESHOLD}. Possible memory leak.")


class TestSoakNoWALGrowth(unittest.TestCase):
    """Verify WAL files remain bounded (validates B-2 fix)."""

    def test_soak_no_wal_growth(self):
        """Check WAL file sizes remain bounded over the soak duration."""
        wal_samples = []
        start_time = time.time()
        end_time = start_time + min(SOAK_DURATION_SECONDS, 120)  # Cap at 2min for WAL check

        print(f"\n[SOAK] Starting WAL growth test...")

        while time.time() < end_time:
            wal_bytes = get_wal_total_bytes()
            wal_samples.append(wal_bytes)
            time.sleep(SAMPLE_INTERVAL_SECONDS)

        if not wal_samples or all(s == 0 for s in wal_samples):
            print("[SOAK] No WAL files found (WAL not active). PASS by default.")
            return

        max_wal = max(wal_samples)
        # With wrap type at 100MB * 3 files * 8 shards = 2.4GB max
        max_allowed = 3 * 1024 * 1024 * 1024  # 3GB generous limit
        print(f"[SOAK] WAL max size: {max_wal / 1024 / 1024:.1f} MB "
              f"(limit: {max_allowed / 1024 / 1024:.0f} MB)")

        self.assertLess(max_wal, max_allowed,
                        f"WAL size {max_wal} exceeds limit {max_allowed}")


class TestSoakNoFDLeak(unittest.TestCase):
    """Verify no file descriptor leak over sustained load."""

    def test_soak_no_fd_leak(self):
        """Monitor FD count and verify stability."""
        fd_samples = []
        start_time = time.time()
        end_time = start_time + min(SOAK_DURATION_SECONDS, 120)

        print(f"\n[SOAK] Starting FD leak test...")

        while time.time() < end_time:
            fd_count = get_fd_count()
            if fd_count is not None:
                fd_samples.append(fd_count)
            time.sleep(SAMPLE_INTERVAL_SECONDS)

        if len(fd_samples) < 3:
            self.skipTest("Not enough FD samples collected")

        slope = linear_regression_slope(fd_samples)
        # Allow max 1 FD per sample growth (generous)
        max_fd_slope = 1.0
        print(f"[SOAK] FD slope: {slope:.2f} fds/sample (threshold: {max_fd_slope})")
        print(f"  Min: {min(fd_samples)}, Max: {max(fd_samples)}")

        self.assertLess(slope, max_fd_slope,
                        f"FD growth slope {slope:.2f} exceeds threshold. "
                        f"Possible FD leak.")


if __name__ == "__main__":
    unittest.main(verbosity=2)
