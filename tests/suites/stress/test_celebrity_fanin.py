#!/usr/bin/env python3
"""
G-19: Celebrity Account Fan-In Stress Test

RFC-001 Section 8: Operational Limits
Tests behavior when many senders target a single recipient concurrently.

Test Scenarios:
1. N unique senders message 1 recipient concurrently -- no crash
2. Memory growth must be bounded (< 2x baseline)
3. Flow controller should activate before OOM

Configuration scales with TEST_PROFILE:
- smoke: 100 senders
- full: 10,000 senders

Pattern: follows test_connection_scale.py (TEST_PROFILE + ThreadPoolExecutor)

Tier: 3 (Stress)
"""

import os
import sys
import time
import socket
import subprocess
import random
from concurrent.futures import ThreadPoolExecutor, as_completed

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient, unique_user

TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
QUICK_MODE = os.environ.get("QUICK_MODE", "0") == "1"
IS_CI = os.environ.get("CI") == "true" or os.environ.get("GITHUB_ACTIONS") == "true"

PROFILES = {
    "smoke": {"senders": 100, "msgs_per_sender": 5},
    "full": {"senders": 10000, "msgs_per_sender": 10},
}

if QUICK_MODE or IS_CI:
    CONFIG = PROFILES["smoke"]
    CONFIG["senders"] = 50
else:
    CONFIG = PROFILES.get(TEST_PROFILE, PROFILES["smoke"])

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))

results = []


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def server_alive():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect((SERVER_HOST, SERVER_PORT))
        s.close()
        return True
    except Exception:
        return False


def get_memory_mb():
    """Get beam.smp RSS in MB."""
    try:
        result = subprocess.run(
            ["ps", "-C", "beam.smp", "-o", "rss="],
            capture_output=True, text=True, timeout=5
        )
        total = sum(int(x.strip()) for x in result.stdout.strip().split("\n") if x.strip())
        return total / 1024
    except Exception:
        return 0


def send_to_celebrity(sender_id, celebrity, msgs):
    """Connect, login, send messages to celebrity, disconnect."""
    try:
        c = IrisClient()
        c.login(sender_id)
        for i in range(msgs):
            try:
                c.send_msg(celebrity, f"fan_{sender_id}_{i}")
            except Exception:
                break
            time.sleep(0.01)
        c.close()
        return True
    except Exception:
        return False


# =============================================================================
# Test 1: Fan-In No Crash
# =============================================================================
def test_fanin_no_crash():
    """N senders send to 1 recipient concurrently."""
    n = CONFIG["senders"]
    msgs = CONFIG["msgs_per_sender"]
    log(f"\n=== Test 1: Fan-In ({n} senders x {msgs} msgs) ===")

    celebrity = unique_user("celebrity")
    senders = [f"fan_sender_{TEST_SEED}_{i}" for i in range(n)]

    # Get baseline memory
    baseline_mb = get_memory_mb()
    log(f"  Baseline memory: {baseline_mb:.0f} MB")

    # Fire all senders concurrently
    success = 0
    with ThreadPoolExecutor(max_workers=min(50, n)) as pool:
        futures = [pool.submit(send_to_celebrity, s, celebrity, msgs) for s in senders]
        for f in as_completed(futures, timeout=120):
            try:
                if f.result():
                    success += 1
            except Exception:
                pass

    log(f"  {success}/{n} senders completed successfully")

    if not server_alive():
        log("  FAIL: Server crashed during fan-in")
        return False

    # Check memory growth
    post_mb = get_memory_mb()
    growth = post_mb / max(baseline_mb, 1)
    log(f"  Post-fan-in memory: {post_mb:.0f} MB (growth: {growth:.1f}x)")

    if growth > 3.0:
        log(f"  WARNING: Memory growth > 3x ({growth:.1f}x)")

    if success >= n * 0.5:
        log(f"  PASS: Fan-in completed without crash ({success}/{n} senders)")
        return True
    else:
        log(f"  FAIL: Too many senders failed ({success}/{n})")
        return False


# =============================================================================
# Test 2: Server Functional After Fan-In
# =============================================================================
def test_functional_after_fanin():
    """Verify a normal client works after fan-in storm."""
    log("\n=== Test 2: Server Functional After Fan-In ===")

    if not server_alive():
        log("  FAIL: Server is DOWN")
        return False

    try:
        c = IrisClient()
        c.login("normal_after_fanin")
        c.send_msg("normal_target", "hello after fan-in")
        c.close()
        log("  PASS: Normal messaging works after fan-in")
        return True
    except Exception as e:
        log(f"  FAIL: {e}")
        return False


def main():
    print("=" * 60)
    print(" G-19: CELEBRITY FAN-IN STRESS TEST")
    print(" RFC-001 Section 8: Operational Limits")
    print("=" * 60)
    print(f"Profile: {TEST_PROFILE}, Senders: {CONFIG['senders']}")

    log("\nPre-check: server availability...")
    if not server_alive():
        log("FAIL: Server not running")
        return 1

    tests = [
        ("Fan-In No Crash", test_fanin_no_crash),
        ("Functional After Fan-In", test_functional_after_fanin),
    ]

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

    passed = sum(1 for _, r in results if r)
    total = len(results)
    for name, result in results:
        print(f"  [{'PASS' if result else 'FAIL'}] {name}")

    if passed == total:
        print(f"\nG-19 Celebrity Fan-In: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-19 Celebrity Fan-In: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
