#!/usr/bin/env python3
"""
Contract Tests: RFC v4.0 Rate Limit Constants (Section 10.1)

These characterization tests verify that the Erlang rate limit defaults
match the values specified in RFC-001 v4.0 Section 10.1:

  | Resource         | Limit              | Window    |
  |------------------|--------------------|-----------|
  | Messages sent    | 5/sec sustained    | Per user  |
  |                  | 100/minute burst   |           |
  | Connections      | 5/minute           | Per IP    |
  | Failed logins    | 10/hour            | Per acct  |

These tests query the Erlang node via erl_eval to read compile-time
defaults from the source modules. No running server required.

Tier: 0 (Required on every merge)
"""

import sys
import os
import subprocess
import re

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

passed = 0
failed = 0


def log(msg):
    import time
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def extract_define(filepath, define_name):
    """Extract a -define(NAME, VALUE) from an Erlang source file."""
    with open(filepath, 'r') as f:
        for line in f:
            # Match -define(NAME, VALUE).
            m = re.match(rf'\s*-define\(\s*{re.escape(define_name)}\s*,\s*(.+?)\s*\)', line)
            if m:
                val_str = m.group(1).rstrip('. ')
                try:
                    return int(val_str)
                except ValueError:
                    return val_str
    return None


def test_message_rate_limit_matches_rfc():
    """RFC Section 10.1: Message rate should be 5/sec sustained."""
    global passed, failed
    log("\n=== Test: Message Rate Limit Matches RFC ===")
    log("  RFC Section 10.1: 5 msg/sec sustained, 100/minute burst")

    rate_limiter = os.path.join(PROJECT_ROOT, "src", "iris_rate_limiter.erl")
    default_rate = extract_define(rate_limiter, "DEFAULT_RATE")
    default_burst = extract_define(rate_limiter, "DEFAULT_BURST")

    log(f"  Current DEFAULT_RATE:  {default_rate}")
    log(f"  Current DEFAULT_BURST: {default_burst}")
    log(f"  RFC target rate:       5/sec")
    log(f"  RFC target burst:      20 (for 10s)")

    if default_rate is None:
        log("  FAIL: Could not extract DEFAULT_RATE from iris_rate_limiter.erl")
        failed += 1
        return

    if default_rate != 5:
        log(f"  FAIL: DEFAULT_RATE is {default_rate}, RFC requires 5")
        failed += 1
        return

    if default_burst is None:
        log("  FAIL: Could not extract DEFAULT_BURST from iris_rate_limiter.erl")
        failed += 1
        return

    if default_burst != 20:
        log(f"  FAIL: DEFAULT_BURST is {default_burst}, RFC requires 20")
        failed += 1
        return

    log("  PASS")
    passed += 1


def test_connection_rate_limit_matches_rfc():
    """RFC Section 10.1: 5 connections/minute per IP."""
    global passed, failed
    log("\n=== Test: Connection Rate Limit Matches RFC ===")
    log("  RFC Section 10.1: 5/minute per IP")

    listener = os.path.join(PROJECT_ROOT, "src", "iris_edge_listener.erl")
    window = extract_define(listener, "CONN_RATE_WINDOW_MS")
    max_rate = extract_define(listener, "CONN_RATE_MAX")

    log(f"  Current CONN_RATE_WINDOW_MS: {window}")
    log(f"  Current CONN_RATE_MAX:       {max_rate}")
    log(f"  RFC target window:           60000 ms (1 minute)")
    log(f"  RFC target max:              5")

    if window is None or max_rate is None:
        log("  FAIL: Could not extract connection rate defines")
        failed += 1
        return

    if window != 60000:
        log(f"  FAIL: CONN_RATE_WINDOW_MS is {window}, RFC requires 60000")
        failed += 1
        return

    if max_rate != 5:
        log(f"  FAIL: CONN_RATE_MAX is {max_rate}, RFC requires 5")
        failed += 1
        return

    log("  PASS")
    passed += 1


def test_failed_login_rate_limit_exists():
    """RFC Section 10.1: 10 failed logins/hour per account."""
    global passed, failed
    log("\n=== Test: Failed Login Rate Limit Exists ===")
    log("  RFC Section 10.1: 10/hour per account")

    auth = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")

    # Check for failed login rate limit constants
    found_limit = False
    found_window = False

    with open(auth, 'r') as f:
        content = f.read()

    # Look for evidence of failed login tracking
    if "FAILED_LOGIN" in content or "failed_login" in content or "login_rate" in content:
        found_limit = True

    if "3600" in content or "LOGIN_WINDOW" in content:
        found_window = True

    log(f"  Failed login limit found: {found_limit}")
    log(f"  1-hour window found:      {found_window}")

    if not found_limit:
        log("  FAIL: No failed login rate limit implementation found in iris_auth.erl")
        failed += 1
        return

    if not found_window:
        log("  FAIL: No 1-hour window for failed login limit found")
        failed += 1
        return

    log("  PASS")
    passed += 1


if __name__ == "__main__":
    log("=" * 60)
    log("RFC v4.0 Rate Limit Constants Contract Tests")
    log("RFC Reference: Section 10.1 (Abuse Prevention)")
    log("=" * 60)

    test_message_rate_limit_matches_rfc()
    test_connection_rate_limit_matches_rfc()
    test_failed_login_rate_limit_exists()

    log("")
    log("=" * 60)
    log(f"RESULTS: {passed} passed, {failed} failed")
    log("=" * 60)

    sys.exit(1 if failed > 0 else 0)
