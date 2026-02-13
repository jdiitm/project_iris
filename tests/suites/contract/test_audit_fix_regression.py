#!/usr/bin/env python3
"""
AUDIT MITIGATION P2-1: Audit Fix Regression Tests

Canary tests that verify critical audit fixes have not been accidentally
reverted during refactoring. Each test greps the source for the specific
pattern that constitutes the fix.

Tier: 0 (Contract — no running server needed)
"""
import sys
import os
import time
import re

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

passed = 0
failed = 0


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def check(name, condition, detail=""):
    global passed, failed
    if condition:
        log(f"  PASS: {name}")
        passed += 1
    else:
        log(f"  FAIL: {name}")
        if detail:
            log(f"        {detail}")
        failed += 1


def read_source(relative_path):
    """Read a source file from the project root."""
    full_path = os.path.join(PROJECT_ROOT, relative_path)
    with open(full_path, 'r') as f:
        return f.read()


# =============================================================================
# Audit Fix Regression Tests
# =============================================================================

def test_audit_fix_comments_present():
    """Critical AUDIT FIX comments must still exist in source files."""
    log("\n=== Test: Critical audit fix comments present ===")

    checks = [
        ("src/iris_core.erl", "F1 AUDIT FIX", "RFC 7.1.1 union merge"),
        ("src/iris_core.erl", "GAP-2 FIX", "generic table reconciliation"),
        ("src/iris_core.erl", "AUDIT 6.5", "dedup key generation"),
        ("src/iris_session.erl", "AUDIT 2.3a FIX", "graceful heap limit"),
        ("src/iris_session.erl", "VIOLATION-4 FIX", "rate limit on send"),
        ("src/iris_async_router.erl", "AUDIT FIX: Silent Loss Prevention", "offline fallback"),
        ("src/iris_partition_guard.erl", "CB-1 FIX", "static membership"),
    ]

    for filepath, pattern, description in checks:
        source = read_source(filepath)
        check(f"{os.path.basename(filepath)}: {description}",
              pattern in source,
              f"Pattern '{pattern}' not found in {filepath}")


def test_core_reconcile_uses_transaction():
    """AUDIT P1-1: reconcile_batch must use sync_transaction for durability."""
    log("\n=== Test: reconcile_batch uses sync_transaction ===")
    source = read_source("src/iris_core.erl")
    # The reconciliation code must use sync_transaction
    check("reconcile uses sync_transaction",
          "sync_transaction" in source,
          "sync_transaction not found in iris_core.erl")


def test_session_heap_limit_not_kill():
    """AUDIT 2.3a: Session must use {kill, false} for graceful heap limit."""
    log("\n=== Test: Session heap limit uses kill => false ===")
    source = read_source("src/iris_session.erl")
    check("heap limit uses kill => false",
          "kill => false" in source,
          "kill => false not found in iris_session.erl")


def test_dedup_key_uses_strong_hash():
    """AUDIT 6.5: make_dedup_key must use crypto:hash, NOT phash2."""
    log("\n=== Test: Dedup key uses strong hash ===")
    source = read_source("src/iris_core.erl")
    # Find the make_dedup_key function spec (the actual implementation)
    idx = source.find("-spec make_dedup_key(")
    if idx < 0:
        idx = source.find("make_dedup_key(User,")
    assert idx > 0, "make_dedup_key function not found"
    # Get the function body (next 500 chars from spec)
    func_body = source[idx:idx+500]
    check("make_dedup_key uses crypto:hash",
          "crypto:hash" in func_body,
          "crypto:hash not found in make_dedup_key")
    check("make_dedup_key does NOT use phash2",
          "phash2" not in func_body,
          "phash2 found in make_dedup_key -- should use crypto:hash")


def test_force_load_checks_peers():
    """AUDIT V2 P1-3: repair_failed_tables must check active_replicas before force_load."""
    log("\n=== Test: force_load checks active_replicas ===")
    source = read_source("src/iris_core.erl")
    check("iris_core checks active_replicas before force_load",
          "active_replicas" in source,
          "active_replicas not found in iris_core.erl")


def test_group_force_load_checks_peers():
    """AUDIT MITIGATION P0-2: iris_group must check active_replicas before force_load."""
    log("\n=== Test: iris_group checks active_replicas ===")
    source = read_source("src/iris_group.erl")
    check("iris_group checks active_replicas before force_load",
          "active_replicas" in source,
          "active_replicas not found in iris_group.erl (P0-2 fix missing)")


def test_reconcile_checks_partition_guard():
    """AUDIT MITIGATION P0-2: reconcile_after_partition must check partition guard mode."""
    log("\n=== Test: reconcile checks partition guard ===")
    source = read_source("src/iris_core.erl")
    check("reconcile_after_partition checks partition guard",
          "iris_partition_guard:get_status" in source,
          "iris_partition_guard:get_status not found in reconcile path")


def test_per_type_rate_limiting_exists():
    """AUDIT MITIGATION P1-1: Per-message-type rate limiting must exist."""
    log("\n=== Test: Per-type rate limiting ===")
    source = read_source("src/iris_rate_limiter.erl")
    check("check_typed/2 exists in iris_rate_limiter",
          "check_typed" in source,
          "check_typed not found in iris_rate_limiter.erl")


# =============================================================================
# Main
# =============================================================================

if __name__ == '__main__':
    log("=" * 60)
    log("AUDIT MITIGATION P2-1: Audit Fix Regression Tests")
    log("=" * 60)

    test_audit_fix_comments_present()
    test_core_reconcile_uses_transaction()
    test_session_heap_limit_not_kill()
    test_dedup_key_uses_strong_hash()
    test_force_load_checks_peers()
    test_group_force_load_checks_peers()
    test_reconcile_checks_partition_guard()
    test_per_type_rate_limiting_exists()

    log(f"\nResults: {passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
