#!/usr/bin/env python3
"""
Contract Tests: Routing Ordering Semantics

Validates audit finding 2.2: iris_async_router provides TWO routing paths
with DIFFERENT ordering guarantees. This test serves as an executable
specification of that contract.

1. UNSEQUENCED ({route, User, Msg, MsgId}):
   - route_to_remote/4 spawns a separate process per message
   - Delivery is guaranteed but ORDER IS NOT

2. SEQUENCED ({route_sequenced, User, Msg, SeqNo}):
   - route_sequenced_remote/4 spawns to avoid HOL blocking during partitions
   - FIFO ordering IS guaranteed via SeqNo in stored records + retrieval sort

The module header MUST document this contract explicitly.

Tier: 0 (Required on every merge)
"""

import sys
import os
import re

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

passed = 0
failed = 0


def log(msg):
    import time
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


def read_file(path):
    with open(path, 'r') as f:
        return f.read()


# =============================================================================
# Test 1: Module header documents the ordering contract
# =============================================================================

def test_ordering_contract_documented():
    """
    Contract: iris_async_router.erl MUST have a module-level comment
    documenting the dual-path ordering semantics.
    """
    log("\n=== Test: Ordering Contract Documented ===")
    log("  Audit finding 2.2: Async Routing Reordering")

    router_erl = os.path.join(PROJECT_ROOT, "src", "iris_async_router.erl")
    source = read_file(router_erl)

    # Must document the ORDERING CONTRACT
    has_ordering_contract = bool(re.search(
        r'ORDERING CONTRACT',
        source
    ))

    check(
        "Module header contains ORDERING CONTRACT section",
        has_ordering_contract,
        "Must explicitly document the dual-path ordering guarantees"
    )

    # Must mention UNSEQUENCED path and its spawn behavior
    has_unsequenced_doc = bool(re.search(
        r'UNSEQUENCED.*spawn|unsequenced.*ORDER IS NOT',
        source, re.DOTALL | re.IGNORECASE
    ))

    check(
        "Documents UNSEQUENCED path (no order guarantee)",
        has_unsequenced_doc,
        "Must document that route_to_remote spawns per-message, so order is not guaranteed"
    )

    # Must mention SEQUENCED path and its FIFO behavior
    has_sequenced_doc = bool(re.search(
        r'SEQUENCED.*FIFO|SEQUENCED.*ordering.*guaranteed|sequenced.*SeqNo',
        source, re.DOTALL | re.IGNORECASE
    ))

    check(
        "Documents SEQUENCED path (FIFO guaranteed)",
        has_sequenced_doc,
        "Must document that route_sequenced_remote preserves FIFO via SeqNo"
    )


# =============================================================================
# Test 2: Code structure matches the contract
# =============================================================================

def extract_erlang_function(source, func_name):
    """Extract an Erlang function body (handles comments with periods)."""
    lines = source.split('\n')
    collecting = False
    body_lines = []
    for line in lines:
        if re.match(rf'^{re.escape(func_name)}\(', line):
            collecting = True
        if collecting:
            body_lines.append(line)
            stripped = line.strip()
            # End of function: non-comment line ending with '.'
            if stripped.endswith('.') and not stripped.startswith('%'):
                break
    return '\n'.join(body_lines)


def test_code_matches_contract():
    """
    Verify that the code structure matches the documented contract:
    - route_to_remote uses spawn (unsequenced, no order)
    - route_sequenced_remote spawns to avoid HOL blocking; FIFO via SeqNo
    """
    log("\n=== Test: Code Structure Matches Contract ===")

    router_erl = os.path.join(PROJECT_ROOT, "src", "iris_async_router.erl")
    source = read_file(router_erl)

    # Extract and check route_to_remote
    route_to_remote_src = extract_erlang_function(source, "route_to_remote")
    has_spawn_in_unsequenced = bool(re.search(r'spawn\(', route_to_remote_src))
    check(
        "route_to_remote uses spawn (unsequenced path)",
        has_spawn_in_unsequenced,
        "Unsequenced path must spawn to avoid blocking the GenServer"
    )

    # Extract and check route_sequenced_remote spawns to avoid HOL blocking.
    # FIFO ordering is maintained by SeqNo in stored records + retrieval sort,
    # not by inline processing. This was changed to prevent GenServer blocking
    # during network partitions (5s RPC timeout * N messages = massive backlog).
    route_sequenced_src = extract_erlang_function(source, "route_sequenced_remote")
    has_spawn_in_sequenced = bool(re.search(r'spawn\(', route_sequenced_src))
    check(
        "route_sequenced_remote uses spawn (avoids HOL blocking)",
        has_spawn_in_sequenced,
        "Sequenced path must spawn; FIFO preserved via SeqNo in stored records"
    )


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Contract Tests: Routing Ordering Semantics")
    log("Audit Finding 2.2: Async Routing Reordering")
    log("=" * 60)

    test_ordering_contract_documented()
    test_code_matches_contract()

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    log(f"\nTotal: {passed + failed}")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed == 0:
        log("\n✓ All routing ordering contract tests passed!")
        return 0
    else:
        log(f"\n✗ {failed} contract test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
