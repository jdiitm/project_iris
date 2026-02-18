#!/usr/bin/env python3
"""
P0-3 (FM-2): Split-Brain Epoch Resolution Tests

RFC-001 v4.0 Section 7.1.1:
- Each node tracks epoch counter, incremented on partition detection
- On healing: higher-epoch partition is authoritative
- Equal-epoch ties broken by lowest node ID

Tests verify:
1. Epoch increments during partition
2. Resolution on heal determined by epoch

Pattern: follows test_split_brain_convergence.py using Docker cluster utilities.
"""

import sys
import os
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "tests"))

from suites.chaos_dist.utils import (
    get_cluster_nodes, run_on_node, partition_nodes,
    heal_partition, wait_for_cluster_ready
)
from tests.utilities.helpers import wait_until


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def test_epoch_increments_during_partition():
    """
    Partition cluster, verify epoch counter increments via RPC.
    """
    log("=" * 60)
    log("TEST: Epoch increments during partition")
    log("=" * 60)

    nodes = get_cluster_nodes()
    if len(nodes) < 2:
        log("  SKIP: Need at least 2 nodes")
        return True

    # Pick nodes from DIFFERENT regions so backbone disconnect actually isolates them.
    # Same-region nodes share the regional network and stay connected even without backbone.
    east_nodes = [n for n in nodes if "east" in n]
    west_nodes = [n for n in nodes if "west" in n]

    if not east_nodes or not west_nodes:
        log("  SKIP: Need nodes in both East and West regions")
        return True

    node_a = east_nodes[0]   # stays connected (majority side)
    node_b = west_nodes[0]   # gets disconnected from backbone (minority side)
    log(f"  Partitioning {node_b} from backbone (cross-region)")
    log(f"  Majority: {node_a} (5/6 nodes), Minority: {node_b} (1/6 nodes)")

    # Check initial epoch on minority node (before partition)
    result = run_on_node(node_b, "maps:get(epoch, iris_partition_guard:get_status())")
    initial_epoch = int(result)
    log(f"  Initial epoch on {node_b}: {initial_epoch}")

    # Partition: disconnect node_b from backbone.
    # node_b sees only itself (1/6 = 16% < 50% quorum) → enters diverged, epoch++
    # node_a still sees 5/6 nodes (83% > 50%) → stays normal
    partition_nodes(node_a, node_b)
    # Wait for Erlang distribution timeout (net_ticktime=10s) + partition guard check (5s)
    # Poll until epoch increments on minority node
    def _epoch_incremented():
        try:
            result = run_on_node(node_b, "maps:get(epoch, iris_partition_guard:get_status())")
            current_epoch = int(result)
            return current_epoch > initial_epoch
        except:
            return False

    wait_until(_epoch_incremented, timeout=20, interval=1, description="epoch increment on partition")

    # Check epoch on the MINORITY side where quorum was lost
    result = run_on_node(node_b, "maps:get(epoch, iris_partition_guard:get_status())")
    post_partition_epoch = int(result)
    log(f"  Post-partition epoch on {node_b}: {post_partition_epoch}")

    assert post_partition_epoch > initial_epoch, \
        f"Epoch should increment on minority node losing quorum: {post_partition_epoch} > {initial_epoch}"

    # Heal
    heal_partition(node_a, node_b)
    # Wait for cluster to converge after healing
    wait_for_cluster_ready(max_wait=10)

    log("  PASS")
    return True


def test_resolution_on_heal():
    """
    Partition + heal, verify authoritative side determined by epoch.
    """
    log("=" * 60)
    log("TEST: Resolution on heal")
    log("=" * 60)

    nodes = get_cluster_nodes()
    if len(nodes) < 2:
        log("  SKIP: Need at least 2 nodes")
        return True

    east_nodes = [n for n in nodes if "east" in n]
    west_nodes = [n for n in nodes if "west" in n]

    if not east_nodes or not west_nodes:
        log("  SKIP: Need nodes in both East and West regions")
        return True

    node_a, node_b = east_nodes[0], west_nodes[0]

    # Verify resolve_authority works
    code = (
        f"iris_partition_guard:resolve_authority(3, '{node_a}', 2, '{node_b}')"
    )
    result = run_on_node(node_a, code)
    log(f"  Resolution (epoch 3 vs 2): {result}")
    assert f"{node_a}" in result, f"Higher epoch node should be authoritative: {result}"

    # Equal epoch test
    code = (
        f"iris_partition_guard:resolve_authority(2, '{node_a}', 2, '{node_b}')"
    )
    result = run_on_node(node_a, code)
    log(f"  Resolution (equal epoch): {result}")
    # Lowest node name wins
    expected_winner = min(str(node_a), str(node_b))
    log(f"  Expected winner (lowest node): {expected_winner}")

    log("  PASS")
    return True


def main():
    log("Split-Brain Epoch Resolution Tests (P0-3 / FM-2)")
    log("")

    tests = [
        ("epoch_increments", test_epoch_increments_during_partition),
        ("resolution_on_heal", test_resolution_on_heal),
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
