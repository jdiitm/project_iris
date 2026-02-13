#!/usr/bin/env python3
"""
AUDIT MITIGATION P0-2: Production Config Safety Contract Tests

Validates that production.config does not ship with dangerous defaults.

Tier: 0 (Contract — no running server needed)
"""
import sys
import os
import time

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


def read_production_config():
    """Read and return the raw production config content."""
    config_path = os.path.join(PROJECT_ROOT, 'config', 'production.config')
    with open(config_path, 'r') as f:
        return f.read()


def test_production_config_deployment_mode_is_production():
    """deployment_mode must be 'production', not 'development'."""
    log("\n=== Test: deployment_mode is production ===")
    content = read_production_config()
    check("iris_core deployment_mode is production",
          '{deployment_mode, production}' in content,
          "Expected deployment_mode = production in iris_core section")


def test_production_config_has_expected_cluster_nodes():
    """expected_cluster_nodes must not be empty."""
    log("\n=== Test: expected_cluster_nodes is non-empty ===")
    content = read_production_config()
    check("expected_cluster_nodes is non-empty",
          '{expected_cluster_nodes, []}' not in content,
          "expected_cluster_nodes must not be empty")


def test_production_config_has_core_nodes():
    """core_nodes must not be empty."""
    log("\n=== Test: core_nodes is non-empty ===")
    content = read_production_config()
    check("core_nodes is non-empty",
          '{core_nodes, []}' not in content,
          "core_nodes must not be empty")


if __name__ == '__main__':
    log("=" * 60)
    log("AUDIT MITIGATION P0-2: Production Config Safety Tests")
    log("=" * 60)

    test_production_config_deployment_mode_is_production()
    test_production_config_has_expected_cluster_nodes()
    test_production_config_has_core_nodes()

    log(f"\nResults: {passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
