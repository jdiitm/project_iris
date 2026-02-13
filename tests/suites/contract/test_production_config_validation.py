#!/usr/bin/env python3
"""
AUDIT V2 P0-1: Production Config Validation Contract Tests

Contract tests verifying that scripts/validate_production_config.escript
correctly rejects unsafe production configurations and accepts valid ones.

Tests verify:
1. Rejects deployment_mode = development
2. Rejects empty expected_cluster_nodes
3. Rejects empty join_seeds
4. Rejects empty core_nodes
5. Rejects placeholder jwt_secret
6. Accepts a fully valid production config

Tier: 0 (Contract — no running server needed)
"""

import sys
import os
import subprocess
import tempfile
import time

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

ESCRIPT_PATH = os.path.join(PROJECT_ROOT, "scripts", "validate_production_config.escript")

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


def write_config(content):
    """Write an Erlang config to a temp file and return its path."""
    fd, path = tempfile.mkstemp(suffix=".config")
    with os.fdopen(fd, 'w') as f:
        f.write(content)
    return path


def run_validator(config_path):
    """Run the validation escript against a config file. Returns (stdout, stderr, exit_code)."""
    try:
        result = subprocess.run(
            ["escript", ESCRIPT_PATH, config_path],
            capture_output=True, text=True, timeout=15,
            cwd=PROJECT_ROOT
        )
        return result.stdout.strip(), result.stderr.strip(), result.returncode
    except FileNotFoundError:
        return "", "escript not found", 127
    except subprocess.TimeoutExpired:
        return "", "timeout", 124


# =============================================================================
# Valid production config template (all fields correctly populated)
# =============================================================================

VALID_PRODUCTION_CONFIG = """[
 {iris_core, [
    {deployment_mode, production},
    {auto_init_db, true},
    {join_seeds, ['iris_core@seed1', 'iris_core@seed2']},
    {presence_backend, ets},
    {region_id, <<"us-east">>},
    {consistency_mode, hardened_ap},
    {expected_cluster_nodes, ['iris_core@host1', 'iris_core@host2', 'iris_core@host3']},
    {enforce_mtls, true},
    {health_port, 9090}
 ]},
 {iris_edge, [
    {deployment_mode, production},
    {port, 8085},
    {auth_enabled, true},
    {auth_mode, signer},
    {tls_enabled, true},
    {allow_insecure, false},
    {core_nodes, ['iris_core@core1', 'iris_core@core2']},
    {health_port, 9090}
 ]},
 {kernel, [
    {logger_level, notice}
 ]}
].
"""


# =============================================================================
# Tests
# =============================================================================

def test_escript_exists():
    """Validation escript must exist at scripts/validate_production_config.escript."""
    log("\n=== Test: Escript exists ===")
    exists = os.path.isfile(ESCRIPT_PATH)
    check("escript exists at scripts/validate_production_config.escript", exists,
          f"Expected file at {ESCRIPT_PATH}")
    return exists


def test_rejects_development_mode():
    """Config with deployment_mode=development must be rejected."""
    log("\n=== Test: Rejects development mode ===")
    config = VALID_PRODUCTION_CONFIG.replace(
        "{deployment_mode, production}",
        "{deployment_mode, development}",
    )
    path = write_config(config)
    try:
        stdout, stderr, rc = run_validator(path)
        check("rejects deployment_mode=development", rc != 0,
              f"exit_code={rc}, stdout={stdout}")
        check("mentions deployment_mode in error",
              "deployment_mode" in stdout or "deployment_mode" in stderr,
              f"stdout={stdout}, stderr={stderr}")
    finally:
        os.unlink(path)


def test_rejects_empty_cluster_nodes():
    """Config with empty expected_cluster_nodes must be rejected."""
    log("\n=== Test: Rejects empty expected_cluster_nodes ===")
    config = VALID_PRODUCTION_CONFIG.replace(
        "{expected_cluster_nodes, ['iris_core@host1', 'iris_core@host2', 'iris_core@host3']}",
        "{expected_cluster_nodes, []}",
    )
    path = write_config(config)
    try:
        stdout, stderr, rc = run_validator(path)
        check("rejects empty expected_cluster_nodes", rc != 0,
              f"exit_code={rc}, stdout={stdout}")
    finally:
        os.unlink(path)


def test_rejects_empty_join_seeds():
    """Config with empty join_seeds must be rejected."""
    log("\n=== Test: Rejects empty join_seeds ===")
    config = VALID_PRODUCTION_CONFIG.replace(
        "{join_seeds, ['iris_core@seed1', 'iris_core@seed2']}",
        "{join_seeds, []}",
    )
    path = write_config(config)
    try:
        stdout, stderr, rc = run_validator(path)
        check("rejects empty join_seeds", rc != 0,
              f"exit_code={rc}, stdout={stdout}")
    finally:
        os.unlink(path)


def test_rejects_empty_core_nodes():
    """Config with empty core_nodes must be rejected."""
    log("\n=== Test: Rejects empty core_nodes ===")
    config = VALID_PRODUCTION_CONFIG.replace(
        "{core_nodes, ['iris_core@core1', 'iris_core@core2']}",
        "{core_nodes, []}",
    )
    path = write_config(config)
    try:
        stdout, stderr, rc = run_validator(path)
        check("rejects empty core_nodes", rc != 0,
              f"exit_code={rc}, stdout={stdout}")
    finally:
        os.unlink(path)


def test_rejects_placeholder_jwt():
    """Config with placeholder jwt_secret must be rejected."""
    log("\n=== Test: Rejects placeholder jwt_secret ===")
    # Add the placeholder jwt_secret to edge config
    config = VALID_PRODUCTION_CONFIG.replace(
        "{auth_mode, signer}",
        '{auth_mode, signer},\n    {jwt_secret, <<"REPLACE_WITH_32_BYTE_SECRET_KEY!!">>}',
    )
    path = write_config(config)
    try:
        stdout, stderr, rc = run_validator(path)
        check("rejects placeholder jwt_secret", rc != 0,
              f"exit_code={rc}, stdout={stdout}")
    finally:
        os.unlink(path)


def test_accepts_valid_production_config():
    """A fully valid production config must be accepted."""
    log("\n=== Test: Accepts valid production config ===")
    path = write_config(VALID_PRODUCTION_CONFIG)
    try:
        stdout, stderr, rc = run_validator(path)
        check("accepts valid production config", rc == 0,
              f"exit_code={rc}, stdout={stdout}, stderr={stderr}")
    finally:
        os.unlink(path)


def test_rejects_current_shipped_config():
    """The actual production.config as shipped must be rejected (it's in dev mode)."""
    log("\n=== Test: Rejects current shipped config ===")
    shipped_config = os.path.join(PROJECT_ROOT, "config", "production.config")
    stdout, stderr, rc = run_validator(shipped_config)
    check("rejects shipped production.config (dev mode)", rc != 0,
          f"exit_code={rc}, stdout={stdout}")


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    log("=" * 60)
    log("AUDIT V2 P0-1: Production Config Validation Tests")
    log("=" * 60)

    # Gate: escript must exist first
    if not test_escript_exists():
        log("\nESCRIPT NOT FOUND — all remaining tests will fail")

    test_rejects_development_mode()
    test_rejects_empty_cluster_nodes()
    test_rejects_empty_join_seeds()
    test_rejects_empty_core_nodes()
    test_rejects_placeholder_jwt()
    test_accepts_valid_production_config()
    test_rejects_current_shipped_config()

    log("\n" + "=" * 60)
    log(f"Results: {passed} passed, {failed} failed")
    log("=" * 60)

    sys.exit(0 if failed == 0 else 1)
