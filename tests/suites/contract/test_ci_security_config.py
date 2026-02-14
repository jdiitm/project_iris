#!/usr/bin/env python3
"""
B-3 AUDIT MITIGATION: CI Security Configuration Contract Test

Validates that production-mode configurations enforce TLS and auth.
This test is a release gate -- it fails if security features can be
bypassed in production deployment mode.

Tier: 0 (Required on every merge)
"""

import os
import sys
import re

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))


def log(msg):
    print(f"[CI-SEC] {msg}", flush=True)


def read_config(path):
    """Read an Erlang config file and return its content."""
    full_path = os.path.join(PROJECT_ROOT, path)
    if not os.path.exists(full_path):
        return None
    with open(full_path, 'r') as f:
        return f.read()


def test_production_config_enforces_tls():
    """Production config MUST have tls_enabled = true."""
    log("=" * 60)
    log("TEST: Production config enforces TLS (RFC NFR-14)")
    log("=" * 60)

    content = read_config('config/production.config')
    if content is None:
        log("FAIL: config/production.config not found")
        return False

    # Check tls_enabled is true
    if re.search(r'\{tls_enabled,\s*true\}', content):
        log("PASS: tls_enabled = true in production.config")
        return True
    else:
        log("FAIL: tls_enabled is NOT true in production.config")
        log("  RFC NFR-14: TLS MUST be mandatory for all client connections")
        return False


def test_production_config_enforces_auth():
    """Production config MUST have auth_enabled = true."""
    log("=" * 60)
    log("TEST: Production config enforces auth (RFC NFR-16)")
    log("=" * 60)

    content = read_config('config/production.config')
    if content is None:
        log("FAIL: config/production.config not found")
        return False

    if re.search(r'\{auth_enabled,\s*true\}', content):
        log("PASS: auth_enabled = true in production.config")
        return True
    else:
        log("FAIL: auth_enabled is NOT true in production.config")
        log("  RFC FR-9/NFR-16: JWT validation MUST be enabled")
        return False


def test_production_config_enforces_production_mode():
    """Production config MUST have deployment_mode = production."""
    log("=" * 60)
    log("TEST: Production config sets deployment_mode = production")
    log("=" * 60)

    content = read_config('config/production.config')
    if content is None:
        log("FAIL: config/production.config not found")
        return False

    if re.search(r'\{deployment_mode,\s*production\}', content):
        log("PASS: deployment_mode = production in production.config")
        return True
    else:
        log("FAIL: deployment_mode is NOT production in production.config")
        return False


def test_production_config_disallows_insecure():
    """Production config MUST have allow_insecure = false."""
    log("=" * 60)
    log("TEST: Production config disallows insecure (allow_insecure=false)")
    log("=" * 60)

    content = read_config('config/production.config')
    if content is None:
        log("FAIL: config/production.config not found")
        return False

    if re.search(r'\{allow_insecure,\s*false\}', content):
        log("PASS: allow_insecure = false in production.config")
        return True
    else:
        log("FAIL: allow_insecure is NOT false in production.config")
        return False


def test_secure_test_config_exists():
    """A secure test config MUST exist for CI security validation."""
    log("=" * 60)
    log("TEST: Secure test config exists (config/test_secure.config)")
    log("=" * 60)

    path = os.path.join(PROJECT_ROOT, 'config', 'test_secure.config')
    if os.path.exists(path):
        content = read_config('config/test_secure.config')
        has_auth = re.search(r'\{auth_enabled,\s*true\}', content) is not None
        has_tls = re.search(r'\{tls_enabled,\s*true\}', content) is not None

        if has_auth and has_tls:
            log("PASS: test_secure.config has auth_enabled=true and tls_enabled=true")
            return True
        else:
            log(f"FAIL: test_secure.config missing auth_enabled=true ({has_auth}) or tls_enabled=true ({has_tls})")
            return False
    else:
        log("FAIL: config/test_secure.config does not exist")
        log("  Create it with auth_enabled=true, tls_enabled=true for CI security tier")
        return False


if __name__ == "__main__":
    results = []

    results.append(("Production enforces TLS", test_production_config_enforces_tls()))
    results.append(("Production enforces auth", test_production_config_enforces_auth()))
    results.append(("Production mode set", test_production_config_enforces_production_mode()))
    results.append(("Production disallows insecure", test_production_config_disallows_insecure()))
    results.append(("Secure test config exists", test_secure_test_config_exists()))

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)

    for name, result in results:
        status = "PASS" if result else "FAIL"
        log(f"  [{status}] {name}")

    log(f"\n{passed}/{total} tests passed")

    sys.exit(0 if passed == total else 1)
