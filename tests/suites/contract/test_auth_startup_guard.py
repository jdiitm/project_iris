#!/usr/bin/env python3
"""
Contract Tests: Auth Startup Guard (Ephemeral Key Rejection)

Validates the auth startup invariant from the forensic audit finding 2.1a:

    When auth_enabled=true, iris_auth MUST refuse to start with ephemeral
    EdDSA keys. Ephemeral keys cause a thundering herd on restart because
    ALL tokens signed by the old ephemeral key become invalid.

This test reads the Erlang source directly (Tier 0 -- no running server needed).

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
    """Read file contents as string."""
    with open(path, 'r') as f:
        return f.read()


# =============================================================================
# Test 1: iris_auth.erl init/1 must guard against ephemeral keys + auth_enabled
# =============================================================================

def test_auth_init_guards_ephemeral_key():
    """
    Contract: iris_auth:init/1 MUST check auth_enabled before allowing
    ephemeral EdDSA keys. If auth_enabled=true and jwt_eddsa_private_key
    is not configured, init/1 MUST return {stop, ...} to prevent startup.

    This prevents the thundering herd scenario where a node restart
    invalidates all previously-issued tokens.
    """
    log("\n=== Test: Auth Init Guards Ephemeral Key ===")
    log("  Audit finding 2.1a: Thundering Herd on Restart")

    auth_erl = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")
    source = read_file(auth_erl)

    # The init function must contain a check for auth_enabled in the context
    # of ephemeral key generation. We look for the pattern where:
    # 1. auth_enabled is checked
    # 2. jwt_eddsa_private_key absence is detected
    # 3. A {stop, ...} or error() is returned

    # Check that init/1 references auth_enabled near the ephemeral key logic
    has_auth_enabled_check = bool(re.search(
        r'auth_enabled.*ephemeral|ephemeral.*auth_enabled|'
        r'auth_enabled.*jwt_eddsa_private_key.*stop|'
        r'auth_enabled.*misconfiguration',
        source, re.DOTALL
    ))

    check(
        "init/1 checks auth_enabled before allowing ephemeral keys",
        has_auth_enabled_check,
        "iris_auth:init/1 must reject ephemeral keys when auth_enabled=true. "
        "Expected pattern: check auth_enabled -> check jwt_eddsa_private_key -> {stop, ...}"
    )

    # Also verify the stop/error return exists for this case
    has_stop_for_misconfiguration = bool(re.search(
        r'\{stop\s*,\s*\{misconfiguration\s*,\s*ephemeral_key_with_auth_enabled\}',
        source
    ))

    check(
        "init/1 returns {stop, {misconfiguration, ephemeral_key_with_auth_enabled}}",
        has_stop_for_misconfiguration,
        "Must return {stop, {misconfiguration, ephemeral_key_with_auth_enabled}} "
        "to prevent startup with ephemeral keys in production"
    )


# =============================================================================
# Test 2: Ephemeral key generation still works when auth_enabled=false
# =============================================================================

def test_ephemeral_key_allowed_when_auth_disabled():
    """
    Contract: When auth_enabled=false (testing/development), ephemeral
    EdDSA keys MUST still be generated. The guard only applies to
    production (auth_enabled=true).
    """
    log("\n=== Test: Ephemeral Key Allowed When Auth Disabled ===")

    auth_erl = os.path.join(PROJECT_ROOT, "src", "iris_auth.erl")
    source = read_file(auth_erl)

    # The ephemeral key generation path must still exist
    has_ephemeral_generation = bool(re.search(
        r'crypto:generate_key\(eddsa\s*,\s*ed25519\)',
        source
    ))

    check(
        "Ephemeral key generation path still exists",
        has_ephemeral_generation,
        "crypto:generate_key(eddsa, ed25519) must remain for testing mode"
    )

    # The log message for ephemeral generation must still exist
    has_ephemeral_log = bool(re.search(
        r'Generated ephemeral EdDSA key pair',
        source
    ))

    check(
        "Ephemeral key info log still present",
        has_ephemeral_log,
        "Logger info for ephemeral key generation must remain"
    )


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("Contract Tests: Auth Startup Guard")
    log("Audit Finding 2.1a: Ephemeral Key Thundering Herd")
    log("=" * 60)

    test_auth_init_guards_ephemeral_key()
    test_ephemeral_key_allowed_when_auth_disabled()

    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    log(f"\nTotal: {passed + failed}")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")

    if failed == 0:
        log("\n✓ All auth startup guard contract tests passed!")
        return 0
    else:
        log(f"\n✗ {failed} contract test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
