#!/usr/bin/env python3
"""
P1-7: UUIDv7 Validation for Idempotency Keys

RFC-001 v4.0 Section 5.2 specifies UUIDv7 (RFC 9562) for idempotency keys:
- Version nibble MUST be 7 (bits 48-51)
- Variant bits MUST be 10 (bits 64-65)
- Timestamp in milliseconds since epoch in the high 48 bits

This is a pure validation test — no server needed. Runs in Phase 1 (unit).

Test Scenarios:
1. Valid UUIDv7 accepted
2. UUIDv4 rejected (version nibble = 4)
3. Empty string rejected
4. Non-hex rejected
5. Boundary: all-zero UUID rejected, all-F UUID rejected

Pattern: standalone unit test, no IrisClient needed.
"""

import sys
import os
import time
import uuid as uuid_lib

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


# =============================================================================
# UUIDv7 Validation Logic
# (This would normally live in a shared module; tested here as standalone)
# =============================================================================

def is_valid_uuidv7(value: str) -> bool:
    """
    Validate a string as UUIDv7 per RFC 9562.

    UUIDv7 format (128 bits):
      - Bits 0-47:   48-bit Unix timestamp (milliseconds)
      - Bits 48-51:  Version = 0b0111 (7)
      - Bits 52-63:  12-bit random
      - Bits 64-65:  Variant = 0b10
      - Bits 66-127: 62-bit random

    Returns True if valid UUIDv7, False otherwise.
    """
    if not isinstance(value, str):
        return False

    # Remove hyphens for canonical UUID format
    clean = value.replace('-', '')

    # Must be exactly 32 hex characters
    if len(clean) != 32:
        return False

    # Must be valid hex
    try:
        int(clean, 16)
    except ValueError:
        return False

    # Convert to integer for bit inspection
    uuid_int = int(clean, 16)

    # Version nibble: bits 48-51 (4 bits after the 48-bit timestamp)
    # In hex: the 13th character (index 12) of the 32-char string
    version = (uuid_int >> 76) & 0xF
    if version != 7:
        return False

    # Variant: bits 64-65 must be 0b10
    # In hex: the 17th character (index 16) high 2 bits
    variant = (uuid_int >> 62) & 0x3
    if variant != 0b10:
        return False

    # Timestamp must be non-zero (all-zero UUID is invalid)
    timestamp = (uuid_int >> 80)
    if timestamp == 0:
        return False

    return True


def generate_uuidv7() -> str:
    """Generate a valid UUIDv7 string."""
    import random

    # Current time in milliseconds
    timestamp_ms = int(time.time() * 1000)

    # Build 128-bit UUID
    uuid_int = 0

    # Bits 0-47: timestamp
    uuid_int |= (timestamp_ms & 0xFFFFFFFFFFFF) << 80

    # Bits 48-51: version = 7
    uuid_int |= 7 << 76

    # Bits 52-63: random (12 bits)
    uuid_int |= (random.getrandbits(12)) << 64

    # Bits 64-65: variant = 0b10
    uuid_int |= 0b10 << 62

    # Bits 66-127: random (62 bits)
    uuid_int |= random.getrandbits(62)

    # Format as hex string with hyphens
    hex_str = f"{uuid_int:032x}"
    return f"{hex_str[:8]}-{hex_str[8:12]}-{hex_str[12:16]}-{hex_str[16:20]}-{hex_str[20:]}"


# =============================================================================
# TESTS
# =============================================================================

def test_valid_uuidv7_accepted():
    """Valid UUIDv7 must be accepted."""
    log("=" * 60)
    log("TEST: Valid UUIDv7 accepted")
    log("=" * 60)

    for i in range(10):
        v7 = generate_uuidv7()
        assert is_valid_uuidv7(v7), f"Valid UUIDv7 '{v7}' was rejected"
        log(f"  {v7} -> accepted")

    log("  PASS")
    return True


def test_uuidv4_rejected():
    """UUIDv4 (version nibble = 4) must be rejected."""
    log("=" * 60)
    log("TEST: UUIDv4 rejected")
    log("=" * 60)

    for _ in range(5):
        v4 = str(uuid_lib.uuid4())
        result = is_valid_uuidv7(v4)
        assert result is False, f"UUIDv4 '{v4}' was incorrectly accepted as UUIDv7"
        log(f"  {v4} -> rejected (v4)")

    log("  PASS")
    return True


def test_empty_string_rejected():
    """Empty string must be rejected."""
    log("=" * 60)
    log("TEST: Empty string rejected")
    log("=" * 60)

    assert is_valid_uuidv7("") is False, "Empty string was accepted"
    log("  '' -> rejected")
    log("  PASS")
    return True


def test_non_hex_rejected():
    """Non-hexadecimal strings must be rejected."""
    log("=" * 60)
    log("TEST: Non-hex rejected")
    log("=" * 60)

    invalid = [
        "not-a-uuid-at-all-no-way",
        "zzzzzzzz-zzzz-7zzz-8zzz-zzzzzzzzzzzz",
        "12345678-1234-7234-8234-12345678901g",  # 'g' is not hex
        "   ",
        "\x00\x01\x02\x03",
    ]

    for v in invalid:
        result = is_valid_uuidv7(v)
        assert result is False, f"Non-hex string '{v}' was incorrectly accepted"
        log(f"  '{v[:30]}...' -> rejected")

    log("  PASS")
    return True


def test_all_zero_uuid_rejected():
    """All-zero UUID must be rejected (timestamp = 0 is invalid)."""
    log("=" * 60)
    log("TEST: All-zero UUID rejected")
    log("=" * 60)

    zero = "00000000-0000-0000-0000-000000000000"
    result = is_valid_uuidv7(zero)
    assert result is False, "All-zero UUID was accepted"
    log(f"  {zero} -> rejected")
    log("  PASS")
    return True


def test_all_f_uuid_rejected():
    """All-F UUID must be rejected (version bits would be 0xF, not 7)."""
    log("=" * 60)
    log("TEST: All-F UUID rejected")
    log("=" * 60)

    all_f = "ffffffff-ffff-ffff-ffff-ffffffffffff"
    result = is_valid_uuidv7(all_f)
    assert result is False, "All-F UUID was accepted as UUIDv7"
    log(f"  {all_f} -> rejected")
    log("  PASS")
    return True


def test_wrong_variant_rejected():
    """UUID with correct version (7) but wrong variant must be rejected."""
    log("=" * 60)
    log("TEST: Wrong variant (not 0b10) rejected")
    log("=" * 60)

    # Build UUID with version=7 but variant=0b00 (not 0b10)
    v7 = generate_uuidv7()
    clean = v7.replace('-', '')
    uuid_int = int(clean, 16)

    # Clear variant bits and set to 0b00
    uuid_int &= ~(0x3 << 62)  # Clear bits 64-65
    # variant 0b00 = 0 → doesn't set any bits

    hex_str = f"{uuid_int:032x}"
    bad_variant = f"{hex_str[:8]}-{hex_str[8:12]}-{hex_str[12:16]}-{hex_str[16:20]}-{hex_str[20:]}"

    result = is_valid_uuidv7(bad_variant)
    assert result is False, f"UUID with wrong variant was accepted: {bad_variant}"
    log(f"  {bad_variant} -> rejected (bad variant)")
    log("  PASS")
    return True


def test_uuidv7_monotonic_timestamp():
    """Consecutive UUIDv7s must have non-decreasing timestamps."""
    log("=" * 60)
    log("TEST: UUIDv7 monotonic timestamps")
    log("=" * 60)

    uuids = []
    for _ in range(20):
        v7 = generate_uuidv7()
        uuids.append(v7)
        time.sleep(0.001)  # 1ms gap

    # Extract timestamps
    timestamps = []
    for v7 in uuids:
        clean = v7.replace('-', '')
        uuid_int = int(clean, 16)
        ts = (uuid_int >> 80) & 0xFFFFFFFFFFFF
        timestamps.append(ts)

    # Verify monotonic
    for i in range(1, len(timestamps)):
        assert timestamps[i] >= timestamps[i-1], \
            f"Timestamp not monotonic: {timestamps[i-1]} -> {timestamps[i]}"

    log(f"  {len(uuids)} UUIDv7s with monotonic timestamps")
    log(f"  Timestamp range: {timestamps[0]} -> {timestamps[-1]}")
    log("  PASS")
    return True


# =============================================================================
# MAIN
# =============================================================================
def main():
    log("UUIDv7 Validation Tests (P1-7, RFC 9562)")
    log("")

    tests = [
        ("valid_uuidv7_accepted", test_valid_uuidv7_accepted),
        ("uuidv4_rejected", test_uuidv4_rejected),
        ("empty_string_rejected", test_empty_string_rejected),
        ("non_hex_rejected", test_non_hex_rejected),
        ("all_zero_rejected", test_all_zero_uuid_rejected),
        ("all_f_rejected", test_all_f_uuid_rejected),
        ("wrong_variant_rejected", test_wrong_variant_rejected),
        ("monotonic_timestamp", test_uuidv7_monotonic_timestamp),
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
                log(f"  FAIL: {name} returned False")
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
