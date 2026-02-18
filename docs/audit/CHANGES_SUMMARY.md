# Adversarial Test Audit: Changes Summary

> **Frozen artifact** (2026-02-07). This document records a one-time audit and will not be updated. All changes are also captured in [CHANGELOG.md](../../CHANGELOG.md). For current test status, see [TESTING.md](../TESTING.md).

All 19 mitigations from the adversarial test audit plan have been implemented.

---

## Immediate Fixes (M1-M10)

### M1: test_tls_certificate_attacks.py
- Missing cert files now FAIL instead of silently SKIP
- TLS 1.2 downgrade acceptance now FAILS (RFC NFR-14 violation)
- Each test verifies server_alive() after attack attempt
- `test_no_client_certificate` and `test_server_survives` left unchanged (correct as-is)

### M2: test_connection_rate_limit.py
- "All 100 connections succeeded = PASS" branch replaced with FAIL
- Rate limiting must now produce evidence of throttling (refused/timeout connections)

### M3: test_message_ordering.py
- Both `test_message_ordering` and `test_interleaved_conversations` now require ALL messages delivered
- Partial delivery (e.g., 1/20 messages) now FAILs instead of passing

### M4: test_edge_core_contract.py
- `test_live_message_contract`: timeout and unexpected format now return False
- Each branch explicitly returns True/False, no universal fallback

### M5: test_rfc_v4_protocol_contract.py
- `test_live_deprecated_0x02_rejected`: now tracks whether server actually rejected 0x02
- Connection reset or error response = rejection confirmed
- Server timeout logged as warning, not acceptance

### M6: test_cbor_malformed.py
- All 4 CBOR tests now track rejection counts (connection close, error opcode, reset)
- `send_cbor_msg` return value now indicates whether connection was closed
- Tests still pass on survivability (can't mandate specific rejection method) but log counts

### M7: test_token_expiry_boundary.py
- Bare `except Exception: pass` blocks replaced with specific exception handling
- Tests now inspect server response for error opcode (0xFE) or connection close
- Rejection status tracked and logged

### M8: test_slowloris.py
- `test_legitimate_during_attack`: removed fallback that passed when legit client failed
- Legitimate client MUST succeed for test to pass; failure = FAIL

### M9: test_plaintext_rejection.py
- `test_multiple_plaintext_rejected`: changed from `>= 1` to `== 3` rejections required
- Partial acceptance (2/3 accepted) now FAILs

### M10: test_dedup_window_boundary.py
- Renamed `test_dedup_stats_reflect_entries` to `test_dedup_message_flow_operational`
- Updated main() test list reference to match
- Test now tracks received message count explicitly

---

## Structural Improvements (M11-M14)

### M11: Duplicate Test Removal
- **iris_dedup_tests.erl**: Removed `test_check_and_mark_new` (duplicate of `test_new_message`), removed `test_check_and_mark_dup` (duplicate of `test_duplicate_rejected`)
- **iris_auth_tests.erl**: Removed `test_revocation_is_synchronous` (duplicate of `test_revoked_token`)
- **iris_proto_tests.erl**: Removed duplicate `test_unpack_batch_empty` entry in test generator

### M12: Self-Referential Contract Tests Reclassified
- **test_rfc_v4_protocol_contract.py**: Static tests now labeled "Schema Documentation Tests (self-referential, not counted)". Separate doc_passed/doc_failed counters; only live tests counted for pass/fail
- **test_edge_core_contract.py**: Same treatment. Static tests not counted toward pass/fail

### M13: Auth Security Tests Fixed
- `test_constant_time_equal/unequal/length`: Now call actual `iris_auth:constant_time_compare/2` instead of Erlang `assertEqual/assertNotEqual`
- `test_jwt_secret_minimum_length`: Now actually tests that iris_auth REJECTS secrets shorter than 32 bytes by temporarily setting a short secret, verifying start_link fails with `{error, {jwt_secret_too_short, 16}}`, then restoring valid secret

### M14: Key Verification Tests Fixed
- All 3 tests in `test_key_verification.py` now call `iris_keys:compute_safety_number/2` instead of reimplementing with raw `crypto:hash(sha256, ...)`

---

## New Tests (M15-M19)

### M15: test_negative_delivery.py (NEW)
- Verifies messages to offline/non-existent users are NOT delivered to other connected users
- Message isolation test: A sends to offline target, B should NOT receive it

### M16: TLS 1.3 Enforcement
- Covered by M1's fix to `test_tls_downgrade_attack` which now FAILs if TLS 1.2 is accepted

### M17: Dedup Hot Tier Expiration Tests (NEW)
- Added `hot_tier_expiration_test_` generator in `iris_dedup_tests.erl`
- `test_old_entries_evicted`: inserts entry with 6-minute-old timestamp, triggers cleanup, verifies removal
- `test_fresh_entries_survive`: verifies fresh entries survive cleanup

### M18: test_e2ee_data_inspection.py (NEW)
- Sends message with known plaintext marker through server
- Inspects raw received bytes to verify plaintext is NOT present verbatim
- Smoke test for E2EE: proves server doesn't transmit plaintext

### M19: Backpressure Smoke Profile Thresholds
- `min_successful_during_overload`: raised from 0.01 (1%) to 0.10 (10%)
- `max_errors_during_recovery`: lowered from 0.92 (92%) to 0.50 (50%)

---

### M20: Real Distributed Key Bundle Durability Test
- **Finding**: `test_key_bundle_durability.py` was a fake distributed test. It claimed to test NFR-23 (key bundle durability = 99.999%) with a SIGKILL scenario but actually ran a single local Erlang VM with no Docker cluster involvement.
- **Fix**: Rewrote `tests/suites/chaos_dist/test_key_bundle_durability.py` as a real distributed test following the `test_ack_durability.py` pattern: upload bundle to core-east-1, SIGKILL it, verify bundle survives on core-east-2 replica.
- **Bug Found**: During testing, discovered `e2ee_key_bundle` table was NOT replicated across cluster nodes (missing from `init_cross_region_replication/0`). Added `disc_copies` replication for `e2ee_key_bundle` and `key_contact` tables.
- **File**: `src/iris_core.erl` (replication fix), `tests/suites/chaos_dist/test_key_bundle_durability.py` (full rewrite)

### M21: Relocate Misplaced Local API Tests
- **Finding**: Three valid local API tests (upload, OPK consumption, SPK fallback) were in `chaos_dist/` masquerading as distributed chaos tests.
- **Fix**: Moved to `tests/suites/integration/test_key_bundle_api.py` with corrected docstring.
- **File**: `tests/suites/integration/test_key_bundle_api.py` (new file)

### M22: Document CP Exception for Key Bundles
- **Finding**: `consistency-modes.md` says "AP-only for v1.0" but `iris_keys:store_key_bundle_durable/2` intentionally uses CP semantics (quorum write, fail-on-no-quorum).
- **Fix**: Added "CP Exception: Key Bundles (E2EE)" section to `docs/rfc/consistency-modes.md` documenting the intentional CP exception.
- **File**: `docs/rfc/consistency-modes.md`

---

## Files Modified (18 files)

1. `tests/suites/security/test_tls_certificate_attacks.py` (M1)
2. `tests/suites/security/test_connection_rate_limit.py` (M2)
3. `tests/suites/integration/test_message_ordering.py` (M3)
4. `tests/suites/contract/test_edge_core_contract.py` (M4, M12)
5. `tests/suites/contract/test_rfc_v4_protocol_contract.py` (M5, M12)
6. `tests/suites/security/test_cbor_malformed.py` (M6)
7. `tests/suites/security/test_token_expiry_boundary.py` (M7)
8. `tests/suites/security/test_slowloris.py` (M8)
9. `tests/suites/security/test_plaintext_rejection.py` (M9)
10. `tests/suites/integration/test_dedup_window_boundary.py` (M10)
11. `test_utils/iris_dedup_tests.erl` (M11, M17)
12. `test_utils/iris_auth_tests.erl` (M11, M13)
13. `test_utils/iris_proto_tests.erl` (M11)
14. `tests/suites/e2e/test_key_verification.py` (M14)
15. `tests/suites/stress/test_backpressure_collapse.py` (M19)

16. `src/iris_core.erl` (M20 - e2ee_key_bundle replication)
17. `tests/suites/chaos_dist/test_key_bundle_durability.py` (M20 - full rewrite)
18. `docs/rfc/consistency-modes.md` (M22)

## Files Created (3 files)

19. `tests/suites/integration/test_negative_delivery.py` (M15)
20. `tests/suites/integration/test_e2ee_data_inspection.py` (M18)
21. `tests/suites/integration/test_key_bundle_api.py` (M21)
