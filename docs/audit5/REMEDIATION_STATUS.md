# Audit5 Remediation Status

**Last Updated**: 2026-01-18
**Status**: Tests Hardened - Real Issues Now Visible

---

## Executive Summary

Per Audit5 recommendations, we have hardened tests to **fail on data loss** and **fail on security issues**. This has correctly revealed:

1. **Offline message durability issues** - messages not always delivered
2. **JWT security gaps** - expired tokens and invalid signatures accepted

These are real bugs that were previously hidden by permissive "probe tests".

---

## Test Hardening Applied ✅

| Test | Before | After |
|------|--------|-------|
| `test_hard_kill.py` | Returns True always | FAILS on any data loss |
| `test_durability.py` | 2/3 is "pass" | Requires 3/3 pass |
| `test_protocol_fuzz.py` | NEW | 6 fuzz tests, all must pass |
| `test_jwt_security.py` | NEW | 4 JWT security tests |

---

## Issues Revealed by Hardened Tests

### 1. Offline Message Durability (CRITICAL)
- **Finding**: Messages sent to offline users are not always delivered
- **Root Cause**: Mnesia persistence timing, test environment restarts
- **Fix Required**: Improve offline storage sync or increase test timeouts

### 2. JWT Expired Token Acceptance (SECURITY)
- **Finding**: Server accepts expired JWT tokens
- **Root Cause**: `iris_auth.erl` may not validate `exp` claim
- **Fix Required**: Add expiry validation in `validate_token/2`

### 3. JWT Invalid Signature Acceptance (SECURITY)
- **Finding**: Server accepts tokens with wrong signature
- **Root Cause**: Auth may be disabled or signature not checked
- **Fix Required**: Enforce signature verification

---

## Tests Now Passing ✅

| Test | Status |
|------|--------|
| Protocol Fuzzing | ✅ Server survives garbage input |
| Algorithm "none" Attack | ✅ Blocked |
| Tampered Payload | ✅ Rejected or not privileged |
| test_security_basics | ✅ All pass |
| test_resilience | ✅ Pass |

---

## Next Steps (Priority Order)

1. **Fix JWT validation** in `iris_auth.erl`:
   - Validate `exp` claim against current time
   - Always verify signature (don't skip when disabled)

2. **Fix offline durability**:
   - Ensure Mnesia sync before test continues
   - Add retry logic with exponential backoff

3. **Enable tests in CI**:
   - Once underlying issues fixed, all tests should pass

---

## Verification Commands

```bash
# Run hardened security tests
python3 tests/run_tests.py --suite security

# Run hardened resilience tests  
python3 tests/run_tests.py --suite resilience

# Run all Tier 0 tests
python3 tests/run_tests.py --tier 0
```

---

**Signed**: Audit5 Remediation Complete - Tests Now Reveal Truth
