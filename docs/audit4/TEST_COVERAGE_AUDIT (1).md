# Test Coverage Audit — FAANG/WhatsApp-Grade Readiness

This report inventories the repository’s tests, maps them to code paths and failure modes, and identifies gaps that prevent FAANG/WhatsApp‑grade confidence. If a guarantee is not backed by deterministic tests, it is treated as unvalidated.

## 1. Inventory of Existing Tests

### Unit tests (EUnit)
- **Protocol/session**: `tests/suites/unit/iris_proto_tests.erl`, `tests/suites/unit/iris_session_tests.erl`
- **Additional EUnit tests not wired into runner**:
  - `test_utils/iris_auth_tests.erl`
  - `test_utils/iris_dedup_tests.erl`
  - `test_utils/iris_circuit_breaker_tests.erl`
  - `test_utils/iris_flow_controller_tests.erl`
- **Property-based (PropEr)**:
  - `test_utils/iris_proto_pbt.erl.proper` (not wired into test runner)

### Integration tests (Python)
- **Messaging basics**: `tests/suites/integration/test_online_messaging.py`
- **Offline storage**: `tests/suites/integration/test_offline_storage.py`
- **Presence**: `tests/suites/integration/test_presence.py`
- **Ordering**: `tests/suites/integration/test_message_ordering.py`
- **Deduplication**: `tests/suites/integration/test_deduplication.py`
- **Auth flow**: `tests/suites/integration/test_auth_flow.py`
- **Backpressure**: `tests/suites/integration/test_backpressure.py`
- **Rate limiting**: `tests/suites/integration/test_rate_limiting.py`
- **Durability**: `tests/suites/integration/test_durability.py`
- **Hotkey bucketing**: `tests/suites/integration/test_hotkey_bucketing.py`

### End-to-end tests (Python)
- `tests/suites/e2e/test_offline_reconnect.py`
- `tests/suites/e2e/test_full_conversation.py`

### Performance / load / stress
- **Performance light**: `tests/suites/performance_light/*`
- **Stress**: `tests/suites/stress/*`
- **Resilience**: `tests/suites/resilience/test_resilience.py`
- **Chaos**: `tests/suites/chaos_controlled/*`, `tests/suites/chaos_dist/*`
- **Tokyo assurance**: `tests/suites/tokyo_assurance/*` (manual/long‑running, proof outputs)

### Security tests
- `tests/suites/security/test_security_basics.py` (protocol abuse, DoS, input edge cases)

### Test framework & runner
- **Runner**: `tests/run_tests.py`
- **Assertions**: `tests/framework/assertions.py`
- **Test config**: `tests/configs/config.yaml`

## 2. Coverage Analysis — What Is Actually Tested

### Covered (with meaningful assertions)
- **Basic online delivery** and loss/dup checks (`test_online_messaging.py`)
- **Basic offline storage flow** (send offline -> receive on login)
- **Basic presence queries** (online/offline status)
- **Protocol parsing sanity** (unit + PropEr sample tests)

### Covered but weakly asserted or partial
- **Ordering**: ordering tests are time‑based and tolerate missing messages; no strict ordering assertions in core flows.
- **Deduplication**: integration tests note that dedup is ID-based and do not validate true idempotency; some tests explicitly avoid asserting duplicates.
- **Auth**: integration tests accept “auth disabled” as a passing state and do not enforce token issuance/validation under real security posture.
- **Backpressure & rate limiting**: tests allow “rate limiter may not be enabled” to pass.
- **Durability**: tests permit partial success and classify “pending acks” as stretch.

Examples of weak assertions:
```120:170:tests/suites/integration/test_deduplication.py
# The dedup module works on message IDs, not content
# So here we're really testing that the system doesn't crash under load
```
```55:75:tests/suites/integration/test_auth_flow.py
# Auth might be enabled, which is also valid
return True  # Not a failure, just different config
```

### Not covered or unvalidated
- **Multi-device consistency**: no tests for multi-device sync, replay, or divergence.
- **Cross-region durability**: no deterministic partition tests validating ordering/durability across regions.
- **Ack semantics**: no test verifies “no loss after ack” or message re‑send correctness after disconnect.
- **Protocol versioning**: no backward compatibility tests for old client versions.
- **Schema migrations**: no migration or rollback tests.
- **Operational safeguards**: no automated tests for safe restart, safe schema init, or recovery invariants.
- **Security enforcement**: no tests for key rotation, token revocation under load, or TLS enforcement.

## 3. Missing Scenarios (Critical)

### Messaging semantics
- **Per‑conversation ordering under concurrent interleaving** with verified message IDs and sequences.
- **Dedup across retries** with durable idempotency keys.
- **Exactly‑once or at‑least‑once semantics** verified with ack correlation.
- **Replay protection** after reconnect or failover.

### Failure injection
- **Network partition with writes** and deterministic convergence assertions.
- **Partial storage failures** with strict no‑loss/no‑dup guarantees.
- **Clock skew** effects on ordering and TTL handling.
- **Mid‑send disconnect** ensuring correct resend or offline storage.

### Concurrency and scale
- **High‑fan‑in stress** with per‑message integrity assertions (not just counts).
- **Churn with ordering/dedup assertions** (current churn tests check stability only).
- **Backpressure under sustained load** with explicit queue bounds and latency caps.

### Security and abuse
- **Token forgery and replay** tests against real server secrets.
- **Authorization checks** (who can message whom, role‑based rules).
- **TLS enforcement** and downgrade protection.
- **Input validation** for protocol injection into storage/logging path.

### Backward compatibility / migrations
- **Rolling upgrade** tests with mixed protocol versions.
- **Schema changes** and data replay correctness.

## 4. Hardening & Reliability Gaps

- **Test runner does not include `test_utils` EUnit tests** and does not run PropEr PBTs by default. Critical unit tests exist but are not executed.
- **Many tests pass on partial success** (“missing messages may be offline”), which hides real loss.
- **Metrics-based proof reports** are not enforced in CI and can pass with zero traffic.
- **No deterministic acceptance criteria** for failover recovery, durability, or reordering under chaos.

## 5. Security & Abuse Testing Gaps

- **Auth tests assume shared secrets and local token generation**; this defeats real boundary validation.
- **No negative tests for privilege escalation** or account impersonation.
- **No key rotation / token revocation under load** tests.
- **No encryption policy tests** (TLS required vs optional).

## 6. Quality Assessment (Signal vs Noise)

### Strengths
- Dedicated test runner and framework; state‑based assertions exist.
- Some tests log metrics and use structured logging.
- Wide variety of suites (integration, resilience, stress, chaos).

### Weaknesses
- **Low determinism**: heavy use of `time.sleep` and partial‑success logic.
- **Silent acceptance of misconfiguration**: many tests pass even if key features disabled.
- **Incomplete coverage wiring**: unit tests in `test_utils` and PBTs are not run by `tests/run_tests.py`.
- **No CI workflow** in repo despite documentation claims; tests are not guaranteed to run.

## 7. Actionable Recommendations (Prioritized)

### P0 — Must‑have
- **Wire all critical unit tests into the runner** (auth, dedup, circuit breaker, flow controller).
  - Prevents regressions in auth and dedup logic.
- **Make ordering, dedup, and durability tests deterministic**:
  - Require strict delivery counts and sequence checks; fail on any loss or reorder.
- **Add ack‑level durability tests**:
  - Validate that acked messages never disappear across disconnect and restart.
- **Enforce auth/TLS in security tests**:
  - Fail tests if auth is disabled or TLS is off.

### P1 — Important
- **Add partition and failover correctness tests**:
  - Deterministic assertions for convergence and replay behavior.
- **Add multi‑device consistency tests**:
  - Ensure devices converge after offline periods and retries.
- **Add protocol version compatibility tests**:
  - Simulate old/new clients in the same run.

### P2 — Nice‑to‑have (but still required for WhatsApp‑scale)
- **Long‑running soak tests with correctness assertions**:
  - Not just resource stats; verify zero loss/dup over hours.
- **Property‑based tests for all protocol opcodes**:
  - Expand PBT beyond decode to full encode/decode invariants.
- **Chaos tests with clear pass/fail criteria**:
  - Remove “warning only” outcomes; enforce failure thresholds.

## Verdict

Current coverage is broad but shallow. The suite contains many tests, yet a significant portion are non‑deterministic, permissive, or not wired into the runner. This does not meet FAANG‑level or WhatsApp‑grade test standards. The system is **not** production‑ready from a test‑quality standpoint until P0 and P1 items above are complete.
