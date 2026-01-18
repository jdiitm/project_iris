# Comprehensive Test Coverage Audit: Project Iris

**Date**: 2026-01-18
**Role**: Staff Engineer (Reliability & Quality)
**Target Standard**: FAANG / WhatsApp-Grade Durability
**Verdict**: **FAIL / PROTOTYPE ONLY**

---

## 1. Inventory & Mapping

We have identified **43 test files** across **11 suites**. While the *volume* appears high, the *value* is severely compromised by "happy path" bias and lack of failure gating.

| Suite | Criticality | Key Tests | Coverage Target | Findings |
| :--- | :--- | :--- | :--- | :--- |
| **Unit** | High | `iris_session_tests.erl` | `iris_session`, `iris_edge_conn` (Logic) | **Good**. Covers basic pack/unpack logic. Mocks all dependencies. |
| **Integration** | **CRITICAL** | `test_durability.py` | `iris_durable_batcher` (Persistence) | **Dangerous**. Tests "clean" shutdowns only. Fails to test `kill -9` or power loss. |
| **Resilience** | **CRITICAL** | `test_hard_kill.py` | `iris_storage` (Recovery) | **FATAL**. Returns `True` (Pass) even if data is lost. Defined as "exploratory". |
| **Chaos** | High | `chaos_combined.py` | System-wide Stability | **Weak**. Runs against `localhost`. Validates logic, not distributed systems physics. |
| **Security** | High | `test_security_basics.py` | `iris_auth` (DoS, Injection) | **Medium**. Good inputs/outputs, but lacks **Protocol Fuzzing** or **Crypto Failure** tests. |
| **Dist** | Critical | `test_dist_failover.py` | `iris_cluster` (HA) | **Manual**. Requires human intervention ("Kill node now"). Not CI-capable. |

---

## 2. Coverage Analysis

### 2.1 The "Validation Illusion"
The repository suffers from a dangerous antipattern: **Probe Tests**.
*   **What it is**: Tests like `test_hard_kill.py` run scenarios but **suppress failures**.
*   **Code Evidence**:
    ```python
    # test_hard_kill.py:107
    return True  # Partial is OK
    return True  # Don't fail CI - this is exploratory
    ```
*   **Verdict**: You have **Zero Automated Coverage** for data durability. If Mnesia corrupts the schema on Tuesday, you won't know until the post-mortem on Wednesday.

### 2.2 Untested Logic
1.  **Reconnection Storms**: No test checks behavior when 10k clients reconnect simultaneously (`iris_edge_conn` backpressure).
2.  **Clock Skew**: `iris_auth` relies on JWT `exp`. No test checks behavior when Node A is +5s ahead of Node B.
3.  **Partial Writes**: What if `disk_log` writes 4KB of an 8KB frame and crashes?

---

## 3. Missing Scenarios (Production Blockers)

| ID | Scenario | Risk | Severity |
| :--- | :--- | :--- | :--- |
| **M1** | **Process Hard Kill** | Data Loss. Without this, you are not a database. | **P0** |
| **M2** | **Disk Full (ENOSPC)** | Crash Loop. System enters unrecoverable state. | **P0** |
| **M3** | **Network Partition** | Split Brain. Users on Edge A cannot talk to Edge B. | **P0** |
| **M4** | **Corrupt Protocol** | Node Crash. A single fuzzed packet crashes the BEAM. | **P1** |
| **M5** | **Expired JWT Replay** | Security Breach. Attackers reuse old tokens. | **P1** |

---

## 4. Hardening & Reliability Gaps

*   **No Backward Compatibility Tests**:
    *   If you ship v2.0, does v1.0 Client still work? `compatibility/` directory is present but contains no rigorous version-skew tests.
*   **Testing in a Bubble**:
    *   All "distributed" tests run on 127.0.0.1.
    *   *Real World*: Latency, Jitter, Packet Loss, MTU truncation. None of this is tested.

---

## 5. Security & Abuse Testing

*   **Auth Bypass**: No test attempts to sign a token with `alg: None` or a different HMAC secret.
*   **Rate Limiting**: `test_security_basics.py` tests "floods" but doesn't verify the *precision* of the token bucket. Can I send 1001 messages if the limit is 1000?

---

## 6. Quality Assessment

*   **Clarity**: High. Python scripts are readable and well-structured.
*   **Determinism**: Low. `chaos` tests rely on `time.sleep()` for coordination, which is flaky on loaded CI runners.
*   **Signal-to-Noise**: Very Low. Because `hard_kill` passes when it fails, a "Green" build means nothing.

---

## 7. Actionable Recommendations

### P0: Must-Have (The "Don't Lose Data" Tier)
1.  **Fix `test_hard_kill.py` Assertions**
    *   *Action*: Change `return True` to `assert found == len(messages)`. Make CI **FAIL** on data loss.
2.  **Automate Distributed Failover**
    *   *Action*: Convert `test_dist_failover.py` to use a Docker control plane so it runs headless in CI.
3.  **Implement `test_disk_full.py`**
    *   *Action*: Use a mocked file system or small loopback mount to simulate running out of space.

### P1: Important (The "Stay Up" Tier)
1.  **Add `test_protocol_fuzz.py`**
    *   *Action*: Use `wb` (Erlang) or `AFL` to blast garbage at port 8085.
2.  **Containerize Stress Tests**
    *   *Action*: Move stress tests to `docker-compose` to enforce real network constraints (`pumba`).

### P2: Nice-to-Have
1.  **Visual Metrics Reports**: Add checking of Prometheus metrics during tests (e.g., Assert "Error Rate < 0.1%").

---

**Signed**:
*Principal Engineer, Infrastructure*
