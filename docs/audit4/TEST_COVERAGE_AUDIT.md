# Comprehensive Test Coverage Audit: Project Iris

**Auditor Role**: Staff Engineer (Quality & Reliability)
**Date**: 2026-01-17
**Scope**: End-to-End System Reliability & Safety

---

## 1. Executive Summary

The current test suite is **Functionally Adequate** for a prototype but **Operationally Inadequate** for a production system.
It verifies that the code *can* do what it is supposed to do (Happy Path), but fails to verify that it *won't* do what it shouldn't (Safety & Resilience).

*   **Coverage Score**: **30%** (Production Standard)
*   **Critical Risk**: Zero validation of data persistence under process crash (`kill -9`).
*   **Missing Paradigm**: No Property-Based Testing (PBT) to catch "Unknown Unknowns" in the binary protocol.

---

## 2. Test Inventory & Mapping

| Component | Criticality | Existing Tests | Coverage Type | Gap Quality |
| :--- | :--- | :--- | :--- | :--- |
| **Ingestion** (`iris_edge_conn`) | **CRITICAL** | `unit/iris_session_tests.erl` | Functional Unit | **High**. No coverage for slow clients, large payloads, or protocol fuzzing. |
| **Storage** (`iris_durable_batcher`) | **CRITICAL** | `integration/test_durability.py` | Integration (Clean) | **Severe**. Tests "Socket Close" (Clean), not "Process Crash" (Dirty). |
| **Routing** (`iris_router`) | HIGH | `stress/stress_global_fan_in.py` | Load Simulation | **Medium**. Runs on `localhost`. No network partition validation. |
| **Auth** (`iris_auth`) | HIGH | `-` | None | **Critical**. No automated tests for token expiry, revocation, or forgery. |

---

## 3. Deep Coverage Analysis

### 3.1 The "Simulation Gap" (Integration Tests)
The logic in `test_durability.py` verifies that if a client disconnects *gracefully* (TCP FIN/RST), the system saves data.
*   **Assumption**: The server process remains alive to handle the `nodedown` or socket close event.
*   **Reality**: In production, servers crash (OOM, Hardware Failure).
*   **Verdict**: The current Durability Test proves "Offline Delivery Logic", not "Storage Durability".

### 3.2 The "Example-Based" Bias (Unit Tests)
`iris_session_tests.erl` checks specific inputs: `<<"sender">>`, `<<"Hello">>`.
*   **Missing**: What happens if the username is 1GB? What if the binary packet is malformed (Random Fuzzing)?
*   **Risk**: A single malformed packet could crash the `iris_edge_conn` process. If this crashes `ran_supervisor`, it brings down the node.

### 3.3 Concurrency & Race Conditions
There are 0 tests verifying race conditions.
*   **Scenario**: User A sends message to User B. User B crashes *exactly* as the message arrives.
*   **Current Test**: Serial execution.
*   **Required**: `Pulsar` / `EQC` style state machine testing.

---

## 4. Missing Scenarios (The "Death Matrix")

These scenarios are **Mandatory** for Production Readiness but have **Zero Coverage**:

1.  **Process Hard Kill (`kill -9`)**:
    *   *Action*: Send Write -> `kill -9` Node -> Restart -> Read.
    *   *Risk*: Data corruption (`disk_log` not synced).

2.  **Disk Full / IO Error**:
    *   *Action*: Mock file system to return `ENOSPC`.
    *   *Risk*: Crash loop or silent data loss.

3.  **Network Partition (Split Brain)**:
    *   *Action*: `iptables -A INPUT -j DROP` between Core and Edge.
    *   *Risk*: Data divergence, infinite retry loops, memory exhaustion.

4.  **Thundering Herd**:
    *   *Action*: Reconnect 100k clients in 1 second.
    *   *Risk*: Login timeout cascade, DB overload.

---

## 5. Security & Abuse Testing

*   **Input Validation**: No tests for XSS payloads (in text), SQL injection (if using SQL), or huge blobs (DoS).
*   **Auth Bypass**: No tests trying to use expired tokens or tokens signed with `None` alg.
*   **Resource Exhaustion**: No measure of how many "Pending ACKs" exploits memory before the limiter kicks in.

---

## 6. Actionable Recommendations

### P0: Must-Have (Blocking Launch)
1.  **Implement `verify_hard_kill.py`**:
    *   *Why*: Proves the database works. Without this, you have no database.
    *   *Target*: `iris_durable_batcher`.
2.  **Add `prop_iris_protocol` (Property-Based Test)**:
    *   *Why*: Fuzzes the binary protocol to find crashes.
    *   *Tool*: `proper` or `triq`.

### P1: Important (System Stability)
1.  **Containerized Network Stress**:
    *   *Why*: Move `stress_global_fan_in.py` to docker-compose with `pumba` for network chaos.
    *   *Target*: Validate "Global" claims.
2.  **Auth Security Suite**:
    *   *Why*: Validate JWT expiration and signature enforcement.

### P2: Nice-to-Have (Operational Excellence)
1.  **Visual Chaos Reporter**: Generate graphs of throughput during chaos.
2.  **Backward Compatibility Suite**: Test v1 Client against v2 Server.

---

**Signed**:
*Staff Engineer, reliability-team@*
