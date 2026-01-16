# Project Iris: Comprehensive Audit Report (2026-01-17)

**Auditor Role**: Distinguished Engineer
**Verdict**: 🔴 **NO-GO (Not Production Ready)**
**Test Readiness Level**: **L1 (Ad-Hoc Scripts)**

---

## 1. Executive Summary

This document consolidates findings from the **System Architecture Audit** and the **Test Coverage Audit**.
Project Iris masquerades as a global messaging platform but fundamentally operates as a **single-cluster Proof-of-Concept**.

*   **Architecture Gap**: The "Target Architecture" (Federated, ScyllaDB-backed, Kafka-coupled) described in documentation **does not exist** in the codebase. The system relies on local `disk_log` and synchronous `rpc:call`.
*   **Test Gap**: The test suite relies on "Simulation" (running on `localhost`, asking operators to manually kill nodes) rather than "Evidence" (Property-Based Testing, Distributed Chaos).

**Conclusion**: The system cannot handle "WhatsApp-scale" traffic today. It would fail catastrophically under partition or heavy load due to synchronous coupling and unproven durability.

---

## 2. Architectural Findings (The "Scalability Lie")

### 2.1 Storage Engine Mismatch
*   **Goal**: Users/Messages stored in ScyllaDB for limitless horizontal scale.
*   **Reality**: Users/Messages stored in `Mnesia` and `disk_log` (local files).
*   **Impact**: Data is tied to specific nodes. If a node dies, that data is unavailable (AP violation) or lost if the disk fails.

### 2.2 Synchronous Coupling
*   **Goal**: Global Federation.
*   **Reality**: Inter-node communication uses `rpc:call` (Erlang Distribution).
*   **Impact**: Head-of-Line blocking. One slow node slows down the entire cluster. In a global mesh, this guarantees unreliability.

### 2.3 Security Void
*   **Missing**: End-to-End Encryption (Signal Protocol).
*   **Missing**: Robust Authorization scopes.

---

## 3. Test Coverage Findings (The "Simulation Gap")

### 3.1 "Happy Path" Bias
*   **Finding**: `stress_global_fan_in.py` and other load tests run on `localhost` aliases. They test the OS kernel's ability to loopback packets, not a distributed network's reality.
*   **Risk**: MTU issues, TCP retransmission storms, and Split-Brain scenarios are completely untested.

### 3.2 Lack of Rigor
*   **Finding**: **Property-Based Testing (PBT)** is nonexistent. Complex state machines (`iris_edge_conn`) are only tested with simple inputs, leaving 99% of state space unexplored.
*   **Finding**: **Chaos Tests** use RPC to "kill" nodes. If the network is actually broken (the scenario we want to test), the "kill" command fails, and the test **falsely passes**.

### 3.3 "God Mode" Scripts
*   **Finding**: `ten_minute_hidden_proof.py` relies on manual operator intervention ("MANUALLY TERMINATE CORE1"). This is a demo script, not an automated regression test.

---

## 4. Required Remediation (The Road to Production)

To exit the "Proof of Concept" phase, the following engineering initiatives are **mandatory**:

### Phase 1: Storage & Durability Verification
1.  **Migrate Storage**: Implement ScyllaDB/Cassandra driver to replace `disk_log`.
2.  **Verify Hard Kill**: Implement `verify_hard_kill.py` to prove data survives a `kill -9` (sudden process death).

### Phase 2: Asynchronous Transport
1.  **Decouple Nodes**: Replace `rpc:call` with an async message bus (RabbitMQ/Kafka/NATS).
2.  **Backpressure**: Implement end-to-end flow control that propagates from DB to Client.

### Phase 3: Advanced Testing
1.  **Property-Based Testing**: Use `PropEr` or `Hypothesis` to fuzz the Core Protocol.
2.  **True Chaos**: Use `network namespaces` or `docker-compose` to test real network partitions (blocking ports, not just failing RPCs).

---

**Signed**:
*Codex 5.2, Distinguished Engineer*
