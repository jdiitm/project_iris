# Staff+ Engineering Audit: Project Iris

**Date**: 2026-01-17
**Reviewer**: Principal Engineer (Infrastructure & Security)
**Target**: Global Production Readiness (WhatsApp Scale)
**Verdict**: **CRITICAL RISK / PROTOTYPE ONLY**

---

## 1. Executive Summary

**Project Iris is not production-ready.**
While the documentation describes a planetary-scale system (ScyllaDB, Kafka, Federation), the actual codebase is a **monolithic Erlang cluster** utilizing `localhost` storage and broadcast-spam replication.

If deployed to a cluster of >50 nodes, the `ets_cluster` replication strategy (O(N²) RPC broadcast) would cause an immediate network storm and catastrophic self-DDoS.
The security implementation "rolls its own crypto" for JWTs and stores revocation lists in non-replicated memory, meaning a banned user is only banned on *one* server.

**Maturity Score**: **2/10** (Advanced Hobbyist / Hackathon Quality)

---

## 2. Architecture & Design Review

### 2.1 The "Scalability Lie"
*   **Claim**: "Planetary Scale Architecture" (Docs).
*   **Reality**:
    *   **Storage**: `iris_storage.erl` implements a `redis` backend that *only logs* (`logger:debug`). It writes nothing. The actual data lives in `disk_log` (local files) or Mnesia (cluster-limited).
    *   **Replication**: `iris_storage:replicate_to_peers/2` does `rpc:cast` to `nodes()`. This is **Full Mesh Broadcast**.
        *   *Math*: At 1,000 nodes, 1 write = 999 outgoing TCP connections + RPC serialization overhead.
        *   *Outcome*: Hardware saturation at <100 nodes.
    *   **Federation**: Non-existent. No code handles cross-region routing.

### 2.2 Coupling & Failure Modes
*   **Synchronous RPC**: `iris_router` uses `rpc:call`.
    *   *Risk*: A "Head-of-Line" block. If one node enters GC pause or disk stall, all callers block. In a mesh, this cascades instantly (The "Freeze of Death").
*   **Single Point of Failure (SPoF)**:
    *   `global` naming or `pg` (Process Groups) used for user lookups. Standard Erlang `pg` is eventually consistent but chatty.
    *   No definition of "Shard Master" or "Leader".

---

## 3. Security Audit (Threat Model: Critical)

### 3.1 Authentication (`iris_auth.erl`)
*   **Custom JWT Implementation**: The code manually parses JWT JSON and computes HMACs.
    *   *Vulnerability*: "Rolling your own crypto" is essentially a guaranteed CVE. It likely fails to handle edge cases (e.g., `None` alg attack, timing attacks on non-constant comparison parts).
    *   *Fix*: Delete this module. Use `jose` or `jwt` libraries (standard HEX packages).
*   **Revocation is Local**: `revoke_token` writes to a local `ets` table.
    *   *Scenario*: User is banned on Node A. User reconnects, LB routes to Node B. Node B has no record of ban. User continues abuse.
    *   *Fix*: Revocations must be distributed (Redis/Cassandra).

### 3.2 Secrets Management
*   **Ephemeral Secrets**: `init/1` generates `crypto:strong_rand_bytes(32)` if no config is found.
    *   *Scenario*: Node A and Node B start up with different random secrets. User logs into Node A, gets Token. LB moves User to Node B. Node B rejects token.
    *   *Outcome*: Users are effectively session-locked to a single server (Sticky Sessions required), defeating the "Global Mesh" goal.

---

## 4. Code Quality & Maintainability

### 4.1 Dependency Management
*   **Missing Rebar3**: The project uses a raw `Createfile` (`Makefile`).
    *   *Impact*: No managed dependencies. Libraries (`cowboy`, `jiffy`, `jose`) must be manually vendored or are missing.
    *   *Staff Note*: I cannot build this project reproducibly on a clean CI agent.

### 4.2 Error Handling
*   **"Let It Crash" Abuse**: Erlang's philosophy is abused here. Critical failures (Disk Full) crash the process, but `disk_log` repair might take minutes.
*   **Swallowed Errors**: `iris_storage:apply_replication` does `rpc:cast`. It fails silently. Data inconsistency is guaranteed.

---

## 5. Testing & Observability

### 5.1 Test Coverage
*   **Simulation Gap**: All tests run on `localhost`.
    *   `stress_global_fan_in.py`: Tests `loopback` interface throughput.
    *   Missing: Network Partition tests (`iptables DROP`), Packet Loss simulation, Client Reconnection Storms.

### 5.2 Observability
*   **Logs**: Uses `logger:info/error`.
*   **Metrics**: No Prometheus/StatsD exporter found. "Observability" is `print` statements in test scripts.
*   **Operational Blindness**: If this runs in production, you have no queries per second (QPS) graph, no latency histogram, and no error rate alert.

---

## 6. Actionable Upgrade Plan (The "Defibrillator")

### P0: Immediate Safety (Stop the Bleeding)
1.  **Delete `iris_auth` crypto**: Replace with `hex_core` / `jose`.
2.  **Fix Revocation**: Store `jti` in a consistent store (Mnesia replicated or Ext Redis).
3.  **Adopt Rebar3**: Delete `Makefile` logic. Create `rebar.config`.

### P1: Correctness (Make it Work)
1.  **Implement Redis/Scylla Driver**: Make `iris_storage` actually write data to a shared backend.
2.  **Remove Full-Mesh Replication**: Stop `rpc:cast` to all nodes. Write to DB, let DB replicate.

### P2: Scale (Make it Fast)
1.  **Async Messaging**: Replace `rpc:call` with `message_queue` (Kafka/Pulsar).
2.  **Telemetry**: Add `telemetry` library and Prometheus exporter.

---

**Final Verdict**:
This repository is a **Learning Exercise**, not a Product.
Do not accept "Go" for launch.
**Rewrite Required.**
