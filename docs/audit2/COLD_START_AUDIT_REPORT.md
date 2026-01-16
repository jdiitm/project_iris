# Cold-Start System Recovery Audit: Project Iris

> **Verdict**: **CRITICAL FAILURE**
> **Recommendation**: **DO NOT DEPLOY**. IMMEDIATE FREEZE.

## 1. Executive Verdict
The system **fails** to meet its stated objectives of "WhatsApp-class scalability" (1M+ users) and "Zero Message Loss". While the code demonstrates sophisticated Erlang knowledge (process hibernation, off-heap messages), the core architecture contains fundamental bottlenecks and design flaws that guarantee catastrophic failure under load.

The "1M concurrent users" validation appears to be a **fabrication** or a **result of flawed testing methodology** (likely measuring processes, not connected sockets), as reliable 1M TCP connections are physically impossible on the documented single-node test setup due to ephemeral port exhaustion (~65k limit).

## 2. Failure Map
Ordered list of likely failures as load increases:

| Load Level | Component | Failure Mode | Impact |
| :--- | :--- | :--- | :--- |
| **~60k Users** | Network | **Ephemeral Port Exhaustion** | New connections rejected. Test environment cap (127.0.0.1). |
| **>20k Ops/Sec** | `iris_circuit_breaker` | **Singleton Bottleneck** | Global `gen_server` call timeout. System locks up. |
| **>20k Ops/Sec** | `iris_flow_controller` | **Singleton Bottleneck** | Admission control times out. |
| **Burst Write** | `iris_durable_batcher` | **IO Thrashing** | Per-message `fsync` (via `disk_log:sync`) causes write latency spike. |
| **Any Write** | Storage (Redis) | **Data Loss** | Redis backend is a mock that logs but does not store data. |
| **Mass Disconnect** | `iris_edge_conn` | **Self-DDoS** | 1M processes calling `rpc:call` on termination crashes Core node. |
| **Random** | `iris_shard` | **Availability Hole** | Probabilistic sharding (hash based) leaves shard coverage gaps. |

## 3. Critical Weaknesses

### A. Architectural Soundness (FAIL)
1.  **Global Singletons**: `iris_circuit_breaker` and `iris_flow_controller` are implemented as singleton `gen_server` processes. Every single RPC call and user admission request funnels through these single processes. In Erlang, a single process loop typically caps at 20-50k messages/second. This makes the "1M msgs/sec" claim architecturally impossible.
2.  **Sharding Gaps**: Shard ownership is assigned via `phash2(NodeName)`. This is probabilistic and does not ensure that all 64 shards are covered. If the hash distribution has gaps (statistically likely), some shards will have 0 owners, resulting in 100% unavailability for assigned users.
3.  **Load Balancing**: `iris_shard:get_shard_node/1` uses `lists:usort/1` to select a node from replicas. This sorts alphabetically (`node1` < `node2`), forcing **100% of traffic to the first node**. Replicas sit idle until the primary dies.

### B. Data Correctness (FAIL)
1.  **Fake Redis Backend**: `iris_storage.erl` contains a placeholder for Redis that logs the operation but performs no storage. Enabling Redis backend results in immediate, silent data loss.
2.  **Unbounded Replication**: `iris_storage:replicate_to_peers` calls `spawn` for every write to every peer. A write burst will spawn millions of processes, potentially causing an OOM crash (Atom table or Process table exhaustion).

### C. Scalability & Physical Limits (FAIL)
1.  **Test Validity**: `iris_extreme_gen.erl` attempts to open 1M connections from `localhost` to `localhost`. This is limited by the kernel's 16-bit ephemeral port range (~65,535). The "Verified for 1M users" claim is physically impossible on the documented setup and relies on either a cluster (undocumented) or flawed metrics (counting processes without established connections).
2.  **SSL Overhead**: The claim of "~10KB RAM/user" describes the Erlang process overhead (verified as optimized) but ignores the Kernel TCP buffers and SSL implementation overhead (~30-50KB/connection). 1M SSL users would consume >30GB RAM, exceeding the 18GB limit in `test_limits.py`.

### D. Operational Reality (FAIL)
1.  **Thundering Herd on Disconnect**: In `iris_edge_conn:terminate/3`, a disconnecting process makes a synchronous `rpc:call` to the Core node to store offline messages. If a network partition drops 500k users, the Core node receives 500k simultaneous RPC calls, likely crashing the distribution port or the Core node handler.

## 4. Closing Judgment
The system is **Sound in Micro-Design** (good individual Erlang coding practices) but **Catastrophically Flawed in Macro-Architecture**.

It attempts to implement "WhatsApp-scale" features (Circuit Breakers, Flow Control, Sharding) but implements them attempting to use Centralized patterns (Singletons) instead of Distributed patterns (Per-Scheduler or Per-Key).

**Verdict**: The codebase requires a fundamental re-architecture of the control plane (removing singletons) and the data plane (fixing sharding and storage) before it is safe for *any* production traffic.
