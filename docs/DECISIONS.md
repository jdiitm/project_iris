# Architectural Decisions & Design Rationale

**Last Updated**: 2026-01-30  
**Status**: Current (RFC-001 v3.0 Aligned)

---

## Executive Summary

Project Iris is a global-scale messaging platform implementing **Hardened AP** semantics with optional **CP mode** for critical data. This document records key architectural decisions and their rationale.

**Key Architectural Principles (RFC-001 v3.0)**:
- **Server as Log**: The server is an append-only, ordered Inbox Log per user
- **Client as Oracle**: Encryption, decryption, and view state live on client
- **Sync over Push**: Synchronization is the primary primitive; push is optimization
- **User ID Partitioning**: All data partitioned by User ID (not Conversation ID)

---

## 1. Consistency Model

### Decision: Hardened AP (Default) with Optional CP

**Default Mode**: Hardened AP semantics
- Writes use `sync_transaction` for durability
- Split-brain detection via `iris_partition_guard`
- Explicit acknowledgment guarantees

**Optional CP Mode**: True linearizable consistency via Raft
- Enabled via `{consistency_mode, cp}` config
- Uses `ra` library (RabbitMQ's Raft implementation)
- Module: `iris_raft.erl`

**Rationale**:
- True CP requires replacing Mnesia (2-3 month effort)
- Hardened AP provides explicit guarantees for most use cases
- CP mode available for critical data (audit logs, financial transactions)

---

## 2. Storage Architecture

### Decision: Simplified Storage with Quorum Writes

**Primary Interface**: `iris_store.erl`
- Single API with clear durability options: `guaranteed | best_effort | quorum`
- Partition-aware writes (blocked during detected partitions)
- Legacy `iris_storage.erl` maintained for backwards compatibility

**Quorum Writes**: `iris_quorum_write.erl`
- Majority (N/2+1) of replicas must ACK before returning
- Failed replicas repaired asynchronously
- Tolerates minority failures without blocking

**Rationale**:
- `sync_transaction` waits for ALL replicas (slow node blocks everyone)
- Quorum writes provide better failure tolerance
- Simplifies storage API for developers

### Configuration

```erlang
%% Replication factor (default: 3)
{iris_core, [{replication_factor, 3}]}

%% Durability options in code:
iris_store:put(Table, Key, Value, #{durability => guaranteed})  %% sync_transaction
iris_store:put(Table, Key, Value, #{durability => quorum})      %% quorum writes
iris_store:put(Table, Key, Value, #{durability => best_effort}) %% async
```

---

## 3. Regional Scaling

### Decision: Regional Sharding via `iris_region_router`

**Architecture**:
```
┌─────────────────────────────────────────────────────────────────────┐
│                     GLOBAL ROUTING LAYER                            │
│   (iris_region_router - Routes users to home region)               │
└─────────────────────────────────────────────────────────────────────┘
                      │                    │                    │
           ┌─────────▼─────────┐ ┌────────▼────────┐ ┌─────────▼─────────┐
           │   REGION: US      │ │ REGION: EU      │ │ REGION: APAC      │
           │   Mnesia Cluster  │ │ Mnesia Cluster  │ │ Mnesia Cluster    │
           │   (50 nodes max)  │ │ (50 nodes max)  │ │ (50 nodes max)    │
           └───────────────────┘ └─────────────────┘ └───────────────────┘
```

**Scaling Math**:
- 20 regions × 50 nodes/region × 2M users/node = 2 Billion users
- Each user has a "home region" determined by `hash(UserID)`
- Cross-region messages use async bridge or direct RPC

**Rationale**:
- Mnesia uses full-mesh gossip (O(N²) at >100 nodes)
- Regional sharding limits Mnesia mesh to 50 nodes per region
- Consistent hashing ensures deterministic user-to-region mapping

### Configuration

```erlang
{iris_core, [
    {region_id, <<"us-east-1">>},
    {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
    {region_endpoints, #{
        <<"us-east-1">> => ['core@us-east-1.iris.io'],
        <<"eu-west-1">> => ['core@eu-west-1.iris.io']
    }}
]}
```

---

## 4. Partitioning Strategy (RFC-001 v3.0 Section 5.2)

### Decision: User ID Partitioning with Jump Consistent Hash + Vnodes

**Primary Decision**: Partition all user data by **User ID**, not Conversation ID.

**Rationale - Why User ID, not Conversation ID?**

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        OPERATION FREQUENCY ANALYSIS                          │
├─────────────────────────────────────────────────────────────────────────────┤
│ Operation              │ Frequency │ Partition by User │ Partition by Conv  │
├────────────────────────┼───────────┼───────────────────┼────────────────────┤
│ Sync my inbox          │ 90%       │ Single shard ✓    │ N shards (scatter) │
│ Send message           │ 8%        │ 2 shards (async)  │ Single shard ✓     │
│ Fetch conversation     │ 2%        │ Single shard ✓    │ Single shard ✓     │
└─────────────────────────────────────────────────────────────────────────────┘
```

Key insight: **"Sync my inbox"** is the dominant operation at 90% of all traffic.
- User ID partitioning makes inbox sync a single-shard query (fast, local)
- Conversation partitioning would require scatter-gather across N shards (slow, expensive)

**Hash Function: Jump Consistent Hash**

```erlang
%% Jump Consistent Hash implementation in iris_shard.erl
%% Properties:
%% - O(ln(N)) time complexity
%% - Only 1/N keys move when adding new shard (vs 100% for naive modulo)
%% - No external dependencies
jump_consistent_hash(Key, NumBuckets) ->
    Hash = erlang:phash2(Key, 16#100000000),
    jump_consistent_hash_loop(Hash, NumBuckets, -1, 0).
```

**Why Jump Consistent Hash over alternatives?**

| Algorithm | Data Movement on Scale | Memory | Complexity |
|-----------|------------------------|--------|------------|
| Modulo `hash(k) % N` | 100% (catastrophic) | O(1) | Trivial |
| Consistent Hash Ring | ~1/N | O(N) | Moderate |
| **Jump Consistent Hash** | ~1/N | O(1) | Moderate |
| Rendezvous Hash | ~1/N | O(N) | Higher |

**Virtual Nodes (Vnodes)**

**Recommendation**: 256-1024 vnodes per physical node cluster.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         VNODE DISTRIBUTION                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Physical Node 1        Physical Node 2        Physical Node 3              │
│  ┌─────────────┐        ┌─────────────┐        ┌─────────────┐              │
│  │ vnode 0-85  │        │ vnode 86-170│        │ vnode 171-255│             │
│  │ (86 vnodes) │        │ (85 vnodes) │        │ (85 vnodes)  │             │
│  └─────────────┘        └─────────────┘        └─────────────┘              │
│                                                                              │
│  Adding Node 4: Only ~64 vnodes move (25%), not 100%                        │
│  ┌─────────────┐        ┌─────────────┐        ┌─────────────┐              │
│  │ vnode 0-63  │        │ vnode 64-127│        │ vnode 128-191│             │
│  └─────────────┘        └─────────────┘        └─────────────┘              │
│                         ┌─────────────┐                                      │
│                         │vnode 192-255│  (New Node 4)                        │
│                         └─────────────┘                                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Benefits of Vnodes**:
1. **Smooth distribution**: Even with heterogeneous node capacity
2. **Minimal rebalancing**: Adding a node moves 1/(N+1) of data
3. **Failure handling**: Vnodes from failed node spread across all remaining nodes
4. **Capacity planning**: Can assign more vnodes to larger machines

**Vnode Sizing Recommendations**:

| Cluster Size | Vnodes/Cluster | Vnodes/Node | Rationale |
|--------------|----------------|-------------|-----------|
| 3-10 nodes | 256 | 26-85 | Minimum for smooth distribution |
| 10-50 nodes | 512 | 10-51 | Standard production |
| 50-100 nodes | 1024 | 10-20 | Large clusters |
| 100+ nodes | 2048+ | 20+ | Hyperscale |

**Configuration**:

```erlang
{iris_core, [
    %% Number of virtual nodes (vnodes) in the hash ring
    {vnode_count, 256},
    
    %% Replication factor (how many nodes store each vnode)
    {replication_factor, 3},
    
    %% Partitioning algorithm
    {partition_algorithm, jump_consistent_hash}  %% Options: modulo, consistent_ring, jump_consistent_hash
]}
```

### Rebalancing During Scale Operations

**Principle**: Rebalancing must NEVER impact live traffic.

**Strategy**: Background, throttled vnode migration

```erlang
%% iris_shard.erl - Rebalancing configuration
-define(REBALANCE_BATCH_SIZE, 100).      %% Keys per batch
-define(REBALANCE_DELAY_MS, 10).         %% Delay between batches
-define(REBALANCE_MAX_RATE, 10000).      %% Max keys/second
```

**Process**:
1. Calculate new vnode assignment
2. Mark affected vnodes as "migrating"
3. Copy data in background batches
4. Dual-write during migration (old + new location)
5. Switch reads to new location
6. Clean up old copies

---

## 5. Router Pool Auto-Tuning

### Decision: Dynamic Pool Size Based on Scheduler Count

**Implementation**: `iris_async_router:get_pool_size/0`

```erlang
get_pool_size() ->
    case application:get_env(iris_edge, router_pool_size) of
        {ok, Size} -> Size;  %% Explicitly configured
        _ ->
            %% Auto-tune: 75% of schedulers, bounded [4, 128]
            Schedulers = erlang:system_info(schedulers_online),
            min(128, max(4, (Schedulers * 3) div 4))
    end.
```

**Rationale**:
- Fixed pool size (was hardcoded to 8) underutilizes large machines
- Auto-tuning adapts to available cores
- Bounded to prevent over-subscription

---

## 6. Data Safety

### Decision: Safe Table Recovery with `allow_table_nuke` Gate

**Problem**: Original `nuke_and_recreate_table/1` silently deleted corrupted tables.

**Solution**: Safety gate requiring explicit opt-in

```erlang
nuke_and_recreate_table(Table) ->
    case application:get_env(iris_core, allow_table_nuke, false) of
        true ->
            %% Operator explicitly enabled - proceed
            do_nuke_and_recreate(Table);
        false ->
            %% Safe default: Crash and alert operator
            exit({table_corrupted_manual_intervention, Table})
    end.
```

**Rationale**:
- Silent data deletion is unacceptable in production
- Operator must explicitly enable nuke mode
- Crash behavior ensures issues are noticed

---

## 7. Test Infrastructure

### Decision: Deterministic Test Execution

**Key Principles**:
1. All tests use seeded random (`TEST_SEED` env var)
2. Exit codes: 0=pass, 1=fail, 2=skip
3. No CI-mode tricks that change pass/fail behavior
4. Docker as canonical execution environment

**Test Counts** (as of 2026-01-29 - Post Principal Audit):
- EUnit tests: 77 (all passing) - includes P0/P1/P2 audit coverage
- Integration tests: 22 (all passing)
- Total tests: 99+ (including stress/chaos suites)
- See [TEST_STATUS.md](TEST_STATUS.md) for current counts and detailed results

**Principal Test Audit Coverage** (Jan 29, 2026):
- P0 (Safety): State machine, idempotency tests
- P1 (Correctness): Fault injection, consensus, crypto attack tests
- P2 (Scale): Concurrency torture, 24h soak tests

---

## 8. Deferred Work

### P0 (Blocked)

| Item | Effort | Blocker |
|------|--------|---------|
| Cross-region Mnesia replication | 2-3 days | Docker volume config |
| `ra` library integration | 1-2 days | External dependency |

### P1 (Next Sprint)

| Item | Effort | Dependency |
|------|--------|------------|
| mTLS inter-node (NFR-15) | 1-2 days | Cert infrastructure |
| UUIDv7 message IDs | 3-5 days | Protocol breaking change |

### P2 (Backlog)

| Item | Effort | Dependency |
|------|--------|------------|
| 5B user architecture | 2-3 months | Storage rewrite |
| Full CP consistency | 3-6 months | New storage layer |

---

## 9. Module Overview

### New Modules (2026-01-24)

| Module | Purpose | Tests |
|--------|---------|-------|
| `iris_quorum_write` | Quorum-based writes with hot failover | `iris_quorum_write_tests.erl` |
| `iris_store` | Simplified storage API | `iris_store_tests.erl` |
| `iris_region_router` | Cross-region message routing | `iris_region_router_tests.erl` |
| `iris_raft` | CP mode via Raft consensus | `iris_raft_tests.erl` |

### Modified Modules (2026-01-24)

| Module | Change |
|--------|--------|
| `iris_core` | Added `allow_table_nuke` safety gate |
| `iris_async_router` | Added `get_pool_size/0` auto-tuning |
| `iris_edge_sup` | Dynamic router pool sizing |
| `iris_storage` | Simplified to Mnesia-only |

---

## 10. Deferred Architectural Work (Forensic Audit 2026-01-29)

The Chief Architect forensic audit identified several items requiring separate RFCs or infrastructure work. These are tracked here for future implementation.

### P1 - Cross-Region Persistent Queue

| Attribute | Value |
|-----------|-------|
| **Issue** | Messages to offline cross-region users are tried once then stored locally |
| **Impact** | Messages can be stranded on local nodes if cross-region link is down |
| **Fix** | Implement persistent queue using `disk_log` or `khepri` |
| **Effort** | 2-3 weeks |
| **Blocked By** | Design RFC required for queue semantics (FIFO vs priority, TTL, overflow) |

### P1 - Mailbox Overflow Protection (AQM)

| Attribute | Value |
|-----------|-------|
| **Issue** | "Messi Test" - 1M messages to single user causes mailbox overflow |
| **Impact** | Celebrity/popular user accounts can crash their shard process |
| **Fix** | Active Queue Management with backpressure signaling |
| **Effort** | 1-2 weeks |
| **Blocked By** | Requires RFC for drop policy (tail drop vs RED vs CoDel) |

### P2 - Network Partition Drill Infrastructure

| Attribute | Value |
|-----------|-------|
| **Issue** | No automated chaos testing for network partitions |
| **Impact** | Cannot validate partition tolerance in CI |
| **Fix** | Integrate `pumba` or `tc`-based network simulation in CI |
| **Effort** | 1 week |
| **Blocked By** | Docker infrastructure changes, CI pipeline updates |

### P3 - iris_router_pool Cleanup

| Attribute | Value |
|-----------|-------|
| **Issue** | Orphaned worker pool module not used |
| **Impact** | Dead code, maintenance burden |
| **Decision** | Deprecate (ephemeral spawn + circuit breaker sufficient) |
| **Effort** | 0.5 days |
| **Blocked By** | None (low priority cleanup) |

### Implemented Fixes (2026-01-29)

The following audit findings were addressed:

| Finding | Fix | Module |
|---------|-----|--------|
| HOL Blocking | Spawn ephemeral tasks for remote lookups | `iris_async_router.erl` |
| Mnesia Global Lock | Default to ETS presence backend | `iris_core.erl` |
| Missing Cluster Manager | Created self-healing topology manager | `iris_cluster_manager.erl` |

---

## References

- [RFC-001 System Requirements](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
- [RFC-001 Amendment (E2EE + Groups)](rfc/RFC-001-AMENDMENT-001.md)
- [Test Status](TEST_STATUS.md)
- [Cluster Setup](CLUSTER_SETUP.md)
- [5B DAU Roadmap](ROADMAP.md)
