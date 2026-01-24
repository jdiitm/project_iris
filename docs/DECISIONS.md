# Architectural Decisions & Design Rationale

**Last Updated**: 2026-01-24  
**Status**: Current

---

## Executive Summary

Project Iris is a global-scale messaging platform implementing **Hardened AP** semantics with optional **CP mode** for critical data. This document records key architectural decisions and their rationale.

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

## 4. Router Pool Auto-Tuning

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

## 5. Data Safety

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

## 6. Test Infrastructure

### Decision: Deterministic Test Execution

**Key Principles**:
1. All tests use seeded random (`TEST_SEED` env var)
2. Exit codes: 0=pass, 1=fail, 2=skip
3. No CI-mode tricks that change pass/fail behavior
4. Docker as canonical execution environment

**Test Counts** (as of 2026-01-24):
- Unit tests: 220 (all passing)
- Integration tests: Covered by Python suite
- Total active tests: 50+ (see TEST_STATUS.md)

---

## 7. Deferred Work

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

## 8. Module Overview

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

## References

- [RFC-001 System Requirements](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
- [RFC-001 Amendment (E2EE + Groups)](rfc/RFC-001-AMENDMENT-001.md)
- [Test Status](TEST_STATUS.md)
- [Cluster Setup](CLUSTER_SETUP.md)
