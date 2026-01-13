# Planetary Scale Architecture: Iris 2.0

## Target Scale

| Metric | Target |
|--------|--------|
| **Concurrent Users** | 5 billion |
| **Messages/Day** | 1 trillion |
| **Messages/Second** | ~11.6 million |
| **Nodes** | 10,000+ |
| **Countries** | 180+ |

---

## Current Architecture Limitations

The current design has synchronous cross-region RPCs:

```
Edge (Tokyo) --[sync RPC]--> Core (Bangalore)
                └── 100-200ms RTT
                └── Blocks on every lookup
                └── Can't scale past ~500 users
```

**Root Cause**: The architecture assumes co-located nodes. Global distribution breaks this.

---

## Target Architecture: Federated Regional Clusters

### Core Principles

1. **No cross-region synchronous calls** - Everything async or local
2. **Users are homed to regions** - Presence is regional, not global
3. **Messages route through async queues** - Not blocking RPC
4. **Eventually consistent state** - Accept 1-5s staleness
5. **Edge nodes are stateful** - Local caching, local decisions

---

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           GLOBAL LAYER                                  │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐        │
│  │ Region:    │  │ Region:    │  │ Region:    │  │ Region:    │        │
│  │ Tokyo      │  │ Singapore  │  │ Mumbai     │  │ Frankfurt  │  ...   │
│  └─────┬──────┘  └─────┬──────┘  └─────┬──────┘  └─────┬──────┘        │
│        │               │               │               │               │
│        └───────────────┴───────────────┴───────────────┘               │
│                              │                                          │
│                    ┌─────────┴─────────┐                               │
│                    │  KAFKA / PULSAR   │  (Cross-region msg bus)       │
│                    │  Async Replication│                               │
│                    └───────────────────┘                               │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────┐
│                         REGIONAL CLUSTER (per region)                   │
│                                                                         │
│   ┌──────────────────────────────────────────────────────────────┐     │
│   │                    EDGE LAYER (100s of nodes)                 │     │
│   │   ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │     │
│   │   │ Edge-1  │ │ Edge-2  │ │ Edge-3  │ │ Edge-N  │           │     │
│   │   │ Local   │ │ Local   │ │ Local   │ │ Local   │           │     │
│   │   │ Cache   │ │ Cache   │ │ Cache   │ │ Cache   │           │     │
│   │   └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘           │     │
│   │        └───────────┴───────────┴───────────┘                 │     │
│   │                         │                                     │     │
│   │              ┌──────────┴──────────┐                         │     │
│   │              │   REGIONAL KAFKA    │                         │     │
│   │              │   (presence events) │                         │     │
│   │              └──────────┬──────────┘                         │     │
│   └─────────────────────────┼────────────────────────────────────┘     │
│                             │                                           │
│   ┌─────────────────────────┼────────────────────────────────────┐     │
│   │                   CORE LAYER (10s of nodes)                   │     │
│   │   ┌─────────┐ ┌────────┴┐ ┌─────────┐                        │     │
│   │   │ Core-1  │ │ Core-2  │ │ Core-3  │  (Sharded by user_id) │     │
│   │   │ Shard-A │ │ Shard-B │ │ Shard-C │                        │     │
│   │   └─────────┘ └─────────┘ └─────────┘                        │     │
│   └──────────────────────────────────────────────────────────────┘     │
│                                                                         │
│   ┌──────────────────────────────────────────────────────────────┐     │
│   │                   STORAGE LAYER                               │     │
│   │   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐          │     │
│   │   │ ScyllaDB    │  │ Redis       │  │ S3/MinIO    │          │     │
│   │   │ (messages)  │  │ (sessions)  │  │ (media)     │          │     │
│   │   └─────────────┘  └─────────────┘  └─────────────┘          │     │
│   └──────────────────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Key Design Changes

### 1. Local Presence Cache (No Cross-Region Lookup)

**Before (Current)**:
```erlang
%% Edge synchronously asks Core for every message
CoreNode = get_core_node(),  % May be in another region!
iris_circuit_breaker:call(CoreNode, iris_core, lookup_user, [User])
```

**After (Proposed)**:
```erlang
%% Edge maintains LOCAL presence cache, updated via pub/sub
case ets:lookup(local_presence, User) of
    [{User, Pid}] -> 
        Pid ! {deliver_msg, Msg};  % LOCAL delivery
    [] ->
        %% User not on this Edge - route via message bus
        iris_router:publish_to_region(User, Msg)
end
```

### 2. Message Routing via Async Queue

**Before**: Synchronous RPC
```erlang
rpc:call(TargetNode, iris_core, deliver, [User, Msg])  % BLOCKS
```

**After**: Publish to Kafka/Pulsar
```erlang
iris_mq:publish(user_shard(User), {deliver, User, Msg})  % NON-BLOCKING
```

### 3. User Sharding by Region + ID

```erlang
%% User is "homed" to a region based on signup or preference
user_region(User) ->
    case ets:lookup(user_home_region, User) of
        [{User, Region}] -> Region;
        [] -> hash_to_region(User)  % Consistent hash fallback
    end.

%% Within region, shard by user_id hash
user_shard(User) ->
    erlang:phash2(User) rem num_shards().
```

### 4. Cross-Region Message Delivery

When user_A (Tokyo) messages user_B (Frankfurt):

```
1. Edge-Tokyo receives message
2. Lookup: user_B home region = Frankfurt
3. Publish to global Kafka topic: {route, frankfurt, user_B, Msg}
4. Frankfurt consumer receives, delivers locally
5. ACK propagates back via Kafka
```

**Latency**: 200-500ms acceptable for cross-region (async)

### 5. Presence Propagation

```erlang
%% On user login
handle_login(User, Socket) ->
    %% Update local ETS
    ets:insert(local_presence, {User, self()}),
    
    %% Publish presence event to regional Kafka
    iris_presence:publish({online, User, node(), self()}),
    
    %% Other edges in region subscribe and update their cache
    ok.
```

---

## Data Storage at Scale

### Message Storage: ScyllaDB (not Mnesia)

Mnesia doesn't scale beyond ~10 nodes. Use ScyllaDB:

```cql
CREATE TABLE messages (
    user_id UUID,
    msg_id TIMEUUID,
    sender_id UUID,
    content BLOB,
    created_at TIMESTAMP,
    delivered BOOLEAN,
    PRIMARY KEY ((user_id), msg_id)
) WITH CLUSTERING ORDER BY (msg_id DESC);
```

**Capacity**: 500B msgs/day = ~6PB/year at 100 bytes/msg avg

### Session Storage: Redis Cluster

```
Key: session:{user_id}
Value: {node, pid, region, last_seen}
TTL: 24 hours
```

### Presence: In-Memory + Pub/Sub

- Local ETS on each Edge (sub-millisecond lookup)
- Redis Pub/Sub for regional propagation
- Kafka for cross-region events

---

## Capacity Planning

### Per Region (10M users)

| Component | Count | Instance |
|-----------|-------|----------|
| Edge Nodes | 100 | c6i.2xlarge |
| Core Nodes | 20 | c6i.4xlarge |
| Kafka Brokers | 6 | r6i.2xlarge |
| ScyllaDB | 9 | i3.4xlarge |
| Redis | 6 | r6i.xlarge |

### Global (1B users, 50 regions)

| Component | Total Count |
|-----------|-------------|
| Edge Nodes | 5,000 |
| Core Nodes | 1,000 |
| Kafka Brokers | 300 |
| ScyllaDB | 450 |
| Redis | 300 |
| **Total Nodes** | **~7,000** |

---

## Migration Path

### Phase 1: Remove Synchronous RPCs (2-4 weeks)
- Replace `rpc:call` with async message passing
- Add local presence ETS on Edge
- Pub/sub for presence updates within cluster

### Phase 2: Add Message Queue (4-6 weeks)
- Deploy Kafka/Pulsar for message routing
- Edge publishes to queue instead of direct RPC
- Core consumes from queue

### Phase 3: Regional Clustering (8-12 weeks)
- Deploy clusters in multiple regions
- Implement user homing
- Cross-region message routing via global Kafka

### Phase 4: Storage Migration (6-8 weeks)
- Migrate from Mnesia to ScyllaDB
- Deploy Redis cluster for sessions
- Implement offline message storage

---

## Estimated Investment

| Item | Cost/Month |
|------|------------|
| Compute (7000 nodes) | $2-3M |
| Storage (6PB/year) | $500K |
| Network (global) | $300K |
| Kafka/Pulsar | $200K |
| **Total** | **~$3-4M/month** |

---

## Summary

| Aspect | Current | Target |
|--------|---------|--------|
| RPC Model | Sync | Async (Kafka) |
| Presence | Global Mnesia | Local ETS + Pub/Sub |
| Message Storage | Mnesia | ScyllaDB |
| Session Storage | Mnesia | Redis Cluster |
| Routing | Direct RPC | Message Queue |
| Scaling | ~500 users | 1B+ users |

**The fundamental change**: Move from synchronous, centralized architecture to asynchronous, federated regional clusters.
