# Operations Guide

**Last Updated**: 2026-02-03 | **TLS Enforced**

## Quick Reference

### Emergency Commands

```bash
# Stop all nodes
make stop

# Force kill all Erlang
killall -9 beam.smp

# Check node status
epmd -names

# Clean Mnesia (DATA LOSS)
rm -rf Mnesia.* /tmp/mnesia*

# Fresh start
make start
```

### Alert Thresholds

| Metric | Warning | Critical |
|--------|---------|----------|
| Memory | >16GB | >24GB |
| Error Rate | >1% | >5% |
| Latency p99 | >500ms | >2s |
| Connections | >800k | >1M |

---

## Incident Response

### Priority Levels

| Level | Description | Response |
|-------|-------------|----------|
| P0 | Service Down | Immediate |
| P1 | Major Degradation | 15 min |
| P2 | Partial Impact | 1 hour |

### P0: Complete Outage

1. **Check nodes**: `epmd -names && ps aux | grep beam`
2. **Check ports**: `lsof -i :8085`
3. **Check logs**: `tail -100 edge1.log core.log`
4. **Restart**: `make stop && sleep 5 && make start`
5. **Verify**: Quick login test

### P1: High Latency

1. **System resources**: `top -b -n 1 | head -20`
2. **Erlang metrics**: `erlang:memory()`, `erlang:system_info(process_count)`
3. **Mnesia**: `mnesia:info()`

**Memory pressure fix**:
```erlang
[erlang:garbage_collect(P) || P <- erlang:processes()].
```

### P2: Message Delivery Issues

```erlang
%% Check offline storage
mnesia:table_info(offline_msg, size).

%% Check specific user
mnesia:dirty_read(offline_msg, <<"user_id">>).

%% Force redeliver
iris_offline_storage:redeliver(<<"user_id">>).
```

---

## Failover Scenarios

### Single Edge Failure

**Impact**: Users reconnect to other edges  
**Recovery**: Auto (stateless)  
**Data Loss**: None

```bash
make start_edge1  # Restart failed edge
```

### Single Core Failure

**Impact**: Reduced capacity  
**Recovery**: Auto if quorum available  
**Data Loss**: None (with quorum writes)

```erlang
%% Check quorum
iris_quorum_write:get_replicas(<<"test_key">>).
%% 2+ replicas = quorum still available
```

### Network Partition (Split-Brain)

**Detection**: Automatic via `iris_partition_guard`
```erlang
iris_partition_guard:is_safe_for_writes().
%% {error, partition_detected} = writes blocked
```

**Behavior**:
- Minority side: Writes blocked, reads continue (stale)
- Majority side: Full operation
- Recovery: Automatic when network heals

### Complete Cluster Failure

1. `make stop && killall -9 beam.smp`
2. Verify Mnesia data: `ls -la Mnesia.*/`
3. Start core first: `make start_core && sleep 10`
4. Verify: `mnesia:system_info(tables)`
5. Start edges: `make start_edge1`
6. Mesh: `make mesh`

---

## Data Recovery

### From Mnesia Backup

```erlang
application:stop(iris_core).
mnesia:restore("/backup/mnesia.bak", [{default_op, recreate_tables}]).
application:start(iris_core).
```

### From WAL

```erlang
iris_durable_batcher:replay_wal().
```

### Corruption Recovery

**Safe (recommended)**: Restore from backup

**Emergency (data loss)**:
```erlang
%% Enable table recreation
application:set_env(iris_core, allow_table_nuke, true).
init:restart().
%% IMMEDIATELY disable after
application:set_env(iris_core, allow_table_nuke, false).
```

### Quorum-Based Recovery

```erlang
%% Find replicas
Replicas = iris_quorum_write:get_replicas(Key).

%% Read from healthy replica
rpc:call(HealthyNode, mnesia, dirty_read, [Table, Key]).
```

---

## Cross-Region Operations

### Architecture

```
GLOBAL ROUTING (iris_region_router)
         │
    ┌────┼────┐
    ▼    ▼    ▼
 US-EAST  EU   APAC   (Mnesia clusters, 50 nodes max each)
    │    │    │
    └────┴────┘
         │
  iris_region_bridge (async relay)
```

### Diagnostics

```erlang
%% Bridge stats
iris_region_bridge:get_stats().
%% #{sent => N, delivered => N, failed => N, queue_depth => N}

%% Region connectivity
iris_region_router:get_region_endpoint(<<"eu-west-1">>).

%% Queue depth
iris_region_bridge:get_queue_depth(<<"eu-west-1">>).
```

### High Queue Depth

Indicates messages not reaching target region:

1. Check network: `ping target-region`
2. Check Erlang: `net_adm:ping('core@eu-west-1').`
3. Check target health: `rpc:call(Node, iris_core, get_status, [<<"test">>], 5000).`

### Dead Letter Queue

```erlang
%% View dead letters
mnesia:dirty_all_keys(cross_region_dead_letter).

%% Replay message
[Msg] = mnesia:dirty_read(cross_region_dead_letter, MsgId),
mnesia:dirty_delete(cross_region_dead_letter, MsgId),
iris_region_bridge:send_cross_region(Msg#outbound_msg.target_region, 
                                      Msg#outbound_msg.user_id, 
                                      Msg#outbound_msg.msg).
```

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

## Scaling

### When to Scale

| Metric | Threshold | Action |
|--------|-----------|--------|
| Node CPU | >70% sustained | Add node |
| Node RAM | >70% | Add node or increase RAM |
| Region nodes | >40 | Plan new region |
| P99 latency | >50ms | Add node or optimize |
| Cross-region latency | >200ms | Add closer region |

### Capacity Planning

- Each node: ~40K concurrent users (practical limit)
- 50-node region: ~2M users
- Mnesia mesh: O(N²) gossip - hard limit at 50 nodes

### Adding a Node

1. **Verify cluster health**: `nodes()`, `mnesia:system_info(tables)`
2. **Start new node** (without joining)
3. **Join cluster**:
```erlang
net_adm:ping('iris_core@EXISTING').
mnesia:change_config(extra_db_nodes, ['iris_core@EXISTING']).
mnesia:change_table_copy_type(schema, node(), disc_copies).
[mnesia:add_table_copy(T, node(), disc_copies) || T <- Tables].
application:ensure_all_started(iris_core).
```
4. **Update config**: Add to `expected_cluster_nodes` on all nodes

### Adding a Region

1. Initialize new Mnesia cluster (3+ nodes)
2. Configure `region_endpoints` on all regions
3. Start `iris_region_bridge`
4. Deploy edge nodes
5. Test: `python3 tests/suites/chaos_dist/test_cross_region_latency.py`

### Removing a Node

```erlang
mnesia:stop().  %% On node being removed
%% On another node:
mnesia:del_table_copy(schema, 'iris_core@NODE_TO_REMOVE').
[mnesia:del_table_copy(T, 'iris_core@NODE_TO_REMOVE') || T <- Tables].
```

---

## Monitoring

### Key Metrics

```erlang
%% Cluster health
length(nodes()).
iris_partition_guard:is_safe_for_writes().

%% Connections
iris_async_router:get_local_count().

%% Router stats  
iris_async_router:get_stats().

%% Quorum health
iris_quorum_write:get_replicas(<<"test_key">>).

%% Memory
erlang:memory().

%% Mnesia
mnesia:table_info(offline_msg, size).
```

### Alerts to Configure

| Condition | Severity |
|-----------|----------|
| `length(nodes()) < expected` | Critical |
| `iris_partition_guard:is_safe_for_writes() = {error, _}` | Critical |
| Queue depth > 10000 | Warning |
| Queue depth > 100000 | Critical |
| Dead letter queue > 1000 | Warning |

---

## Prevention Checklist

1. **Regular backups**: `mnesia:backup("/backup/mnesia_$(date).bak")`
2. **Monitor disk space**: Alert if < 10GB free
3. **Quorum writes for critical data**: `#{durability => quorum}`
4. **Test failover monthly**: Kill random node, verify recovery < 5 min
5. **Never enable `allow_table_nuke` permanently**
6. **Monitor certificate expiry**: Alert 30 days before expiration

---

## Troubleshooting

### TLS Connection Issues

**Connection refused / TLS handshake failed**:
```bash
# Verify TLS is working
openssl s_client -connect localhost:8085 -CAfile certs/ca.pem

# Check server is listening with TLS
ss -tlnp | grep 8085

# Check server started with TLS config
ps aux | grep beam.smp | grep test_tls
```

**Certificate verification failed**:
```bash
# Check certificate validity
openssl x509 -in certs/edge-east-1.pem -noout -dates

# Verify CA chain
openssl verify -CAfile certs/ca.pem certs/edge-east-1.pem
```

### General Issues

**Edge can't reach Core**: Hidden nodes don't auto-reconnect.
```erlang
net_adm:ping('core_node').
```

**Data lost after restart**: Ensure `-mnesia dir` points to persistent storage.

**Tables missing**: Check `mnesia:system_info(directory)` matches config.

**Quorum not reached**: Check nodes available with `iris_quorum_write:get_replicas/1`.

**Cross-region routing fails**: Verify `region_endpoints` config and network.
