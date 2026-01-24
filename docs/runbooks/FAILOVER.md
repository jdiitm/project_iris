# Failover Runbook

**Last Updated**: 2026-01-24

## Node Failure Scenarios

### Single Edge Node Failure

**Impact**: Users on that edge lose connection, other edges continue.

**Detection**:
```bash
# Check if edge is running
epmd -names | grep edge

# Or via Docker
docker ps | grep edge
```

**Recovery**:
```bash
# Restart failed edge
make start_edge1

# Verify mesh
erl -sname check -setcookie iris_secret -eval "
  net_adm:ping('iris_edge1@$(hostname -s)'),
  init:stop()."
```

**No data loss** - Edge is stateless.

---

### Single Core Node Failure

**Impact**: Reduced capacity, quorum may still be available.

**Detection**:
```bash
epmd -names | grep core

# Check from remaining node
erl -sname check -setcookie iris_secret -eval "
  io:format('Nodes: ~p~n', [nodes()]),
  init:stop()."
```

**Check Quorum Status**:
```erlang
%% On a surviving node
iris_quorum_write:get_replicas(<<"test_key">>).
%% If 2+ replicas available, quorum writes still succeed
```

**Recovery**:
```bash
# Restart core
make start_core

# Wait for Mnesia recovery
sleep 10

# Verify tables
erl -sname check -setcookie iris_secret -eval "
  rpc:call('iris_core@$(hostname -s)', mnesia, system_info, [tables]),
  init:stop()."
```

**Data safety**: With quorum writes (RF=3), 1 node failure causes NO data loss.

---

### Network Partition (Split-Brain)

**Impact**: Cluster divides into groups that can't communicate.

**Detection**:

Iris automatically detects partitions via `iris_partition_guard`:

```erlang
%% Check partition status
iris_partition_guard:is_safe_for_writes().
%% Returns: ok | {error, partition_detected}
```

**Symptoms**:
- Log message: "PARTITION DETECTED - entering safe mode"
- Writes blocked on minority side
- Reads continue but may be stale

**Automatic Behavior**:
1. Partition guard detects < 50% of expected nodes visible
2. Enters "safe mode" - writes are blocked
3. Reads continue (eventual consistency)
4. When partition heals, automatically exits safe mode

**Manual Verification**:
```erlang
%% On each side of partition
mnesia:system_info(running_db_nodes).
%% Should show different nodes on each side
```

**Recovery**:

Usually automatic when network heals. If data diverged:

1. **Identify authoritative node** (most writes during partition)
   ```erlang
   mnesia:table_info(offline_msg, size).
   ```

2. **Resync from authoritative**
   ```erlang
   %% On non-authoritative node
   mnesia:del_table_copy(offline_msg, node()).
   mnesia:add_table_copy(offline_msg, node(), disc_copies).
   ```

---

### Complete Cluster Failure

**Impact**: All functionality down.

**Recovery Procedure**:

1. **Stop everything**
   ```bash
   make stop
   killall -9 beam.smp
   ```

2. **Verify Mnesia data**
   ```bash
   ls -la Mnesia.*/
   # Should see .DAT files
   ```

3. **Start core first**
   ```bash
   make start_core
   sleep 10
   ```

4. **Verify Mnesia healthy**
   ```bash
   erl -sname check -setcookie iris_secret -eval "
     Tables = rpc:call('iris_core@$(hostname -s)', mnesia, system_info, [tables]),
     io:format('Tables: ~p~n', [Tables]),
     init:stop()."
   ```

5. **Start edges**
   ```bash
   make start_edge1
   ```

6. **Mesh cluster**
   ```bash
   make mesh
   ```

7. **Smoke test**
   ```bash
   python3 tests/suites/integration/test_online_messaging.py
   ```

---

## Regional Failover

### Single Region Failure

**Impact**: Users with home region in failed region can't send messages.

**Detection**:
```erlang
%% Check region endpoints
iris_region_router:get_region_endpoint(<<"us-east-1">>).
%% Returns: {ok, [nodes]} or {error, not_found}
```

**Automatic Behavior**:
1. Circuit breaker opens for failed region
2. Messages to failed region stored in `cross_region_queue`
3. When region recovers, messages delivered from queue

**Manual Rerouting** (optional):
```erlang
%% Temporarily redirect region
iris_region_router:set_region_endpoint(<<"us-east-1">>, ['backup@host']).
```

### Cross-Region Routing Failure

**Detection**:
```erlang
%% Check if cross-region messages are queued
mnesia:table_info(cross_region_queue, size).
```

**Recovery**:
1. Restore network connectivity
2. Queued messages will be delivered automatically

---

## CP Mode (Raft) Failover

If using `{consistency_mode, cp}`:

### Leader Failure

**Automatic**: Raft automatically elects new leader within seconds.

**Verification**:
```erlang
%% Check new leader
iris_raft:get_leader().
```

### Majority Lost

**Impact**: Raft cannot make progress, all writes fail.

**Recovery**: Restore at least majority of nodes.

```erlang
%% Check members
iris_raft:get_members().
%% Need majority available for operations
```

---

## Preventive Measures

1. **Monitor cluster health**
   ```erlang
   %% Key metrics to alert on
   length(nodes()).                        %% < expected
   iris_partition_guard:is_safe_for_writes().  %% {error, _}
   iris_quorum_write:get_replicas(Key).    %% length < quorum
   ```

2. **Regular backups**
   ```erlang
   mnesia:backup("/backup/mnesia_$(date +%Y%m%d).bak").
   ```

3. **Test failover monthly**
   - Kill random node
   - Verify quorum maintained
   - Verify recovery < 5 min

4. **Use quorum writes for critical data**
   ```erlang
   iris_store:put(Table, Key, Value, #{durability => quorum}).
   ```

5. **Monitor partition guard**
   - Alert if partition detected
   - Alert if safe_mode entered

---

## Quick Reference

| Scenario | Data Loss Risk | Recovery Time | Action Required |
|----------|---------------|---------------|-----------------|
| Single edge | None | Seconds | Auto-recover |
| Single core (with quorum) | None | < 1 minute | Auto-recover |
| Single core (no quorum) | Possible | < 5 minutes | Verify data |
| Network partition | None (blocked) | Auto on heal | Monitor |
| Complete cluster | Possible | 5-10 minutes | Manual start |
| Region failure | None (queued) | Auto on heal | Monitor queue |
