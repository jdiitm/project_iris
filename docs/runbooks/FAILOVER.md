# Failover Runbook

## Node Failure Scenarios

### Single Edge Node Failure

**Impact**: Users on that edge lose connection, other edges continue.

**Detection**:
```bash
# Check if edge is running
epmd -names | grep edge
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

---

### Single Core Node Failure

**Impact**: All edges lose core connectivity, routing fails.

**Detection**:
```bash
epmd -names | grep core
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

## Mnesia Split-Brain Recovery

**Symptoms**: Nodes have divergent data.

**Detection**:
```erlang
% On each node
mnesia:system_info(running_db_nodes).
% Should match across cluster
```

**Recovery**:

1. **Identify master node** (most recent data)
   ```erlang
   mnesia:table_info(offline_msg, size).
   ```

2. **Stop slave nodes**
   ```bash
   # On slaves only
   make stop
   ```

3. **Delete slave Mnesia data**
   ```bash
   rm -rf Mnesia.iris_*
   ```

4. **Rejoin from master**
   ```erlang
   % On master
   mnesia:change_config(extra_db_nodes, ['slave@host']).
   ```

---

## Preventive Measures

1. **Monitor cluster health**
   - Check `iris_connected_nodes` metric
   - Alert if < expected count

2. **Regular backups**
   ```erlang
   mnesia:backup("/backup/mnesia_$(date +%Y%m%d).bak").
   ```

3. **Test failover monthly**
   - Kill random node
   - Verify recovery < 5 min
