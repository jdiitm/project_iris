# Data Recovery Runbook

**Last Updated**: 2026-01-24

## Message Loss Investigation

### Step 1: Determine Scope

```erlang
% Count offline messages
mnesia:table_info(offline_msg, size).

% Check specific user
User = <<"affected_user">>,
Msgs = mnesia:dirty_read(offline_msg, User),
length(Msgs).

% Check quorum health
iris_quorum_write:get_replicas(User).
```

### Step 2: Check Disk Logs

```bash
# Find Mnesia transaction logs
ls -la Mnesia.*/LATEST.LOG

# Check for corruption
strings Mnesia.*/offline_msg.DAT | head
```

### Step 3: Verify WAL

```erlang
% Check durable batcher status
iris_durable_batcher:get_stats().
```

### Step 4: Check Partition Status

```erlang
% Were writes blocked during partition?
iris_partition_guard:is_safe_for_writes().
```

---

## Recovery Procedures

### From Mnesia Backup

```erlang
% Stop writes first
application:stop(iris_core).

% Restore backup
mnesia:restore("/backup/mnesia_20260117.bak", [{default_op, recreate_tables}]).

% Restart
application:start(iris_core).
```

### From WAL

```erlang
% Force WAL replay
iris_durable_batcher:replay_wal().
```

### Manual Message Injection

```erlang
% If you have message content from logs
User = <<"user_id">>,
Msg = #{from => <<"sender">>, body => <<"content">>},
iris_offline_storage:store(User, Msg, 1).

% Or use the new store API
iris_store:put(offline_msg, {User, erlang:system_time()}, Msg, #{durability => guaranteed}).
```

---

## Automatic Recovery on Restart

Iris Core automatically recovers Mnesia data when a node restarts. The `init_db()` 
function detects existing data in the Mnesia directory and recovers it instead of 
creating a fresh schema.

### How It Works

1. On startup, checks for `schema.DAT` in Mnesia directory
2. If found: starts Mnesia and waits for tables to load from disk
3. If not found: creates fresh schema (seed node) or joins cluster

### Verifying Recovery

```erlang
%% Check tables loaded
mnesia:system_info(tables).
%% Expected: [presence, offline_msg, user_meta, user_status, revoked_tokens, schema]

%% Check message count
mnesia:table_info(offline_msg, size).

%% Verify a specific user's messages
iris_core:retrieve_offline(<<"user_id">>).
```

### Docker Volume Persistence

For Docker deployments, ensure Mnesia data is persisted:

```yaml
# docker-compose.yml
volumes:
  - mnesia-data:/data/mnesia

# In command, specify directory:
erl ... -mnesia dir '"/data/mnesia"' ...
```

### Edge Reconnection After Core Restart

When a core node restarts, hidden edge nodes don't auto-reconnect. Either:

1. **Wait for traffic** - Edge will reconnect on next routing attempt
2. **Force reconnect**:
   ```erlang
   %% On edge node
   net_adm:ping('core_east_1@coreeast1').
   ```

---

## Corruption Recovery

### Symptoms
- Mnesia errors in logs
- Tables not loading
- Checksum failures

### SAFE Approach (Recommended)

By default, Iris will **crash** when it detects a corrupted table rather than silently delete data. This is the safe behavior.

**When you see this error:**
```
CRITICAL: Table offline_msg corrupted!
Manual intervention required.
Options:
  1. Fix: Restore from backup
  2. Risk: Set allow_table_nuke=true and restart
```

**Recommended action:**
1. Restore from the most recent backup
2. Verify message counts match expected

### Emergency Table Recreation (DATA LOSS)

⚠️ **WARNING: Only use if backup is not available and data loss is acceptable**

```erlang
%% Enable table recreation (DANGEROUS)
application:set_env(iris_core, allow_table_nuke, true).

%% Restart the affected node - corrupted table will be deleted and recreated
init:restart().

%% IMMEDIATELY disable after restart
application:set_env(iris_core, allow_table_nuke, false).
```

### Full Reset (Complete Data Loss)

⚠️ **WARNING: This deletes ALL data**

```bash
# Stop cluster
make stop

# Backup current data (even if corrupt)
mv Mnesia.* /backup/corrupt_$(date +%s)/

# Fresh start
make start
```

### Partial Table Recovery

```erlang
% Delete and recreate single table
mnesia:delete_table(offline_msg).
mnesia:create_table(offline_msg, [
    {disc_copies, [node()]},
    {attributes, [key, timestamp, msg]},
    {type, bag}
]).
```

---

## Quorum-Based Recovery

If using quorum writes, data may be recoverable from other replicas:

### Check Replica Health

```erlang
%% Find all replicas for a key
Key = <<"affected_user">>,
Replicas = iris_quorum_write:get_replicas(Key).
%% Returns: [node1@host, node2@host, node3@host]
```

### Read from Specific Replica

```erlang
%% If local data is corrupted, try reading from another replica
Node = 'node2@host',
rpc:call(Node, mnesia, dirty_read, [offline_msg, Key]).
```

### Repair from Healthy Replica

```erlang
%% Copy data from healthy node to corrupted node
HealthyNode = 'node2@host',
{ok, Data} = rpc:call(HealthyNode, mnesia, dirty_read, [offline_msg, Key]),
%% Write to local node
iris_store:put(offline_msg, Key, Data, #{durability => guaranteed}).
```

---

## Prevention

1. **Regular backups**
   ```bash
   # Daily backup cron
   0 3 * * * /scripts/backup_mnesia.sh
   ```

2. **Monitor disk space**
   - Alert if < 10GB free
   - Mnesia needs 2x table size for operations

3. **Use quorum writes for critical data**
   ```erlang
   iris_store:put(critical_data, Key, Value, #{durability => quorum}).
   ```

4. **Test recovery quarterly**
   - Restore from backup
   - Verify message counts match

5. **Never enable `allow_table_nuke` permanently**
   - Only enable for emergency recovery
   - Disable immediately after use

---

## CP Mode Recovery (Raft)

If using CP mode (`consistency_mode = cp`), recovery is handled by Raft:

### Check Raft Status

```erlang
%% Get current leader
iris_raft:get_leader().

%% Get cluster members
iris_raft:get_members().
```

### Raft Data Recovery

Data is automatically recovered as long as a majority of nodes are available:

```erlang
%% Linearizable read (reads from leader)
iris_raft:get(critical_table, Key).
```

If majority is lost, manual intervention is required - contact system administrator.
