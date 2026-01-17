# Data Recovery Runbook

## Message Loss Investigation

### Step 1: Determine Scope

```erlang
% Count offline messages
mnesia:table_info(offline_msg, size).

% Check specific user
User = <<"affected_user">>,
Msgs = mnesia:dirty_read(offline_msg, User),
length(Msgs).
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
```

---

## Corruption Recovery

### Symptoms
- Mnesia errors in logs
- Tables not loading
- Checksum failures

### Nuclear Option (Full Reset)

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

## Prevention

1. **Regular backups**
   ```bash
   # Daily backup cron
   0 3 * * * /scripts/backup_mnesia.sh
   ```

2. **Monitor disk space**
   - Alert if < 10GB free
   - Mnesia needs 2x table size for operations

3. **Test recovery quarterly**
   - Restore from backup
   - Verify message counts match
