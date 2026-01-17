# Incident Response Runbook

## Priority Levels

| Level | Description | Response Time |
|-------|-------------|---------------|
| P0 | Service Down | Immediate |
| P1 | Major Degradation | 15 min |
| P2 | Partial Impact | 1 hour |
| P3 | Minor Issue | Next business day |

---

## P0: Complete Service Outage

### Symptoms
- No clients can connect
- All health checks failing
- Monitoring shows 0 active connections

### Immediate Actions (First 5 minutes)

1. **Check node status**
   ```bash
   epmd -names
   ps aux | grep beam
   ```

2. **Check ports**
   ```bash
   lsof -i :8085  # Edge node
   lsof -i :4369  # EPMD
   ```

3. **Check logs**
   ```bash
   tail -100 edge1.log
   tail -100 core.log
   ```

4. **Attempt restart**
   ```bash
   make stop
   sleep 5
   make start
   ```

5. **Verify recovery**
   ```bash
   # Simple login test
   python3 -c "
   from tests.utilities import IrisClient
   c = IrisClient()
   c.login('test')
   print('SUCCESS')
   "
   ```

### Escalation
- If not recovered in 15 minutes: Page on-call SRE
- If not recovered in 30 minutes: Page service owner

---

## P1: High Latency / Partial Failures

### Symptoms
- Elevated latency (>500ms p99)
- Some messages failing
- Connection timeouts

### Diagnosis

1. **Check system resources**
   ```bash
   top -b -n 1 | head -20
   free -h
   df -h
   ```

2. **Check Erlang metrics**
   ```bash
   # Connect to running node
   erl -sname admin -remsh iris_edge1@$(hostname -s) -setcookie iris_secret
   
   # In Erlang shell:
   erlang:memory().
   erlang:system_info(process_count).
   length(ets:all()).
   ```

3. **Check Mnesia status**
   ```erlang
   mnesia:info().
   mnesia:system_info(tables).
   ```

### Common Fixes

**Memory pressure:**
```bash
# Force GC on all processes
erl -sname admin -eval "
  [erlang:garbage_collect(P) || P <- erlang:processes()],
  init:stop()."
```

**Mnesia overload:**
```erlang
% Increase transaction pool
mnesia:system_info(transaction_failures).
```

---

## P2: Message Delivery Issues

### Symptoms
- Some users not receiving messages
- Offline messages not delivered
- Duplicate messages

### Diagnosis

1. **Check offline storage**
   ```erlang
   mnesia:table_info(offline_msg, size).
   mnesia:dirty_all_keys(offline_msg).
   ```

2. **Check presence**
   ```erlang
   mnesia:dirty_all_keys(presence).
   ```

3. **Check specific user**
   ```erlang
   User = <<"problematic_user">>,
   mnesia:dirty_read(offline_msg, User).
   ```

### Fixes

**Clear stuck messages:**
```erlang
% WARNING: Data loss
mnesia:dirty_delete(offline_msg, <<"stuck_user">>).
```

**Force redeliver:**
```erlang
iris_offline_storage:redeliver(<<"user_id">>).
```

---

## Contact Information

| Role | Contact |
|------|---------|
| Primary On-Call | pager: #iris-oncall |
| Secondary | slack: #iris-ops |
| Escalation | email: iris-team@ |
