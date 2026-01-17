# Operational Runbooks

This directory contains incident response and operational procedures for Project Iris.

## Contents

- [INCIDENT_RESPONSE.md](INCIDENT_RESPONSE.md) - Initial incident triage and escalation
- [FAILOVER.md](FAILOVER.md) - Node failure and cluster recovery
- [DATA_RECOVERY.md](DATA_RECOVERY.md) - Message loss and data corruption recovery

## Quick Reference

### Critical Metrics to Monitor
- `iris_active_connections` - Current connection count
- `iris_messages_sent_total` - Message throughput
- `iris_errors_total` - Error rate
- `iris_memory_bytes` - Memory usage

### Emergency Commands
```bash
# Stop all nodes
make stop

# Check Erlang node status
epmd -names

# Force kill all beam processes
killall -9 beam.smp

# Clean Mnesia data (DANGER)
rm -rf Mnesia.*

# Start fresh cluster
make start
```

### Alert Thresholds
| Metric | Warning | Critical |
|--------|---------|----------|
| Memory | >16GB | >24GB |
| Error Rate | >1% | >5% |
| Latency p99 | >500ms | >2s |
| Connection Count | >800k | >1M |
