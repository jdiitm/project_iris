# Production Deployment Guide

**Last Updated**: 2026-01-24

## Architecture Overview

```
                    ┌─────────────────┐
                    │   Load Balancer │
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
   ┌────▼────┐          ┌────▼────┐          ┌────▼────┐
   │  Edge   │          │  Edge   │          │  Edge   │
   │ (Cloud) │          │ (Cloud) │          │ (Cloud) │
   └────┬────┘          └────┬────┘          └────┬────┘
        │                    │                    │
        └────────────────────┼────────────────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
   ┌────▼────┐          ┌────▼────┐          ┌────▼────┐
   │  Core   │◄────────►│  Core   │◄────────►│  Core   │
   │(Primary)│  Mnesia  │(Replica)│  Mnesia  │(Replica)│
   └─────────┘          └─────────┘          └─────────┘
```

## Hardware Requirements

| Role | CPU | RAM | Disk | Network |
|------|-----|-----|------|---------|
| Core | 4+ cores | 16GB+ | 100GB SSD | 1Gbps |
| Edge | 2+ cores | 4GB+ | 20GB | 1Gbps |

## Deployment Steps

### 1. OS Configuration (All Nodes)

```bash
# /etc/sysctl.conf
net.core.somaxconn = 65535
net.ipv4.tcp_max_syn_backlog = 65535

# /etc/security/limits.conf
* soft nofile 1048576
* hard nofile 1048576

# Apply
sysctl -p
```

### 2. Install Erlang

```bash
# Ubuntu/Debian
apt-get install -y erlang

# Verify OTP 25+
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
```

### 3. Deploy Core Nodes

**Primary Core:**
```bash
cd /opt/iris
git clone <repo> . && make

erl -name iris_core@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core)."
```

**Secondary Cores:**
```bash
erl -name iris_core@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core), timer:sleep(5000), iris_core:join_cluster('iris_core@PRIMARY_IP')."
```

### 4. Deploy Edge Nodes

```bash
erl -name iris_edge@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -hidden \
    -pa ebin \
    -iris_edge port 8085 \
    +P 1000000 +K true \
    -eval "application:ensure_all_started(iris_edge), net_adm:ping('iris_core@CORE_IP')."
```

## Configuration Options

### Application Environment

```erlang
%% config/prod.config
[
    {iris_core, [
        %% Data safety: Require explicit opt-in to delete corrupted tables
        {allow_table_nuke, false},  %% NEVER enable in production without operator
        
        %% Replication
        {replication_factor, 3},
        
        %% Regional routing (optional)
        {region_id, <<"us-east-1">>},
        {regions, [<<"us-east-1">>, <<"eu-west-1">>]},
        
        %% Consistency mode: ap | hardened_ap | cp
        {consistency_mode, hardened_ap}
    ]},
    
    {iris_edge, [
        %% Auto-tune router pool (default) or set explicitly
        %% {router_pool_size, 16},
        
        %% Authentication
        {jwt_secret, <<"CHANGE_ME_IN_PRODUCTION">>},
        {auth_enabled, true}
    ]}
].
```

### Auto-Tuning

The system auto-tunes based on available resources:

```bash
# View auto-tune decisions
make start
# Output: [AUTO-TUNE] Detected: 24 cores, 16384 MB RAM
#         [AUTO-TUNE] Target Capacity: 1000000 Connections
```

Override with explicit settings if needed:

```erlang
{iris_edge, [{router_pool_size, 32}]}
```

## Failover Behavior

### Core Node Failure

1. Mnesia continues on surviving nodes
2. Edge nodes auto-discover remaining cores via `pg`
3. Quorum writes continue if majority available
4. No manual intervention required

### Edge Node Failure

1. Clients reconnect to another Edge via load balancer
2. In-flight messages stored offline
3. User re-login retrieves pending messages

### Partition (Split-Brain)

1. `iris_partition_guard` detects partition
2. Writes blocked on minority side
3. Reads continue (may be stale)
4. Alert: "PARTITION DETECTED - writes blocked"

## Monitoring

### Health Checks

```erlang
%% Cluster health
[net_adm:ping(N) || N <- nodes()].

%% Mnesia replication status
mnesia:table_info(offline_msg, disc_copies).

%% Router stats
iris_async_router:get_stats().

%% Connection count
iris_async_router:get_local_count().

%% Partition status
iris_partition_guard:is_safe_for_writes().

%% Quorum health
iris_quorum_write:get_replicas(<<"test_key">>).
```

### Key Metrics

| Metric | Source | Alert Threshold |
|--------|--------|-----------------|
| Connected nodes | `length(nodes())` | < expected |
| Message queue | `erlang:process_info(Pid, message_queue_len)` | > 10000 |
| Memory usage | `erlang:memory()` | > 80% |
| Partition status | `iris_partition_guard:is_safe_for_writes()` | `{error, _}` |

## Recovery Procedures

### Single Core Recovery

Node auto-recovers Mnesia data on restart:

```bash
# Just restart - data recovers automatically
erl ... -mnesia dir '"/var/lib/iris/mnesia"' ...
```

### Full Cluster Recovery

1. Start primary core first (has most recent data)
2. Join other cores one by one
3. Verify replication: `mnesia:table_info(offline_msg, disc_copies)`

### Corrupted Table Recovery

**Safe approach** (recommended):

1. Restore from backup
2. Or manually repair Mnesia files

**Emergency approach** (data loss):

```erlang
%% Enable table recreation (WILL DELETE DATA)
application:set_env(iris_core, allow_table_nuke, true).
%% Restart the node
```

## Security Checklist

- [ ] TLS certificates configured for client connections
- [ ] mTLS configured for inter-node communication (optional)
- [ ] Erlang cookie secured (`chmod 400 ~/.erlang.cookie`)
- [ ] Firewall rules: 4369 (epmd), 9000-9010 (distribution)
- [ ] JWT secret configured (not default value)
- [ ] `allow_table_nuke` set to `false`

## Multi-Region Deployment

For global deployments, see [CLUSTER_SETUP.md](CLUSTER_SETUP.md) for:

- Regional routing configuration
- Cross-region communication
- Quorum write setup
- CP mode (Raft) setup
