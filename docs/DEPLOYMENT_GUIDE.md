# Production Deployment Guide

**Last Updated**: 2026-01-27

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

### Critical: Mnesia Memory Requirements

**WARNING**: Mnesia `disc_copies` tables load ALL data into RAM on node startup.

| Data Size | Required RAM | Recommendation |
|-----------|--------------|----------------|
| < 8 GB | 16 GB | Standard deployment |
| 8-32 GB | 64 GB | Large deployment |
| > 32 GB | Consider sharding | Multi-region required |

**Failure Mode**: If dataset exceeds available RAM, the node will OOM during Mnesia table loading and fail to start.

**Monitoring**: Track `mnesia:table_info(TableName, memory)` for all tables. Alert if total approaches 70% of available RAM.

```erlang
%% Check total Mnesia memory usage (in words, multiply by 8 for bytes)
lists:sum([mnesia:table_info(T, memory) || T <- mnesia:system_info(tables)]).
```

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
        %% WARNING: cp mode is EXPERIMENTAL - not recommended for production
        %% See DECISIONS.md Section 7 for CP mode roadmap
        {consistency_mode, hardened_ap},
        
        %% P1-H1: Split-brain protection (REQUIRED for production)
        %% List all expected core nodes - partition guard will block writes
        %% when quorum is lost to prevent data divergence
        {expected_cluster_nodes, ['core1@host1', 'core2@host2', 'core3@host3']},
        
        %% P1-H6: WAL directory for durable batcher
        %% MUST be on persistent storage (NOT tmpfs/ramfs)
        %% Default: data/wal (relative to application directory)
        {wal_directory, "/var/lib/iris/wal"}
    ]},
    
    {iris_edge, [
        %% Auto-tune router pool (default) or set explicitly
        %% {router_pool_size, 16},
        
        %% P0-C4: JWT Authentication (REQUIRED for production)
        %% Secret MUST be at least 32 bytes and identical across all nodes
        %% Without this, auth will fail when users connect to different nodes
        {jwt_secret, <<"CHANGE_ME_TO_32_BYTES_OR_MORE!!!">>},
        {auth_enabled, true},
        
        %% For testing ONLY - allows random secret generation
        %% NEVER enable in production - causes auth failures
        %% {allow_random_secret, true}
    ]}
].
```

### Critical Configuration Notes

#### JWT Secret (P0-C4)
- **MUST** be at least 32 bytes
- **MUST** be identical across all nodes in the cluster
- If not configured with `allow_random_secret=false`, startup will fail
- Random secrets cause auth failures when users reconnect to different nodes

#### Expected Cluster Nodes (P1-H1)
- **MUST** list all expected core nodes for split-brain protection
- Partition guard will block writes when fewer than half the nodes are reachable
- Without this, silent data divergence can occur during network partitions

#### WAL Directory (P1-H6)
- **MUST** be on persistent storage (real disk, not tmpfs)
- `/tmp` is often tmpfs on modern Linux - DO NOT use for WAL
- System will warn on startup if WAL appears to be on tmpfs

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

### Required for Production
- [ ] **JWT secret**: 32+ bytes, identical across all nodes (P0-C4)
- [ ] **Expected cluster nodes**: Listed for partition guard (P1-H1)
- [ ] **WAL directory**: On persistent storage, not tmpfs (P1-H6)
- [ ] **TLS certificates**: Configured for client connections
- [ ] **Erlang cookie**: Secured (`chmod 400 ~/.erlang.cookie`)
- [ ] **`allow_table_nuke`**: Set to `false`

### Recommended
- [ ] mTLS configured for inter-node communication
- [ ] Firewall rules: 4369 (epmd), 9000-9010 (distribution)
- [ ] Monitoring for partition events
- [ ] Token revocation propagation verified across nodes (P1-H2)

## Multi-Region Deployment

For global deployments, see [CLUSTER_SETUP.md](CLUSTER_SETUP.md) for:

- Regional routing configuration
- Cross-region communication
- Quorum write setup
- CP mode (Raft) setup
