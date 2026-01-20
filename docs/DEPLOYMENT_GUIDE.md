# Production Deployment Guide

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

## Failover Behavior

### Core Node Failure
- Mnesia continues on surviving nodes
- Edge nodes auto-discover remaining cores via `pg`
- No manual intervention required

### Edge Node Failure
- Clients reconnect to another Edge via load balancer
- In-flight messages stored offline
- User re-login retrieves pending messages

## Monitoring

```erlang
%% Cluster health
[net_adm:ping(N) || N <- nodes()].

%% Mnesia replication status
mnesia:table_info(offline_msg, disc_copies).

%% Router stats
iris_router:get_stats().

%% Connection count
ets:info(local_presence_v2, size).
```

## Recovery Procedures

### Single Core Recovery
Node auto-recovers Mnesia data on restart:
```bash
# Just restart - data recovers automatically
erl ... -mnesia dir '"/var/lib/iris/mnesia"' ...
```

### Full Cluster Recovery
Start primary core first (has most recent data), then join others.

## Security Checklist

- [ ] TLS certificates configured
- [ ] Erlang cookie secured (`chmod 400 ~/.erlang.cookie`)
- [ ] Firewall rules: 4369 (epmd), 9000-9010 (distribution)
- [ ] JWT secret configured in `config/prod.config`
