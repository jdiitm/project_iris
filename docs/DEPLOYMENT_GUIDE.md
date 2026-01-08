# Project Iris: 3-Core Cluster Deployment Guide

## Overview

This guide covers deploying Project Iris across a 3-laptop Core cluster with global Edge nodes for seamless failover.

## Core Node Specifications

| Node | Hardware | Role |
|------|----------|------|
| core1 | i7 + Ubuntu + 32GB | Primary Mnesia, Heavy Writes |
| core2 | i7 + Ubuntu + 16GB | Secondary Mnesia, Read Replica |
| core3 | M1 + Mac + 16GB | Tertiary Mnesia, Cold Standby |

## Prerequisites

### All Nodes
```bash
# Install Erlang OTP 26+
# Ubuntu:
sudo apt-get update
sudo apt-get install -y erlang

# Mac (with Homebrew):
brew install erlang

# Verify
erl -version
```

### Network Configuration
- All Core nodes must be on the same network (or VPN)
- Ports: 4369 (epmd), 9000-9010 (Erlang distribution)
- Same cookie file: `~/.erlang.cookie`

```bash
# On each Core node:
echo "iris_production_cookie_2026" > ~/.erlang.cookie
chmod 400 ~/.erlang.cookie
```

## Deployment Steps

### Step 1: Setup Core1 (Primary)

```bash
# On core1 (i7/32GB Ubuntu)
cd /opt/iris
git clone <repo> .
make clean && make

# Initialize the distributed schema
export ERL_FLAGS="+P 2000000 +Q 2000000 +K true"
erl -sname iris_core -setcookie iris_production_cookie_2026 \
    -pa ebin \
    -eval "
        iris_core:init_db(['iris_core@core1', 'iris_core@core2', 'iris_core@core3']),
        application:ensure_all_started(iris_core),
        io:format('Core1 started~n').
    "
```

### Step 2: Join Core2

```bash
# On core2 (i7/16GB Ubuntu)
cd /opt/iris
git clone <repo> .
make clean && make

erl -sname iris_core -setcookie iris_production_cookie_2026 \
    -pa ebin \
    -eval "
        iris_core:join_cluster('iris_core@core1'),
        application:ensure_all_started(iris_core),
        io:format('Core2 joined~n').
    "
```

### Step 3: Join Core3

```bash
# On core3 (M1 Mac)
cd /opt/iris
git clone <repo> .
make clean && make

erl -sname iris_core -setcookie iris_production_cookie_2026 \
    -pa ebin \
    -eval "
        iris_core:join_cluster('iris_core@core1'),
        application:ensure_all_started(iris_core),
        io:format('Core3 joined~n').
    "
```

### Step 4: Verify Cluster

```erlang
% From any Core node:
mnesia:info().
% Should show 3 running db nodes

nodes().
% Should show ['iris_core@core2', 'iris_core@core3']
```

## Edge Node Deployment (Cloud Free Tier)

### AWS Free Tier (us-east-1)

```bash
# On EC2 t2.micro
sudo apt-get install -y erlang
cd /opt/iris && git clone <repo> .
make

# Configure to connect to Core cluster
export CORE_NODES="iris_core@core1.local,iris_core@core2.local,iris_core@core3.local"

erl -name iris_edge_aws_us -setcookie iris_production_cookie_2026 \
    -pa ebin \
    -iris_edge port 8085 \
    -eval "application:ensure_all_started(iris_edge)."
```

### GCP Free Tier (asia-east1)

Same as AWS, with node name `iris_edge_gcp_asia`.

### Azure Free Tier (brazilsouth)

Same as AWS, with node name `iris_edge_azure_br`.

## Failover Behavior

### Core Node Failure
1. Edge nodes automatically discover remaining Core nodes via `pg`
2. Mnesia continues on surviving nodes
3. No manual intervention required

```
[Edge] -> get_core() -> pg:get_members(iris_core_nodes)
                     -> [core1, core2, core3]
                     -> (core1 down) -> routes to core2 or core3
```

### Edge Node Failure
1. Clients reconnect to another Edge
2. Messages in flight are moved to offline storage
3. User re-login retrieves pending messages

## Monitoring Commands

### Check Cluster Health
```erlang
% On any Core node:
[net_adm:ping(N) || N <- ['iris_core@core1', 'iris_core@core2', 'iris_core@core3']].
% Should return [pong, pong, pong]

% Check Mnesia replication:
mnesia:table_info(presence, disc_copies).
% Should return [iris_core@core1, iris_core@core2, iris_core@core3]
```

### Check Router Stats
```erlang
% On any Edge node:
iris_router:get_stats().
```

## Recovery Procedures

### Core Node Crash Recovery
```bash
# Node will auto-rejoin on restart
erl -sname iris_core -setcookie iris_production_cookie_2026 \
    -pa ebin \
    -mnesia dir '"/opt/iris/Mnesia.iris_core"' \
    -eval "
        mnesia:start(),
        application:ensure_all_started(iris_core).
    "
```

### Full Cluster Recovery (All Cores Down)
```bash
# On core1 (has most recent data):
erl -mnesia dir '"/opt/iris/Mnesia.iris_core"' \
    -eval "mnesia:start()."

# Then join core2 and core3 as normal
```

## Performance Tuning

### OS Level (All Nodes)
```bash
# /etc/sysctl.conf
net.core.somaxconn = 65535
net.ipv4.tcp_max_syn_backlog = 65535
net.core.netdev_max_backlog = 65535

# /etc/security/limits.conf
*  soft nofile 1048576
*  hard nofile 1048576
```

### Erlang VM Flags
```bash
ERL_FLAGS="+P 2000000 +Q 2000000 +K true +A 128 +S 8:8"
```

## Testing Failover

### Simulate Core Failure
```bash
# Kill core1
ssh core1 "pkill -9 -f beam"

# Verify routing continues
python3 test_iris.py  # Should pass

# Restart core1
ssh core1 "cd /opt/iris && ./scripts/start_core.sh"

# Verify cluster reformed
```

## Files Modified in This Release

| File | Description |
|------|-------------|
| `iris_edge_sup.erl` | NEW - Root supervisor, owns ETS |
| `iris_edge_app.erl` | Delegates to supervisor |
| `iris_core_registry.erl` | NEW - Dynamic Core discovery |
| `iris_router_worker.erl` | Uses registry for failover |
| `iris_session.erl` | Uses registry, reliable msg delivery |
| `iris_edge_conn.erl` | Reliable message wrapping |
| `iris_status_batcher_sup.erl` | NEW - Status batcher pool |
| `iris_core.erl` | Removed telemetry dependency |
