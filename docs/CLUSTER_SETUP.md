# Cluster Setup Guide

## Quick Start (Docker)

```bash
# Start 5-region cluster (6 cores, 11 edges)
make cluster-up

# Run distributed tests
make cluster-test

# Stop cluster
make cluster-down

# Full cleanup (removes volumes)
make cluster-clean
```

## Docker Cluster Architecture

| Region | Cores | Edges | Ports |
|--------|-------|-------|-------|
| East | core-east-1, core-east-2 | edge-east-1, edge-east-2 | 8085, 8086 |
| West | core-west-1, core-west-2 | edge-west-1, edge-west-2 | 8087, 8088 |
| EU | core-eu-1, core-eu-2 | edge-eu-1, edge-eu-2 | 8089, 8094 |
| Sydney | - | edge-sydney-1, edge-sydney-2 | 8090, 8091 |
| Sao Paulo | - | edge-saopaulo | 8092 |

## Manual Setup (Bare Metal)

### Prerequisites

- Erlang OTP 25+
- Same cookie: `echo "iris_secret" > ~/.erlang.cookie && chmod 400 ~/.erlang.cookie`

### Start Primary Core

```bash
erl -sname iris_core -setcookie iris_secret -pa ebin \
    -mnesia dir '"/data/mnesia"' \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core)."
```

### Join Secondary Cores

```bash
erl -sname iris_core -setcookie iris_secret -pa ebin \
    -mnesia dir '"/data/mnesia"' \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core), timer:sleep(5000), iris_core:join_cluster('iris_core@primary')."
```

### Start Edge Nodes

```bash
erl -sname iris_edge -setcookie iris_secret -hidden -pa ebin \
    -iris_edge port 8085 \
    -eval "application:ensure_all_started(iris_edge), net_adm:ping('iris_core@core')."
```

## Verification

```erlang
%% Check cluster
mnesia:system_info(running_db_nodes).
nodes(connected).

%% Check routing
iris_router:get_stats().
```

## Troubleshooting

**Edge can't reach Core**: Hidden nodes don't auto-reconnect. Run `net_adm:ping('core_node').`

**Data lost after restart**: Ensure `-mnesia dir` points to persistent storage.

**Tables missing**: Check `mnesia:system_info(directory)` matches your config.
