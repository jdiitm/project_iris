# Cluster Setup Guide

**Last Updated**: 2026-01-24

## Quick Start (Docker)

```bash
# Start 5-region cluster (6 cores, 11 edges)
make cluster-up

# Run distributed tests
python3 tests/run_tests.py --all

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

## Multi-Region Setup

### Configuration

For multi-region deployments, configure regional routing:

```erlang
%% config/prod.config or sys.config
[
    {iris_core, [
        {region_id, <<"us-east-1">>},
        {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
        {region_endpoints, #{
            <<"us-east-1">> => ['core@us-east-1.example.com'],
            <<"eu-west-1">> => ['core@eu-west-1.example.com'],
            <<"ap-south-1">> => ['core@ap-south-1.example.com']
        }},
        %% Quorum writes for durability
        {replication_factor, 3}
    ]}
].
```

### Region Routing

Messages are routed based on user's home region:

```erlang
%% Automatic routing via iris_region_router
iris_region_router:route_to_user(UserId, Msg).

%% Get user's home region
iris_region_router:get_home_region(UserId).
%% Returns: <<"us-east-1">> (deterministic based on hash)
```

### Cross-Region Communication

1. **Direct RPC** (low latency, requires connectivity)
2. **Bridge Mode** (async, for high-latency regions)

```erlang
%% Force specific strategy
iris_region_router:route_to_user(UserId, Msg, #{strategy => bridge}).
```

## Quorum Writes

For enhanced durability, use quorum writes:

```erlang
%% Write with majority acknowledgment
iris_store:put(offline_msg, Key, Value, #{durability => quorum}).

%% Read with linearizable consistency
iris_store:get(offline_msg, Key, #{consistency => quorum}).
```

## Auto-Tuning

The router pool size is auto-tuned based on available cores:

```erlang
%% Check current pool size
iris_async_router:get_pool_size().

%% Override if needed
{iris_edge, [{router_pool_size, 16}]}
```

## CP Mode (Raft)

For use cases requiring true linearizable consistency:

```erlang
%% Enable CP mode
{iris_core, [{consistency_mode, cp}]}

%% Start Raft cluster
iris_raft:start_cluster([node() | OtherNodes]).

%% Linearizable operations
iris_raft:put(audit_log, Key, Value).
iris_raft:get(audit_log, Key).
```

**Note**: Requires `ra` library to be installed.

## Verification

```erlang
%% Check cluster
mnesia:system_info(running_db_nodes).
nodes(connected).

%% Check routing
iris_async_router:get_stats().

%% Check regional config
iris_region_router:get_current_region().
iris_region_router:get_all_regions().

%% Check replication
iris_quorum_write:get_replicas(<<"test_key">>).
```

## Troubleshooting

**Edge can't reach Core**: Hidden nodes don't auto-reconnect. Run `net_adm:ping('core_node').`

**Data lost after restart**: Ensure `-mnesia dir` points to persistent storage.

**Tables missing**: Check `mnesia:system_info(directory)` matches your config.

**Quorum not reached**: Check that enough nodes are available with `iris_quorum_write:get_replicas/1`.

**Cross-region routing fails**: Verify `region_endpoints` config and network connectivity.

## Safety Features

### Table Corruption Recovery

By default, corrupted tables will crash the node (safe behavior). To enable automatic table recreation:

```erlang
%% WARNING: Enables data deletion on corruption!
{iris_core, [{allow_table_nuke, true}]}
```

### Partition Guard

Split-brain detection is automatic. During detected partitions:
- Writes are blocked
- Reads return stale data
- Node logs warning messages

Check partition status:

```erlang
iris_partition_guard:is_safe_for_writes().
```
