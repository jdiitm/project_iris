# Scale-Out Runbook

**Last Updated**: 2026-01-29

This runbook covers procedures for scaling the Iris cluster horizontally by adding nodes or regions.

---

## Overview

Iris supports two scaling dimensions:

| Dimension | When to Use | Limit |
|-----------|-------------|-------|
| Add nodes to region | User count approaching node capacity | 50 nodes/region (Mnesia mesh limit) |
| Add new region | Region at 50 nodes OR latency optimization | 20+ regions supported |

**Rule of Thumb**: Each node handles ~40K concurrent users. A 50-node region supports ~2M users.

---

## 1. Adding Nodes to Existing Region

### Pre-Flight Checklist

- [ ] New node meets hardware requirements (see [DEPLOYMENT_GUIDE.md](../DEPLOYMENT_GUIDE.md))
- [ ] RAM exceeds current Mnesia dataset size (critical for disc_copies)
- [ ] Network connectivity to all existing nodes verified
- [ ] Same Erlang/OTP version as cluster
- [ ] Erlang cookie matches cluster (`$IRIS_COOKIE`)

### Procedure

#### Step 1: Verify Cluster Health

```bash
# On any existing core node
erl -name check@localhost -setcookie $IRIS_COOKIE -noshell -eval "
    Nodes = rpc:call('iris_core@EXISTING_NODE', erlang, nodes, []),
    io:format('Connected nodes: ~p~n', [Nodes]),
    Tables = rpc:call('iris_core@EXISTING_NODE', mnesia, system_info, [tables]),
    io:format('Tables: ~p~n', [Tables]),
    init:stop().
"
```

#### Step 2: Start New Node (Without Joining)

```bash
erl -name iris_core@NEW_NODE_IP \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "application:ensure_all_started(mnesia)."
```

#### Step 3: Join Cluster

```erlang
%% On the NEW node's Erlang shell
net_adm:ping('iris_core@EXISTING_NODE').

%% Should return 'pong'. If 'pang', check network/cookie.

%% Add tables as disc_copies
mnesia:change_config(extra_db_nodes, ['iris_core@EXISTING_NODE']).
mnesia:change_table_copy_type(schema, node(), disc_copies).

%% For each table that needs replication:
Tables = [offline_msg, user_session, dedup_log, presence],
[mnesia:add_table_copy(T, node(), disc_copies) || T <- Tables].

%% Start Iris application
application:ensure_all_started(iris_core).
```

#### Step 4: Verify Join

```erlang
%% Check node is in cluster
nodes().  %% Should list all cluster nodes

%% Check table replication
[{T, mnesia:table_info(T, disc_copies)} || T <- mnesia:system_info(tables)].
```

#### Step 5: Update Configuration

Add the new node to `expected_cluster_nodes` in config on ALL nodes:

```erlang
{expected_cluster_nodes, [
    'iris_core@node1',
    'iris_core@node2',
    'iris_core@NEW_NODE'  %% Add this
]}
```

Reload config or restart nodes for partition guard to recognize new node.

### Verification

```bash
# Test message routing through new node
python3 tests/suites/integration/test_online_messaging.py
```

---

## 2. Adding a New Region

### When to Add a Region

- Current region approaching 50 nodes
- Users in new geographic area (latency optimization)
- Regulatory requirements (data residency)

### Pre-Flight Checklist

- [ ] Region ID chosen (e.g., `<<"ap-south-1">>`)
- [ ] DNS/endpoints configured for new region
- [ ] At least 3 core nodes provisioned (for quorum)
- [ ] Cross-region network connectivity verified
- [ ] TLS certificates generated for new region

### Procedure

#### Step 1: Initialize New Region's Mnesia Cluster

```bash
# On FIRST core node of new region (becomes primary)
erl -name iris_core@NEW_REGION_NODE1 \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "
        application:ensure_all_started(mnesia),
        iris_core:init_db(),
        application:ensure_all_started(iris_core).
    "
```

```bash
# On subsequent core nodes of new region
erl -name iris_core@NEW_REGION_NODE2 \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "
        application:ensure_all_started(mnesia),
        iris_core:init_db(),
        application:ensure_all_started(iris_core),
        timer:sleep(5000),
        iris_core:join_cluster('iris_core@NEW_REGION_NODE1').
    "
```

#### Step 2: Configure Region Routing

Update config on ALL nodes (existing and new regions):

```erlang
{iris_core, [
    {region_id, <<"us-east-1">>},  %% This node's region
    {regions, [
        <<"us-east-1">>,
        <<"eu-west-1">>,
        <<"ap-south-1">>  %% Add new region
    ]},
    {region_endpoints, #{
        <<"us-east-1">> => ['iris_core@us-east-1-node1'],
        <<"eu-west-1">> => ['iris_core@eu-west-1-node1'],
        <<"ap-south-1">> => ['iris_core@ap-south-1-node1']  %% Add new
    }}
]}
```

#### Step 3: Start Cross-Region Bridge

The `iris_region_bridge` module handles cross-region message delivery. Verify it's running:

```erlang
%% On new region nodes
whereis(iris_region_bridge).  %% Should return a PID

%% Test cross-region connectivity
iris_region_router:probe_region(<<"us-east-1">>).  %% Should return ok
```

#### Step 4: Deploy Edge Nodes

```bash
# For each edge node in new region
erl -name iris_edge@NEW_REGION_EDGE1 \
    -setcookie $IRIS_COOKIE \
    -hidden \
    -pa ebin \
    -iris_edge port 8085 \
    +P 1000000 +K true \
    -eval "
        application:ensure_all_started(iris_edge),
        net_adm:ping('iris_core@NEW_REGION_NODE1').
    "
```

### Verification

```bash
# Test cross-region message delivery
python3 tests/suites/chaos_dist/test_cross_region_latency.py
```

---

## 3. Capacity Planning

### Users Per Node Calculation

```
Users per node = (Available RAM - Mnesia overhead) / Memory per user
               = (16GB - 2GB) / 10KB
               = ~1.4M theoretical, ~40K practical (with safety margin)
```

### When to Scale

| Metric | Threshold | Action |
|--------|-----------|--------|
| Node CPU | > 70% sustained | Add node |
| Node RAM | > 70% | Add node OR increase RAM |
| Region nodes | > 40 | Plan new region |
| P99 latency | > 50ms | Add node OR optimize |
| Cross-region latency | > 200ms | Add region closer to users |

### Mnesia Mesh Limits

Mnesia uses full-mesh gossip which scales O(N^2):

| Nodes | Gossip Messages/sec | Recommendation |
|-------|---------------------|----------------|
| 10 | ~100 | Comfortable |
| 30 | ~900 | Monitor closely |
| 50 | ~2500 | Hard limit, add region |
| 100+ | Unstable | NOT SUPPORTED |

---

## 4. Rollback Procedures

### Removing a Node

```erlang
%% On the node being removed
mnesia:stop().

%% On any OTHER node in cluster
mnesia:del_table_copy(schema, 'iris_core@NODE_TO_REMOVE').

%% For each table:
Tables = [offline_msg, user_session, dedup_log, presence],
[mnesia:del_table_copy(T, 'iris_core@NODE_TO_REMOVE') || T <- Tables].

%% Update expected_cluster_nodes config on all remaining nodes
```

### Decommissioning a Region

1. **Migrate users**: Update region routing to redirect users to another region
2. **Drain connections**: Wait for existing connections to close
3. **Stop edge nodes**: `application:stop(iris_edge).`
4. **Stop core nodes**: `application:stop(iris_core).`
5. **Remove from config**: Update `regions` and `region_endpoints` on all other regions
6. **Archive data**: Backup Mnesia directory before deletion

---

## See Also

- [DEPLOYMENT_GUIDE.md](../DEPLOYMENT_GUIDE.md) - Initial deployment
- [FAILOVER.md](FAILOVER.md) - Failure recovery
- [DATA_RECOVERY.md](DATA_RECOVERY.md) - Data recovery procedures
- [CROSS_REGION.md](CROSS_REGION.md) - Cross-region operations
