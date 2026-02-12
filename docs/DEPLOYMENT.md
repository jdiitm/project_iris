# Deployment Guide

**Last Updated**: 2026-02-11 | **TLS Required**

## Architecture

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

### Critical: Mnesia Memory

**WARNING**: Mnesia `disc_copies` loads ALL data into RAM on startup.

| Data Size | Required RAM |
|-----------|--------------|
| < 8 GB | 16 GB |
| 8-32 GB | 64 GB |
| > 32 GB | Multi-region sharding |

---

## Quick Start (Docker)

```bash
# Start 5-region cluster (6 cores, 11 edges)
make cluster-up

# Run tests
./tests/run_all_tests.sh

# Stop
make cluster-down
```

### Docker Cluster Layout

| Region | Cores | Edges | Ports |
|--------|-------|-------|-------|
| East | core-east-1, core-east-2 | edge-east-1, edge-east-2 | 8085, 8086 |
| West | core-west-1, core-west-2 | edge-west-1, edge-west-2 | 8087, 8088 |
| EU | core-eu-1, core-eu-2 | edge-eu-1, edge-eu-2 | 8089, 8094 |
| Sydney | - | edge-sydney-1, edge-sydney-2 | 8090, 8091 |
| Sao Paulo | - | edge-saopaulo | 8092 |

---

## Bare Metal Setup

### Prerequisites

- Erlang OTP 26+
- Same cookie across cluster: `echo "iris_secret" > ~/.erlang.cookie && chmod 400 ~/.erlang.cookie`

### OS Configuration

```bash
# /etc/sysctl.conf
net.core.somaxconn = 65535
net.ipv4.tcp_max_syn_backlog = 65535

# /etc/security/limits.conf
* soft nofile 1048576
* hard nofile 1048576

sysctl -p
```

### Start Primary Core

```bash
erl -name iris_core@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core)."
```

### Join Secondary Cores

```bash
erl -name iris_core@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -pa ebin \
    -mnesia dir '"/var/lib/iris/mnesia"' \
    +P 2000000 +Q 2000000 +K true \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core), timer:sleep(5000), iris_core:join_cluster('iris_core@PRIMARY_IP')."
```

### Start Edge Nodes

```bash
erl -name iris_edge@$(hostname -I | awk '{print $1}') \
    -setcookie $IRIS_COOKIE \
    -hidden \
    -pa ebin \
    -iris_edge port 8085 \
    +P 1000000 +K true \
    -eval "application:ensure_all_started(iris_edge), net_adm:ping('iris_core@CORE_IP')."
```

---

## Configuration

### Application Environment

```erlang
%% Example production config (create as config/prod.config)
[
    {iris_core, [
        %% Data safety (NEVER enable in prod without operator)
        {allow_table_nuke, false},
        
        %% Replication
        {replication_factor, 3},
        
        %% Split-brain protection (REQUIRED)
        {expected_cluster_nodes, ['core1@host1', 'core2@host2', 'core3@host3']},
        
        %% WAL directory (MUST be persistent storage, NOT tmpfs)
        {wal_directory, "/var/lib/iris/wal"},
        
        %% Regional routing (optional)
        {region_id, <<"us-east-1">>},
        {regions, [<<"us-east-1">>, <<"eu-west-1">>]},
        {region_endpoints, #{
            <<"us-east-1">> => ['core@us-east-1.example.com'],
            <<"eu-west-1">> => ['core@eu-west-1.example.com']
        }},
        
        %% Consistency: ap | hardened_ap | cp (cp is EXPERIMENTAL)
        {consistency_mode, hardened_ap}
    ]},
    
    {iris_edge, [
        %% JWT secret (REQUIRED, 32+ bytes, identical across all nodes)
        {jwt_secret, <<"CHANGE_ME_TO_32_BYTES_OR_MORE!!!">>},
        {auth_enabled, true}
    ]}
].
```

### Critical Configuration

| Setting | Requirement |
|---------|-------------|
| `jwt_secret` | 32+ bytes, identical across all nodes |
| `expected_cluster_nodes` | List all cores for partition guard |
| `wal_directory` | Persistent storage, NOT tmpfs |
| `allow_table_nuke` | `false` in production |

---

## Multi-Region Setup

### Region Routing

Messages route based on user's home region (deterministic hash):

```erlang
%% Automatic routing
iris_region_router:route_to_user(UserId, Msg).

%% Get user's home region
iris_region_router:get_home_region(UserId).
%% <<"us-east-1">>
```

### Cross-Region Communication

- **Direct RPC**: Low latency, requires connectivity
- **Bridge Mode**: Async, for high-latency regions

```erlang
%% Force bridge strategy
iris_region_router:route_to_user(UserId, Msg, #{strategy => bridge}).
```

---

## Storage Durability

```erlang
%% Guaranteed (default): sync_transaction to all replicas
iris_store:put(Table, Key, Value, #{durability => guaranteed}).

%% Quorum: Majority ACK, tolerates minority failures
iris_store:put(Table, Key, Value, #{durability => quorum}).

%% Best effort: Async, for non-critical data
iris_store:put(Table, Key, Value, #{durability => best_effort}).
```

---

## Verification

```erlang
%% Cluster nodes
mnesia:system_info(running_db_nodes).
nodes(connected).

%% Routing
iris_async_router:get_stats().

%% Regional config
iris_region_router:get_current_region().

%% Replication
iris_quorum_write:get_replicas(<<"test_key">>).

%% Partition status
iris_partition_guard:is_safe_for_writes().
```

---

## Failover & Operations

See [OPERATIONS.md](OPERATIONS.md) for incident response, failover procedures, data recovery, and scaling operations.

---

## Configuration Reference

### `iris_core` Options

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `auto_init_db` | boolean | `true` | Create Mnesia schema on first start |
| `allow_table_nuke` | boolean | `false` | **NEVER `true` in production.** Allow corrupted table recreation |
| `allow_schema_delete` | boolean | `false` | Allow Mnesia schema deletion |
| `presence_backend` | `ets \| mnesia` | `ets` | Presence storage. `ets` avoids global lock |
| `replication_factor` | integer | `3` | Number of Mnesia replicas |
| `consistency_mode` | atom | `hardened_ap` | `ap \| hardened_ap \| cp` (cp is experimental) |
| `durability_mode` | atom | `local` | `local \| cluster \| quorum` for offline storage |
| `expected_cluster_nodes` | list | `[]` | **Required for production.** All core node names for partition guard |
| `wal_directory` | string | `"data/wal"` | WAL path. **Must be persistent storage, NOT tmpfs** |
| `join_seeds` | list | `[]` | Seed nodes for cluster join |
| `region_id` | binary | `<<"local">>` | This node's region identifier |
| `regions` | list | `[]` | All known region IDs |
| `region_endpoints` | map | `#{}` | `#{RegionId => [CoreNode]}` mapping |
| `multimaster_durability` | boolean | `false` | Use `sync_transaction` for RPO=0 |

### `iris_edge` Options

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `port` | integer | `8080` | Client-facing listen port |
| `core_nodes` | list | `[]` | Core nodes to connect to for routing |
| `auth_enabled` | boolean | `false` | Enable JWT authentication |
| `jwt_secret` | binary | - | **Required if auth enabled.** 32+ bytes, identical across nodes |
| `jwt_eddsa_private_key` | binary | auto-generated | Ed25519 private key for EdDSA JWT |
| `auth_mode` | atom | `signer` | `signer` (has private key) or `verifier` (public key only) |
| `allow_insecure` | boolean | `false` | Allow plaintext connections. **`false` in production** |
| `tls_enabled` | boolean | `false` | Enable TLS for client connections |
| `tls_certfile` | string | - | Path to TLS certificate PEM |
| `tls_keyfile` | string | - | Path to TLS private key |
| `tls_cacertfile` | string | - | Path to CA certificate for client verification |
| `tls_verify` | atom | `verify_none` | `verify_none` or `verify_peer` (for mTLS) |
| `tls_versions` | list | `['tlsv1.3', 'tlsv1.2']` | Allowed TLS versions |
| `conn_rate_max` | integer | `5` | Per-IP connections/min limit (RFC 10.1) |
| `router_pool_size` | integer | auto-tuned | Override auto-tuned router pool (75% of schedulers) |

---

## TLS Configuration

**TLS is mandatory for all client connections.**

### Certificate Setup

The `certs/` directory contains:
- `ca.pem` / `ca.key` - Certificate Authority
- `edge-*.pem` / `edge-*.key` - Edge node certificates
- `core-*.pem` / `core-*.key` - Core node certificates
- `test-client.pem` / `test-client.key` - Client certificates (for mTLS)

### Generate New Certificates

```bash
cd certs/
./generate_certs.sh
```

### Server TLS Configuration

```erlang
%% config/test_tls.config (development)
[
    {iris_edge, [
        {port, 8085},
        {tls, [
            {certfile, "certs/edge-east-1.pem"},
            {keyfile, "certs/edge-east-1.key"},
            {cacertfile, "certs/ca.pem"},
            {verify, verify_none}  %% Use verify_peer for mTLS
        ]}
    ]}
].
```

### Starting with TLS

```bash
# Development
erl -pa ebin -config config/test_tls -eval \
    "application:ensure_all_started(iris_core), application:ensure_all_started(iris_edge)."

# Production (with mTLS) — requires creating config/prod_mtls first
# See config/sys.config.example and config/test_mtls.config as templates
erl -pa ebin -config config/prod_mtls -eval \
    "application:ensure_all_started(iris_core), application:ensure_all_started(iris_edge)."
```

### Client TLS (Python)

```python
import ssl
from pathlib import Path

context = ssl.create_default_context()
context.load_verify_locations('certs/ca.pem')
# For mTLS:
# context.load_cert_chain('certs/test-client.pem', 'certs/test-client.key')

sock = socket.create_connection(('localhost', 8085))
tls_sock = context.wrap_socket(sock, server_hostname='localhost')
```

### Verify TLS is Working

```bash
# Test TLS handshake
openssl s_client -connect localhost:8085 -CAfile certs/ca.pem

# Should see: Verify return code: 0 (ok)
```

---

## Security Checklist

### Required

- [x] **TLS certificates**: Client connections (enforced)
- [ ] JWT secret: 32+ bytes, identical across nodes
- [ ] Expected cluster nodes: Listed for partition guard
- [ ] WAL directory: Persistent storage, not tmpfs
- [ ] Erlang cookie: Secured (`chmod 400`)
- [ ] `allow_table_nuke`: Set to `false`

### Recommended

- [ ] mTLS for inter-node communication (certificates in `certs/`)
- [ ] Firewall: 4369 (epmd), 9000-9010 (distribution)
- [ ] Monitoring for partition events
- [ ] Certificate rotation (before expiry)

---

## Troubleshooting

**Edge can't reach Core**: Hidden nodes don't auto-reconnect.
```erlang
net_adm:ping('core_node').
```

**Data lost after restart**: Ensure `-mnesia dir` points to persistent storage.

**Tables missing**: Check `mnesia:system_info(directory)` matches config.

**Quorum not reached**: Check nodes available with `iris_quorum_write:get_replicas/1`.

**Cross-region routing fails**: Verify `region_endpoints` config and network.
