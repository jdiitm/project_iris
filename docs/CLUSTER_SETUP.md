# Hybrid Cloud Cluster Setup Guide (Project Iris)

This guide details how to deploy a **Hybrid Cloud Cluster** for Project Iris, using the **Live Environment** configuration verified on 2026-01-10.

| Role | Node Name | IP | Function |
| :--- | :--- | :--- | :--- |
| **Core 1** | `iris_core1@100.95.21.52` | 100.95.21.52 | Bootstrap Master |
| **Core 2** | `iris_core2@100.68.74.48` | 100.68.74.48 | Replica |
| **Edge 1** | `iris_edge1@100.82.212.50` | 100.82.212.50 | Edge Gateway |

## 1. Prerequisites (All Nodes)
Ensure Tailscale is up and machines can ping each other.

## 2. Start the Cluster

### 2.1 Start Core Tier (Local Laptops)

**Laptop A (Core 1 - Bootstrap Node):**
```bash
# Start master node with Mnesia initialization
erl -name iris_core1@100.95.21.52 -setcookie iris_secret -pa ebin \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core)."
```

**Laptop B (Core 2 - Replica):**
```bash
# Start replica node and join cluster
erl -name iris_core2@100.68.74.48 -setcookie iris_secret -pa ebin \
    -eval "application:ensure_all_started(mnesia), iris_core:init_db(), application:ensure_all_started(iris_core)."
```

*(On Core 2, join the cluster)*:
```erlang
(iris_core2@100.68.74.48)1> iris_core:join_cluster('iris_core1@100.95.21.52').
% Expected log: "Joined cluster with iris_core1@100.95.21.52"
```

### 2.2 Start Edge Tier (Cloud VMs)

**Cloud VM (Edge 1):**
```bash
# IMPORTANT: Increase file descriptor limit for high concurrency
ulimit -n 65536

# Start Edge in HIDDEN NODE MODE (prevents global disconnects during partition)
erl -name iris_edge1@100.82.212.50 -setcookie iris_secret -hidden -pa ebin \
    -eval "application:ensure_all_started(iris_edge), net_adm:ping('iris_core1@100.95.21.52'), net_adm:ping('iris_core2@100.68.74.48')."
```

## 3. Verification

### 3.1 Check Cluster Status (Any Node)
```erlang
nodes(connected).
% Expected: ['iris_core1@...', 'iris_core2@...', ...] (varies by perspective)
```

## 4. Running Assurance Tests

### 4.1 Failover Proof Test (Recommended)
Validates resilience with verifiable assertions:
```bash
python3 tests/suites/tokyo_assurance/test_failover_proof.py \
    --edge-node iris_edge1@100.82.212.50 \
    --core1 iris_core1@100.95.21.52 \
    --core2 iris_core2@100.68.74.48 \
    --users 1000 --duration 300 --no-recovery
```

### 4.2 Legacy 10-Minute Proof Run
```bash
python3 tests/suites/tokyo_assurance/ten_minute_proof.py --edge-node iris_edge1@100.82.212.50
```

