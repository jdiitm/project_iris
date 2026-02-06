# Project Iris: WhatsApp-Class Messaging Engine

[![Tests](https://img.shields.io/badge/tests-75%2B%20passing-brightgreen)](tests/run_all_tests.sh)
[![TLS](https://img.shields.io/badge/TLS-enforced-green)](docs/DEPLOYMENT.md)
[![Erlang](https://img.shields.io/badge/Erlang-OTP%2026%2B-blue)](https://www.erlang.org/)

> **Current Status**: Development. Tested at **10K concurrent connections** locally.  
> Full test suite (75+ tests) passing with TLS enforced.  
> Architecture designed for 1M+ users per region (see [Scalability Analysis](docs/SCALABILITY_ANALYSIS.md)).  
> Planet-scale deployment (2B+ users) requires multi-region infrastructure.

## Overview

Project Iris is a high-performance distributed messaging system built in **Erlang/OTP**, designed to demonstrate WhatsApp-class scalability and reliability.

### Key Capabilities

| Metric | Tested | Designed For |
|--------|--------|--------------|
| Concurrent Users | 10K (local) | 1M+ per region |
| Throughput | 8K msg/s | 100K+ msg/s |
| Memory per User | ~12 KB | ~10-15 KB |
| P99 Latency | < 25ms (local) | < 50ms cross-region |
| Message Durability | Zero loss (guaranteed mode)* | Zero loss |

*Durability guarantee applies to `durability => guaranteed` writes. See [Scalability Analysis](docs/SCALABILITY_ANALYSIS.md) for methodology.*

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                     GLOBAL ROUTING LAYER                            │
│   (iris_region_router - Routes users to home region)               │
└─────────────────────────────────────────────────────────────────────┘
                      │                    │                    │
           ┌─────────▼─────────┐ ┌────────▼────────┐ ┌─────────▼─────────┐
           │   REGION: US      │ │ REGION: EU      │ │ REGION: APAC      │
           │   Mnesia Cluster  │ │ Mnesia Cluster  │ │ Mnesia Cluster    │
           │   (50 nodes max)  │ │ (50 nodes max)  │ │ (50 nodes max)    │
           └───────────────────┘ └─────────────────┘ └───────────────────┘
```

### Node Types

1. **Core Node** (`iris_core`): User registry, offline storage, Mnesia replication
2. **Edge Node** (`iris_edge`): Connection handling, TLS termination, message routing

### Core Modules

| Module | Purpose |
|--------|---------|
| `iris_store` | Simplified storage API with durability options |
| `iris_quorum_write` | Quorum-based writes (RF=3, majority ACK) |
| `iris_region_router` | Regional sharding for 2B+ users |
| `iris_durable_batcher` | WAL + batched sync_transaction |
| `iris_flow_controller` | Multi-level adaptive backpressure |
| `iris_circuit_breaker` | Fallback routing with adaptive timeout |
| `iris_partition_guard` | Split-brain detection and safe mode |
| `iris_async_router` | Auto-tuned worker pool for message routing |
| `iris_auth` | JWT authentication (HMAC-SHA256) |
| `iris_rate_limiter` | Per-user token bucket rate limiting |

### E2EE Modules (Signal Protocol)

| Module | Purpose |
|--------|---------|
| `iris_x3dh` | X3DH key agreement |
| `iris_ratchet` | Double Ratchet for forward secrecy |
| `iris_keys` | Key management and storage |
| `iris_group` | Group membership management |
| `iris_sender_keys` | Sender Keys for group E2EE |

## Quick Start

### Prerequisites

- **Runtime**: Erlang/OTP 26+
- **Python**: 3.11+ (for tests)
- **Docker**: For cluster simulation (optional)

### Build & Run

```bash
# Compile (auto-tunes VM flags)
make clean && make

# Start local cluster
make start

# Run ALL tests
./tests/run_all_tests.sh
```

### Docker Cluster

```bash
# Start 5-region cluster (6 cores, 11 edges)
cd docker/global-cluster
./cluster.sh up

# Run distributed chaos tests
./run_chaos_tests.sh

# Stop
./cluster.sh down
```

## Testing

**Status**: 75+ tests passing | **Last Verified**: 2026-02-05

```bash
# Run ALL tests (recommended)
./tests/run_all_tests.sh

# Run non-Docker tests only (faster)
./tests/run_all_tests.sh --quick

# Run Docker chaos tests only
./tests/run_all_tests.sh --docker-only

# Run single chaos test
cd docker/global-cluster
./cluster.sh down && ./cluster.sh up && python3 ../../tests/suites/chaos_dist/test_network_partition.py
```

### Proven Test Scripts

| Script | Purpose |
|--------|---------|
| `tests/run_all_tests.sh` | Main test runner |
| `docker/global-cluster/cluster.sh` | Cluster up/down |
| `docker/global-cluster/init_cluster.sh` | Mnesia initialization |
| `docker/global-cluster/run_chaos_tests.sh` | Chaos tests with fresh cluster |

See [TESTING.md](docs/TESTING.md) for details.

## Configuration

### Storage Durability Options

```erlang
%% Guaranteed (default): sync_transaction to all replicas
iris_store:put(Table, Key, Value, #{durability => guaranteed}).

%% Quorum: Majority ACK, tolerates minority failures
iris_store:put(Table, Key, Value, #{durability => quorum}).

%% Best effort: Async, for non-critical data
iris_store:put(Table, Key, Value, #{durability => best_effort}).
```

### Regional Routing

```erlang
{iris_core, [
    {region_id, <<"us-east-1">>},
    {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
    {replication_factor, 3}
]}.
```

## Security Features

| Feature | Status |
|---------|--------|
| TLS 1.2/1.3 | ✅ **Enforced** (all client connections) |
| mTLS (inter-node) | ✅ Configurable |
| JWT Authentication | ✅ HMAC-SHA256 |
| Rate Limiting | ✅ Token bucket |
| DoS Protection | ✅ Protocol limits |
| E2EE | ✅ Signal Protocol |

> **Note**: TLS is enforced for all client connections. Tests use certificates in `certs/` directory.
> See [DEPLOYMENT.md](docs/DEPLOYMENT.md) for TLS configuration.

## Documentation

| Guide | Description |
|-------|-------------|
| [DEPLOYMENT.md](docs/DEPLOYMENT.md) | Setup, configuration, cluster management |
| [OPERATIONS.md](docs/OPERATIONS.md) | Incident response, failover, scaling |
| [TESTING.md](docs/TESTING.md) | Test suite, proven scripts, coverage |
| [DECISIONS.md](docs/DECISIONS.md) | Architecture decisions |
| [RFC-001](docs/rfc/RFC-001-SYSTEM-REQUIREMENTS.md) | System requirements spec |

## Project Structure

```
project_iris/
├── src/                    # Erlang source modules (46 modules)
├── test_utils/             # Erlang test utilities and unit tests
├── tests/
│   ├── run_all_tests.sh    # Main test runner (PROVEN)
│   ├── suites/             # Test suites (12 categories)
│   │   ├── unit/           # Property-based tests
│   │   ├── integration/    # Core message flow tests
│   │   ├── e2e/            # End-to-end scenarios
│   │   ├── security/       # Security validation
│   │   ├── resilience/     # Fault tolerance
│   │   ├── stress/         # Load testing
│   │   ├── chaos_dist/     # Docker-based chaos tests
│   │   ├── compatibility/  # Protocol version tests
│   │   ├── contract/       # API contract tests
│   │   └── performance_light/ # CPU/resource tests
│   ├── framework/          # ClusterManager, assertions
│   └── utilities/          # IrisClient (TLS-enabled)
├── config/                 # Erlang config files
├── certs/                  # TLS certificates
├── docker/
│   └── global-cluster/     # Docker cluster (PROVEN scripts)
│       ├── cluster.sh      # Cluster management
│       ├── init_cluster.sh # Mnesia initialization
│       └── run_chaos_tests.sh  # Chaos test runner
├── docs/                   # Documentation
└── Makefile                # Build and test commands
```

---

**License**: MIT
