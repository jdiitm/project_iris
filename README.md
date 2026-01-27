# Project Iris: WhatsApp-Class Messaging Engine

[![Tests](https://img.shields.io/badge/tests-85%20passing-brightgreen)](tests/run_tests.py)
[![Erlang](https://img.shields.io/badge/Erlang-OTP%2025%2B-blue)](https://www.erlang.org/)

> **Production-Grade**: Validated for **1M+ concurrent users** with zero message loss.

## Overview

Project Iris is a high-performance distributed messaging system built in **Erlang/OTP**, designed to demonstrate WhatsApp-class scalability and reliability.

### Key Capabilities

| Metric | Value |
|--------|-------|
| Max Connections | 1M+ per node |
| Memory per User | ~10 KB |
| P99 Latency | < 25ms |
| Message Durability | Zero loss (WAL + sync_transaction) |

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

- **Runtime**: Erlang/OTP 25+
- **Python**: 3.9+ (for tests)
- **Docker**: For cluster simulation (optional)

### Build & Run

```bash
# Compile (auto-tunes VM flags)
make clean && make

# Start local cluster
make start

# Run tests
python3 tests/run_tests.py --all
```

### Docker Cluster

```bash
# Start 5-region simulation
cd docker/global-cluster
./cluster.sh up

# Run distributed tests
python3 tests/run_tests.py --all

# Stop
./cluster.sh down
```

## Testing

```bash
# All tests (85 tests)
python3 tests/run_tests.py --all

# Specific suite
python3 tests/run_tests.py --suite integration
python3 tests/run_tests.py --suite unit
python3 tests/run_tests.py --suite stress

# Unit tests only (fast)
make test-unit
```

### Test Suites

| Suite | Tests | Purpose |
|-------|-------|---------|
| unit | 21 | Erlang module unit tests |
| integration | 17 | Python integration tests |
| stress | 13 | Load and scale testing |
| chaos_dist | 9 | Distributed failure tests |
| security | 7 | Auth, TLS, injection tests |
| performance_light | 6 | Performance benchmarks |
| e2e | 5 | End-to-end flows |
| resilience | 3 | Recovery and failover |
| chaos_controlled | 2 | Controlled chaos scenarios |
| contract | 1 | Protocol contract tests |
| compatibility | 1 | Version compatibility |

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

### Optional CP Mode (Raft)

```erlang
%% Enable linearizable consistency for critical data
{iris_core, [{consistency_mode, cp}]}.
```

## Security Features

| Feature | Status |
|---------|--------|
| TLS 1.2/1.3 | ✅ Supported |
| mTLS (inter-node) | ✅ Configurable |
| JWT Authentication | ✅ HMAC-SHA256 |
| Rate Limiting | ✅ Token bucket |
| DoS Protection | ✅ Protocol limits |
| E2EE | ✅ Signal Protocol |

## Documentation

- [Deployment Guide](docs/DEPLOYMENT_GUIDE.md)
- [Cluster Setup](docs/CLUSTER_SETUP.md)
- [Architecture Decisions](docs/DECISIONS.md)
- [RFC-001 System Requirements](docs/rfc/RFC-001-SYSTEM-REQUIREMENTS.md)
- [RFC-001 Amendment (E2EE + Groups)](docs/rfc/RFC-001-AMENDMENT-001.md)

### Operational

- [Data Recovery Runbook](docs/runbooks/DATA_RECOVERY.md)
- [Failover Runbook](docs/runbooks/FAILOVER.md)
- [Test Determinism](docs/TEST_DETERMINISM.md)

## Project Structure

```
project_iris/
├── src/                    # Erlang source modules (46 modules)
├── test_utils/             # Erlang test utilities and unit tests
├── tests/
│   ├── run_tests.py        # Unified test runner
│   ├── suites/             # Test suites (11 categories)
│   └── framework/          # Test framework utilities
├── config/                 # Erlang config files
├── certs/                  # TLS certificates
├── docker/
│   └── global-cluster/     # Docker cluster simulation
├── docs/                   # Documentation
└── Makefile                # Build and test commands
```

---

**License**: MIT
