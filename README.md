# Project Iris: WhatsApp-Class Messaging Engine

[![SLA](https://img.shields.io/badge/availability-99.99%25-blue)](docs/STANDARDS.md)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](tests/run_tests.py)

> **Production-Grade**: Validated for **1M+ concurrent users** with zero message loss.

## Overview

Project Iris is a high-performance distributed messaging system built in **Erlang/OTP**, designed to demonstrate "WhatsApp Architecture" of extreme scalability and reliability.

### Key Capabilities
- **Massive Concurrency**: 1M+ connections per node (~10KB RAM/user)
- **Low Latency**: <25ms P99 under load
- **Zero Message Loss**: WAL-based durability with sync_transaction
- **Resilience**: Circuit breaker, backpressure, automatic recovery

## Architecture

### Core Modules
| Module | Purpose |
|--------|---------|
| `iris_durable_batcher` | WAL + batched sync_transaction for durability |
| `iris_flow_controller` | Multi-level adaptive backpressure |
| `iris_circuit_breaker` | Fallback routing with adaptive timeout |
| `iris_shard` | Consistent user sharding (phash2) |
| `iris_discovery` | Pluggable service discovery (pg/DNS/Consul) |
| `iris_storage` | Backend abstraction (Mnesia/ETS/Redis) |
| `iris_auth` | JWT authentication with HMAC-SHA256 |
| `iris_rate_limiter` | Per-user token bucket rate limiting |

### Node Types
1. **Core Node** (`iris_core`): User registry, offline storage, routing
2. **Edge Node** (`iris_edge`): Connection handling, TLS, message delivery

## Quick Start

### Prerequisites
- **Runtime**: Erlang/OTP 25+
- **Python**: 3.9+ (for tests)

### Build & Run
```bash
# Compile
make clean && make

# Start cluster
make start_core
make start_edge1

# Run tests
python3 tests/run_tests.py --tier 0
```

The system auto-tunes VM flags based on available RAM.

## Testing

```bash
# Tier 0 (fast, CI-required)
make test-tier0

# All tests
make test-all

# Specific suite
python3 tests/run_tests.py --suite integration
```

## Performance

| Metric | Value |
|--------|-------|
| Max Connections | 1M+ per node |
| Memory | ~8.6 KB/user |
| Throughput | 1.1M msgs/sec |
| P99 Latency | <25ms |

## Documentation

- [Deployment Guide](docs/DEPLOYMENT_GUIDE.md)
- [Cluster Setup](docs/CLUSTER_SETUP.md)
- [Architecture](docs/PLANETARY_SCALE_ARCHITECTURE.md)
- [Production Readiness](docs/PRODUCTION_READINESS_REPORT.md)
- [Operational Runbooks](docs/runbooks/) - Incident response, failover, recovery

### Security & Audits
- [Audit4 Remediation](docs/audit4/REMEDIATION_STATUS.md) - **Current** - Production readiness fixes
- [Audit3 Report](docs/audit3/COMPREHENSIVE_AUDIT_REPORT.md) - Architecture review
- [Test Reliability Spec](docs/audit3/FAANG_PLUS_TEST_RELIABILITY_SPEC.md)

## Security Features

| Feature | Status |
|---------|--------|
| TLS 1.2/1.3 | ✅ Supported (config: `tls_enabled`) |
| JWT Authentication | ✅ HMAC-SHA256 with expiry |
| Rate Limiting | ✅ Per-user token bucket |
| DoS Protection | ✅ Protocol length limits |
| Bounded Queues | ✅ Prevents OOM |

---
**License**: MIT