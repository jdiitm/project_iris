# Project Iris Test Suite

A lightweight, resource-aware test framework for testing the Iris distributed messaging system.

## Quick Start

```bash
# Run unit tests only
./tests/run_tests.py --suite unit

# Run integration tests
./tests/run_tests.py --suite integration

# Run CI Tier 0 (required on every merge)
./tests/run_tests.py --tier 0

# Run CI Tier 1 (nightly/manual)
./tests/run_tests.py --tier 1

# List all available tests
./tests/run_tests.py --list
```

## Directory Structure

```
/tests/
├── run_tests.py           # Unified test runner
├── README.md              # This file
│
├── framework/             # Test framework core
│   ├── logger.py          # Structured JSON logging
│   ├── cluster.py         # Cluster lifecycle management
│   ├── assertions.py      # State-based assertion utilities
│   ├── resource_monitor.py # CPU/memory sampling
│   └── metrics.py         # Post-process metrics derivation
│
├── suites/                # Test suites by category
│   ├── unit/              # Erlang EUnit tests
│   ├── integration/       # Python integration tests
│   ├── performance_light/ # Laptop-safe benchmarks
│   ├── resilience/        # Failure-mode tests
│   ├── chaos_controlled/  # Controlled chaos tests
│   └── stress/            # Heavy stress tests
│
├── utilities/             # Shared test utilities
│   ├── iris_client.py     # Python client library
│   └── erlang/            # Erlang test helpers
│
├── configs/               # Test configurations
├── data/                  # Test fixtures
├── artifacts/             # Test outputs (gitignored)
└── docs/                  # Extended documentation
```

## Test Tiers

### Tier 0: Required on Every Merge

| Suite | Tests | Purpose |
|-------|-------|---------|
| `unit` | Erlang EUnit | Protocol correctness, session handling |
| `integration` | Online/offline messaging | Core message delivery |

**CI Behavior**: Merge blocked if Tier 0 fails.

### Tier 1: Nightly / Manual

| Suite | Tests | Purpose |
|-------|-------|---------|
| `resilience` | Failure modes | OOM prevention, recovery |
| `performance_light` | Benchmarks | Throughput, latency |
| `chaos_controlled` | Chaos tests | Multi-stressor survival |

**CI Behavior**: Run on schedule or manual trigger.

## Test Philosophy

### State-Based Assertions (Not Timing-Based)

```python
# ❌ Bad: Timing-based (flaky)
time.sleep(5)
assert message_received

# ✅ Good: State-based (deterministic)
wait_for_condition(lambda: tracker.received_count >= expected)
assert_no_message_loss(tracker)
```

### Resource Awareness

All tests are designed for constrained infrastructure:
- 3 personal laptops as core nodes
- Free-tier cloud VMs as edge nodes
- No Prometheus/Grafana/OTel

Resource limits are enforced via `suite.yaml` metadata.

### Observability via Post-Processing

No real-time metrics collection. Instead:
1. Tests emit structured JSON logs
2. Logs are parsed after test completion
3. Metrics (latency percentiles, throughput) derived from logs

## Writing New Tests

### 1. Choose the Right Suite

| If your test... | Use suite |
|-----------------|-----------|
| Tests a single function/module | `unit` |
| Tests end-to-end message flow | `integration` |
| Measures performance | `performance_light` |
| Tests failure recovery | `resilience` |
| Combines multiple stressors | `chaos_controlled` |

### 2. Use the Framework

```python
#!/usr/bin/env python3
from tests.framework import TestLogger, ClusterManager
from tests.framework.assertions import MessageTracker, assert_no_message_loss
from tests.utilities import IrisClient

def test_my_feature():
    with TestLogger("test_my_feature", "integration") as log:
        tracker = MessageTracker()
        
        # Setup
        client = IrisClient()
        client.login("test_user")
        log.connection_event("login", "test_user")
        
        # Test logic
        msg_id = "test_msg_1"
        tracker.record_sent(msg_id, "target_user", "payload")
        client.send_msg("target_user", "payload")
        log.message_sent(msg_id, "target_user")
        
        # Assertions (state-based)
        assert_no_message_loss(tracker)
        
        # Cleanup
        client.close()
        log.info("result", "Test PASSED")
        return True
```

### 3. Add Suite Metadata

Create `suite.yaml` in your suite directory:

```yaml
name: my_suite
description: "Tests for my feature"
resource_requirements:
  cpu_cores_min: 2
  memory_mb_min: 2048
max_execution_time_seconds: 120
safe_for_laptop: true
ci_tier: 1
```

## Artifact Schema

### Log Files (JSON Lines)

```json
{
  "wall_time": "2026-01-08T08:23:04.123Z",
  "monotonic_ns": 1234567890,
  "node_id": "iris_edge1",
  "test_id": "test_online_messaging",
  "suite": "integration",
  "event_type": "message_sent",
  "message_id": "msg_abc123",
  "target_user": "bob",
  "latency_ms": 1.23
}
```

### Derived Metrics

```json
{
  "test_id": "test_online_messaging",
  "latency_p50_ms": 1.2,
  "latency_p99_ms": 5.1,
  "throughput_msgs_per_sec": 99898,
  "messages_lost": 0,
  "messages_duplicated": 0
}
```

## CI Integration

Tests run automatically on GitHub Actions:

- **On push to master**: Tier 0 tests
- **Nightly at 2 AM UTC**: Tier 0 + Tier 1
- **Manual trigger**: Select Tier 1 flag

See `.github/workflows/ci-tests.yml` for configuration.

## Known Limitations

| Constraint | Impact | Mitigation |
|------------|--------|------------|
| No Prometheus | No real-time dashboards | Post-process logs |
| No OTel | No distributed tracing | Correlate via test_id |
| Laptop resources | Limited scale testing | Controlled load levels |
| No paid CI | Limited parallelism | Sequential by default |

## Troubleshooting

### Test hangs on cluster start

```bash
# Force kill all Erlang processes
killall beam.smp
make stop
rm -rf Mnesia.*
```

### Connection refused errors

```bash
# Verify cluster is running
make start_core
sleep 3
make start_edge1
sleep 2
```

### Tests pass locally but fail in CI

1. Check `tests/artifacts/` for detailed logs
2. Verify Erlang version matches (`erl -version`)
3. Check for timing-based assertions (convert to state-based)
