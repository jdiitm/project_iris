# Failure Mode → Test Coverage Matrix

This document maps potential failure modes to the tests that validate system behavior under each condition.

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Directly tested |
| 🔶 | Indirectly tested |
| ❌ | Not covered |

---

## Message Delivery Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Message loss (online→online) | ✅ `test_online_messaging` | integration | MessageTracker validates 0 loss |
| Message loss (online→offline) | ✅ `test_offline_storage` | integration | Validates storage + delivery |
| Message duplication | ✅ `assert_no_duplicates` | integration | All integration tests |
| Message ordering violation | ✅ `test_multi_message_sequence` | integration | Sequence validation |
| Message content corruption | ✅ `test_basic_message_delivery` | integration | Content match assertion |

---

## Network & Connectivity Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| TCP connection drop | 🔶 `chaos_kitchen_sink` | chaos_controlled | Connections randomly cycle |
| Network partition (split brain) | 🔶 `test_failure_modes` | resilience | RPC failure handling |
| High latency | 🔶 `benchmark_latency` | performance_light | Latency distribution |
| Connection flood | ✅ `chaos_kitchen_sink` | chaos_controlled | 100+ concurrent connections |

---

## Resource Exhaustion

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Memory exhaustion (OOM) | ✅ `test_slow_consumer_oom_prevention` | resilience | Memory growth monitoring |
| CPU saturation | 🔶 `benchmark_throughput` | performance_light | CPU usage tracked |
| Disk space exhaustion | ✅ `test_disk_pressure` | resilience | High disk write load |
| File descriptor exhaustion | 🔶 `chaos_kitchen_sink` | chaos_controlled | Connection count monitoring |

---

## Process & Supervision Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Random process crash | ✅ `test_process_crash_recovery` | resilience | Chaos monkey integration |
| Router worker crash | ✅ `test_process_crash_recovery` | resilience | Targeted kills |
| Supervision tree failure | 🔶 `chaos_kitchen_sink` | chaos_controlled | System survival check |
| State corruption | 🔶 Erlang EUnit | unit | Protocol/session validation |

---

## Scaling & Load Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Celebrity hotspot (fan-in) | ✅ `test_hotkey_bucketing` | integration | 100+ fans → 1 user |
| Offline message backlog | ✅ `test_multiple_offline_messages` | integration | Multi-sender offline |
| Throughput degradation | ✅ `benchmark_throughput` | performance_light | msgs/sec tracking |
| Connection scalability | ✅ `benchmark_multi_connection` | performance_light | Multi-pair benchmark |

---

## Protocol & Data Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Malformed packet | ✅ `iris_proto_tests` | unit | 27 protocol tests |
| Partial packet | ✅ `test_decode_*_partial_*` | unit | Incremental parsing |
| Unknown opcode | ✅ `test_decode_unknown` | unit | Error handling |
| Unicode handling | ✅ `test_unicode_username` | unit | UTF-8 validation |
| Binary null bytes | ✅ `test_binary_nulls` | unit | Embedded nulls |
| Large payload | ✅ `test_roundtrip_large_message` | unit | 10KB message |

---

## Presence System Failures

| Failure Mode | Test Coverage | Suite | Notes |
|--------------|---------------|-------|-------|
| Stale presence cache | 🔶 `test_presence_cache` | integration | Multiple queries |
| Online/offline transition | ✅ `test_offline_user_status` | integration | Login/logout cycle |
| Last-seen accuracy | 🔶 `test_offline_user_status` | integration | Timestamp check |

---

## Not Yet Covered

These failure modes are identified but not yet tested:

| Failure Mode | Priority | Reason |
|--------------|----------|--------|
| Clock skew between nodes | Medium | Requires multi-node setup |
| Multi-region failover | High | Requires geo-distributed infra |
| Hot code upgrade failure | Medium | Requires appup testing |
| Mnesia table corruption | High | Requires fault injection |
| DNS resolution failure | Low | External dependency |

---

## Adding Coverage

To add coverage for an uncovered failure mode:

1. Identify the appropriate suite (`resilience`, `integration`, etc.)
2. Create test function with proper logging
3. Use state-based assertions
4. Update this matrix

See [ADDING_TESTS.md](./ADDING_TESTS.md) for detailed instructions.
