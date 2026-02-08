# `time.sleep()` Usage Audit

**RFC Reference**: Section 13.2 — "Tests MUST NOT use `time.sleep()` for synchronization"
**Date**: 2026-02-07
**Total instances found**: ~530+ across 108 files in `tests/`

---

## Classification

### Category A: Framework Infrastructure (Acceptable)

These are in polling loops, readiness checks, or retry infrastructure. They represent
event-based waits with timeouts (the pattern the RFC recommends), not blind sleeps.

| File | Count | Purpose |
|------|-------|---------|
| `tests/framework/wait.py` | 5 | Polling loop infrastructure (replaces blind sleep) |
| `tests/framework/readiness.py` | 3 | Explicit readiness polling |
| `tests/framework/assertions.py` | 2 | Eventually-consistent assertion polling |
| `tests/framework/resource_monitor.py` | 1 | Sampling interval |
| `tests/framework/cluster.py` | 14 | Cluster startup/shutdown waits |
| `tests/utilities/cluster_utils.py` | 6 | Docker container readiness |
| `tests/utilities/iris_client.py` | 1 | ETS propagation delay (50ms) |
| **Subtotal** | **32** | |

### Category B: Synchronization Substitutes (Should Fix)

These are `time.sleep()` calls used in test bodies where the test should instead
poll for a condition. These are the calls that violate RFC Section 13.2.

| File | Count | Primary Pattern |
|------|-------|----------------|
| `tests/suites/security/test_group_key_rotation_race.py` | 16 | Sleep between key rotation steps |
| `tests/suites/security/test_stateful_protocol_fuzz.py` | 16 | Sleep between fuzz iterations |
| `tests/suites/chaos_dist/test_ordering_under_failure.py` | 16 | Sleep waiting for message delivery |
| `tests/suites/chaos_dist/test_ack_durability.py` | 16 | Sleep after SIGKILL |
| `tests/suites/chaos_dist/test_cross_region_chaos.py` | 15 | Sleep after network partition |
| `tests/suites/security/test_protocol_fuzz.py` | 15 | Sleep between fuzz rounds |
| `tests/suites/chaos_dist/test_ack_disconnect_race.py` | 13 | Sleep waiting for ack |
| `tests/suites/integration/test_group_e2ee.py` | 13 | Sleep between group operations |
| `tests/suites/integration/test_presence.py` | 12 | Sleep waiting for presence updates |
| `tests/suites/chaos_dist/test_bridge_durability.py` | 12 | Sleep after bridge kill |
| `tests/suites/chaos_dist/test_cross_region_latency.py` | 12 | Sleep waiting for cross-region delivery |
| `tests/suites/chaos_dist/test_multimaster_durability.py` | 11 | Sleep after node kill |
| `tests/suites/chaos_dist/test_dedup_persistence.py` | 11 | Sleep after restart |
| `tests/suites/chaos_dist/test_dist_failover.py` | 11 | Sleep after failover |
| `tests/suites/compatibility/test_v1_frozen_client.py` | 11 | Sleep between protocol steps |
| `tests/suites/security/test_token_expiry_boundary.py` | 10 | Sleep waiting for token expiry |
| `tests/suites/security/test_cbor_malformed.py` | 10 | Sleep between malformed sends |
| `tests/suites/integration/test_idempotency.py` | 10 | Sleep waiting for dedup |
| `tests/suites/chaos_dist/test_region_outage.py` | 10 | Sleep after region kill |
| `tests/suites/chaos_dist/test_quorum_write_failures.py` | 9 | Sleep after quorum loss |
| `tests/suites/chaos_dist/test_network_partition.py` | 18 | Sleep during partition tests |
| `tests/suites/stress/test_soak_memory.py` | 9 | Sleep in soak loop |
| `tests/suites/integration/test_metrics_emission.py` | 8 | Sleep waiting for metric emission |
| `tests/suites/security/test_revocation_integration.py` | 8 | Sleep after revocation |
| `tests/suites/chaos_dist/test_disk_full.py` | 8 | Sleep after disk full |
| `tests/suites/compatibility/test_protocol_versions.py` | 8 | Sleep between version tests |
| `tests/suites/integration/test_typing_indicator.py` | 8 | Sleep waiting for typing indicator |
| `tests/suites/integration/test_offline_e2ee.py` | 7 | Sleep during offline |
| `tests/suites/e2e/test_offline_reconnect.py` | 7 | Sleep waiting for reconnect |
| `tests/suites/e2e/test_sender_key_distribution_sla.py` | 7 | Sleep between key distributions |
| `tests/suites/compatibility/test_keepalive_protocol.py` | 4 | Sleep between keepalives |
| `tests/suites/compatibility/test_version_negotiation_edge.py` | 7 | Sleep between version negotiation steps |
| `tests/suites/stress/stress_global_fan_in.py` | 7 | Sleep in stress loop |
| `tests/suites/stress/stress_hotspot.py` | 7 | Sleep in stress loop |
| `tests/suites/chaos_controlled/ultimate_chaos.py` | 12 | Sleep during chaos |
| `tests/suites/integration/test_degradation_order.py` | 6 | Sleep between degradation levels |
| `tests/suites/performance_light/benchmark_group_1000.py` | 6 | Sleep in benchmark |
| `tests/suites/e2e/test_group_security_lifecycle.py` | 6 | Sleep between lifecycle steps |
| `tests/suites/chaos_dist/test_cascade_failure.py` | 6 | Sleep during cascades |
| `tests/suites/stress/test_reconnect_storm.py` | 6 | Sleep in storm |
| `tests/suites/e2e/test_full_conversation.py` | 5 | Sleep between messages |
| `tests/suites/integration/test_distributed_tracing.py` | 5 | Sleep waiting for trace |
| `tests/suites/security/test_slowloris.py` | 5 | Sleep for slow send |
| `tests/suites/security/test_security_basics.py` | 5 | Sleep between security tests |
| `tests/suites/resilience/test_resilience.py` | 10 | Sleep during resilience checks |
| `tests/suites/resilience/test_clock_skew.py` | 5 | Sleep in clock skew |
| `tests/suites/resilience/test_connection_resume.py` | 5 | Sleep during resume |
| `tests/suites/performance_light/test_cpu_utilization.py` | 5 | Sleep in perf monitoring |
| `tests/suites/chaos_dist/test_failover_time.py` | 5 | Sleep measuring failover |
| `tests/suites/integration/test_offline_storage.py` | 5 | Sleep during offline |
| `tests/suites/integration/test_inbox_size_limit.py` | 5 | Sleep waiting for inbox |
| `tests/suites/integration/test_read_receipt_delivery.py` | 5 | Sleep waiting for receipt |
| `tests/suites/integration/test_deduplication.py` | 6 | Sleep waiting for dedup |
| `tests/suites/chaos_dist/test_server_storage_audit.py` | 5 | Sleep during audit |
| **Subtotal** | **~498** | |

### Category C: Legitimate (Keep)

| File | Count | Purpose |
|------|-------|---------|
| `tests/suites/security/test_slowloris.py` | 5 | Intentional slow-send attack simulation |
| `tests/suites/stress/test_soak_memory.py` | 9 | Intentional sustained load generation |
| `tests/perf/collect_metrics.py` | 2 | Metrics collection interval |
| **Subtotal** | **~16** | (overlap with Category B above) |

---

## Recommended Actions

### Priority 1: Replace synchronization sleeps in integration tests

Focus on test files with 5+ calls where sleeps are between test steps:
- Use `wait_until()` from `tests/framework/wait.py`
- Use `wait_for_condition()` from `tests/framework/readiness.py`

### Priority 2: Replace sleeps in chaos tests

The chaos tests have the most sleeps (18 files, ~180 calls) because they
wait for cluster operations. Use:
- `wait_for_mnesia_ready()` from `tests/framework/readiness.py`
- `wait_for_delivery()` pattern for message receipt verification

### Priority 3: Leave framework infrastructure sleeps

The `tests/framework/` sleeps are in polling loops with timeouts, which is
the correct pattern. These do NOT violate RFC Section 13.2.

---

## Target

- **Current**: ~530 `time.sleep()` calls across 108 files
- **Phase 3 target**: Reduce to <80 (framework + legitimate only)
- **Stretch goal**: <40 (framework-only)
