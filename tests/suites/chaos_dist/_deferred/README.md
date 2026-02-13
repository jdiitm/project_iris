# Previously Deferred Tests - RE-ENABLED

**Date Re-enabled**: 2026-01-24

## Status: ACTIVE

All three tests have been moved back to the active test suite:

- `test_ack_durability.py`
- `test_cross_region_latency.py`
- `test_multimaster_durability.py`

## Infrastructure Fixes Applied

1. **iris_region_bridge.erl**: New module for reliable cross-region message relay
   - Durable message queueing before ACK
   - Automatic retry with exponential backoff
   - Dead-letter queue for failed messages

2. **iris_presence.erl**: Versioned presence to fix race conditions
   - Monotonic version numbers for presence entries
   - Prevents stale routing decisions

3. **iris_async_router.erl**: Guaranteed offline fallback
   - All routing failures result in offline storage
   - Zero silent message drops

## Running These Tests

```bash
# Via the authoritative test runner (fresh cluster per test)
./tests/run_all_tests.sh --docker-only

# Or manually:
cd docker/global-cluster
./cluster.sh up
python3 ../../tests/suites/chaos_dist/test_cross_region_latency.py
./cluster.sh down
```

## Notes

These tests will gracefully SKIP (exit code 2) if:
- Docker is not available
- Docker cluster cannot be started
- Cross-region connectivity fails

Exit code 2 is distinguished from PASS (0) and FAIL (1) in the test runner.

They will FAIL (exit code 1) only if:
- The test runs but RFC requirements are not met
- P99 latency exceeds 500ms threshold
