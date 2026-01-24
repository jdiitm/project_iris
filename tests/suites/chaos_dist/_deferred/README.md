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
# With Docker cluster
make test-cross-region

# Or manually:
cd docker/global-cluster
docker compose up -d
bash init_cluster.sh
python3 tests/suites/chaos_dist/test_cross_region_latency.py
```

## Notes

These tests will gracefully SKIP (exit code 0) if:
- Docker is not available
- Docker cluster cannot be started
- Cross-region connectivity fails

They will FAIL (exit code 1) only if:
- The test runs but RFC requirements are not met
- P99 latency exceeds 500ms threshold
