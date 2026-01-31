# Deferred Test: Clock Skew Ordering

## Status: P2 (Post-Launch)

## Purpose
Verify HLC (Hybrid Logical Clock) ordering guarantees under skewed clocks (NTP failure simulation).

## RFC Reference
- RFC-001 Section 5.3: Message Ordering
- Distributed systems rely on HLCs; testing with perfect clocks is unrealistic

## Test Mechanism
1. Use `libfaketime` or Docker capability to shift one node's clock forward/back by 30s
2. Send interleaved messages from both nodes (normal clock and skewed clock)
3. Verify messages remain strictly ordered by causality (HLC logical component)
4. Physical timestamps may regress but logical order MUST be preserved

## Implementation Notes
```python
# Approach: Use libfaketime in Docker
# 1. Install libfaketime in container
# 2. Set FAKETIME="+30s" environment variable
# 3. Send messages and verify HLC ordering

def skew_container_clock(container: str, offset_seconds: int):
    """Skew container's clock using libfaketime."""
    subprocess.run([
        "docker", "exec", container,
        "sh", "-c", f"export FAKETIME='+{offset_seconds}s'"
    ])
```

## Prerequisites
- Docker containers with libfaketime installed
- Or use `--cap-add=SYS_TIME` for direct clock manipulation

## Why Deferred
- Lower priority than network partition and cross-region chaos tests
- Requires additional Docker image customization (libfaketime)
- Current HLC implementation is well-tested at unit level

## Acceptance Criteria
- Messages from skewed-clock node maintain causal ordering
- HLC logical counter increments correctly despite physical clock regression
- No message reordering observed by recipients

## Related Files
- `src/iris_hlc.erl` - HLC implementation
- `test_utils/iris_hlc_tests.erl` - Unit tests for HLC
