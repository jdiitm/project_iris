# `time.sleep()` Usage Audit

> **Frozen artifact.** Original audit: 2026-02-07. Updated: 2026-02-18 after cleanup effort. For detailed per-file history, see `git log --all -- tests/` with the `time.sleep` removal commits.

**RFC Reference**: Section 13.2 — "Tests MUST NOT use `time.sleep()` for synchronization"

| Metric | Original (2026-02-07) | Current (2026-02-18) |
|--------|----------------------|---------------------|
| Total instances | ~530 | ~401 |
| Files with sleep | 108 | 88 |
| Calls removed | — | ~130 (across 8 commits) |

---

## Classification Summary

| Category | Count | Action |
|----------|-------|--------|
| **A: Framework infrastructure** (polling loops, readiness checks) | ~32 | Keep — correct pattern |
| **B: Synchronization substitutes** (should use `wait_until()`) | ~355 | Replace over time |
| **C: Legitimate** (attack simulation, sustained load) | ~14 | Keep — intentional |

## Recommended Fix Pattern

```python
from tests.framework.wait import wait_until
wait_until(lambda: condition_met(), timeout=10, interval=0.2)
```

## Target

- **Current**: ~401 instances across 88 files
- **Goal**: <80 (framework + legitimate only)
