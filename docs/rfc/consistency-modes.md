# Consistency Modes — Design Decision Record

## Status

**Decided** — AP-only (hardened_ap) for v1.0. CP mode is explicitly unsupported.

## Context

Project Iris is a distributed messaging system deployed across multiple regions. The CAP theorem forces a fundamental choice between Consistency (CP) and Availability (AP) during network partitions.

The `consistency_mode` configuration parameter was introduced early in development, accepting values `cp`, `ap`, and `hardened_ap`. However, CP semantics were never implemented — the codebase has always operated in AP mode.

## Decision

**Iris v1.0 operates exclusively in `hardened_ap` mode.**

- `consistency_mode=cp` causes a fatal startup error in **all** deployment modes (CRIT-01 remediation).
- Unknown `consistency_mode` values cause a fatal startup error.
- The `cp` option will be re-enabled only when quorum-based consensus is implemented (see CP Roadmap below).

## hardened_ap Semantics

`hardened_ap` provides stronger guarantees than naive eventual consistency:

1. **Quorum Writes**: Offline message storage uses `mnesia:sync_transaction` with disc durability. Writes are confirmed only after WAL flush.

2. **Read Repair**: `get_status/1` reads from local Mnesia first. Stale data is corrected during reconciliation.

3. **Reconciliation (RFC 7.1.1)**:
   - `offline_msg` (bag): Union merge — all messages from all partitions are kept.
   - `user_status`: Last-Writer-Wins (LWW) by `last_seen` timestamp.
   - `user_meta`: Last-Writer-Wins (LWW) by `last_modified` timestamp.
   - `presence`: Local-authoritative (ram_copies, ephemeral).
   - `group_member`: LWW by `last_seen` timestamp.

4. **Deduplication**: Message dedup via `dedup_log` table prevents duplicate delivery after partition healing.

## Consistency Guarantees

| Property | Guarantee |
|---|---|
| Message delivery | At-least-once (dedup prevents visible duplicates) |
| Message ordering | Per-sender FIFO within a partition |
| Offline storage | Durable (sync_transaction + disc_only_copies) |
| Presence | Eventual (best-effort, partition-tolerant) |
| User metadata | LWW convergent |

## CP Exception: Key Bundles (E2EE)

Key bundle storage (`e2ee_key_bundle` table) is an intentional exception to AP-only:

- **Quorum writes required**: `iris_keys:store_key_bundle_durable/2` uses `iris_quorum_write:write_durable/3` when available.
- **Failure propagates**: If quorum is not reached, the write is REJECTED (not silently degraded to single-node). Clients must retry.
- **Rationale**: CP > AP for security-critical data. A lost key bundle permanently breaks E2EE session establishment. Availability degradation (temporary write rejection) is preferable to durability degradation.
- **Fallback**: When `iris_quorum_write` is not registered (e.g., single-node development), falls back to `mnesia:sync_transaction` (local durability only).
- **Replication**: The `e2ee_key_bundle` and `key_contact` tables are replicated as `disc_copies` across all core nodes via `init_cross_region_replication/0`.

## What hardened_ap Does NOT Guarantee

- Linearizable reads across regions
- Causal ordering across partitions
- Exactly-once delivery during partition healing (dedup mitigates this)
- Real-time consistency of presence data

## CP Roadmap

If CP semantics are required in the future:

1. Implement Raft-based consensus for the `user_status` and `user_meta` tables.
2. Add a quorum read path (`get_status_consistent/1`) that reads from a majority of nodes.
3. Introduce a partition detector that switches to read-only mode during minority partitions.
4. Update `validate_consistency_mode/0` to allow `cp` in production once implemented.

## References

- RFC-001 Section 7.1: Partition Recovery
- RFC-001 Section 7.1.1: Data Reconciliation
- Eric Brewer, "CAP Twelve Years Later" (2012)
