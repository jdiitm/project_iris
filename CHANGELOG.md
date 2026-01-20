# Changelog

All notable changes to Iris Messaging System.

## [Unreleased]

### Fixed

#### Mnesia Durability & Recovery (2026-01-20)

**Critical fix for message persistence across node restarts.**

- **`iris_core.erl`**: Fixed `init_db()` to detect existing Mnesia data and recover 
  instead of recreating schema. Messages now survive node crashes as required by 
  RFC NFR-6 (99.999% durability) and NFR-8 (RPO=0).

- **`iris_async_router.erl`**: Changed `nodes()` to `nodes(connected)` in 
  `get_discovery_nodes()`. Edge nodes run with `-hidden` flag, so regular `nodes()` 
  returns empty list, breaking offline message storage.

- **`iris_session.erl`**: Updated `legacy_core_node()` to match both `"iris_core"` 
  (Makefile naming) and `"core_"` (Docker naming) patterns for core node discovery.

- **`docker-compose.yml`**: Added explicit `-mnesia dir '"/data/mnesia"'` to ensure 
  data persists in volume mount location.

#### Test Suite Improvements (2026-01-20)

- **`test_dist_failover.py`**: Rewrote from interactive infinite-loop test to 
  automated 5-scenario test suite (~23s runtime):
  - Basic connectivity
  - Node kill & recovery  
  - Concurrent connections under stress
  - Rapid connect/disconnect cycles
  - Container pause/resume (network partition simulation)

- **`test_ack_durability.py`**: Added edge-to-core reconnection after restart 
  (hidden nodes don't auto-reconnect).

### Changed

- Mnesia initialization now distinguishes between fresh start vs recovery
- All chaos_dist tests now pass with Docker cluster

---

## [Previous]

See git history for earlier changes.
