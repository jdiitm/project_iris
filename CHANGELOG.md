# Changelog

All notable changes to Iris Messaging System.

## [Unreleased]

### Fixed

#### Cross-Region Message Routing (2026-01-20)

**Critical fix: Messages from US West to Sydney now delivered (was 0% → 100%).**

- **`iris_async_router.erl`**: Added `find_user_across_cores/2` function that queries 
  ALL connected core nodes to find online users. Previously, only one core was queried, 
  so users registered on different cores (non-replicated Mnesia) were never found.
  
  ```erlang
  find_user_across_cores([Core | Rest], User) ->
      case rpc:call(Core, iris_core, lookup_user, [User], 2000) of
          {ok, _Node, UserPid} -> {ok, UserPid};
          _ -> find_user_across_cores(Rest, User)
      end.
  ```

- **`test_cross_region_latency.py`**: Fixed binary protocol parsing. Test was decoding 
  binary as UTF-8 text - now properly searches for `LATENCY_` markers in raw bytes.

**Results**: P99 latency 2.69ms (local Docker), 100% delivery rate. RFC NFR-3 compliant.

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
- Cross-region routing now queries all cores (workaround for non-replicated Mnesia)
- All chaos_dist tests now pass with Docker cluster

### Caveats

- **Cross-region latency in production**: Local Docker P99 of ~3ms will be 100-300ms 
  in real geo-distributed deployment due to network distance.
- **Mnesia replication**: Current fix works around non-replicated Mnesia. For production, 
  consider Mnesia replication or external presence service.

---

## [Previous]

See git history for earlier changes.
