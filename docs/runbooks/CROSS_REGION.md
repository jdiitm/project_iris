# Cross-Region Operations Runbook

## Overview

This runbook covers diagnosing and resolving cross-region message delivery issues in Iris.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     GLOBAL ROUTING LAYER                                │
│   (iris_region_router - Routes users to their home region)              │
└─────────────────────────────────────────────────────────────────────────┘
                      │                    │                    │
           ┌─────────▼─────────┐ ┌────────▼────────┐ ┌─────────▼─────────┐
           │   REGION: US      │ │ REGION: EU      │ │ REGION: APAC      │
           │   Mnesia Cluster  │ │ Mnesia Cluster  │ │ Mnesia Cluster    │
           │   (50 nodes max)  │ │ (50 nodes max)  │ │ (50 nodes max)    │
           └───────────────────┘ └─────────────────┘ └───────────────────┘
                      │                    │                    │
                      └──────────────────┬─┴────────────────────┘
                                         │
                              ┌──────────▼──────────┐
                              │  iris_region_bridge │
                              │  (Async Relay)      │
                              └─────────────────────┘
```

## Components

### 1. iris_region_router

Routes messages to users based on their home region (determined by `hash(UserID)`).

- Local users: Direct routing via `iris_async_router`
- Remote users: Via `iris_region_bridge` for reliable async delivery

### 2. iris_region_bridge

Handles cross-region message relay with durability guarantees:

- **Outbound Queue**: Messages durably queued in Mnesia before ACK
- **Retry Logic**: Exponential backoff (1s → 60s max)
- **Dead Letter Queue**: After 5 failed attempts

## Diagnosing Issues

### Symptom: Messages Not Delivered Cross-Region

1. **Check bridge status**:
   ```erlang
   iris_region_bridge:get_stats().
   %% Returns: #{sent => N, delivered => N, failed => N, retried => N, queue_depth => N}
   ```

2. **Check queue depth per region**:
   ```erlang
   iris_region_bridge:get_queue_depth(<<"eu-west-1">>).
   ```

3. **Check region connectivity**:
   ```erlang
   iris_region_router:get_region_endpoint(<<"eu-west-1">>).
   %% Returns: {ok, [Node1, Node2]} or {error, not_found}
   ```

### Symptom: High Queue Depth

This indicates messages are not being delivered to target regions.

1. **Check network connectivity**:
   ```bash
   # From source region node
   ping target-region-node
   ```

2. **Check Erlang distribution**:
   ```erlang
   net_adm:ping('core@eu-west-1.iris.io').
   ```

3. **Check target region health**:
   ```erlang
   rpc:call('core@eu-west-1.iris.io', iris_core, get_status, [<<"test_user">>], 5000).
   ```

### Symptom: Messages in Dead Letter Queue

Messages in dead letter queue have failed after 5 attempts.

1. **View dead letter messages**:
   ```erlang
   mnesia:dirty_all_keys(cross_region_dead_letter).
   mnesia:dirty_read(cross_region_dead_letter, MsgId).
   ```

2. **Check last error**:
   ```erlang
   [{_, #outbound_msg{target_region = Region, user_id = User, last_error = Error}}] = 
       mnesia:dirty_read(cross_region_dead_letter, MsgId).
   ```

## Recovery Procedures

### Manual Message Replay

For messages stuck in dead letter queue:

```erlang
%% Get message from dead letter
[Msg] = mnesia:dirty_read(cross_region_dead_letter, MsgId),

%% Re-queue for delivery
mnesia:dirty_delete(cross_region_dead_letter, MsgId),
iris_region_bridge:send_cross_region(
    Msg#outbound_msg.target_region,
    Msg#outbound_msg.user_id,
    Msg#outbound_msg.msg
).
```

### Force Drain Region

When connectivity is restored:

```erlang
iris_region_bridge:drain_region(<<"eu-west-1">>).
```

### Add Region Endpoint

To add a new region endpoint at runtime:

```erlang
iris_region_router:set_region_endpoint(
    <<"ap-south-1">>,
    ['core@ap-south-1-a.iris.io', 'core@ap-south-1-b.iris.io']
).
```

## Monitoring

### Key Metrics

1. **Queue Depth**: Should stay low (< 1000)
   ```erlang
   iris_region_bridge:get_queue_depth().
   ```

2. **Delivery Rate**: `delivered` should increase steadily
   ```erlang
   iris_region_bridge:get_stats().
   ```

3. **Retry Rate**: High `retried` indicates connectivity issues

### Alerts

Configure alerts for:

- Queue depth > 10000 (warning)
- Queue depth > 100000 (critical)
- Dead letter queue size > 1000
- Delivery rate = 0 for > 5 minutes

## Troubleshooting Checklist

1. [ ] Bridge process running: `whereis(iris_region_bridge)`
2. [ ] Outbound table exists: `mnesia:table_info(cross_region_outbound, size)`
3. [ ] Region endpoints configured: `application:get_env(iris_core, region_endpoints)`
4. [ ] Network connectivity: `net_adm:ping(TargetNode)`
5. [ ] Target region healthy: RPC call succeeds
6. [ ] No circuit breaker issues: `iris_circuit_breaker:get_all_status()`

## Configuration

### Application Environment

```erlang
[{iris_core, [
    {region_id, <<"us-east-1">>},
    {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
    {region_endpoints, #{
        <<"us-east-1">> => ['core@us-east-1-a.iris.io', 'core@us-east-1-b.iris.io'],
        <<"eu-west-1">> => ['core@eu-west-1-a.iris.io', 'core@eu-west-1-b.iris.io'],
        <<"ap-south-1">> => ['core@ap-south-1-a.iris.io', 'core@ap-south-1-b.iris.io']
    }}
]}].
```

### Bridge Configuration

Default values (tunable via application:set_env):

- `drain_interval_ms`: 100 (how often to check queue)
- `max_attempts`: 5 (before moving to dead letter)
- `base_backoff_ms`: 1000 (initial retry delay)
- `max_backoff_ms`: 60000 (maximum retry delay)
- `batch_size`: 100 (messages per drain cycle)

## See Also

- [DEPLOYMENT_GUIDE.md](../DEPLOYMENT_GUIDE.md) - Multi-region deployment
- [FAILOVER.md](FAILOVER.md) - Node failure recovery
- [DATA_RECOVERY.md](DATA_RECOVERY.md) - Data recovery procedures
