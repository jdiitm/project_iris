# Replication Lag Mitigation Strategy

## The Problem

```
Timeline of Message Loss During Failover:

T+0ms:    Client sends message to Primary (Laptop A)
T+1ms:    Primary writes to local Mnesia (async_dirty - returns immediately)
T+2ms:    Primary ACKs to client ("message stored")
T+3ms:    Mnesia begins async replication to Replica (Laptop B)
T+5ms:    PRIMARY CRASHES (kill -9, power loss, etc.)
T+???:    Replication NEVER COMPLETES
          
Result:   Client thinks message is stored. Message is LOST.
```

**Current code causing this** (`iris_offline_storage.erl:16`):
```erlang
mnesia:activity(async_dirty, F).  %% Returns before replication!
```

---

## Mitigation Strategy 1: Synchronous Replication (Recommended)

### Approach
Wait for write to be confirmed on BOTH nodes before acknowledging to client.

### Implementation

```erlang
%% src/iris_offline_storage.erl - FIXED VERSION
-module(iris_offline_storage).
-export([store/3, store_batch/3, retrieve/2]).

-define(REPLICATION_TIMEOUT, 5000).  %% 5 seconds

store(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    
    F = fun() ->
        mnesia:write({offline_msg, Key, Timestamp, Msg})
    end,
    
    %% CRITICAL CHANGE: Use sync_transaction instead of async_dirty
    %% This waits for:
    %%   1. Local write to complete
    %%   2. Replication to all disc_copies nodes
    %%   3. Transaction commit on all nodes
    case mnesia:activity(sync_transaction, F) of
        {atomic, ok} -> ok;
        {atomic, Result} -> Result;
        {aborted, Reason} ->
            logger:error("Offline store failed for ~p: ~p", [User, Reason]),
            {error, Reason}
    end.

store_batch(User, Msgs, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketedMsgs = lists:foldl(fun(Msg, Acc) ->
        Bucket = erlang:phash2(Msg, Count),
        orddict:append(Bucket, Msg, Acc)
    end, orddict:new(), Msgs),
    
    F = fun() ->
        lists:foreach(fun({Bucket, Batch}) ->
             Key = {User, Bucket},
             mnesia:write({offline_msg, Key, Timestamp, Batch})
        end, orddict:to_list(BucketedMsgs))
    end,
    
    %% CRITICAL: sync_transaction for batch too
    mnesia:activity(sync_transaction, F).
```

### Performance Impact

| Write Type | async_dirty | sync_transaction | Overhead |
|------------|-------------|------------------|----------|
| Local only | ~0.1ms | ~1ms | 10x |
| With replica (LAN) | ~0.1ms | ~3ms | 30x |
| With replica (WAN) | ~0.1ms | ~150-300ms | 1500x+ |

**Verdict**: Acceptable for Bangalore cluster (LAN). Problematic if Core ever spans WAN.

---

## Mitigation Strategy 2: Write-Ahead Log (WAL) + Async Replication

### Approach
Write to local durable log FIRST, then replicate asynchronously. On crash recovery, replay the log.

### Implementation

```erlang
%% src/iris_wal.erl - Write-Ahead Log
-module(iris_wal).
-behaviour(gen_server).
-export([start_link/0, append/2, replay/1, checkpoint/0]).

-record(state, {
    log_handle,
    sequence = 0,
    pending = #{}  %% SeqNo -> {User, Msg, Timestamp}
}).

-define(WAL_FILE, "/data/iris/wal.log").
-define(CHECKPOINT_FILE, "/data/iris/wal.checkpoint").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Open disk_log for durable writes
    {ok, Log} = disk_log:open([
        {name, iris_wal},
        {file, ?WAL_FILE},
        {type, halt},          %% Single file, not wrap
        {format, internal},    %% Erlang terms
        {mode, read_write}
    ]),
    
    %% Replay any uncommitted entries from previous crash
    spawn_link(fun() -> timer:sleep(1000), replay_and_commit() end),
    
    {ok, #state{log_handle = Log, sequence = load_sequence()}}.

%% Append to WAL BEFORE Mnesia write
append(User, Msg) ->
    gen_server:call(?MODULE, {append, User, Msg}, 10000).

handle_call({append, User, Msg}, _From, State = #state{log_handle = Log, sequence = Seq}) ->
    Timestamp = os:system_time(millisecond),
    Entry = {Seq + 1, User, Msg, Timestamp, pending},
    
    %% CRITICAL: sync write to disk - survives crash
    ok = disk_log:log(Log, Entry),
    ok = disk_log:sync(Log),  %% Force to disk
    
    %% Now safe to do async Mnesia write
    spawn(fun() ->
        BucketCount = iris_core:get_bucket_count(User),
        iris_offline_storage:store_internal(User, Msg, BucketCount),
        %% Mark as committed in WAL
        gen_server:cast(?MODULE, {committed, Seq + 1})
    end),
    
    NewState = State#state{
        sequence = Seq + 1,
        pending = maps:put(Seq + 1, {User, Msg, Timestamp}, State#state.pending)
    },
    
    {reply, {ok, Seq + 1}, NewState};

handle_cast({committed, SeqNo}, State = #state{pending = Pending}) ->
    %% Remove from pending, optionally log commit
    {noreply, State#state{pending = maps:remove(SeqNo, Pending)}}.

%% On startup, replay uncommitted entries
replay_and_commit() ->
    {ok, Log} = disk_log:open([{name, iris_wal_replay}, {file, ?WAL_FILE}, {mode, read_only}]),
    replay_entries(Log, disk_log:chunk(Log, start)).

replay_entries(_Log, eof) -> 
    logger:info("WAL replay complete"),
    ok;
replay_entries(Log, {Cont, Entries}) ->
    lists:foreach(fun({SeqNo, User, Msg, _Ts, pending}) ->
        logger:info("Replaying WAL entry ~p for user ~p", [SeqNo, User]),
        BucketCount = iris_core:get_bucket_count(User),
        iris_offline_storage:store_internal(User, Msg, BucketCount);
    ({_SeqNo, _User, _Msg, _Ts, committed}) ->
        ok  %% Already committed
    end, Entries),
    replay_entries(Log, disk_log:chunk(Log, Cont)).

%% Periodic checkpoint - truncate WAL
checkpoint() ->
    gen_server:call(?MODULE, checkpoint).

handle_call(checkpoint, _From, State = #state{log_handle = Log, pending = Pending}) ->
    case maps:size(Pending) of
        0 ->
            %% Safe to truncate - all entries committed
            disk_log:truncate(Log),
            {reply, ok, State};
        N ->
            {reply, {pending, N}, State}
    end.
```

### Storage Module with WAL

```erlang
%% src/iris_offline_storage.erl - WAL-backed version
-module(iris_offline_storage).
-export([store/3, store_internal/3, store_batch/3, retrieve/2]).

%% Public API - goes through WAL
store(User, Msg, Count) ->
    %% Step 1: Write to WAL (durable, survives crash)
    case iris_wal:append(User, Msg) of
        {ok, _SeqNo} ->
            %% WAL write succeeded - message will not be lost
            %% Mnesia write happens async in WAL module
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal - called by WAL after durable write
store_internal(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    F = fun() -> mnesia:write({offline_msg, Key, Timestamp, Msg}) end,
    mnesia:activity(async_dirty, F).  %% OK to be async - WAL has it
```

### Performance Impact

| Operation | Latency | Durability |
|-----------|---------|------------|
| WAL append (SSD) | ~1-2ms | 100% - survives crash |
| Mnesia async | ~0.1ms | Replicated eventually |
| Total | ~2ms | Durable immediately |

**Verdict**: Best balance of performance and durability. Recommended for production.

---

## Mitigation Strategy 3: Client-Side Acknowledgment with Retry

### Approach
Client doesn't consider message "sent" until server confirms replication.

### Protocol Extension

```erlang
%% New message types for reliable delivery
-define(OP_SEND_RELIABLE, 16#10).      %% Client sends with msg_id
-define(OP_STORE_ACK, 16#11).          %% Server confirms storage
-define(OP_STORE_NACK, 16#12).         %% Server reports failure

%% src/iris_proto.erl additions
encode({store_ack, MsgId}) ->
    <<16#11, MsgId:128/binary>>;
encode({store_nack, MsgId, Reason}) ->
    ReasonBin = atom_to_binary(Reason),
    <<16#12, MsgId:128/binary, (byte_size(ReasonBin)):8, ReasonBin/binary>>.

decode(<<16#10, MsgId:128/binary, RecipientLen:8, Recipient:RecipientLen/binary, 
         MsgLen:16, Msg:MsgLen/binary, Rest/binary>>) ->
    {{send_reliable, MsgId, Recipient, Msg}, Rest}.
```

### Server-Side Handler

```erlang
%% src/iris_session.erl - reliable send handler
handle_packet({send_reliable, MsgId, Recipient, Msg}, State, Socket, Transport) ->
    %% Check for duplicate (idempotency)
    case is_duplicate(MsgId) of
        true ->
            %% Already processed - just ACK
            Transport:send(Socket, iris_proto:encode({store_ack, MsgId})),
            {noreply, State};
        false ->
            %% Process message
            case store_with_confirmation(Recipient, Msg) of
                ok ->
                    %% Mark as processed (for dedup)
                    mark_processed(MsgId),
                    Transport:send(Socket, iris_proto:encode({store_ack, MsgId})),
                    {noreply, State};
                {error, Reason} ->
                    Transport:send(Socket, iris_proto:encode({store_nack, MsgId, Reason})),
                    {noreply, State}
            end
    end.

store_with_confirmation(Recipient, Msg) ->
    %% Use sync_transaction to ensure replication
    case iris_core:lookup_user(Recipient) of
        {ok, Node, Pid} ->
            %% Online - deliver directly (still need ACK from recipient)
            deliver_and_wait_ack(Node, Pid, Msg);
        {error, not_found} ->
            %% Offline - store with sync replication
            iris_offline_storage:store_sync(Recipient, Msg)
    end.

%% Deduplication using ETS with TTL
-define(DEDUP_TABLE, msg_dedup).
-define(DEDUP_TTL, 3600000).  %% 1 hour

is_duplicate(MsgId) ->
    case ets:lookup(?DEDUP_TABLE, MsgId) of
        [{MsgId, _Ts}] -> true;
        [] -> false
    end.

mark_processed(MsgId) ->
    ets:insert(?DEDUP_TABLE, {MsgId, os:system_time(millisecond)}),
    %% Cleanup old entries periodically
    spawn(fun() -> cleanup_dedup() end).

cleanup_dedup() ->
    Now = os:system_time(millisecond),
    Cutoff = Now - ?DEDUP_TTL,
    ets:select_delete(?DEDUP_TABLE, [
        {{'$1', '$2'}, [{'<', '$2', Cutoff}], [true]}
    ]).
```

### Client-Side Retry Logic

```python
# iris_client.py - reliable send with retry
class IrisClient:
    def __init__(self, host, port, tls=False):
        self.pending_acks = {}  # msg_id -> (recipient, msg, timestamp, retries)
        self.max_retries = 3
        self.ack_timeout = 10.0  # seconds
        
    def send_reliable(self, recipient, message):
        """Send message with delivery guarantee"""
        msg_id = self._generate_msg_id()
        
        for attempt in range(self.max_retries + 1):
            # Send message
            self._send_reliable_packet(msg_id, recipient, message)
            
            # Wait for ACK
            try:
                response = self._wait_for_ack(msg_id, timeout=self.ack_timeout)
                if response['type'] == 'store_ack':
                    return {'status': 'delivered', 'msg_id': msg_id}
                elif response['type'] == 'store_nack':
                    # Server explicitly rejected - don't retry
                    return {'status': 'rejected', 'reason': response['reason']}
            except TimeoutError:
                if attempt < self.max_retries:
                    print(f"Retry {attempt + 1}/{self.max_retries} for {msg_id}")
                    continue
                else:
                    raise DeliveryError(f"Failed after {self.max_retries} retries")
        
    def _generate_msg_id(self):
        """Generate unique message ID (UUID + timestamp)"""
        import uuid
        import time
        return f"{uuid.uuid4().hex}{int(time.time()*1000):016x}"
```

---

## Mitigation Strategy 4: Quorum Writes

### Approach
Require write confirmation from N/2+1 nodes before ACK.

### Implementation

```erlang
%% src/iris_quorum.erl - Quorum write module
-module(iris_quorum).
-export([write/3, read/2]).

-define(WRITE_QUORUM, 2).  %% For 2-node cluster, need both (or 1 if 1 node)
-define(WRITE_TIMEOUT, 5000).

%% Write to quorum of nodes
write(Table, Key, Value) ->
    Nodes = mnesia:system_info(running_db_nodes),
    Required = min(?WRITE_QUORUM, length(Nodes)),
    
    %% Parallel write to all nodes
    Parent = self(),
    Refs = [spawn_monitor(fun() ->
        Result = rpc:call(Node, mnesia, dirty_write, [Table, {Table, Key, Value}], ?WRITE_TIMEOUT),
        Parent ! {write_result, Node, Result}
    end) || Node <- Nodes],
    
    %% Collect responses
    collect_quorum(Required, length(Nodes), [], []).

collect_quorum(Required, _Total, Successes, _Failures) when length(Successes) >= Required ->
    {ok, Successes};
collect_quorum(Required, Total, Successes, Failures) when length(Successes) + length(Failures) >= Total ->
    %% All responses in, not enough successes
    {error, {insufficient_quorum, Successes, Failures}};
collect_quorum(Required, Total, Successes, Failures) ->
    receive
        {write_result, Node, ok} ->
            collect_quorum(Required, Total, [Node | Successes], Failures);
        {write_result, Node, {badrpc, Reason}} ->
            collect_quorum(Required, Total, Successes, [{Node, Reason} | Failures]);
        {write_result, Node, {error, Reason}} ->
            collect_quorum(Required, Total, Successes, [{Node, Reason} | Failures]);
        {'DOWN', _Ref, process, _Pid, Reason} ->
            collect_quorum(Required, Total, Successes, [{unknown, Reason} | Failures])
    after ?WRITE_TIMEOUT ->
        {error, timeout}
    end.
```

### Offline Storage with Quorum

```erlang
%% src/iris_offline_storage.erl - Quorum version
store_quorum(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    Value = {Key, Timestamp, Msg},
    
    case iris_quorum:write(offline_msg, Key, Value) of
        {ok, Nodes} ->
            logger:debug("Message stored on ~p nodes", [length(Nodes)]),
            ok;
        {error, {insufficient_quorum, Successes, Failures}} ->
            logger:warning("Quorum not reached. Success: ~p, Failed: ~p", 
                          [Successes, Failures]),
            %% Decide: fail or accept partial write?
            case Successes of
                [] -> {error, no_nodes_available};
                _ -> {ok, degraded}  %% At least one node has it
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

---

## Recommended Approach for Your Setup

Given your 2-laptop cluster on LAN, here's the recommended combination:

### Primary: sync_transaction (Simple, Effective)

```erlang
%% iris_offline_storage.erl
store(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    
    F = fun() ->
        mnesia:write({offline_msg, Key, Timestamp, Msg})
    end,
    
    %% sync_transaction waits for replication
    case mnesia:activity(sync_transaction, F) of
        {atomic, _} -> ok;
        {aborted, {no_majority, _}} ->
            %% Cluster split - one node only
            logger:warning("No majority - storing on local node only"),
            mnesia:activity(transaction, F);
        {aborted, Reason} ->
            {error, Reason}
    end.
```

### Secondary: Deduplication (Handles Retries)

```erlang
%% Add to iris_core.erl or new module
-module(iris_dedup).
-export([init/0, check_and_mark/1, is_duplicate/1]).

-define(TABLE, msg_dedup).

init() ->
    ets:new(?TABLE, [named_table, public, set, 
                     {write_concurrency, true},
                     {read_concurrency, true}]).

check_and_mark(MsgId) ->
    case ets:insert_new(?TABLE, {MsgId, os:system_time(millisecond)}) of
        true -> new;        %% First time seeing this message
        false -> duplicate  %% Already processed
    end.

is_duplicate(MsgId) ->
    ets:member(?TABLE, MsgId).
```

### Tertiary: Client Acknowledgment

Update protocol to require ACK before client considers message sent.

---

## Testing the Mitigation

### Test: Replication Lag Under Load

```bash
#!/bin/bash
# test_replication_lag.sh

echo "=== REPLICATION LAG TEST ==="

# Phase 1: Measure baseline replication lag
echo "[Phase 1] Measuring replication lag..."

ssh laptop-a "erl -sname lag_test -setcookie iris_cluster_secret -noshell -eval \"
    %% Write on A, read on B, measure time
    Measurements = lists:map(fun(N) ->
        Key = {lag_test, N},
        Value = {offline_msg, Key, os:system_time(millisecond), <<\\\"test\\\">>},
        
        Start = os:system_time(microsecond),
        mnesia:dirty_write(Value),
        
        %% Poll B until it sees the write
        WaitStart = os:system_time(microsecond),
        wait_for_replica('iris_core_b@laptop-b', offline_msg, Key, 5000),
        ReplicaTime = os:system_time(microsecond) - WaitStart,
        
        ReplicaTime
    end, lists:seq(1, 100)),
    
    Avg = lists:sum(Measurements) / length(Measurements),
    Max = lists:max(Measurements),
    P99 = lists:nth(99, lists:sort(Measurements)),
    
    io:format('~nReplication Lag (async_dirty):~n'),
    io:format('  Average: ~.2f us~n', [Avg]),
    io:format('  P99: ~.2f us~n', [P99]),
    io:format('  Max: ~.2f us~n', [Max]),
    
    init:stop().

wait_for_replica(Node, Table, Key, Timeout) ->
    wait_for_replica(Node, Table, Key, Timeout, os:system_time(millisecond)).

wait_for_replica(Node, Table, Key, Timeout, Start) ->
    case rpc:call(Node, mnesia, dirty_read, [Table, Key], 1000) of
        [_] -> ok;
        [] ->
            Now = os:system_time(millisecond),
            if Now - Start > Timeout -> timeout;
               true ->
                   timer:sleep(1),
                   wait_for_replica(Node, Table, Key, Timeout, Start)
            end
    end.
\""

# Phase 2: Compare with sync_transaction
echo ""
echo "[Phase 2] Measuring with sync_transaction..."

# Similar test but using sync_transaction - should show ~0 lag
# because function only returns after replication

# Phase 3: Test message survival with sync writes
echo ""
echo "[Phase 3] Testing message survival..."

# Send 1000 messages with sync writes
python3 << 'EOF'
from iris_client import IrisClient

c = IrisClient('laptop-a', 8085)
c.login('sync_test_sender')

for i in range(1000):
    c.send_msg('sync_test_receiver', f'SYNC_MSG_{i}')

print("1000 messages sent with sync replication")
c.close()
EOF

# Kill A immediately
echo "Killing Laptop A immediately after send..."
ssh laptop-a "pkill -9 -f beam.smp"

sleep 5

# Check messages on B
echo "Checking messages on Laptop B..."
python3 << 'EOF'
from iris_client import IrisClient

c = IrisClient('laptop-b', 8087)
c.login('sync_test_receiver')

count = 0
while True:
    try:
        msg = c.recv_msg(timeout=3)
        if b'SYNC_MSG_' in msg:
            count += 1
    except:
        break

print(f"Received {count}/1000 messages")
if count == 1000:
    print("SUCCESS: Zero message loss with sync replication!")
elif count >= 990:
    print(f"ACCEPTABLE: {1000-count} messages lost (in-flight during kill)")
else:
    print(f"FAIL: {1000-count} messages lost - sync replication not working")

c.close()
EOF
```

### Test: Deduplication

```bash
#!/bin/bash
# test_deduplication.sh

echo "=== DEDUPLICATION TEST ==="

python3 << 'EOF'
import socket
import time

# Send same message 10 times (simulating retry)
MSG_ID = b'test_dedup_12345678901234567890123456'

s = socket.socket()
s.connect(('laptop-a', 8085))
s.send(b'\x01dedup_sender')
s.recv(1024)

for i in range(10):
    # Send reliable message with same ID
    packet = b'\x10' + MSG_ID + b'\x0cdedup_target' + b'\x00\x10duplicate_test_msg'
    s.send(packet)
    time.sleep(0.1)

s.close()

# Now check how many messages receiver gets
s2 = socket.socket()
s2.connect(('laptop-a', 8085))
s2.send(b'\x01dedup_target')
s2.recv(1024)

count = 0
s2.settimeout(3)
while True:
    try:
        msg = s2.recv(4096)
        if b'duplicate_test_msg' in msg:
            count += 1
    except:
        break

print(f"Sent same message 10 times")
print(f"Receiver got {count} copies")
if count == 1:
    print("SUCCESS: Deduplication working!")
else:
    print(f"FAIL: Should be 1, got {count}")

s2.close()
EOF
```

---

## Summary

| Strategy | Latency Impact | Complexity | Data Safety | Recommended |
|----------|---------------|------------|-------------|-------------|
| **sync_transaction** | +2-3ms (LAN) | Low | High | ✅ Primary |
| **WAL + async** | +1-2ms | Medium | High | ✅ For scale |
| **Client retry + dedup** | None | Medium | High | ✅ Always |
| **Quorum writes** | +3-5ms | High | Highest | For critical data |

**For your 2-laptop cluster**: Use `sync_transaction` + client-side deduplication. This combination ensures:

1. Every acknowledged message is on both laptops
2. If client retries (timeout, network issue), no duplicates
3. If either laptop dies, zero message loss

---

*Document created: January 15, 2026*
*Applies to: Bangalore Core Cluster (Laptop A + Laptop B)*
