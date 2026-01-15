# Hardware Efficiency: Extreme Cost Optimization

## Design Principle

> **Every CPU cycle, byte of RAM, disk IOPS, and network packet must deliver user value.**

The system must fully utilize all available hardware resources to achieve extreme cost efficiency. Underutilization is waste; waste is unacceptable at scale.

---

## Resource Utilization Targets

| Resource | Target Utilization | Rationale |
|----------|-------------------|-----------|
| **CPU** | 70-85% at peak | Below 70% = overpaying; above 85% = no headroom |
| **Memory** | 80-90% | RAM is expensive; unused RAM is waste |
| **Network** | 60-80% of capacity | Leave room for bursts |
| **Storage IOPS** | 70-80% | SSD endurance and burst capacity |
| **Storage Space** | 70-85% | Growth headroom |

---

## 1. CPU Utilization

### Erlang Scheduler Optimization

Erlang uses one scheduler per CPU core by default. Ensure all cores are utilized.

```erlang
%% Verify scheduler count matches cores
erlang:system_info(schedulers).        %% Should equal core count
erlang:system_info(schedulers_online). %% Actually running

%% Force all schedulers active (prevent sleeping)
erlang:system_flag(scheduler_wall_time, true).

%% Bind schedulers to cores (NUMA optimization)
%% Start Erlang with: +sbt db (default bind)
```

### Startup Flags for Maximum CPU Utilization

```bash
#!/bin/bash
# start_optimized.sh

CORES=$(nproc)

erl \
    +S $CORES:$CORES \           # Schedulers:SchedulersOnline
    +sbt db \                     # Scheduler bind type: default bind
    +sbwt very_long \             # Scheduler busy wait threshold
    +swt very_low \               # Scheduler wakeup threshold  
    +sub true \                   # Scheduler utilization balancing
    +Muacul 0 \                   # Allocator carrier utilization limit
    +P 10000000 \                 # Max processes (10M)
    +Q 10000000 \                 # Max ports (10M)
    -env ERL_FULLSWEEP_AFTER 20 \ # GC tuning
    -pa ebin \
    -s iris_app start
```

### Process Distribution Across Schedulers

```erlang
%% Ensure processes are distributed across schedulers
%% DON'T: All processes on scheduler 1
%% DO: Processes spread evenly

%% Check scheduler utilization
scheduler_utilization() ->
    erlang:statistics(scheduler_wall_time).

%% Monitor for imbalance
check_scheduler_balance() ->
    Times = scheduler_utilization(),
    Utilizations = [{Sched, Active/Total*100} || {Sched, Active, Total} <- Times],
    Avg = lists:sum([U || {_, U} <- Utilizations]) / length(Utilizations),
    Max = lists:max([U || {_, U} <- Utilizations]),
    Min = lists:min([U || {_, U} <- Utilizations]),
    Imbalance = Max - Min,
    #{
        average => Avg,
        max => Max,
        min => Min,
        imbalance => Imbalance,
        healthy => Imbalance < 20  %% < 20% spread is healthy
    }.
```

### CPU-Bound Work Distribution

```erlang
%% Route CPU-intensive work to dedicated schedulers
%% Prevents message processing from starving

%% Option 1: Dirty schedulers for CPU work
%% (encryption, compression, hashing)
encrypt_message(Msg, Key) ->
    erlang:nif_dirty_cpu(fun() ->
        crypto:crypto_one_time(aes_256_gcm, Key, IV, Msg, true)
    end).

%% Option 2: Dedicated worker pool with scheduler affinity
-module(iris_cpu_workers).

start_workers() ->
    NumSchedulers = erlang:system_info(schedulers),
    [begin
        Pid = spawn_opt(fun worker_loop/0, [
            {scheduler, N},  %% Pin to scheduler N
            {priority, low}  %% Don't starve message handling
        ]),
        register(list_to_atom("cpu_worker_" ++ integer_to_list(N)), Pid)
    end || N <- lists:seq(1, NumSchedulers)].
```

### Measuring CPU Efficiency

```erlang
%% src/iris_metrics.erl - CPU metrics
-export([cpu_metrics/0]).

cpu_metrics() ->
    %% Scheduler utilization
    SchedUtil = scheduler_utilization(),
    
    %% Reductions (work done)
    {_, Reductions} = erlang:statistics(reductions),
    
    %% Context switches (overhead)
    {ContextSwitches, _} = erlang:statistics(context_switches),
    
    %% Run queue length (work waiting)
    RunQueue = erlang:statistics(run_queue),
    
    #{
        scheduler_utilization => SchedUtil,
        reductions_per_sec => Reductions,
        context_switches => ContextSwitches,
        run_queue_length => RunQueue,
        
        %% Efficiency indicator
        efficiency => if
            RunQueue > 1000 -> overloaded;
            SchedUtil < 50 -> underutilized;
            SchedUtil > 90 -> near_capacity;
            true -> optimal
        end
    }.
```

---

## 2. Memory Utilization

### Memory Budget Per Connection

```
Target: < 10KB per connection

Breakdown:
├─ TCP socket buffer:     4KB (tunable)
├─ Erlang process heap:   2KB (min_heap_size)
├─ Process dictionary:    0.5KB
├─ Message queue:         1KB (avg)
├─ Application state:     2KB
└─ Overhead:              0.5KB
────────────────────────────────
Total:                    ~10KB
```

### Connection Memory Optimization

```erlang
%% src/iris_edge_conn.erl - Memory-optimized connection process

%% Spawn with minimal heap
start_connection(Socket) ->
    spawn_opt(?MODULE, init, [Socket], [
        {min_heap_size, 233},     %% ~2KB minimum heap
        {min_bin_vheap_size, 46}, %% Binary heap
        {fullsweep_after, 10},    %% Aggressive GC
        {message_queue_data, off_heap}  %% Large messages off-heap
    ]).

%% Hibernate when idle (frees heap)
handle_info(timeout, State) ->
    {noreply, State, hibernate};

%% After processing, consider hibernating
handle_cast(Msg, State) ->
    NewState = process_msg(Msg, State),
    case should_hibernate(NewState) of
        true -> {noreply, NewState, hibernate};
        false -> {noreply, NewState, ?IDLE_TIMEOUT}
    end.

should_hibernate(#state{last_activity = Last}) ->
    Now = erlang:monotonic_time(millisecond),
    Now - Last > 30000.  %% 30s idle → hibernate
```

### ETS Memory Optimization

```erlang
%% Optimize ETS tables for memory efficiency

%% Presence table - read-heavy, write-light
ets:new(presence, [
    set,
    named_table,
    public,
    {read_concurrency, true},   %% Optimizes for concurrent reads
    {write_concurrency, auto},  %% Let Erlang decide
    compressed                   %% Compress data (CPU vs memory tradeoff)
]).

%% Session table - frequent updates
ets:new(sessions, [
    set,
    named_table,
    public,
    {write_concurrency, true},  %% Many concurrent writers
    {decentralized_counters, true}  %% Reduce contention
]).

%% Monitor ETS memory usage
ets_memory_usage() ->
    Tables = ets:all(),
    Usage = [{T, ets:info(T, memory) * erlang:system_info(wordsize)} 
             || T <- Tables],
    TotalBytes = lists:sum([M || {_, M} <- Usage]),
    #{
        tables => lists:sort(fun({_,A}, {_,B}) -> A > B end, Usage),
        total_bytes => TotalBytes,
        total_mb => TotalBytes / 1024 / 1024
    }.
```

### Memory Monitoring and Alerts

```erlang
%% src/iris_memory_monitor.erl
-module(iris_memory_monitor).
-behaviour(gen_server).

-define(CHECK_INTERVAL, 10000).  %% 10 seconds
-define(WARNING_THRESHOLD, 0.80). %% 80% memory
-define(CRITICAL_THRESHOLD, 0.90). %% 90% memory

init([]) ->
    erlang:send_after(?CHECK_INTERVAL, self(), check),
    {ok, #{}}.

handle_info(check, State) ->
    MemData = erlang:memory(),
    Total = proplists:get_value(total, MemData),
    System = proplists:get_value(system, MemData),
    Processes = proplists:get_value(processes, MemData),
    Binary = proplists:get_value(binary, MemData),
    ETS = proplists:get_value(ets, MemData),
    
    %% Get system memory limit
    {ok, MemLimit} = application:get_env(iris_core, memory_limit),
    Utilization = Total / MemLimit,
    
    %% Alert if needed
    if
        Utilization > ?CRITICAL_THRESHOLD ->
            logger:critical("Memory CRITICAL: ~.1f%", [Utilization * 100]),
            trigger_gc_all_processes();
        Utilization > ?WARNING_THRESHOLD ->
            logger:warning("Memory WARNING: ~.1f%", [Utilization * 100]);
        true ->
            ok
    end,
    
    %% Emit metrics
    iris_metrics:gauge(memory_utilization, Utilization),
    iris_metrics:gauge(memory_processes, Processes),
    iris_metrics:gauge(memory_binary, Binary),
    iris_metrics:gauge(memory_ets, ETS),
    
    erlang:send_after(?CHECK_INTERVAL, self(), check),
    {noreply, State}.

trigger_gc_all_processes() ->
    [erlang:garbage_collect(P) || P <- erlang:processes()].
```

### Binary Memory Management

```erlang
%% Binaries > 64 bytes are reference-counted (shared)
%% This is efficient but can cause memory "leaks" if references held

%% Force binary cleanup for long-lived processes
cleanup_binaries(State) ->
    erlang:garbage_collect(self()),
    %% Also trigger binary cleanup
    erlang:garbage_collect(self(), [{type, major}]),
    State.

%% Avoid sub-binary references holding large binaries
%% DON'T:
process_packet(<<Header:10/binary, Rest/binary>>) ->
    store(Header),  %% Header references entire packet!
    Rest.

%% DO:
process_packet(<<Header:10/binary, Rest/binary>>) ->
    store(binary:copy(Header)),  %% Copy small header
    Rest.
```

---

## 3. Network Utilization

### Socket Buffer Optimization

```erlang
%% TCP socket options for maximum throughput
socket_options() ->
    [
        binary,
        {packet, 0},           %% Raw binary, we handle framing
        {active, once},        %% Controlled receive
        {nodelay, true},       %% Disable Nagle (low latency)
        {keepalive, true},     %% Detect dead connections
        {send_timeout, 30000}, %% Send timeout
        {send_timeout_close, true},
        
        %% Buffer sizes - tune based on message sizes
        {sndbuf, 32768},       %% 32KB send buffer
        {recbuf, 32768},       %% 32KB receive buffer
        {buffer, 65536},       %% 64KB user-space buffer
        
        %% High-performance options
        {high_watermark, 131072},  %% 128KB before backpressure
        {low_watermark, 65536},    %% 64KB resume threshold
        {high_msgq_watermark, 10000},
        {low_msgq_watermark, 5000}
    ].
```

### System-Level Network Tuning

```bash
#!/bin/bash
# tune_network.sh - Run on each node

# Increase connection tracking
sudo sysctl -w net.core.somaxconn=65535
sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535

# Increase file descriptors
sudo sysctl -w fs.file-max=10000000
sudo sysctl -w fs.nr_open=10000000

# TCP memory (min, default, max in pages)
sudo sysctl -w net.ipv4.tcp_mem="786432 1048576 1572864"
sudo sysctl -w net.ipv4.tcp_rmem="4096 87380 33554432"
sudo sysctl -w net.ipv4.tcp_wmem="4096 65536 33554432"

# Enable TCP optimizations
sudo sysctl -w net.ipv4.tcp_fastopen=3
sudo sysctl -w net.ipv4.tcp_slow_start_after_idle=0
sudo sysctl -w net.ipv4.tcp_no_metrics_save=1

# Connection reuse
sudo sysctl -w net.ipv4.tcp_tw_reuse=1
sudo sysctl -w net.ipv4.tcp_fin_timeout=15

# Disable TCP timestamps for slight efficiency gain
# (controversial - enables PAWS protection)
# sudo sysctl -w net.ipv4.tcp_timestamps=0

# Increase local port range
sudo sysctl -w net.ipv4.ip_local_port_range="1024 65535"

# Network buffer sizes
sudo sysctl -w net.core.rmem_max=33554432
sudo sysctl -w net.core.wmem_max=33554432
sudo sysctl -w net.core.rmem_default=1048576
sudo sysctl -w net.core.wmem_default=1048576
sudo sysctl -w net.core.netdev_max_backlog=65536

echo "Network tuning applied"
```

### Message Batching for Network Efficiency

```erlang
%% Batch small messages to reduce syscall overhead
-module(iris_batcher).

-define(BATCH_SIZE, 100).
-define(BATCH_TIMEOUT, 10).  %% 10ms max wait

-record(state, {
    socket,
    batch = [],
    batch_count = 0,
    timer_ref
}).

%% Add message to batch
add_message(Msg, State = #state{batch = Batch, batch_count = Count}) ->
    NewBatch = [Msg | Batch],
    NewCount = Count + 1,
    
    case NewCount >= ?BATCH_SIZE of
        true ->
            %% Batch full - send immediately
            flush_batch(State#state{batch = NewBatch, batch_count = NewCount});
        false ->
            %% Start timer if first message
            NewTimer = case State#state.timer_ref of
                undefined -> 
                    erlang:send_after(?BATCH_TIMEOUT, self(), flush);
                Ref -> Ref
            end,
            State#state{batch = NewBatch, batch_count = NewCount, timer_ref = NewTimer}
    end.

flush_batch(State = #state{socket = Socket, batch = Batch, timer_ref = Timer}) ->
    %% Cancel timer
    case Timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% Combine and send
    Combined = iolist_to_binary(lists:reverse(Batch)),
    gen_tcp:send(Socket, Combined),
    
    State#state{batch = [], batch_count = 0, timer_ref = undefined}.
```

### Network Metrics

```erlang
%% Track network efficiency
network_metrics() ->
    %% Per-socket stats (sample)
    SocketStats = [inet:getstat(S) || S <- get_sample_sockets(100)],
    
    %% Aggregate
    TotalRecv = lists:sum([proplists:get_value(recv_oct, S, 0) || {ok, S} <- SocketStats]),
    TotalSent = lists:sum([proplists:get_value(send_oct, S, 0) || {ok, S} <- SocketStats]),
    TotalPkts = lists:sum([proplists:get_value(recv_cnt, S, 0) || {ok, S} <- SocketStats]),
    
    AvgPktSize = case TotalPkts of
        0 -> 0;
        N -> TotalRecv / N
    end,
    
    #{
        bytes_received => TotalRecv,
        bytes_sent => TotalSent,
        packets_received => TotalPkts,
        avg_packet_size => AvgPktSize,
        
        %% Efficiency: larger packets = fewer syscalls
        efficiency => if
            AvgPktSize > 1000 -> excellent;
            AvgPktSize > 500 -> good;
            AvgPktSize > 100 -> acceptable;
            true -> poor_consider_batching
        end
    }.
```

---

## 4. Storage Utilization

### Mnesia Optimization

```erlang
%% Optimize Mnesia for your workload

%% RAM tables for hot data (presence)
mnesia:create_table(presence, [
    {ram_copies, [node()]},     %% RAM only - fast
    {attributes, [user, node, pid]},
    {type, set}
]).

%% Disc tables with write optimization
mnesia:create_table(offline_msg, [
    {disc_only_copies, [node()]},  %% Disc only - saves RAM
    {attributes, [key, timestamp, msg]},
    {type, bag},
    %% Tune DETS (underlying disc storage)
    {dets, [{auto_save, 60000}]}  %% Auto-save every 60s
]).

%% Mnesia configuration for throughput
application:set_env(mnesia, dump_log_write_threshold, 50000),
application:set_env(mnesia, dc_dump_limit, 40),
```

### Disc Write Optimization

```erlang
%% Batch disc writes for efficiency
%% DON'T: Write every message immediately
%% DO: Buffer and batch

-module(iris_disc_batcher).

-define(BATCH_SIZE, 1000).
-define(FLUSH_INTERVAL, 1000).  %% 1 second

-record(state, {
    pending = [],
    pending_count = 0
}).

handle_cast({store, User, Msg}, State) ->
    NewPending = [{User, Msg} | State#state.pending],
    NewCount = State#state.pending_count + 1,
    
    case NewCount >= ?BATCH_SIZE of
        true ->
            flush_to_disc(NewPending),
            {noreply, State#state{pending = [], pending_count = 0}};
        false ->
            {noreply, State#state{pending = NewPending, pending_count = NewCount}}
    end.

handle_info(flush, State = #state{pending = Pending}) when Pending =/= [] ->
    flush_to_disc(Pending),
    erlang:send_after(?FLUSH_INTERVAL, self(), flush),
    {noreply, State#state{pending = [], pending_count = 0}};

flush_to_disc(Messages) ->
    %% Single transaction for entire batch
    F = fun() ->
        [mnesia:write({offline_msg, User, os:system_time(millisecond), Msg}) 
         || {User, Msg} <- Messages]
    end,
    mnesia:activity(sync_transaction, F).
```

### Storage Space Monitoring

```erlang
%% Monitor disc usage
storage_metrics() ->
    %% Mnesia directory size
    MnesiaDir = mnesia:system_info(directory),
    {ok, DirSize} = dir_size(MnesiaDir),
    
    %% Table sizes
    Tables = mnesia:system_info(tables),
    TableSizes = [{T, mnesia:table_info(T, size), mnesia:table_info(T, memory)} 
                  || T <- Tables],
    
    %% Disc space
    {ok, DiscInfo} = disk_free(MnesiaDir),
    
    #{
        mnesia_dir_bytes => DirSize,
        table_sizes => TableSizes,
        disc_free_bytes => DiscInfo,
        disc_utilization => 1 - (DiscInfo / disc_total(MnesiaDir))
    }.
```

---

## 5. Hardware Efficiency Metrics Dashboard

### Real-Time Efficiency Monitor

```erlang
%% src/iris_efficiency_monitor.erl
-module(iris_efficiency_monitor).
-export([report/0, start_link/0]).

report() ->
    #{
        cpu => cpu_efficiency(),
        memory => memory_efficiency(),
        network => network_efficiency(),
        storage => storage_efficiency(),
        overall => overall_efficiency()
    }.

cpu_efficiency() ->
    SchedUtil = lists:sum([U || {_, U, _} <- erlang:statistics(scheduler_wall_time)]) /
                length(erlang:statistics(scheduler_wall_time)),
    #{
        utilization => SchedUtil,
        target => 75,
        status => efficiency_status(SchedUtil, 75)
    }.

memory_efficiency() ->
    MemInfo = erlang:memory(),
    Total = proplists:get_value(total, MemInfo),
    Limit = get_memory_limit(),
    Util = Total / Limit * 100,
    #{
        utilization => Util,
        target => 85,
        status => efficiency_status(Util, 85)
    }.

network_efficiency() ->
    %% Measure actual vs theoretical throughput
    #{recv_oct := Recv, send_oct := Sent} = inet:getstat_all(),
    Connections = ets:info(connections, size),
    BytesPerConn = (Recv + Sent) / max(Connections, 1),
    #{
        bytes_per_connection => BytesPerConn,
        connections => Connections,
        status => if BytesPerConn > 100 -> optimal; true -> underutilized end
    }.

storage_efficiency() ->
    {ok, Used, Total} = disk_usage(mnesia:system_info(directory)),
    Util = Used / Total * 100,
    #{
        utilization => Util,
        target => 75,
        status => efficiency_status(Util, 75)
    }.

overall_efficiency() ->
    Scores = [
        cpu_efficiency_score(),
        memory_efficiency_score(),
        network_efficiency_score(),
        storage_efficiency_score()
    ],
    AvgScore = lists:sum(Scores) / length(Scores),
    #{
        score => AvgScore,
        grade => score_to_grade(AvgScore),
        recommendation => efficiency_recommendation(AvgScore)
    }.

efficiency_status(Actual, Target) ->
    Diff = abs(Actual - Target),
    if
        Diff < 10 -> optimal;
        Actual < Target - 10 -> underutilized;
        Actual > Target + 10 -> overutilized;
        true -> acceptable
    end.

score_to_grade(Score) when Score >= 90 -> 'A';
score_to_grade(Score) when Score >= 80 -> 'B';
score_to_grade(Score) when Score >= 70 -> 'C';
score_to_grade(Score) when Score >= 60 -> 'D';
score_to_grade(_) -> 'F'.

efficiency_recommendation(Score) when Score < 60 ->
    "CRITICAL: Major resource underutilization. Review scaling and consolidation.";
efficiency_recommendation(Score) when Score < 70 ->
    "WARNING: Resources underutilized. Consider reducing instance count.";
efficiency_recommendation(Score) when Score < 80 ->
    "ACCEPTABLE: Room for optimization. Fine-tune batch sizes and buffers.";
efficiency_recommendation(Score) when Score < 90 ->
    "GOOD: Efficient resource usage. Monitor for degradation.";
efficiency_recommendation(_) ->
    "EXCELLENT: Optimal resource utilization.".
```

### Prometheus Metrics Export

```erlang
%% Expose metrics for Prometheus scraping
prometheus_metrics() ->
    [
        %% CPU
        {gauge, iris_scheduler_utilization, "Scheduler utilization percentage",
            cpu_efficiency()},
        {gauge, iris_run_queue_length, "Erlang run queue length",
            erlang:statistics(run_queue)},
        
        %% Memory  
        {gauge, iris_memory_used_bytes, "Memory used",
            proplists:get_value(total, erlang:memory())},
        {gauge, iris_memory_processes_bytes, "Process memory",
            proplists:get_value(processes, erlang:memory())},
        {gauge, iris_memory_ets_bytes, "ETS memory",
            proplists:get_value(ets, erlang:memory())},
        
        %% Connections
        {gauge, iris_connections_total, "Total connections",
            ets:info(connections, size)},
        {gauge, iris_memory_per_connection_bytes, "Memory per connection",
            proplists:get_value(total, erlang:memory()) / max(1, ets:info(connections, size))},
        
        %% Efficiency
        {gauge, iris_efficiency_score, "Overall efficiency score",
            overall_efficiency_score()}
    ].
```

---

## 6. Cost Efficiency Calculations

### Cost Per Connection

```
Target: < $0.0001 per connection per month

Calculation (t3.medium, 4GB RAM, $30/month):
├─ RAM available for connections: 3GB (1GB for OS/Erlang)
├─ Memory per connection: 10KB
├─ Connections per instance: 300,000
├─ Cost per connection: $30 / 300,000 = $0.0001
└─ Status: ON TARGET
```

### Cost Per Message

```
Target: < $0.000001 per message

Calculation:
├─ Instance: t3.medium ($30/month)
├─ Messages per second: 10,000
├─ Messages per month: 10,000 × 86,400 × 30 = 25.9 billion
├─ Cost per message: $30 / 25.9B = $0.0000012
└─ Status: ON TARGET
```

### Efficiency Improvement ROI

```
If improving memory efficiency from 10KB to 8KB per connection:
├─ Before: 300,000 connections per instance
├─ After: 375,000 connections per instance
├─ Improvement: 25% more connections per dollar
├─ At 1M connections: Save 1 instance ($30/month)
├─ At 100M connections: Save 33 instances ($1,000/month)
├─ At 500M connections: Save 167 instances ($5,000/month)
```

---

## 7. Hardware Utilization Tests

### Test: CPU Saturation Point

```bash
#!/bin/bash
# test_cpu_saturation.sh
# Find the point where CPU becomes the bottleneck

echo "=== CPU SATURATION TEST ==="

for LOAD in 1000 5000 10000 20000 50000 100000; do
    echo "Testing with $LOAD messages/sec..."
    
    # Start load
    erl -noshell -pa ebin -eval "
        iris_load_gen:start_rate($LOAD),
        timer:sleep(30000),
        
        %% Measure CPU
        SchedUtil = erlang:statistics(scheduler_wall_time),
        AvgUtil = lists:sum([A/T || {_, A, T} <- SchedUtil]) / length(SchedUtil) * 100,
        RunQueue = erlang:statistics(run_queue),
        
        io:format('Load: ~p msg/s | CPU: ~.1f% | RunQueue: ~p~n', 
                  [$LOAD, AvgUtil, RunQueue]),
        
        iris_load_gen:stop(),
        init:stop().
    "
done

echo ""
echo "CPU saturation point is where:"
echo "  - Utilization exceeds 85%"
echo "  - OR run queue exceeds 1000"
```

### Test: Memory Efficiency

```bash
#!/bin/bash
# test_memory_efficiency.sh

echo "=== MEMORY EFFICIENCY TEST ==="

# Establish connections and measure
for CONNS in 10000 50000 100000 200000 500000; do
    echo "Testing with $CONNS connections..."
    
    # Establish connections
    erl -noshell -pa ebin -sname memtest_$CONNS -eval "
        %% Get baseline memory
        erlang:garbage_collect(),
        timer:sleep(1000),
        BaseMem = proplists:get_value(total, erlang:memory()),
        
        %% Establish connections
        Pids = [begin
            {ok, S} = gen_tcp:connect(\"localhost\", 8085, [binary]),
            gen_tcp:send(S, <<1, \"user_\", (integer_to_binary(N))/binary>>),
            S
        end || N <- lists:seq(1, $CONNS)],
        
        %% Measure memory
        erlang:garbage_collect(),
        timer:sleep(5000),
        FinalMem = proplists:get_value(total, erlang:memory()),
        
        MemUsed = FinalMem - BaseMem,
        PerConn = MemUsed / $CONNS,
        
        io:format('Connections: ~p | Memory: ~.1f MB | Per conn: ~.2f KB~n',
                  [$CONNS, MemUsed/1024/1024, PerConn/1024]),
        
        [gen_tcp:close(S) || S <- Pids],
        init:stop().
    "
done

echo ""
echo "Target: < 10KB per connection"
```

### Test: Network Throughput

```bash
#!/bin/bash
# test_network_throughput.sh

echo "=== NETWORK THROUGHPUT TEST ==="

# Measure maximum sustainable message rate
erl -noshell -pa ebin -eval "
    %% Warmup
    iris_load_gen:start_rate(10000),
    timer:sleep(10000),
    
    %% Get network baseline
    {ok, Stats1} = inet:getstat(get_any_socket()),
    T1 = erlang:monotonic_time(millisecond),
    
    %% Run for 60 seconds
    timer:sleep(60000),
    
    %% Get final stats
    {ok, Stats2} = inet:getstat(get_any_socket()),
    T2 = erlang:monotonic_time(millisecond),
    
    Duration = (T2 - T1) / 1000,
    BytesRecv = proplists:get_value(recv_oct, Stats2) - proplists:get_value(recv_oct, Stats1),
    BytesSent = proplists:get_value(send_oct, Stats2) - proplists:get_value(send_oct, Stats1),
    
    RecvRate = BytesRecv / Duration / 1024 / 1024,
    SendRate = BytesSent / Duration / 1024 / 1024,
    
    io:format('Duration: ~.1f seconds~n', [Duration]),
    io:format('Receive: ~.2f MB/s~n', [RecvRate]),
    io:format('Send: ~.2f MB/s~n', [SendRate]),
    io:format('Total: ~.2f MB/s~n', [RecvRate + SendRate]),
    
    iris_load_gen:stop(),
    init:stop().
"
```

---

## 8. Implementation Checklist

### Week 1: Measurement

- [ ] Add `iris_efficiency_monitor.erl`
- [ ] Integrate Prometheus metrics
- [ ] Establish baseline measurements
- [ ] Identify bottlenecks

### Week 2: CPU Optimization

- [ ] Verify scheduler count matches cores
- [ ] Add optimal startup flags
- [ ] Implement scheduler utilization monitoring
- [ ] Test CPU saturation point

### Week 3: Memory Optimization

- [ ] Optimize connection process spawn options
- [ ] Implement hibernation for idle connections
- [ ] Add binary memory management
- [ ] Test memory per connection

### Week 4: Network/Storage Optimization

- [ ] Apply system-level network tuning
- [ ] Implement message batching
- [ ] Optimize Mnesia configuration
- [ ] Run full efficiency test suite

---

## Summary

| Resource | Current | Target | Action |
|----------|---------|--------|--------|
| CPU | Unknown | 70-85% | Add monitoring, tune schedulers |
| Memory | ~10KB/conn | <10KB/conn | Hibernate idle, optimize binaries |
| Network | Unknown | Batched | Implement batching, tune buffers |
| Storage | Unbatched | Batched writes | Implement disc batcher |

**The goal**: Every hardware resource delivers maximum user value. Waste is unacceptable.

---

*Document created: January 15, 2026*
*Efficiency target: <$0.0001 per connection per month*
