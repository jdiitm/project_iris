#!/bin/bash
# scripts/auto_tune.sh
# Analyzes system resources to output optimal Erlang VM flags.
# Phase 3: Enhanced with scheduler binding, dirty schedulers, memory allocators

# Defaults
DEFAULT_MAX_PROCS=250000
DEFAULT_MAX_PORTS=65536
MIN_RAM_RESERVE_MB=2048 # Leave 2GB for OS

# 1. Detect CPU cores
CPU_CORES=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# 2. Detect Available RAM (in KB)
if [ -f /proc/meminfo ]; then
    KB_AVAILABLE=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
    if [ -z "$KB_AVAILABLE" ]; then
         KB_FREE=$(grep MemFree /proc/meminfo | awk '{print $2}')
         KB_BUFF=$(grep Buffers /proc/meminfo | awk '{print $2}')
         KB_CACH=$(grep ^Cached /proc/meminfo | awk '{print $2}')
         KB_AVAILABLE=$((KB_FREE + KB_BUFF + KB_CACH))
    fi
elif [[ "$(uname)" == "Darwin" ]]; then
    BYTES_TOTAL=$(sysctl -n hw.memsize)
    BYTES_AVAILABLE=$((BYTES_TOTAL * 6 / 10))
    KB_AVAILABLE=$((BYTES_AVAILABLE / 1024))
else
    KB_AVAILABLE=4194304
fi

# 3. Calculate Max Connections based on RAM
# Per connection estimate: ~15KB (Safety margin over 8.6KB actual)
TARGET_RAM_KB=$((KB_AVAILABLE * 80 / 100))
MAX_CONNS=$((TARGET_RAM_KB / 15))

# Cap at 2 Million
if [ "$MAX_CONNS" -gt 2000000 ]; then
    MAX_CONNS=2000000
fi

# Ensure sensible minimum
if [ "$MAX_CONNS" -lt "$DEFAULT_MAX_PROCS" ]; then
    MAX_CONNS=$DEFAULT_MAX_PROCS
fi

# 4. Tuning Values
LIMIT_P=$(echo "$MAX_CONNS * 1.2" | bc | cut -d. -f1)
LIMIT_Q=$LIMIT_P

# 5. Safety Check: ulimit -n
OS_ULIMIT=$(ulimit -n)
if [ "$OS_ULIMIT" != "unlimited" ]; then
    if [ "$LIMIT_Q" -gt "$OS_ULIMIT" ]; then
        echo "[AUTO-TUNE] WARNING: ulimit -n ($OS_ULIMIT) is lower than target ($LIMIT_Q)." >&2
        echo "[AUTO-TUNE] Capping +P/+Q to $OS_ULIMIT (minus buffer) to prevent crash." >&2
        if [ "$OS_ULIMIT" -gt 6000 ]; then
            SAFE_LIMIT=$((OS_ULIMIT - 5000))
        else
            SAFE_LIMIT=$((OS_ULIMIT - 100))
        fi
        if [ "$SAFE_LIMIT" -lt 256 ]; then
            SAFE_LIMIT=256
        fi
        LIMIT_Q=$SAFE_LIMIT
        LIMIT_P=$SAFE_LIMIT
    fi
fi

# 6. Scheduler Configuration
# +S: Schedulers = CPU cores (auto-detected by Erlang, explicit for clarity)
# +SDcpu: Dirty CPU schedulers (for blocking operations)
# +SDio: Dirty IO schedulers (for disk operations)
SCHEDULERS=$CPU_CORES
DIRTY_CPU=$CPU_CORES
DIRTY_IO=$((CPU_CORES * 2))  # 2x cores for IO-bound work

# 7. Scheduler Binding (for NUMA awareness)
# +stbt: Scheduler bind type (db = default bind, u = unbound, ts = thread spread)
# On multi-socket systems, spreading prevents thread migration overhead
if [ "$CPU_CORES" -ge 16 ]; then
    BIND_TYPE="ts"  # Thread spread for large systems
else
    BIND_TYPE="db"  # Default bind for smaller systems
fi

# 8. Busy Wait Configuration
# +sbwt: Scheduler busy wait threshold
# +swt: Scheduler wakeup threshold
# 'none' reduces CPU on idle, 'very_long' maximizes throughput
BUSY_WAIT="none"  # Energy efficient for variable load
WAKEUP="low"      # Quick wakeup from sleep

# 9. Memory Allocator Configuration
# +MBas: aoffcbf (address order first fit with best fit carrier)
# +MHas: Same for heap
# +Musmbcs: Min size for multiblock carriers
MEM_FLAGS="+MBas aoffcbf +MHas aoffcbf +MMmcs 30"

# 10. Async Thread Pool
# +A: Async threads for file I/O (default 1, increase for heavy disk use)
ASYNC_THREADS=$((CPU_CORES * 2))
if [ "$ASYNC_THREADS" -gt 64 ]; then
    ASYNC_THREADS=64  # Cap at 64
fi

# 11. Output Flags
echo "[AUTO-TUNE] Detected: $CPU_CORES cores, $((KB_AVAILABLE / 1024)) MB RAM" >&2
echo "[AUTO-TUNE] Target Capacity: $MAX_CONNS Connections" >&2
echo "[AUTO-TUNE] Schedulers: $SCHEDULERS (dirty CPU: $DIRTY_CPU, IO: $DIRTY_IO)" >&2
echo "[AUTO-TUNE] Final Flags: +P $LIMIT_P +Q $LIMIT_Q +K true" >&2

# Build flags string
FLAGS="+P $LIMIT_P +Q $LIMIT_Q"
FLAGS="$FLAGS +K true"                         # Kernel poll (epoll/kqueue)
FLAGS="$FLAGS +S $SCHEDULERS:$SCHEDULERS"      # Schedulers
FLAGS="$FLAGS +SDcpu $DIRTY_CPU:$DIRTY_CPU"    # Dirty CPU schedulers
FLAGS="$FLAGS +SDio $DIRTY_IO"                 # Dirty IO schedulers
FLAGS="$FLAGS +stbt $BIND_TYPE"                # Scheduler bind type
FLAGS="$FLAGS +sbwt $BUSY_WAIT"                # Busy wait threshold
FLAGS="$FLAGS +swt $WAKEUP"                    # Wakeup threshold
FLAGS="$FLAGS +A $ASYNC_THREADS"               # Async threads
FLAGS="$FLAGS $MEM_FLAGS"                      # Memory allocator

echo "$FLAGS"

