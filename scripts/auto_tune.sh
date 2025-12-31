#!/bin/bash
# scripts/auto_tune.sh
# Analyzes system resources to output optimal Erlang VM flags.

# Defaults
DEFAULT_MAX_PROCS=250000
DEFAULT_MAX_PORTS=65536
MIN_RAM_RESERVE_MB=2048 # Leave 2GB for OS

# 1. Detect Available RAM (in KB)
if [ -f /proc/meminfo ]; then
    # Use MemAvailable if present (newer kernels), else free + buffers + cached
    KB_AVAILABLE=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
    if [ -z "$KB_AVAILABLE" ]; then
         KB_FREE=$(grep MemFree /proc/meminfo | awk '{print $2}')
         KB_BUFF=$(grep Buffers /proc/meminfo | awk '{print $2}')
         KB_CACH=$(grep ^Cached /proc/meminfo | awk '{print $2}')
         KB_AVAILABLE=$((KB_FREE + KB_BUFF + KB_CACH))
    fi
else
    # Fallback for non-Linux (e.g., Mac/BSD - minimal support)
    KB_AVAILABLE=4194304 # Assume 4GB
fi

# 2. Calculate Max Connections based on RAM
# Per connection estimate: ~15KB (Safety margin over 8.6KB actual)
# Target use: 80% of Available RAM
TARGET_RAM_KB=$((KB_AVAILABLE * 80 / 100))
MAX_CONNS=$((TARGET_RAM_KB / 15))

# Cap Max Connections at 2 Million (Soft Limit)
if [ "$MAX_CONNS" -gt 2000000 ]; then
    MAX_CONNS=2000000
fi

# Ensure sensible minimum
if [ "$MAX_CONNS" -lt "$DEFAULT_MAX_PROCS" ]; then
    MAX_CONNS=$DEFAULT_MAX_PROCS
fi

# 3. Tuning Values
# ERL_MAX_PROCS (+P): Connections + System Overhead (factor 1.2)
LIMIT_P=$(echo "$MAX_CONNS * 1.2" | bc | cut -d. -f1)
# ERL_MAX_PORTS (+Q): Same as Procs
LIMIT_Q=$LIMIT_P

# 4. Safety Check: ulimit -n
# If current ulimit is lower than LIMIT_Q, Erlang will crash.
# We must cap LIMIT_Q (and P) to the actual OS limit.
OS_ULIMIT=$(ulimit -n)
if [ "$OS_ULIMIT" != "unlimited" ]; then
    if [ "$LIMIT_Q" -gt "$OS_ULIMIT" ]; then
        echo "[AUTO-TUNE] WARNING: ulimit -n ($OS_ULIMIT) is lower than target ($LIMIT_Q)." >&2
        echo "[AUTO-TUNE] Capping +P/+Q to $OS_ULIMIT (minus buffer) to prevent crash." >&2
        # Leave room for shell/VM overhead
        SAFE_LIMIT=$((OS_ULIMIT - 5000))
        LIMIT_Q=$SAFE_LIMIT
        LIMIT_P=$SAFE_LIMIT
    fi
fi

# 5. Output Flags string for Makefile
echo "[AUTO-TUNE] Detected Available RAM: $((KB_AVAILABLE / 1024)) MB" >&2
echo "[AUTO-TUNE] Target Capacity: $MAX_CONNS Connections" >&2
echo "[AUTO-TUNE] Final Flags: +P $LIMIT_P +Q $LIMIT_Q" >&2

# Scheduler Threads (+S): Default to Core Count (Erlang does this auto, but we can be explicit if needed)
# Busy Wait (+sbwt): 'none' or 'very_short' reduces CPU usage on idle systems.
FLAGS="+P $LIMIT_P +Q $LIMIT_Q +K true +sbwt none"

echo "$FLAGS"
