#!/bin/bash
# start_cluster.sh - Production-ready cluster startup script
# Starts Core node and Edge node with proper configuration

set -e

# Configuration
ERL=${ERL:-/Users/jd/.kerl/26.2/bin/erl}
HOST=$(hostname -s)
COOKIE=${COOKIE:-iris_cookie}

echo "=============================================="
echo "  Project Iris Cluster Startup"
echo "=============================================="
echo "ERL: $ERL"
echo "Host: $HOST"
echo "Cookie: $COOKIE"
echo ""

# Cleanup
echo "[*] Stopping old nodes..."
pkill -9 -f beam || true
rm -rf Mnesia.*
rm -f *.log

# Compile
echo "[*] Compiling..."
make ERL=$ERL all > /dev/null 2>&1 || {
    echo "Compilation failed!"
    make ERL=$ERL all
    exit 1
}

# Start Core
echo "[*] Starting Iris Core..."
nohup $ERL +Bd -noshell -noinput \
    -pa ebin \
    -sname iris_core \
    -setcookie $COOKIE \
    -iris_core auto_init_db true \
    -eval "application:ensure_all_started(iris_core), io:format('CORE_STARTED~n'), timer:sleep(infinity)" \
    < /dev/null > core.log 2>&1 &
CORE_PID=$!
disown $CORE_PID
echo "    Core PID: $CORE_PID"

# Wait for Core to initialize
echo "[*] Waiting for Core to initialize (5s)..."
sleep 5

# Check if Core is running
if ! ps -p $CORE_PID > /dev/null 2>&1; then
    echo "ERROR: Core node failed to start!"
    echo "--- core.log ---"
    cat core.log
    exit 1
fi

# Start Edge
echo "[*] Starting Iris Edge (Port 8085)..."
nohup $ERL +Bd -noshell -noinput \
    -pa ebin \
    -sname iris_edge1 \
    -setcookie $COOKIE \
    -iris_edge port 8085 \
    -eval "application:ensure_all_started(iris_edge), io:format('EDGE_STARTED~n'), timer:sleep(infinity)" \
    < /dev/null > edge.log 2>&1 &
EDGE_PID=$!
disown $EDGE_PID
echo "    Edge PID: $EDGE_PID"

# Wait for Edge to initialize
echo "[*] Waiting for Edge to initialize (5s)..."
sleep 5

# Check if Edge is running
if ! ps -p $EDGE_PID > /dev/null 2>&1; then
    echo "ERROR: Edge node failed to start!"
    echo "--- edge.log ---"
    cat edge.log
    exit 1
fi

# Verify listeners
echo ""
echo "=============================================="
echo "  Cluster Status"
echo "=============================================="

if grep -q "CORE_STARTED" core.log 2>/dev/null; then
    echo "✓ Core: RUNNING"
else
    echo "✗ Core: FAILED"
    cat core.log
fi

if grep -q "EDGE_STARTED" edge.log 2>/dev/null; then
    echo "✓ Edge: RUNNING"
else
    echo "✗ Edge: FAILED"
    cat edge.log
fi

# Check TCP listener
if lsof -i :8085 > /dev/null 2>&1; then
    echo "✓ TCP Listener: Port 8085 OPEN"
else
    echo "✗ TCP Listener: Port 8085 NOT OPEN"
fi

if lsof -i :8086 > /dev/null 2>&1; then
    echo "✓ WS Listener: Port 8086 OPEN"
else
    echo "✗ WS Listener: Port 8086 NOT OPEN"
fi

echo ""
echo "Cluster is ready for testing!"
echo "  - TCP: localhost:8085"
echo "  - WebSocket: localhost:8086"
