#!/bin/bash
set -e

echo "=============================================="
echo "   PROJECT IRIS: A-Z VERIFICATION SUITE       "
echo "=============================================="

clean_env() {
    echo "[*] Cleaning Environment..."
    pkill -9 beam || true
    killall -9 beam.smp || true
    rm -rf Mnesia.* *.log
    sleep 2
}

run_test() {
    TEST_NAME=$1
    CMD=$2
    echo "----------------------------------------------"
    echo "RUNNING: $TEST_NAME"
    echo "----------------------------------------------"
    
    if $CMD; then
        echo ">>> PASS: $TEST_NAME"
    else
        echo ">>> FAIL: $TEST_NAME"
        echo "Stopping Suite."
        exit 1
    fi
}

start_cluster() {
    echo "[*] Starting Standard Cluster (Core + Edge1)..."
    clean_env
    make start_core > /dev/null 2>&1
    make start_edge1 > /dev/null 2>&1
    echo "[*] Waiting for nodes to boot..."
    sleep 5
}

# 1. Functional Regression
start_cluster
run_test "Offline Messaging (Regression)" "python3 test_offline.py"

# 2. Tiered Architecture
# Reuse same cluster (User data persists in Mnesia/Ram)
# test_hotkey_bucketing handles its own cleanup/setup of buckets usually,
# but assumes running cluster.
run_test "Hot-Key Bucketing (VIP)" "python3 test_hotkey_bucketing.py"

# 3. Global Scale Simulation
# This script handles its own Cluster Setup (Client-managed)
run_test "Global Fan-In & Reliability" "python3 stress_global_fan_in.py"

echo "=============================================="
echo "   ALL TESTS PASSED SUCCESSFULLY              "
echo "=============================================="
