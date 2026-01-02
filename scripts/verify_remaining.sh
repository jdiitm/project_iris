#!/bin/bash
set -e

LOG_DIR="verification_logs"
mkdir -p $LOG_DIR

echo "=========================================================="
echo "   PROJECT IRIS: RESUMING VERIFICATION (PHASE 4 & 5)      "
echo "   MODE: EXHAUSTIVE (CONTINUATION)                        "
echo "=========================================================="
echo "Logs will be saved to: $LOG_DIR"

clean_env() {
    echo "[*] Cleaning Environment..."
    pkill -9 beam || true
    killall -9 beam.smp || true
    sleep 2
}

run_test() {
    NAME=$1
    CMD=$2
    TYPE=$3 # "connected" or "standalone"
    
    echo "----------------------------------------------------------"
    echo "TEST: $NAME"
    echo "----------------------------------------------------------"
    
    if [ "$TYPE" == "standalone" ]; then
        clean_env
    fi
    
    # Run command
    if $CMD > "$LOG_DIR/${NAME// /_}.log" 2>&1; then
        echo ">>> PASS: $NAME"
    else
        echo ">>> FAIL: $NAME"
        echo "See $LOG_DIR/${NAME// /_}.log for details."
        head -n 20 "$LOG_DIR/${NAME// /_}.log"
        tail -n 20 "$LOG_DIR/${NAME// /_}.log" 
        echo "Stopping Suite."
        exit 1
    fi
}

# ==========================================
# PHASE 4: RESILIENCE & INTEGRITY (Resumed)
# ==========================================
# Resuming from the failed/interrupted test
run_test "Resilience: Slow Consumer (OOM)" "python3 break_my_system.py oom" "standalone"
run_test "Resilience: Disk Crusher" "python3 break_my_system.py disk" "standalone"
run_test "Integrity: 100k Offline Verify" "python3 extreme_offline_test.py" "standalone"

# ==========================================
# PHASE 5: TOTAL CHAOS (Standalone)
# ==========================================
run_test "Chaos: Kitchen Sink" "python3 kitchen_sink_chaos.py" "standalone"
run_test "Chaos: Total Chaos" "python3 total_chaos_test.py" "standalone"
run_test "Chaos: Ultimate (1M Users)" "python3 ultimate_chaos.py" "standalone"

echo "=========================================================="
echo "   REMAINING TESTS PASSED SUCCESSFULLY                    "
echo "=========================================================="
