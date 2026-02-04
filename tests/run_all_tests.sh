#!/bin/bash
# ============================================================================
# IRIS COMPLETE TEST SUITE RUNNER
# ============================================================================
# Runs ALL 75+ Python tests in optimal order
#
# Usage:
#   ./tests/run_all_tests.sh           # Run all tests
#   ./tests/run_all_tests.sh --quick   # Skip Docker tests
#   sudo ./tests/run_all_tests.sh      # Run with elevated permissions
# ============================================================================

# ============================================================================
# ULIMIT CONFIGURATION (Required for Erlang VM)
# ============================================================================
# Erlang requires a minimum of ~1024 file descriptors. When running with sudo,
# ulimit resets to default (often 1024), causing the auto-tune to set +P too low.
# We set a reasonable minimum here to ensure Erlang can start.
REQUIRED_ULIMIT=65536
CURRENT_ULIMIT=$(ulimit -n)

if [ "$CURRENT_ULIMIT" -lt "$REQUIRED_ULIMIT" ]; then
    # Try to increase ulimit (will work if running as root/sudo)
    ulimit -n "$REQUIRED_ULIMIT" 2>/dev/null || true
    NEW_ULIMIT=$(ulimit -n)
    if [ "$NEW_ULIMIT" -lt 4096 ]; then
        echo "WARNING: ulimit -n is $NEW_ULIMIT (need at least 4096 for Erlang)"
        echo "Try: sudo bash -c 'ulimit -n 65536 && $0 $*'"
        exit 1
    fi
fi

cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Counters
TOTAL_PASS=0
TOTAL_FAIL=0
TOTAL_SKIP=0
FAILED_TESTS=()

# Options
SKIP_DOCKER=false
if [[ "$1" == "--quick" ]]; then
    SKIP_DOCKER=true
fi

# Log directory
LOG_DIR="tests/artifacts/full_run_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG_DIR"

echo "============================================================================"
echo "                    IRIS COMPLETE TEST SUITE"
echo "============================================================================"
echo "Project root: $PROJECT_ROOT"
echo "Log directory: $LOG_DIR"
echo ""

# ============================================================================
# CLEANUP FUNCTION
# ============================================================================
cleanup() {
    echo -e "${YELLOW}[CLEANUP]${NC} Stopping all processes..."
    pkill -9 beam.smp 2>/dev/null || true
    pkill -9 epmd 2>/dev/null || true
    docker compose -f docker/global-cluster/docker-compose.yml down -v 2>/dev/null || true
    
    # Clean up Mnesia directories and data
    rm -rf Mnesia.* MnesiaCore.* data/ 2>/dev/null || true
    find /tmp -maxdepth 1 -name "iris_*" -exec rm -rf {} \; 2>/dev/null || true
    rm -f erl_crash.dump core.log edge1.log edge2.log 2>/dev/null || true
    
    # Verify cleanup succeeded (detect permission issues early)
    if ls -d Mnesia.* MnesiaCore.* data/ 2>/dev/null | grep -q .; then
        echo -e "${RED}[CLEANUP ERROR]${NC} Could not remove Mnesia/data directories."
        echo -e "${RED}These may have root permissions from a previous sudo run.${NC}"
        echo -e "${YELLOW}Fix with: sudo rm -rf Mnesia.* MnesiaCore.* data/ core.log edge1.log${NC}"
        exit 1
    fi
    
    sleep 2
}

# ============================================================================
# START TLS SERVER
# ============================================================================
start_server() {
    echo "Starting local TLS server..."
    CONFIG=config/test_tls make start > "$LOG_DIR/server_start.log" 2>&1
    sleep 5
    
    local attempts=0
    while ! nc -z localhost 8085 2>/dev/null; do
        attempts=$((attempts + 1))
        if [ $attempts -ge 6 ]; then
            echo -e "${RED}ERROR: Server failed to start${NC}"
            tail -10 "$LOG_DIR/server_start.log"
            return 1
        fi
        echo "  Waiting for server (attempt $attempts/6)..."
        sleep 5
    done
    echo -e "  ${GREEN}Server running on port 8085${NC}"
    return 0
}

# ============================================================================
# RUN TEST HELPER
# ============================================================================
run_test() {
    local test_path=$1
    local test_name=$(basename "$test_path" .py)
    local timeout_sec=${2:-180}
    
    printf "  %-50s" "$test_name"
    
    timeout "$timeout_sec" python3 -u "$test_path" > "$LOG_DIR/${test_name}.log" 2>&1
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}PASS${NC}"
        ((TOTAL_PASS++))
    elif [ $exit_code -eq 2 ]; then
        echo -e "${YELLOW}SKIP${NC}"
        ((TOTAL_SKIP++))
    elif [ $exit_code -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC}"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "${RED}FAIL${NC}"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name")
    fi
}

# ============================================================================
# PHASE 0: SETUP
# ============================================================================
echo -e "${BLUE}[PHASE 0]${NC} Setup and cleanup..."
cleanup
echo ""

# ============================================================================
# PHASE 1: UNIT TESTS (No server needed)
# ============================================================================
echo -e "${BLUE}[PHASE 1]${NC} Unit Tests"
echo "============================================================================"

echo "Compiling..."
make all > "$LOG_DIR/compile.log" 2>&1 || {
    echo -e "${RED}Compilation failed!${NC}"
    exit 1
}
echo -e "  ${GREEN}Compilation successful${NC}"

for test in tests/suites/unit/test_*.py; do
    [ -f "$test" ] && run_test "$test" 60
done
echo ""

# ============================================================================
# PHASE 2: STANDALONE SERVER TESTS (pre-started TLS server)
# Most tests expect a running server - only 14 use ClusterManager internally
# ============================================================================
echo -e "${BLUE}[PHASE 2]${NC} Standalone Server Tests"
echo "============================================================================"

start_server || exit 1

echo ""
echo "--- Integration (22 tests) ---"
# Run heavy load tests LAST to prevent Mnesia overload from affecting other tests
HEAVY_TESTS="test_degradation_order"

for test in tests/suites/integration/test_*.py; do
    [ -f "$test" ] || continue
    test_name=$(basename "$test" .py)
    
    # Skip heavy tests in first pass
    if [[ "$HEAVY_TESTS" == *"$test_name"* ]]; then
        continue
    fi
    
    run_test "$test" 180
done

# Now run heavy load tests (which may overwhelm Mnesia)
echo ""
echo "--- Heavy Load Integration Tests ---"
for test_name in $HEAVY_TESTS; do
    test="tests/suites/integration/${test_name}.py"
    [ -f "$test" ] && run_test "$test" 300
done

# Restart server after heavy tests to recover Mnesia
echo ""
echo -e "${YELLOW}[RECOVERY]${NC} Restarting server after heavy load tests..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
start_server || exit 1

echo ""
echo "--- E2E (5 tests) ---"
for test in tests/suites/e2e/test_*.py; do
    [ -f "$test" ] && run_test "$test" 180
done

echo ""
echo "--- Contract (1 test) ---"
for test in tests/suites/contract/test_*.py; do
    [ -f "$test" ] && run_test "$test" 180
done

echo ""
echo "--- Compatibility (1 test) ---"
for test in tests/suites/compatibility/test_*.py; do
    [ -f "$test" ] && run_test "$test" 180
done

echo ""
echo "--- Security (7 tests) ---"
for test in tests/suites/security/test_*.py; do
    [ -f "$test" ] && run_test "$test" 180
done

echo ""
echo "--- Resilience (2 standalone tests) ---"
run_test "tests/suites/resilience/test_clock_skew.py" 300
run_test "tests/suites/resilience/test_hard_kill.py" 300

# Restart server after resilience tests (test_hard_kill kills the server)
echo ""
echo -e "${YELLOW}[RECOVERY]${NC} Restarting server after resilience tests..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
start_server || exit 1

echo ""
echo "--- Performance (3 standalone tests) ---"
run_test "tests/suites/performance_light/benchmark_e2ee_latency.py" 180
run_test "tests/suites/performance_light/benchmark_throughput.py" 300
run_test "tests/suites/performance_light/benchmark_unit_cost.py" 300

echo ""
echo "--- Stress (4 standalone tests) ---"
run_test "tests/suites/stress/stress_offline_delete.py" 180
run_test "tests/suites/stress/test_flow_controller_scale.py" 180
run_test "tests/suites/stress/test_group_fanout.py" 180

# Soak test: Use CI duration (5 minutes instead of 1 hour)
export SOAK_DURATION_HOURS=0.08  # ~5 minutes
export SOAK_SAMPLE_INTERVAL=30
run_test "tests/suites/stress/test_soak_memory.py" 420  # 7 minutes timeout
unset SOAK_DURATION_HOURS
unset SOAK_SAMPLE_INTERVAL

echo ""
echo "Stopping standalone server..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
echo ""

# ============================================================================
# PHASE 3: CLUSTERMANAGER TESTS (self-managed cluster)
# These 14 tests use "with ClusterManager(...)" - they manage their own cluster
# ============================================================================
echo -e "${BLUE}[PHASE 3]${NC} ClusterManager Tests (self-managed)"
echo "============================================================================"

# Tests that actually use "with ClusterManager(...)"
CM_TESTS=(
    # Resilience
    "tests/suites/resilience/test_resilience.py"
    # Performance
    "tests/suites/performance_light/benchmark_memory.py"
    "tests/suites/performance_light/measure_dials.py"
    "tests/suites/performance_light/test_cpu_utilization.py"
    # Stress
    "tests/suites/stress/stress_geo_scale.py"
    "tests/suites/stress/stress_global_fan_in.py"
    "tests/suites/stress/stress_hotspot.py"
    "tests/suites/stress/stress_presence.py"
    "tests/suites/stress/test_backpressure_collapse.py"
    "tests/suites/stress/test_churn.py"
    "tests/suites/stress/test_connection_scale.py"
    "tests/suites/stress/test_fanout.py"
    "tests/suites/stress/test_hot_shard.py"
    "tests/suites/stress/test_limits.py"
)

for test in "${CM_TESTS[@]}"; do
    if [ -f "$test" ]; then
        pkill -9 beam.smp 2>/dev/null || true
        sleep 2
        run_test "$test" 300
    fi
done

# Chaos controlled tests (also use ClusterManager)
echo ""
echo "--- Chaos Controlled (2 tests) ---"
for test in tests/suites/chaos_controlled/*.py; do
    if [ -f "$test" ]; then
        pkill -9 beam.smp 2>/dev/null || true
        sleep 2
        run_test "$test" 300
    fi
done

pkill -9 beam.smp 2>/dev/null || true
echo ""

# ============================================================================
# PHASE 4: DOCKER CHAOS TESTS
# ============================================================================
if [ "$SKIP_DOCKER" = true ]; then
    echo -e "${YELLOW}[PHASE 4]${NC} Docker Chaos Tests - SKIPPED (--quick mode)"
    echo "============================================================================"
else
    echo -e "${BLUE}[PHASE 4]${NC} Docker Chaos Tests (12 tests)"
    echo "============================================================================"

    pkill -9 beam.smp 2>/dev/null || true

    echo "Starting Docker global cluster..."
    docker compose -f docker/global-cluster/docker-compose.yml down -v 2>/dev/null
    docker compose -f docker/global-cluster/docker-compose.yml up -d > "$LOG_DIR/docker_start.log" 2>&1
    echo "Waiting for cluster to stabilize (60s)..."
    sleep 60

    for test in tests/suites/chaos_dist/test_*.py; do
        [ -f "$test" ] && run_test "$test" 300
    done

    echo ""
    echo "Stopping Docker cluster..."
    docker compose -f docker/global-cluster/docker-compose.yml down -v > "$LOG_DIR/docker_stop.log" 2>&1
fi

# ============================================================================
# FINAL SUMMARY
# ============================================================================
echo ""
echo "============================================================================"
echo "                         FINAL RESULTS"
echo "============================================================================"
echo -e "  ${GREEN}PASSED${NC}:  $TOTAL_PASS"
echo -e "  ${RED}FAILED${NC}:  $TOTAL_FAIL"
echo -e "  ${YELLOW}SKIPPED${NC}: $TOTAL_SKIP"
echo ""
TOTAL=$((TOTAL_PASS + TOTAL_FAIL + TOTAL_SKIP))
echo "  TOTAL:   $TOTAL tests"
echo ""

if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo "  Failed tests:"
    for t in "${FAILED_TESTS[@]}"; do
        echo -e "    ${RED}✗${NC} $t"
    done
    echo ""
fi

echo "  Log directory: $LOG_DIR"
echo "============================================================================"

if [ $TOTAL_FAIL -eq 0 ]; then
    echo -e "  ${GREEN}✅ ALL TESTS PASSED${NC}"
    exit 0
else
    echo -e "  ${RED}❌ SOME TESTS FAILED${NC}"
    exit 1
fi
