#!/bin/bash
# ============================================================================
# IRIS COMPLETE TEST SUITE RUNNER
# ============================================================================
# Single authoritative test runner for the entire Iris project.
#
# PHILOSOPHY: Tests are the MOST IMPORTANT part of this repo.
# - NO test skipping: All tests must PASS or FAIL definitively
# - NO weak assertions: Tests verify exact expected behavior
# - NO false positives: If a test passes, the feature works
# - NO false negatives: If a test fails, there's a real bug
#
# PROVEN PATTERNS:
# - Uses docker/global-cluster/cluster.sh for ALL cluster management
# - Uses docker/global-cluster/init_cluster.sh for Mnesia initialization
# - Fresh cluster per Docker test for isolation
#
# Usage:
#   ./tests/run_all_tests.sh                  # Run ALL tests (recommended)
#   ./tests/run_all_tests.sh --docker-only    # Run Docker chaos tests only
#   ./tests/run_all_tests.sh --quick          # Run non-Docker tests only
#   ./tests/run_all_tests.sh --help           # Show help
#
# ============================================================================

set -o pipefail

# ============================================================================
# PATHS
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CLUSTER_DIR="$PROJECT_ROOT/docker/global-cluster"
CLUSTER_SCRIPT="$CLUSTER_DIR/cluster.sh"

# ============================================================================
# ULIMIT CONFIGURATION (Required for Erlang VM)
# ============================================================================
REQUIRED_ULIMIT=65536
CURRENT_ULIMIT=$(ulimit -n)

if [ "$CURRENT_ULIMIT" -lt "$REQUIRED_ULIMIT" ]; then
    ulimit -n "$REQUIRED_ULIMIT" 2>/dev/null || true
    NEW_ULIMIT=$(ulimit -n)
    if [ "$NEW_ULIMIT" -lt 4096 ]; then
        echo "WARNING: ulimit -n is $NEW_ULIMIT (need at least 4096 for Erlang)"
        echo "Try: sudo bash -c 'ulimit -n 65536 && $0 $*'"
        exit 1
    fi
fi

cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Counters
TOTAL_PASS=0
TOTAL_FAIL=0
TOTAL_WARN=0
FAILED_TESTS=()
WARNED_TESTS=()

# Options
SKIP_DOCKER=false
DOCKER_ONLY=false

show_help() {
    echo "IRIS Complete Test Suite Runner"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --help         Show this help"
    echo "  --quick        Run non-Docker tests only (faster)"
    echo "  --docker-only  Run Docker chaos tests only"
    echo "  (no option)    Run ALL tests (recommended)"
    echo ""
    echo "Proven Scripts (dependencies):"
    echo "  docker/global-cluster/cluster.sh      - Cluster up/down"
    echo "  docker/global-cluster/init_cluster.sh - Mnesia initialization"
    echo ""
    echo "Examples:"
    echo "  $0                  # Full test suite"
    echo "  $0 --quick          # Fast iteration (no Docker)"
    echo "  $0 --docker-only    # Only chaos tests"
    exit 0
}

# Parse args
case "$1" in
    --help|-h) show_help ;;
    --quick) SKIP_DOCKER=true; export QUICK_MODE=true ;;
    --docker-only) DOCKER_ONLY=true ;;
esac

# Export CONFIG so Python tests know the server is TLS-enabled.
# Without this, tests using ClusterManager would start non-TLS servers (killing
# the TLS server run_all_tests.sh manages), and tests detecting USE_TLS via
# os.environ.get("CONFIG") would fall back to plain TCP against a TLS server.
export CONFIG=config/test_tls

# Log directory
LOG_DIR="$PROJECT_ROOT/tests/artifacts/full_run_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG_DIR"

get_mode_string() {
    if [ "$SKIP_DOCKER" = true ]; then
        echo 'Quick (non-Docker tests only)'
    elif [ "$DOCKER_ONLY" = true ]; then
        echo 'Docker Only (chaos tests with fresh cluster each)'
    else
        echo 'FULL (ALL tests - recommended)'
    fi
}

echo "============================================================================"
echo "                    IRIS COMPLETE TEST SUITE"
echo "============================================================================"
echo "Project root: $PROJECT_ROOT"
echo "Log directory: $LOG_DIR"
echo "Mode: $(get_mode_string)"
echo ""

# ============================================================================
# CLEANUP FUNCTION (for standalone tests)
# ============================================================================
cleanup_standalone() {
    echo -e "${YELLOW}[CLEANUP]${NC} Stopping local processes..."
    pkill -9 beam.smp 2>/dev/null || true
    pkill -9 epmd 2>/dev/null || true
    rm -rf Mnesia.* MnesiaCore.* data/ 2>/dev/null || true
    find /tmp -maxdepth 1 -name "iris_*" -exec rm -rf {} \; 2>/dev/null || true
    rm -f erl_crash.dump core.log edge1.log edge2.log 2>/dev/null || true
    sleep 2
}

# ============================================================================
# START TLS SERVER (for standalone tests)
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
# HEAVY TESTS (require server restart after running)
# ============================================================================
# These tests generate heavy load that can affect subsequent tests
HEAVY_TESTS=(
    "test_degradation_order"     # 200K+ messages, heavy load
    "test_backpressure"          # Stress tests connections
    "stress_hotspot"             # Heavy single-key load
    "stress_geo_scale"           # Large scale test
    "stress_global_fan_in"       # Fan-in stress
    "test_hot_shard"             # Shard stress
    "test_fanout"                # Fan-out stress
    "test_connection_scale"      # Connection scaling
    "test_soak_memory"           # Memory stress
    "test_dedup_stress"          # High-volume dedup stress
    "benchmark_group_1000"       # High-concurrency group ops (can crash server)
    "benchmark_unit_cost"        # Heavy benchmark
    "ultimate_chaos"             # Combined chaos
    "chaos_combined"             # Combined chaos
)

is_heavy_test() {
    local test_name=$1
    for heavy in "${HEAVY_TESTS[@]}"; do
        if [[ "$test_name" == *"$heavy"* ]]; then
            return 0
        fi
    done
    return 1
}

# ============================================================================
# RUN TEST HELPER (standalone)
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
        TOTAL_PASS=$((TOTAL_PASS + 1))
    elif [ $exit_code -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC}"
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "${RED}FAIL (exit $exit_code)${NC}"
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$test_name")
    fi
    
    # Restart server after heavy tests to ensure clean state for next test
    if is_heavy_test "$test_name"; then
        echo -e "    ${YELLOW}(heavy test - restarting server)${NC}"
        restart_server_quick
    fi
}

# Quick server restart (used after heavy tests)
restart_server_quick() {
    pkill -9 beam.smp 2>/dev/null || true
    # Wait for old beam.smp to actually exit (CI runners can be slow to reap)
    # Without this, ps -C beam.smp sees both old and new processes, inflating
    # memory measurements in benchmark_memory.
    local wait_attempts=0
    while pgrep -x beam.smp > /dev/null 2>&1; do
        wait_attempts=$((wait_attempts + 1))
        if [ $wait_attempts -ge 10 ]; then
            echo -e "    ${YELLOW}Warning: beam.smp still in process table after 10s${NC}"
            break
        fi
        sleep 1
    done
    rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
    CONFIG=config/test_tls make start > "$LOG_DIR/server_restart.log" 2>&1
    sleep 4
    # Wait for server port to be accepting connections
    local attempts=0
    while ! nc -z localhost 8085 2>/dev/null; do
        attempts=$((attempts + 1))
        if [ $attempts -ge 10 ]; then
            echo -e "    ${RED}Warning: Server may not have restarted properly${NC}"
            break
        fi
        sleep 2
    done
}

# Ensure server is healthy before a test category (called before each category)
ensure_server_ready() {
    local category_name=${1:-"tests"}
    
    # Check if server is responding
    if ! nc -z localhost 8085 2>/dev/null; then
        echo -e "  ${YELLOW}[RECOVERY]${NC} Server not responding before $category_name - restarting..."
        restart_server_quick
        if ! nc -z localhost 8085 2>/dev/null; then
            echo -e "  ${RED}[ERROR]${NC} Failed to start server for $category_name"
            return 1
        fi
    fi
    return 0
}

# ============================================================================
# DOCKER CLUSTER MANAGEMENT (uses proven cluster.sh)
# ============================================================================

cluster_down() {
    echo -e "  ${CYAN}[CLUSTER]${NC} Stopping cluster (via cluster.sh)..."
    cd "$CLUSTER_DIR"
    bash "$CLUSTER_SCRIPT" down > "$LOG_DIR/cluster_down.log" 2>&1 || true
    cd "$PROJECT_ROOT"
    sleep 3
}

cluster_up() {
    echo -e "  ${CYAN}[CLUSTER]${NC} Starting cluster (via cluster.sh)..."
    cd "$CLUSTER_DIR"
    if bash "$CLUSTER_SCRIPT" up > "$LOG_DIR/cluster_up.log" 2>&1; then
        cd "$PROJECT_ROOT"
        echo -e "  ${GREEN}Cluster ready${NC}"
        return 0
    else
        cd "$PROJECT_ROOT"
        echo -e "  ${RED}Cluster initialization FAILED${NC}"
        tail -20 "$LOG_DIR/cluster_up.log"
        return 1
    fi
}

# Run Docker test with FRESH cluster (proven pattern)
run_docker_test_fresh() {
    local test_path=$1
    local test_name=$(basename "$test_path" .py)
    local timeout_sec=${2:-300}
    
    echo ""
    echo -e "  ${BLUE}[TEST]${NC} $test_name"
    echo "  ----------------------------------------"
    
    # Fresh cluster for each test
    cluster_down
    
    if ! cluster_up; then
        echo -e "  ${RED}✗ CLUSTER INIT FAILED${NC}"
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$test_name (cluster init)")
        return 1
    fi
    
    # Run the test
    local start_time=$(date +%s)
    SKIP_TEST_CLEANUP=1 timeout "$timeout_sec" python3 -u "$test_path" > "$LOG_DIR/${test_name}.log" 2>&1
    local exit_code=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $exit_code -eq 0 ]; then
        echo -e "  ${GREEN}✓ PASS${NC} (${duration}s)"
        TOTAL_PASS=$((TOTAL_PASS + 1))
    elif [ $exit_code -eq 124 ]; then
        echo -e "  ${RED}✗ TIMEOUT${NC} (${duration}s)"
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "  ${RED}✗ FAIL (exit $exit_code)${NC} (${duration}s)"
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$test_name")
    fi
}

# ============================================================================
# DOCKER CHAOS TESTS (all tests in chaos_dist)
# ============================================================================
DOCKER_CHAOS_TESTS=(
    "tests/suites/chaos_dist/test_server_storage_audit.py"
    "tests/suites/chaos_dist/test_distributed_rate_limit.py"
    "tests/suites/chaos_dist/test_key_bundle_durability.py"
    "tests/suites/chaos_dist/test_dedup_persistence.py"
    "tests/suites/chaos_dist/test_ack_disconnect_race.py"
    "tests/suites/chaos_dist/test_cross_region_chaos.py"
    "tests/suites/chaos_dist/test_multimaster_durability.py"
    "tests/suites/chaos_dist/test_ack_durability.py"
    "tests/suites/chaos_dist/test_bridge_durability.py"
    "tests/suites/chaos_dist/test_network_partition.py"
    "tests/suites/chaos_dist/test_cross_region_latency.py"
    "tests/suites/chaos_dist/test_ordering_under_failure.py"
    "tests/suites/chaos_dist/test_region_outage.py"
    "tests/suites/chaos_dist/test_dist_failover.py"
    "tests/suites/chaos_dist/test_failover_time.py"
    "tests/suites/chaos_dist/test_cascade_failure.py"
    "tests/suites/chaos_dist/test_split_brain.py"
    "tests/suites/chaos_dist/test_disk_full.py"
    "tests/suites/chaos_dist/test_split_brain_convergence.py"
    "tests/suites/chaos_dist/test_outbox_queue_overflow.py"
    "tests/suites/chaos_dist/test_outbox_overflow_enforcement.py"
    "tests/suites/chaos_dist/test_split_brain_epoch_resolution.py"
    "tests/suites/chaos_dist/test_cross_region_node_kill.py"
    "tests/suites/chaos_dist/test_quorum_write_failures.py"
)

# ============================================================================
# MAIN EXECUTION
# ============================================================================

if [ "$DOCKER_ONLY" = true ]; then
    echo -e "${YELLOW}[INFO]${NC} Running Docker tests only (--docker-only mode)"
    echo ""

    # Compilation is required: Docker containers mount ebin/ and need .beam files.
    # In non-docker-only mode, Phase 1 handles this. Here we must do it explicitly.
    echo "Compiling..."
    make all > /dev/null 2>&1 || {
        echo -e "${RED}Compilation failed!${NC}"
        make all 2>&1
        exit 1
    }
    echo -e "  ${GREEN}Compilation successful${NC}"
    echo ""
else
    # ==========================================================================
    # PHASE 0: SETUP
    # ==========================================================================
    echo -e "${BLUE}[PHASE 0]${NC} Setup and cleanup..."
    cleanup_standalone
    echo ""

    # ==========================================================================
    # PHASE 1: UNIT TESTS (No server needed)
    # ==========================================================================
    echo -e "${BLUE}[PHASE 1]${NC} Unit Tests"
    echo "============================================================================"

    echo "Compiling..."
    make all > "$LOG_DIR/compile.log" 2>&1 || {
        echo -e "${RED}Compilation failed!${NC}"
        cat "$LOG_DIR/compile.log"
        exit 1
    }
    echo -e "  ${GREEN}Compilation successful${NC}"

    for test in tests/suites/unit/test_*.py; do
        [ -f "$test" ] && run_test "$test" 60
    done
    echo ""

    # ==========================================================================
    # PHASE 2: STANDALONE SERVER TESTS
    # ==========================================================================
    echo -e "${BLUE}[PHASE 2]${NC} Standalone Server Tests"
    echo "============================================================================"

    start_server || exit 1

    echo ""
    echo "--- Integration Tests ---"
    echo "  (Server will restart automatically after heavy tests)"
    
    # Sort tests to run heavy tests last within integration suite
    INTEGRATION_TESTS_LIGHT=()
    INTEGRATION_TESTS_HEAVY=()
    for test in tests/suites/integration/test_*.py; do
        if [ -f "$test" ]; then
            test_name=$(basename "$test" .py)
            if is_heavy_test "$test_name"; then
                INTEGRATION_TESTS_HEAVY+=("$test")
            else
                INTEGRATION_TESTS_LIGHT+=("$test")
            fi
        fi
    done
    
    # Run light tests first
    for test in "${INTEGRATION_TESTS_LIGHT[@]}"; do
        run_test "$test" 180
        sleep 0.5  # Brief pause between tests
    done
    
    # Then run heavy tests (each will trigger server restart after)
    for test in "${INTEGRATION_TESTS_HEAVY[@]}"; do
        run_test "$test" 240  # Longer timeout for heavy tests
    done

    echo ""
    echo "--- E2E Tests ---"
    for test in tests/suites/e2e/test_*.py; do
        [ -f "$test" ] && run_test "$test" 180
    done

    echo ""
    echo "--- Contract Tests ---"
    for test in tests/suites/contract/test_*.py; do
        [ -f "$test" ] && run_test "$test" 180
    done

    echo ""
    echo "--- Compatibility Tests ---"
    for test in tests/suites/compatibility/test_*.py; do
        [ -f "$test" ] && run_test "$test" 180
    done

    echo ""
    echo "--- Security Tests ---"
    for test in tests/suites/security/test_*.py; do
        [ -f "$test" ] && run_test "$test" 180
    done

    echo ""
    echo "--- Resilience Tests ---"
    for test in tests/suites/resilience/test_*.py; do
        [ -f "$test" ] && run_test "$test" 300
    done

    # Restart server before conformance tests (resilience tests may degrade it)
    echo ""
    echo -e "${YELLOW}[RECOVERY]${NC} Restarting server before conformance tests..."
    restart_server_quick

    echo ""
    echo "--- Conformance Tests ---"
    for test in tests/suites/conformance/test_*.py; do
        [ -f "$test" ] && run_test "$test" 180
    done

    # Restart server
    echo ""
    echo -e "${YELLOW}[RECOVERY]${NC} Restarting server..."
    pkill -9 beam.smp 2>/dev/null || true
    sleep 3
    rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
    start_server || exit 1

    echo ""
    echo "--- Performance Tests ---"
    echo "  (Server will restart automatically after heavy tests)"
    ensure_server_ready "Performance Tests"
    # Quick mode: 300s per test (CI workloads are already scaled down)
    # Full mode: 600s per test (heavy benchmarks need more time)
    perf_timeout=600
    perf_heavy_timeout=600
    if [ "$QUICK_MODE" = "true" ]; then
        perf_timeout=300
        perf_heavy_timeout=600
    fi

    # Sort performance tests: light first, heavy last.
    # Heavy tests (e.g. benchmark_group_1000) trigger server restarts that
    # destabilize subsequent memory measurements. Same pattern as integration
    # and stress test sections.
    PERF_TESTS_LIGHT=()
    PERF_TESTS_HEAVY=()
    for test in tests/suites/performance_light/benchmark_*.py tests/suites/performance_light/measure_*.py tests/suites/performance_light/test_*.py; do
        if [ -f "$test" ]; then
            test_name=$(basename "$test" .py)
            if is_heavy_test "$test_name"; then
                PERF_TESTS_HEAVY+=("$test")
            else
                PERF_TESTS_LIGHT+=("$test")
            fi
        fi
    done

    # Run light tests first (includes benchmark_memory before any restart)
    for test in "${PERF_TESTS_LIGHT[@]}"; do
        run_test "$test" "$perf_timeout"
    done

    # Then run heavy tests (each will trigger server restart after)
    for test in "${PERF_TESTS_HEAVY[@]}"; do
        run_test "$test" "$perf_heavy_timeout"
    done

    # ======================================================================
    # STRESS TESTS — all tests run, no skipping
    # ======================================================================
    # Quick mode: stress tests use CI-scaled parameters (QUICK_MODE env var
    # detected by each test) with tighter timeouts. Full mode: original scale.
    echo ""
    echo "--- Stress Tests ---"
    echo "  (Server will restart automatically after heavy tests)"
    ensure_server_ready "Stress Tests"
    
    # Quick mode: 180s per light test, 300s per heavy test (CI-scaled workloads)
    # Full mode:  300s per light test, 600s per heavy test
    stress_light_timeout=300
    stress_heavy_timeout=600
    if [ "$QUICK_MODE" = "true" ]; then
        stress_light_timeout=180
        stress_heavy_timeout=300
    fi
    
    # Sort stress tests - run lighter ones first
    STRESS_TESTS_LIGHT=()
    STRESS_TESTS_HEAVY=()
    for test in tests/suites/stress/stress_*.py tests/suites/stress/test_*.py; do
        if [ -f "$test" ]; then
            test_name=$(basename "$test" .py)
            if is_heavy_test "$test_name"; then
                STRESS_TESTS_HEAVY+=("$test")
            else
                STRESS_TESTS_LIGHT+=("$test")
            fi
        fi
    done
    
    # Run light tests first
    for test in "${STRESS_TESTS_LIGHT[@]}"; do
        run_test "$test" "$stress_light_timeout"
        sleep 0.5
    done
    
    # Then run heavy tests with longer timeout
    for test in "${STRESS_TESTS_HEAVY[@]}"; do
        run_test "$test" "$stress_heavy_timeout"
    done

    echo ""
    echo "Stopping standalone server..."
    pkill -9 beam.smp 2>/dev/null || true
    sleep 3

    # ==========================================================================
    # PHASE 3: CLUSTERMANAGER TESTS
    # ==========================================================================
    echo ""
    echo -e "${BLUE}[PHASE 3]${NC} ClusterManager Tests (self-managed)"
    echo "============================================================================"
    echo "  (Each test manages its own cluster - server restart between tests)"

    # Unset CONFIG for Phase 3: chaos_controlled tests start their OWN cluster
    # via ClusterManager or direct `make` calls. The Erlang load generator
    # (iris_extreme_gen) connects via gen_tcp (plain TCP, not SSL) because
    # Erlang's ssl:connect requires additional setup that iris_extreme_gen
    # doesn't implement. Without unsetting CONFIG, the child `make start_*`
    # commands inherit CONFIG=config/test_tls and start TLS-only servers,
    # causing iris_extreme_gen's gen_tcp:connect to fail silently (0 messages).
    # Phase 2 TLS testing is already complete at this point.
    unset CONFIG

    for test in tests/suites/chaos_controlled/*.py; do
        if [ -f "$test" ]; then
            # Full cleanup before each chaos_controlled test
            pkill -9 beam.smp 2>/dev/null || true
            sleep 2
            rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
            run_test "$test" 300
        fi
    done

    pkill -9 beam.smp 2>/dev/null || true
    echo ""
fi

# ============================================================================
# PHASE 4: DOCKER CHAOS TESTS
# ============================================================================
if [ "$SKIP_DOCKER" = true ]; then
    echo -e "${YELLOW}[PHASE 4]${NC} Docker Chaos Tests - SKIPPED (--quick mode)"
    echo "============================================================================"
    echo "  Use './tests/run_all_tests.sh' (no flags) to run ALL tests"
    echo ""
else
    echo -e "${BLUE}[PHASE 4]${NC} Docker Chaos Tests (Fresh Cluster per Test)"
    echo "============================================================================"
    echo ""
    echo "Running ${#DOCKER_CHAOS_TESTS[@]} chaos tests. Each test gets a FRESH cluster"
    echo "using the proven cluster.sh script for isolation."
    echo ""
    
    pkill -9 beam.smp 2>/dev/null || true
    
    for test in "${DOCKER_CHAOS_TESTS[@]}"; do
        if [ -f "$test" ]; then
            run_docker_test_fresh "$test" 300
        fi
    done
    
    echo ""
    echo -e "${BLUE}[DOCKER]${NC} Final cleanup..."
    cluster_down
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
echo ""
TOTAL=$((TOTAL_PASS + TOTAL_FAIL))
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
