#!/bin/bash
# ============================================================================
# IRIS COMPLETE TEST SUITE RUNNER (Hardened)
# ============================================================================
# Runs ALL 75+ Python tests in optimal order with proper cluster initialization
#
# PHILOSOPHY: Tests are the MOST IMPORTANT part of this repo.
# - NO test skipping: All tests must PASS or FAIL definitively
# - NO weak assertions: Tests verify exact expected behavior
# - NO false positives: If a test passes, the feature works
# - NO false negatives: If a test fails, there's a real bug
#
# Key improvements from debugging sessions:
# - Uses ./cluster.sh up for proper Mnesia replication initialization
# - Reconnects Docker networks between destructive tests
# - Verifies cluster health before each Docker test
# - Sequential Docker tests to prevent container conflicts
# - Full Docker cleanup between destructive tests
#
# Usage:
#   sudo ./tests/run_all_tests.sh                  # Run ALL tests (recommended)
#   sudo ./tests/run_all_tests.sh --docker-core    # Run 8 core Docker chaos tests (fresh cluster each)
#   sudo ./tests/run_all_tests.sh --docker-only    # Run all Docker chaos tests only
#   sudo ./tests/run_all_tests.sh --quick          # Run non-Docker tests only (for quick iteration)
#
# DEFAULT MODE: Runs ALL tests. Use other modes only for targeted testing.
# ============================================================================

set -o pipefail

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

cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

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
FAILED_TESTS=()

# Options
SKIP_DOCKER=false
DOCKER_ONLY=false
DOCKER_CORE=false
if [[ "$1" == "--quick" ]]; then
    SKIP_DOCKER=true
elif [[ "$1" == "--docker-only" ]]; then
    DOCKER_ONLY=true
elif [[ "$1" == "--docker-core" ]]; then
    DOCKER_CORE=true
fi

# Log directory
LOG_DIR="tests/artifacts/full_run_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG_DIR"

# Determine mode string
get_mode_string() {
    if [ "$SKIP_DOCKER" = true ]; then
        echo 'Quick (non-Docker tests only)'
    elif [ "$DOCKER_CORE" = true ]; then
        echo 'Docker Core (8 essential chaos tests with fresh cluster each)'
    elif [ "$DOCKER_ONLY" = true ]; then
        echo 'Docker Only (all Docker chaos tests)'
    else
        echo 'FULL (ALL tests - recommended)'
    fi
}

echo "============================================================================"
echo "                    IRIS COMPLETE TEST SUITE (Hardened)"
echo "============================================================================"
echo "Project root: $PROJECT_ROOT"
echo "Log directory: $LOG_DIR"
echo "Mode: $(get_mode_string)"
echo ""

# ============================================================================
# CLEANUP FUNCTION
# ============================================================================
cleanup() {
    echo -e "${YELLOW}[CLEANUP]${NC} Stopping all processes..."
    pkill -9 beam.smp 2>/dev/null || true
    pkill -9 epmd 2>/dev/null || true
    
    # Clean up Mnesia directories and data
    rm -rf Mnesia.* MnesiaCore.* data/ 2>/dev/null || true
    find /tmp -maxdepth 1 -name "iris_*" -exec rm -rf {} \; 2>/dev/null || true
    rm -f erl_crash.dump core.log edge1.log edge2.log 2>/dev/null || true
    
    # Verify cleanup succeeded
    if ls -d Mnesia.* MnesiaCore.* data/ 2>/dev/null | grep -q .; then
        echo -e "${RED}[CLEANUP ERROR]${NC} Could not remove Mnesia/data directories."
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
    elif [ $exit_code -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC}"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "${RED}FAIL (exit $exit_code)${NC}"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name")
    fi
}

# ============================================================================
# DOCKER CLUSTER HELPERS
# ============================================================================

# Full cleanup of Docker resources (volumes, networks, containers)
full_docker_cleanup() {
    echo -e "  ${CYAN}[CLEANUP]${NC} Full Docker cleanup..."
    cd "$PROJECT_ROOT/docker/global-cluster"
    
    # Stop and remove all containers with volumes
    docker compose down -v --remove-orphans 2>/dev/null || true
    
    # Prune volumes (especially Mnesia data)
    docker volume prune -f 2>/dev/null || true
    
    # Remove any lingering iris/mnesia volumes
    docker volume ls -q 2>/dev/null | grep -E "mnesia|iris" | xargs -r docker volume rm 2>/dev/null || true
    
    # Prune networks
    docker network prune -f 2>/dev/null || true
    
    cd "$PROJECT_ROOT"
    sleep 2
}

start_docker_cluster() {
    echo -e "${BLUE}[DOCKER]${NC} Starting global cluster with proper initialization..."
    
    # Full cleanup first for clean state
    full_docker_cleanup
    
    # Use cluster.sh which runs init_cluster.sh for proper Mnesia setup
    cd docker/global-cluster
    
    # This runs docker-compose up AND init_cluster.sh for Mnesia replication
    ./cluster.sh up > "$PROJECT_ROOT/$LOG_DIR/docker_cluster_init.log" 2>&1
    local result=$?
    cd "$PROJECT_ROOT"
    
    if [ $result -ne 0 ]; then
        echo -e "${RED}[ERROR]${NC} Cluster initialization failed!"
        tail -50 "$LOG_DIR/docker_cluster_init.log"
        return 1
    fi
    
    # Additional stabilization wait
    echo "  Waiting 10s for additional cluster stabilization..."
    sleep 10
    
    # Verify cluster health
    local running=$(docker ps --format '{{.Names}}' | grep -cE "^(core|edge)-" || echo "0")
    if [ "$running" -lt 10 ]; then
        echo -e "${RED}[ERROR]${NC} Only $running containers running (expected 15+)"
        return 1
    fi
    
    echo -e "  ${GREEN}Cluster ready with $running containers${NC}"
    return 0
}

stop_docker_cluster() {
    echo -e "${BLUE}[DOCKER]${NC} Stopping global cluster..."
    cd docker/global-cluster
    docker compose down -v --remove-orphans > "$PROJECT_ROOT/$LOG_DIR/docker_stop.log" 2>&1
    cd "$PROJECT_ROOT"
}

reconnect_docker_networks() {
    # Reconnect any containers that may have been disconnected by tests
    echo -e "  ${CYAN}[NETWORK]${NC} Reconnecting Docker networks..."
    
    for container in core-eu-1 core-eu-2 edge-eu-1 edge-eu-2 \
                     core-west-1 core-west-2 edge-west-1 edge-west-2 \
                     core-east-1 core-east-2 edge-east-1 edge-east-2; do
        docker network connect global-cluster_iris_backbone "$container" 2>/dev/null || true
    done
    
    # Brief stabilization
    sleep 3
}

ensure_containers_running() {
    # Restart any stopped containers and ensure all are up
    cd "$PROJECT_ROOT/docker/global-cluster"
    docker compose up -d 2>/dev/null || true
    cd "$PROJECT_ROOT"
    sleep 5
}

verify_cluster_health() {
    local running=$(docker ps --format '{{.Names}}' | grep -cE "^(core|edge)-" || echo "0")
    if [ "$running" -lt 10 ]; then
        echo -e "  ${YELLOW}[HEALTH]${NC} Only $running containers, attempting recovery..."
        ensure_containers_running
        reconnect_docker_networks
        sleep 5
        running=$(docker ps --format '{{.Names}}' | grep -cE "^(core|edge)-" || echo "0")
    fi
    return 0
}

run_docker_test() {
    local test_path=$1
    local test_name=$(basename "$test_path" .py)
    local timeout_sec=${2:-300}
    
    # Pre-test health check and network reconnection
    verify_cluster_health
    reconnect_docker_networks
    
    printf "  %-45s" "$test_name"
    
    local start_time=$(date +%s)
    timeout "$timeout_sec" python3 -u "$test_path" > "$LOG_DIR/${test_name}.log" 2>&1
    local exit_code=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}PASS${NC} (${duration}s)"
        ((TOTAL_PASS++))
    elif [ $exit_code -eq 124 ]; then
        echo -e "${RED}TIMEOUT${NC} (${duration}s)"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "${RED}FAIL (exit $exit_code)${NC} (${duration}s)"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name")
    fi
    
    # Post-test recovery - ensure all containers are running
    ensure_containers_running
}

# Run a single Docker test with a FRESH cluster (for destructive tests)
run_docker_test_fresh_cluster() {
    local test_path=$1
    local test_name=$(basename "$test_path" .py)
    local timeout_sec=${2:-300}
    
    echo ""
    echo -e "  ${BLUE}[TEST]${NC} $test_name"
    echo "  ----------------------------------------"
    
    # Start fresh cluster for this test
    echo -e "  ${CYAN}[CLUSTER]${NC} Starting fresh cluster..."
    cd "$PROJECT_ROOT/docker/global-cluster"
    docker compose down -v 2>/dev/null || true
    docker volume prune -f 2>/dev/null || true
    ./cluster.sh up > "$PROJECT_ROOT/$LOG_DIR/${test_name}_cluster.log" 2>&1
    local cluster_result=$?
    cd "$PROJECT_ROOT"
    
    if [ $cluster_result -ne 0 ]; then
        echo -e "  ${RED}CLUSTER INIT FAILED${NC}"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name (cluster init)")
        return 1
    fi
    
    # Run the test (skip internal cleanup since we manage cluster lifecycle)
    local start_time=$(date +%s)
    SKIP_TEST_CLEANUP=1 timeout "$timeout_sec" python3 -u "$test_path" > "$LOG_DIR/${test_name}.log" 2>&1
    local exit_code=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $exit_code -eq 0 ]; then
        echo -e "  ${GREEN}✓ PASS${NC} (${duration}s)"
        ((TOTAL_PASS++))
    elif [ $exit_code -eq 124 ]; then
        echo -e "  ${RED}✗ TIMEOUT${NC} (${duration}s)"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name (timeout)")
    else
        echo -e "  ${RED}✗ FAIL (exit $exit_code)${NC} (${duration}s)"
        ((TOTAL_FAIL++))
        FAILED_TESTS+=("$test_name")
    fi
}

# ============================================================================
# PHASE 0: SETUP
# ============================================================================
if [ "$DOCKER_ONLY" = false ] && [ "$DOCKER_CORE" = false ]; then
    echo -e "${BLUE}[PHASE 0]${NC} Setup and cleanup..."
    cleanup
    echo ""
fi

# ============================================================================
# Skip to Docker tests if --docker-only or --docker-core
# ============================================================================
if [ "$DOCKER_ONLY" = true ] || [ "$DOCKER_CORE" = true ]; then
    echo -e "${YELLOW}[INFO]${NC} Skipping to Docker tests ($([ "$DOCKER_CORE" = true ] && echo '--docker-core' || echo '--docker-only') mode)"
    echo ""
else

# ============================================================================
# PHASE 1: UNIT TESTS (No server needed)
# ============================================================================
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

# ============================================================================
# PHASE 2: STANDALONE SERVER TESTS
# ============================================================================
echo -e "${BLUE}[PHASE 2]${NC} Standalone Server Tests"
echo "============================================================================"

start_server || exit 1

echo ""
echo "--- Integration Tests ---"
HEAVY_TESTS="test_degradation_order"

for test in tests/suites/integration/test_*.py; do
    [ -f "$test" ] || continue
    test_name=$(basename "$test" .py)
    
    if [[ "$HEAVY_TESTS" == *"$test_name"* ]]; then
        continue
    fi
    
    run_test "$test" 180
done

echo ""
echo "--- Heavy Load Integration Tests ---"
for test_name in $HEAVY_TESTS; do
    test="tests/suites/integration/${test_name}.py"
    [ -f "$test" ] && run_test "$test" 300
done

# Restart server after heavy tests
echo ""
echo -e "${YELLOW}[RECOVERY]${NC} Restarting server after heavy load tests..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
start_server || exit 1

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
echo "--- Resilience Tests (standalone) ---"
run_test "tests/suites/resilience/test_clock_skew.py" 300
run_test "tests/suites/resilience/test_hard_kill.py" 300

# Restart server after resilience tests
echo ""
echo -e "${YELLOW}[RECOVERY]${NC} Restarting server after resilience tests..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
start_server || exit 1

echo ""
echo "--- Performance Tests (standalone) ---"
run_test "tests/suites/performance_light/benchmark_e2ee_latency.py" 180
run_test "tests/suites/performance_light/benchmark_throughput.py" 300
run_test "tests/suites/performance_light/benchmark_unit_cost.py" 300

echo ""
echo "--- Stress Tests (standalone) ---"
run_test "tests/suites/stress/stress_offline_delete.py" 180
run_test "tests/suites/stress/test_flow_controller_scale.py" 180
run_test "tests/suites/stress/test_group_fanout.py" 180

# Soak test with short duration for CI
export SOAK_DURATION_HOURS=0.08
export SOAK_SAMPLE_INTERVAL=30
run_test "tests/suites/stress/test_soak_memory.py" 420
unset SOAK_DURATION_HOURS
unset SOAK_SAMPLE_INTERVAL

echo ""
echo "Stopping standalone server..."
pkill -9 beam.smp 2>/dev/null || true
sleep 3
echo ""

# ============================================================================
# PHASE 3: CLUSTERMANAGER TESTS
# ============================================================================
echo -e "${BLUE}[PHASE 3]${NC} ClusterManager Tests (self-managed)"
echo "============================================================================"

CM_TESTS=(
    "tests/suites/resilience/test_resilience.py"
    "tests/suites/performance_light/benchmark_memory.py"
    "tests/suites/performance_light/measure_dials.py"
    "tests/suites/performance_light/test_cpu_utilization.py"
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

echo ""
echo "--- Chaos Controlled Tests ---"
for test in tests/suites/chaos_controlled/*.py; do
    if [ -f "$test" ]; then
        pkill -9 beam.smp 2>/dev/null || true
        sleep 2
        run_test "$test" 300
    fi
done

pkill -9 beam.smp 2>/dev/null || true
echo ""

fi  # End of DOCKER_ONLY skip block

# ============================================================================
# PHASE 4: DOCKER CHAOS TESTS
# ============================================================================
if [ "$SKIP_DOCKER" = true ]; then
    echo -e "${YELLOW}[PHASE 4]${NC} Docker Chaos Tests - NOT INCLUDED (--quick mode)"
    echo "============================================================================"
    echo "  Note: Use 'sudo ./tests/run_all_tests.sh' (no flags) to run ALL tests"
    echo ""
elif [ "$DOCKER_CORE" = true ]; then
    # ========================================================================
    # DOCKER CORE TESTS (8 essential chaos tests with fresh cluster each)
    # ========================================================================
    echo -e "${BLUE}[PHASE 4]${NC} Docker Core Chaos Tests (Fresh Cluster per Test)"
    echo "============================================================================"
    echo ""
    echo "Running 8 essential Docker chaos tests. Each test gets a FRESH cluster"
    echo "to ensure isolation and prevent cross-test interference."
    echo ""
    echo "Tests:"
    echo "  1. test_multimaster_durability  - RPO=0 with SIGKILL"
    echo "  2. test_dedup_persistence       - Dedup survives crash"
    echo "  3. test_network_partition       - Netsplit handling"
    echo "  4. test_ordering_under_failure  - FIFO ordering"
    echo "  5. test_cross_region_latency    - Cross-region P99"
    echo "  6. test_region_outage           - Region failure recovery"
    echo "  7. test_dist_failover           - Failover scenarios"
    echo "  8. test_disk_full               - Disk-full handling"
    echo ""
    
    pkill -9 beam.smp 2>/dev/null || true
    
    # 8 Core Docker Chaos Tests (fresh cluster for each)
    DOCKER_CORE_TESTS=(
        "tests/suites/chaos_dist/test_multimaster_durability.py"
        "tests/suites/chaos_dist/test_dedup_persistence.py"
        "tests/suites/chaos_dist/test_network_partition.py"
        "tests/suites/chaos_dist/test_ordering_under_failure.py"
        "tests/suites/chaos_dist/test_cross_region_latency.py"
        "tests/suites/chaos_dist/test_region_outage.py"
        "tests/suites/chaos_dist/test_dist_failover.py"
        "tests/suites/chaos_dist/test_disk_full.py"
    )
    
    for test in "${DOCKER_CORE_TESTS[@]}"; do
        if [ -f "$test" ]; then
            run_docker_test_fresh_cluster "$test" 300
        fi
    done
    
    echo ""
    echo -e "${BLUE}[DOCKER]${NC} Final cleanup..."
    stop_docker_cluster
else
    # ========================================================================
    # FULL DOCKER TESTS (all chaos tests, shared cluster with recovery)
    # ========================================================================
    echo -e "${BLUE}[PHASE 4]${NC} Docker Chaos Tests (Full Suite)"
    echo "============================================================================"
    echo ""
    echo "These tests require a properly initialized Docker cluster with Mnesia"
    echo "replication. Tests run SEQUENTIALLY to prevent container conflicts."
    echo ""

    pkill -9 beam.smp 2>/dev/null || true

    # Start cluster with proper initialization
    if ! start_docker_cluster; then
        echo -e "${RED}[FATAL]${NC} Failed to start Docker cluster"
        exit 1
    fi
    
    echo ""
    echo "--- Running Docker Chaos Tests (sequential) ---"
    
    # Define test order: less destructive first, more destructive later
    DOCKER_TESTS=(
        # Read-only / light tests first
        "tests/suites/chaos_dist/test_server_storage_audit.py"
        "tests/suites/chaos_dist/test_cross_region_latency.py"
        "tests/suites/chaos_dist/test_distributed_rate_limit.py"
        "tests/suites/chaos_dist/test_key_bundle_durability.py"
        
        # Durability tests (moderate disruption)
        "tests/suites/chaos_dist/test_ack_durability.py"
        "tests/suites/chaos_dist/test_dedup_persistence.py"
        "tests/suites/chaos_dist/test_ordering_under_failure.py"
        "tests/suites/chaos_dist/test_multimaster_durability.py"
        
        # Cross-region tests
        "tests/suites/chaos_dist/test_cross_region_chaos.py"
        "tests/suites/chaos_dist/test_region_outage.py"
        "tests/suites/chaos_dist/test_bridge_durability.py"
        
        # Heavy disruption tests (container kills, network partitions)
        "tests/suites/chaos_dist/test_ack_disconnect_race.py"
        "tests/suites/chaos_dist/test_dist_failover.py"
        "tests/suites/chaos_dist/test_failover_time.py"
        "tests/suites/chaos_dist/test_cascade_failure.py"
        "tests/suites/chaos_dist/test_split_brain.py"
        "tests/suites/chaos_dist/test_network_partition.py"
        "tests/suites/chaos_dist/test_disk_full.py"
    )
    
    for test in "${DOCKER_TESTS[@]}"; do
        if [ -f "$test" ]; then
            run_docker_test "$test" 300
        fi
    done

    echo ""
    stop_docker_cluster
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
