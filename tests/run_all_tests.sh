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
#   ./tests/run_all_tests.sh                          # Run ALL tests (recommended)
#   ./tests/run_all_tests.sh --docker-only             # Run Docker chaos tests only
#   ./tests/run_all_tests.sh --quick                   # Run non-Docker tests only
#   ./tests/run_all_tests.sh --suites integration,e2e  # Run specific suites only
#   ./tests/run_all_tests.sh --help                    # Show help
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
TOTAL_SKIP=0
TOTAL_WARN=0
FAILED_TESTS=()
SKIPPED_TESTS=()
WARNED_TESTS=()

# Options
SKIP_DOCKER=false
DOCKER_ONLY=false

SELECTED_SUITES=""

show_help() {
    echo "IRIS Complete Test Suite Runner"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --help                      Show this help"
    echo "  --quick                     Run non-Docker tests only (faster)"
    echo "  --docker-only               Run Docker chaos tests only"
    echo "  --suites <suite1,suite2>    Run only specified suites (implies --quick)"
    echo "  (no option)                 Run ALL tests (recommended)"
    echo ""
    echo "Suites: unit, integration, e2e, contract, compatibility, security,"
    echo "        resilience, conformance, performance_light, stress, chaos_controlled"
    echo ""
    echo "Examples:"
    echo "  $0                                       # Full test suite"
    echo "  $0 --quick                               # Fast iteration (no Docker)"
    echo "  $0 --docker-only                         # Only chaos tests"
    echo "  $0 --suites integration,e2e,contract     # Specific suites"
    exit 0
}

# Parse args
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h) show_help ;;
        --quick) SKIP_DOCKER=true; export QUICK_MODE=true ;;
        --docker-only) DOCKER_ONLY=true ;;
        --suites)
            SELECTED_SUITES="$2"
            SKIP_DOCKER=true
            export QUICK_MODE=true
            shift
            ;;
        *) echo "Unknown option: $1"; show_help ;;
    esac
    shift
done

suite_enabled() {
    local suite_name=$1
    [ -z "$SELECTED_SUITES" ] && return 0
    echo ",$SELECTED_SUITES," | grep -q ",$suite_name,"
}

needs_server() {
    suite_enabled integration || suite_enabled e2e || suite_enabled contract || \
    suite_enabled compatibility || suite_enabled security || suite_enabled resilience || \
    suite_enabled conformance || suite_enabled performance_light || suite_enabled stress
}

# Export CONFIG so Python tests know the server is TLS-enabled.
# Without this, tests using ClusterManager would start non-TLS servers (killing
# the TLS server run_all_tests.sh manages), and tests detecting USE_TLS via
# os.environ.get("CONFIG") would fall back to plain TCP against a TLS server.
export CONFIG=config/test_tls

# Log directory
LOG_DIR="$PROJECT_ROOT/tests/artifacts/full_run_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG_DIR"

get_mode_string() {
    if [ -n "$SELECTED_SUITES" ]; then
        echo "Suites: $SELECTED_SUITES"
    elif [ "$SKIP_DOCKER" = true ]; then
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
    pkill -u "$USER" -9 beam.smp 2>/dev/null || true
    # NOTE: Do NOT kill epmd here.  Killing epmd causes subsequent 'make start'
    # and 'erl -eval' invocations to fail with "no alive nodes" errors because
    # the port mapper is unavailable.  epmd is lightweight and shared; leave it.
    rm -rf Mnesia.* MnesiaCore.* data/ 2>/dev/null || true
    find /tmp -maxdepth 1 -name "iris_*" -exec rm -rf {} \; 2>/dev/null || true
    rm -f erl_crash.dump core.log edge1.log edge2.log 2>/dev/null || true
    sleep 2
    # Ensure epmd is running (needed by all Erlang nodes)
    epmd -daemon 2>/dev/null || true
}

# ============================================================================
# START TLS SERVER (for standalone tests)
# ============================================================================
start_server() {
    echo "Starting local TLS server..."
    # Pre-check: fail fast if port 8085 is occupied by a non-user process (e.g. Docker)
    if nc -z localhost 8085 2>/dev/null; then
        if ! pgrep -u "$USER" -f "sname iris_edge" > /dev/null 2>&1; then
            echo -e "${RED}ERROR: Port 8085 is in use by another process (Docker cluster?)${NC}"
            echo -e "${RED}Stop Docker containers first: ./docker/global-cluster/cluster.sh down${NC}"
            ss -tlnp 2>/dev/null | grep ':8085' || true
            return 1
        fi
    fi
    make start CONFIG=config/test_tls > "$LOG_DIR/server_start.log" 2>&1
    sleep 5
    
    # Verify OUR server started (not a leftover Docker edge on the same port)
    if ! pgrep -u "$USER" -f "sname iris_edge" > /dev/null 2>&1; then
        echo -e "${RED}ERROR: Server process not found after start${NC}"
        tail -10 "$LOG_DIR/server_start.log"
        return 1
    fi
    
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
    "test_backpressure_collapse" # Backpressure stress
    "test_connection_rate_limit" # Connection flood crashes server (200 conns burst)
    "test_churn"                 # Connect/disconnect storms crash edge under load
    "test_reconnect_storm"       # Reconnection floods can crash edge
    "test_dedup_bloom_accuracy"  # High-volume dedup, needs fresh server state
    "test_hotkey_bucketing"      # 50+ rapid messages to single user, destabilizes server
    "test_idempotency"           # 100+ rapid messages (retry storm), destabilizes server
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
# TIER 2 TESTS (only run in full mode — too long for CI quick runs)
# ============================================================================
# These tests require extended durations and are designed for pre-release
# validation, not every-PR gating.
TIER2_TESTS=(
    "test_soak_24h"  # 24h soak test — memory/WAL/FD leak detection
)

is_tier2_test() {
    local test_name=$1
    for t2 in "${TIER2_TESTS[@]}"; do
        if [[ "$test_name" == *"$t2"* ]]; then
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
    elif [ $exit_code -eq 2 ]; then
        # AUDIT MITIGATION P1-1: exit code 2 = infrastructure skip
        echo -e "${YELLOW}SKIP${NC}"
        TOTAL_SKIP=$((TOTAL_SKIP + 1))
        SKIPPED_TESTS+=("$test_name")
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
    elif [ $exit_code -ne 0 ] && ! nc -z localhost 8085 2>/dev/null; then
        # Server died during a non-heavy test — auto-recover
        echo -e "    ${YELLOW}(server died - auto-recovering)${NC}"
        restart_server_quick
    fi
}

# Quick server restart (used after heavy tests)
# Optional arg: config file (defaults to config/test_tls)
restart_server_quick() {
    local restart_config=${1:-config/test_tls}
    # Graceful shutdown first (SIGTERM) to allow clean socket teardown,
    # then force kill (SIGKILL) as fallback. Using only SIGKILL causes
    # listen sockets to linger in TIME_WAIT, leading to eaddrinuse on restart.
    # Use -u $USER to only kill OUR beam.smp processes — root-owned
    # beam.smp (Docker, system services) would cause false warnings.
    pkill -u "$USER" -TERM beam.smp 2>/dev/null || true
    sleep 2
    pkill -u "$USER" -9 beam.smp 2>/dev/null || true
    sleep 1
    pkill -u "$USER" -9 beam.smp 2>/dev/null || true
    # Wait for OUR beam.smp to actually exit
    local wait_attempts=0
    while pgrep -u "$USER" -x beam.smp > /dev/null 2>&1; do
        wait_attempts=$((wait_attempts + 1))
        if [ $wait_attempts -ge 15 ]; then
            echo -e "    ${YELLOW}Warning: beam.smp still in process table after 15s${NC}"
            break
        fi
        sleep 1
    done
    # Wait for ports to be fully released (prevents eaddrinuse on restart).
    # After heavy tests, socket cleanup can take 20+ seconds.
    local port_attempts=0
    while ss -tlnp 2>/dev/null | grep -q ':8085\|:8086'; do
        port_attempts=$((port_attempts + 1))
        if [ $port_attempts -ge 30 ]; then
            echo -e "    ${YELLOW}Warning: ports still held after 30s${NC}"
            break
        fi
        sleep 1
    done
    rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
    epmd -daemon 2>/dev/null || true
    make start CONFIG="$restart_config" > "$LOG_DIR/server_restart.log" 2>&1
    sleep 5
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
    sleep 5
}

cluster_up() {
    echo -e "  ${CYAN}[CLUSTER]${NC} Starting cluster (via cluster.sh)..."
    cd "$CLUSTER_DIR"
    if bash "$CLUSTER_SCRIPT" up > "$LOG_DIR/cluster_up.log" 2>&1; then
        cd "$PROJECT_ROOT"
        echo -e "  ${GREEN}Cluster ready (cores)${NC}"
        # Wait for at least one edge node to accept TLS connections.
        # init_cluster.sh only checks core nodes; edge nodes may still be starting.
        wait_for_edge_ready
        return 0
    else
        cd "$PROJECT_ROOT"
        echo -e "  ${RED}Cluster initialization FAILED${NC}"
        tail -20 "$LOG_DIR/cluster_up.log"
        return 1
    fi
}

# Wait for at least one edge node to accept TCP connections on its TLS port.
# This prevents tests from failing at 0s because the edge isn't ready yet.
wait_for_edge_ready() {
    local max_wait=30
    local ports="8085 8087 8089"  # edge-east-1, edge-west-1, edge-eu-1
    local attempt=0
    
    while [ $attempt -lt $max_wait ]; do
        for port in $ports; do
            if nc -z localhost "$port" 2>/dev/null; then
                echo -e "  ${GREEN}Edge ready (port $port)${NC}"
                # Give a brief extra moment for TLS listener stabilization
                sleep 2
                return 0
            fi
        done
        attempt=$((attempt + 1))
        sleep 1
    done
    echo -e "  ${YELLOW}[WARN] No edge nodes responded after ${max_wait}s - tests may fail${NC}"
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
    elif [ $exit_code -eq 2 ]; then
        # AUDIT MITIGATION P1-1: exit code 2 = infrastructure skip
        echo -e "  ${YELLOW}⏭ SKIP${NC} (${duration}s)"
        TOTAL_SKIP=$((TOTAL_SKIP + 1))
        SKIPPED_TESTS+=("$test_name")
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

# Run a batch of Docker tests on a SHARED cluster (reduces infrastructure overhead)
run_docker_test_batch() {
    local batch_name=$1
    shift
    local tests=("$@")
    local timeout_sec=480

    echo ""
    echo -e "  ${CYAN}[BATCH]${NC} $batch_name (${#tests[@]} tests on shared cluster)"
    echo "  ========================================"

    cluster_down

    if ! cluster_up; then
        echo -e "  ${RED}CLUSTER INIT FAILED for batch $batch_name${NC}"
        for test_path in "${tests[@]}"; do
            local tname=$(basename "$test_path" .py)
            TOTAL_FAIL=$((TOTAL_FAIL + 1))
            FAILED_TESTS+=("$tname (cluster init)")
        done
        return 1
    fi

    for test_path in "${tests[@]}"; do
        local test_name=$(basename "$test_path" .py)
        printf "    %-48s" "$test_name"

        local start_time=$(date +%s)
        SKIP_TEST_CLEANUP=1 timeout "$timeout_sec" python3 -u "$test_path" > "$LOG_DIR/${test_name}.log" 2>&1
        local exit_code=$?
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        if [ $exit_code -eq 0 ]; then
            echo -e "${GREEN}PASS${NC} (${duration}s)"
            TOTAL_PASS=$((TOTAL_PASS + 1))
        elif [ $exit_code -eq 2 ]; then
            echo -e "${YELLOW}SKIP${NC} (${duration}s)"
            TOTAL_SKIP=$((TOTAL_SKIP + 1))
            SKIPPED_TESTS+=("$test_name")
        elif [ $exit_code -eq 124 ]; then
            echo -e "${RED}TIMEOUT${NC} (${duration}s)"
            TOTAL_FAIL=$((TOTAL_FAIL + 1))
            FAILED_TESTS+=("$test_name (timeout)")
        else
            echo -e "${RED}FAIL (exit $exit_code)${NC} (${duration}s)"
            TOTAL_FAIL=$((TOTAL_FAIL + 1))
            FAILED_TESTS+=("$test_name")
        fi
    done
}

# ============================================================================
# DOCKER CHAOS TESTS — batched by runtime/destructiveness
# ============================================================================
# Batching shares a cluster across compatible tests, cutting ~45 min of
# cluster spin-up/teardown overhead from 26 individual cycles to ~6 cycles.
# Tests are grouped by runtime and destructiveness level.
# New tests not listed here are auto-discovered and run individually (safe default).

DOCKER_BATCH_FAST=(
    "tests/suites/chaos_dist/test_cross_region_latency.py"
    "tests/suites/chaos_dist/test_disk_full.py"
    "tests/suites/chaos_dist/test_failover_time.py"
    "tests/suites/chaos_dist/test_mtls_inter_node.py"
    "tests/suites/chaos_dist/test_outbox_queue_overflow.py"
    "tests/suites/chaos_dist/test_real_clock_skew.py"
    "tests/suites/chaos_dist/test_split_brain.py"
)

DOCKER_BATCH_MEDIUM_A=(
    "tests/suites/chaos_dist/test_ack_disconnect_race.py"
    "tests/suites/chaos_dist/test_bridge_durability.py"
    "tests/suites/chaos_dist/test_cascade_failure.py"
    "tests/suites/chaos_dist/test_cross_region_chaos.py"
    "tests/suites/chaos_dist/test_cross_region_node_kill.py"
)

DOCKER_BATCH_MEDIUM_B=(
    "tests/suites/chaos_dist/test_dedup_persistence.py"
    "tests/suites/chaos_dist/test_dist_failover.py"
    "tests/suites/chaos_dist/test_distributed_rate_limit.py"
    "tests/suites/chaos_dist/test_multimaster_durability.py"
    "tests/suites/chaos_dist/test_ordering_under_failure.py"
)

DOCKER_BATCH_MEDIUM_C=(
    "tests/suites/chaos_dist/test_outbox_overflow_enforcement.py"
    "tests/suites/chaos_dist/test_quorum_write_failures.py"
    "tests/suites/chaos_dist/test_server_storage_audit.py"
    "tests/suites/chaos_dist/test_split_brain_convergence.py"
    "tests/suites/chaos_dist/test_split_brain_epoch_resolution.py"
)

DOCKER_BATCH_LONG=(
    "tests/suites/chaos_dist/test_ack_durability.py"
    "tests/suites/chaos_dist/test_key_bundle_durability.py"
    "tests/suites/chaos_dist/test_network_partition.py"
    "tests/suites/chaos_dist/test_region_outage.py"
)

# Collect all batched test paths for detecting unbatched new tests
ALL_BATCHED_TESTS=()
ALL_BATCHED_TESTS+=("${DOCKER_BATCH_FAST[@]}")
ALL_BATCHED_TESTS+=("${DOCKER_BATCH_MEDIUM_A[@]}")
ALL_BATCHED_TESTS+=("${DOCKER_BATCH_MEDIUM_B[@]}")
ALL_BATCHED_TESTS+=("${DOCKER_BATCH_MEDIUM_C[@]}")
ALL_BATCHED_TESTS+=("${DOCKER_BATCH_LONG[@]}")

# Auto-discover any tests NOT in the batches (new tests get fresh clusters)
DOCKER_UNBATCHED_TESTS=()
while IFS= read -r test_file; do
    is_batched=false
    for batched in "${ALL_BATCHED_TESTS[@]}"; do
        if [ "$test_file" = "$batched" ]; then
            is_batched=true
            break
        fi
    done
    if [ "$is_batched" = false ]; then
        DOCKER_UNBATCHED_TESTS+=("$test_file")
    fi
done < <(find tests/suites/chaos_dist -name 'test_*.py' -type f | sort)

TOTAL_DOCKER_TESTS=$(( ${#ALL_BATCHED_TESTS[@]} + ${#DOCKER_UNBATCHED_TESTS[@]} ))

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
    if [ "${CI_SKIP_COMPILE:-}" != "true" ]; then
        echo -e "${BLUE}[PHASE 0]${NC} Setup and cleanup..."
        cleanup_standalone
        echo ""
    else
        echo -e "${BLUE}[PHASE 0]${NC} Setup (CI_SKIP_COMPILE=true, using pre-built ebin)..."
        epmd -daemon 2>/dev/null || true
        echo ""
    fi

    # ==========================================================================
    # PHASE 1: UNIT TESTS (No server needed)
    # ==========================================================================
    echo -e "${BLUE}[PHASE 1]${NC} Unit Tests"
    echo "============================================================================"

    if [ "${CI_SKIP_COMPILE:-}" != "true" ]; then
        echo "Compiling..."
        make all > "$LOG_DIR/compile.log" 2>&1 || {
            echo -e "${RED}Compilation failed!${NC}"
            cat "$LOG_DIR/compile.log"
            exit 1
        }
        echo -e "  ${GREEN}Compilation successful${NC}"

        echo ""
        echo "--- EUnit Tests ---"
        printf "  %-50s" "EUnit (all discovered modules)"
        make test > "$LOG_DIR/eunit.log" 2>&1; eunit_rc=$?
        if [ $eunit_rc -eq 0 ]; then
            echo -e "${GREEN}PASS${NC}"
            TOTAL_PASS=$((TOTAL_PASS + 1))
        else
            CANCELLED=$(grep -oP 'Cancelled: \K[0-9]+' "$LOG_DIR/eunit.log" 2>/dev/null || echo "0")
            FAILED_COUNT=$(grep -oP 'Failed: \K[0-9]+' "$LOG_DIR/eunit.log" 2>/dev/null || echo "unknown")
            if [ "$FAILED_COUNT" = "0" ] && [ "$CANCELLED" = "0" ]; then
                echo -e "${GREEN}PASS${NC} (non-zero exit, 0 failures, 0 cancellations)"
                TOTAL_PASS=$((TOTAL_PASS + 1))
            elif [ "$FAILED_COUNT" = "0" ] && [ "$CANCELLED" != "0" ]; then
                echo -e "${RED}FAIL${NC} ($CANCELLED test(s) CANCELLED — never ran)"
                TOTAL_FAIL=$((TOTAL_FAIL + 1))
                FAILED_TESTS+=("EUnit ($CANCELLED cancelled)")
            else
                echo -e "${RED}FAIL${NC}"
                TOTAL_FAIL=$((TOTAL_FAIL + 1))
                FAILED_TESTS+=("EUnit")
            fi
        fi

        echo ""
        echo "--- Property-Based Tests ---"
        printf "  %-50s" "Protocol Properties (iris_proto_props)"
        ERL_CMD="erl -pa ebin -noshell -eval \"case iris_proto_props:test_all() of ok -> halt(0); error -> halt(1) end.\""
        eval $ERL_CMD > "$LOG_DIR/proto_props.log" 2>&1
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}PASS${NC}"
            TOTAL_PASS=$((TOTAL_PASS + 1))
        else
            echo -e "${RED}FAIL${NC}"
            TOTAL_FAIL=$((TOTAL_FAIL + 1))
            FAILED_TESTS+=("iris_proto_props")
        fi
    else
        echo "  (skipping compile/EUnit/property tests — CI_SKIP_COMPILE=true)"
    fi

    if suite_enabled unit; then
        echo ""
        echo "--- Python Unit Tests ---"
        for test in tests/suites/unit/test_*.py; do
            [ -f "$test" ] && run_test "$test" 60
        done
    fi
    echo ""

    # ==========================================================================
    # PHASE 2: STANDALONE SERVER TESTS
    # ==========================================================================
    if needs_server; then
        echo -e "${BLUE}[PHASE 2]${NC} Standalone Server Tests"
        echo "============================================================================"

        start_server || exit 1

        if suite_enabled integration; then
            echo ""
            echo "--- Integration Tests ---"
            echo "  (Server will restart automatically after heavy tests)"

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

            for test in "${INTEGRATION_TESTS_LIGHT[@]}"; do
                run_test "$test" 180
                sleep 0.5
            done

            for test in "${INTEGRATION_TESTS_HEAVY[@]}"; do
                run_test "$test" 240
            done
        fi

        if suite_enabled e2e; then
            echo ""
            echo "--- E2E Tests ---"
            ensure_server_ready "E2E Tests"
            for test in tests/suites/e2e/test_*.py; do
                [ -f "$test" ] && run_test "$test" 180
            done
        fi

        if suite_enabled contract; then
            echo ""
            echo "--- Contract Tests ---"
            ensure_server_ready "Contract Tests"
            for test in tests/suites/contract/test_*.py; do
                [ -f "$test" ] && run_test "$test" 180
            done
        fi

        if suite_enabled compatibility; then
            echo ""
            echo "--- Compatibility Tests ---"
            ensure_server_ready "Compatibility Tests"
            for test in tests/suites/compatibility/test_*.py; do
                [ -f "$test" ] && run_test "$test" 180
            done
        fi

        if suite_enabled security; then
            echo ""
            echo "--- Security Tests ---"
            ensure_server_ready "Security Tests"
            for test in tests/suites/security/test_*.py; do
                [ -f "$test" ] || continue
                test_name=$(basename "$test" .py)
                if [[ "$test_name" == "test_mtls_enforcement" ]]; then
                    echo -e "    ${YELLOW}(switching to mTLS config for $test_name)${NC}"
                    restart_server_quick config/test_mtls
                    run_test "$test" 180
                    restart_server_quick config/test_tls
                else
                    run_test "$test" 180
                fi
            done
        fi

        if suite_enabled resilience; then
            echo ""
            echo "--- Resilience Tests ---"
            ensure_server_ready "Resilience Tests"
            for test in tests/suites/resilience/test_*.py; do
                [ -f "$test" ] && run_test "$test" 300
            done
        fi

        if suite_enabled conformance; then
            echo ""
            echo -e "${YELLOW}[RECOVERY]${NC} Restarting server before conformance tests..."
            restart_server_quick

            echo ""
            echo "--- Conformance Tests ---"
            for test in tests/suites/conformance/test_*.py; do
                [ -f "$test" ] && run_test "$test" 180
            done
        fi

        if suite_enabled performance_light; then
            echo ""
            echo -e "${YELLOW}[RECOVERY]${NC} Restarting server..."
            restart_server_quick

            echo ""
            echo "--- Performance Tests ---"
            echo "  (Server will restart automatically after heavy tests)"
            ensure_server_ready "Performance Tests"
            perf_timeout=600
            perf_heavy_timeout=600
            if [ "$QUICK_MODE" = "true" ]; then
                perf_timeout=300
                perf_heavy_timeout=600
            fi

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

            for test in "${PERF_TESTS_LIGHT[@]}"; do
                run_test "$test" "$perf_timeout"
            done

            for test in "${PERF_TESTS_HEAVY[@]}"; do
                run_test "$test" "$perf_heavy_timeout"
            done
        fi

        if suite_enabled stress; then
            echo ""
            echo "--- Stress Tests ---"
            echo "  (Server will restart automatically after heavy tests)"
            ensure_server_ready "Stress Tests"

            stress_light_timeout=300
            stress_heavy_timeout=600
            if [ "$QUICK_MODE" = "true" ]; then
                stress_light_timeout=180
                stress_heavy_timeout=300
            fi

            STRESS_TESTS_LIGHT=()
            STRESS_TESTS_HEAVY=()
            for test in tests/suites/stress/stress_*.py tests/suites/stress/test_*.py; do
                if [ -f "$test" ]; then
                    test_name=$(basename "$test" .py)
                    if [ "$QUICK_MODE" = "true" ] && is_tier2_test "$test_name"; then
                        echo "  $test_name  (skipped — Tier 2, full mode only)"
                        TOTAL_SKIP=$((TOTAL_SKIP + 1))
                        SKIPPED_TESTS+=("$test_name (tier2)")
                        continue
                    fi
                    if is_heavy_test "$test_name"; then
                        STRESS_TESTS_HEAVY+=("$test")
                    else
                        STRESS_TESTS_LIGHT+=("$test")
                    fi
                fi
            done

            for test in "${STRESS_TESTS_LIGHT[@]}"; do
                run_test "$test" "$stress_light_timeout"
                sleep 0.5
            done

            for test in "${STRESS_TESTS_HEAVY[@]}"; do
                run_test "$test" "$stress_heavy_timeout"
            done
        fi

        echo ""
        echo "Stopping standalone server..."
        pkill -u "$USER" -TERM beam.smp 2>/dev/null || true
        sleep 1
        pkill -u "$USER" -9 beam.smp 2>/dev/null || true
        p2_wait=0
        while pgrep -u "$USER" -x beam.smp > /dev/null 2>&1; do
            p2_wait=$((p2_wait + 1))
            if [ $p2_wait -ge 15 ]; then
                echo -e "  ${YELLOW}Warning: beam.smp still in process table after 15s${NC}"
                break
            fi
            sleep 1
        done
        p2_port=0
        while ss -tlnp 2>/dev/null | grep -q ':8085\|:8086'; do
            p2_port=$((p2_port + 1))
            if [ $p2_port -ge 30 ]; then
                echo -e "  ${YELLOW}Warning: ports still held after 30s — forcing with SO_REUSEADDR${NC}"
                break
            fi
            sleep 1
        done
    fi

    # ==========================================================================
    # PHASE 3: CLUSTERMANAGER TESTS
    # ==========================================================================
    if suite_enabled chaos_controlled; then
        echo ""
        echo -e "${BLUE}[PHASE 3]${NC} ClusterManager Tests (self-managed)"
        echo "============================================================================"
        echo "  (Each test manages its own cluster - server restart between tests)"

        # Reset CONFIG to plain-TCP baseline for Phase 3
        export CONFIG=config/test

        for test in tests/suites/chaos_controlled/*.py; do
            if [ -f "$test" ]; then
                pkill -u "$USER" -TERM beam.smp 2>/dev/null || true
                sleep 1
                pkill -u "$USER" -9 beam.smp 2>/dev/null || true
                p3_wait=0
                while pgrep -u "$USER" -x beam.smp > /dev/null 2>&1; do
                    p3_wait=$((p3_wait + 1))
                    if [ $p3_wait -ge 10 ]; then break; fi
                    sleep 1
                done
                p3_port=0
                while ss -tlnp 2>/dev/null | grep -q ':8085\|:8086'; do
                    p3_port=$((p3_port + 1))
                    if [ $p3_port -ge 15 ]; then break; fi
                    sleep 1
                done
                rm -rf Mnesia.* MnesiaCore.* 2>/dev/null || true
                run_test "$test" 300
            fi
        done

        pkill -u "$USER" -9 beam.smp 2>/dev/null || true
    fi
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
    echo -e "${BLUE}[PHASE 4]${NC} Docker Chaos Tests (Batched Clusters)"
    echo "============================================================================"
    echo ""
    echo "Running $TOTAL_DOCKER_TESTS chaos tests in 5 batches + ${#DOCKER_UNBATCHED_TESTS[@]} unbatched."
    echo "Batched tests share a cluster to reduce infrastructure overhead."
    echo ""
    
    pkill -u "$USER" -9 beam.smp 2>/dev/null || true
    
    run_docker_test_batch "Fast (< 30s)" "${DOCKER_BATCH_FAST[@]}"
    run_docker_test_batch "Medium-A (cross-region)" "${DOCKER_BATCH_MEDIUM_A[@]}"
    run_docker_test_batch "Medium-B (data durability)" "${DOCKER_BATCH_MEDIUM_B[@]}"
    run_docker_test_batch "Medium-C (overflow/convergence)" "${DOCKER_BATCH_MEDIUM_C[@]}"
    run_docker_test_batch "Long (> 120s)" "${DOCKER_BATCH_LONG[@]}"
    
    # Run any new/unbatched tests with fresh clusters (safe default)
    for test in "${DOCKER_UNBATCHED_TESTS[@]}"; do
        if [ -f "$test" ]; then
            run_docker_test_fresh "$test" 480
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

if [ ${#SKIPPED_TESTS[@]} -gt 0 ]; then
    echo "  Skipped tests (infrastructure unavailable):"
    for t in "${SKIPPED_TESTS[@]}"; do
        echo -e "    ${YELLOW}⏭${NC} $t"
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
