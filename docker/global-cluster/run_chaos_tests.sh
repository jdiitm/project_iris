#!/bin/bash
# =============================================================================
# Chaos Test Runner with Proper Logging
# =============================================================================
# This script runs chaos/distributed tests with a fresh cluster per test,
# using cluster.sh for consistent cluster management.
# =============================================================================

# Don't use set -e as it causes premature exit on expected failures
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_DIR="$PROJECT_ROOT/tests/artifacts/logs"
LOG_FILE="$LOG_DIR/chaos_test_results_$(date +%Y%m%d_%H%M%S).log"

# Colors for terminal output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Test list - all tests verified to exist
TESTS=(
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
)

# Initialize counters
PASS=0
FAIL=0
CLUSTER_FAIL=0
declare -a PASSED_TESTS=()
declare -a FAILED_TESTS=()
declare -a CLUSTER_FAILED_TESTS=()

mkdir -p "$LOG_DIR"

log() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo -e "$msg" | tee -a "$LOG_FILE"
}

log_header() {
    echo "============================================================" | tee -a "$LOG_FILE"
    log "$1"
    echo "============================================================" | tee -a "$LOG_FILE"
}

# =============================================================================
# Cluster Management - Delegate to proven cluster.sh script
# =============================================================================

cluster_down() {
    log "Stopping cluster (via cluster.sh down)..."
    cd "$SCRIPT_DIR"
    
    # Use the PROVEN cluster.sh script directly
    bash "$SCRIPT_DIR/cluster.sh" down 2>&1 | tee -a "$LOG_FILE" || true
    
    # Brief wait for Docker to settle
    sleep 3
    
    log "Cluster stopped"
}

cluster_up() {
    log "Starting cluster (via cluster.sh up)..."
    cd "$SCRIPT_DIR"
    
    # Use the PROVEN cluster.sh script directly
    if bash "$SCRIPT_DIR/cluster.sh" up 2>&1 | tee -a "$LOG_FILE"; then
        log "Cluster initialization SUCCESS"
        return 0
    else
        log "Cluster initialization FAILED"
        return 1
    fi
}

run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .py)
    
    log_header "Running: $test_name"
    log "Test file: $test_file"
    
    cd "$PROJECT_ROOT"
    
    # Run the test with timeout (5 minutes max)
    # These are standalone Python scripts, not pytest tests
    # Use PIPESTATUS to capture the actual exit code, not tee
    local start_time=$(date +%s)
    timeout 300 python3 "$test_file" 2>&1 | tee -a "$LOG_FILE"
    local exit_code=${PIPESTATUS[0]}
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $exit_code -eq 0 ]; then
        log "RESULT: ${GREEN}PASS${NC} ($test_name) - ${duration}s"
        return 0
    else
        log "RESULT: ${RED}FAIL${NC} ($test_name) - exit code $exit_code - ${duration}s"
        return 1
    fi
}

# =============================================================================
# Main
# =============================================================================

log_header "Chaos Test Suite - Started"
log "Log file: $LOG_FILE"
log "Total tests: ${#TESTS[@]}"

# Initial cleanup
cluster_down

for test in "${TESTS[@]}"; do
    test_name=$(basename "$test" .py)
    
    log ""
    log "=========================================="
    log "TEST #$((PASS + FAIL + CLUSTER_FAIL + 1)): $test_name"
    log "=========================================="
    
    # Clean and start fresh cluster for each test
    cluster_down
    
    if cluster_up; then
        # Cluster is ready, run the test
        if run_test "$test"; then
            PASS=$((PASS + 1))
            PASSED_TESTS+=("$test_name")
        else
            FAIL=$((FAIL + 1))
            FAILED_TESTS+=("$test_name")
        fi
    else
        # Cluster failed to initialize
        log "RESULT: ${YELLOW}CLUSTER_INIT_FAILED${NC} ($test_name)"
        CLUSTER_FAIL=$((CLUSTER_FAIL + 1))
        CLUSTER_FAILED_TESTS+=("$test_name")
    fi
    
    # Brief pause between tests
    sleep 5
done

# Final cleanup
cluster_down

# =============================================================================
# Summary
# =============================================================================

log_header "FINAL RESULTS"
log "Total: $((PASS + FAIL + CLUSTER_FAIL)) tests"
log "Passed: ${GREEN}$PASS${NC}"
log "Failed: ${RED}$FAIL${NC}"
log "Cluster Init Failed: ${YELLOW}$CLUSTER_FAIL${NC}"

log ""
log "Passed tests:"
for t in "${PASSED_TESTS[@]}"; do
    log "  ${GREEN}[PASS]${NC} $t"
done

log ""
log "Failed tests:"
for t in "${FAILED_TESTS[@]}"; do
    log "  ${RED}[FAIL]${NC} $t"
done

log ""
log "Cluster init failed tests:"
for t in "${CLUSTER_FAILED_TESTS[@]}"; do
    log "  ${YELLOW}[CLUSTER_FAIL]${NC} $t"
done

log ""
log "Full log: $LOG_FILE"
log_header "Chaos Test Suite - Complete"

# Exit with appropriate code
if [ $FAIL -gt 0 ] || [ $CLUSTER_FAIL -gt 0 ]; then
    exit 1
fi
exit 0
