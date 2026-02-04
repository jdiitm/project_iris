#!/bin/bash
# Run only the previously failing tests to validate fixes quickly

set -e
cd "$(dirname "$0")/.."

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${YELLOW}=== Running Previously Failing Tests ===${NC}"

# Cleanup
echo "Cleaning up..."
pkill -9 beam.smp 2>/dev/null || true
pkill -9 epmd 2>/dev/null || true
sleep 2
rm -rf Mnesia.* MnesiaCore.* data/ core.log edge1.log 2>/dev/null || true

# Set ulimit
ulimit -n 65536 2>/dev/null || ulimit -n 4096 || true
echo "ulimit -n: $(ulimit -n)"

# Start server WITH TLS (tests expect TLS on port 8085)
echo "Starting server with TLS enabled..."
CONFIG=config/test_tls make start &
sleep 10

# Check server
if ! pgrep -f beam.smp > /dev/null; then
    echo -e "${RED}Server failed to start!${NC}"
    exit 1
fi
echo -e "${GREEN}Server running${NC}"

# Track results
PASSED=0
FAILED=0
RESULTS=""

run_test() {
    local name="$1"
    local test="$2"
    local timeout="${3:-120}"
    
    echo ""
    echo -e "${YELLOW}--- $name ---${NC}"
    
    if timeout "$timeout" python3 "$test" 2>&1; then
        echo -e "${GREEN}PASS: $name${NC}"
        PASSED=$((PASSED + 1))
        RESULTS="$RESULTS\n  ${GREEN}[PASS]${NC} $name"
    else
        echo -e "${RED}FAIL: $name${NC}"
        FAILED=$((FAILED + 1))
        RESULTS="$RESULTS\n  ${RED}[FAIL]${NC} $name"
    fi
}

# Run the previously failing tests
run_test "test_edge_core_contract" "tests/suites/contract/test_edge_core_contract.py" 60
run_test "benchmark_e2ee_latency" "tests/suites/performance_light/benchmark_e2ee_latency.py" 120
run_test "test_clock_skew" "tests/suites/resilience/test_clock_skew.py" 180

# Soak test with short duration
export SOAK_DURATION_HOURS=0.02  # ~1 minute
export SOAK_SAMPLE_INTERVAL=15
run_test "test_soak_memory (quick)" "tests/suites/stress/test_soak_memory.py" 180
unset SOAK_DURATION_HOURS SOAK_SAMPLE_INTERVAL

# Summary
echo ""
echo "=============================================="
echo -e "SUMMARY: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}"
echo "=============================================="
echo -e "$RESULTS"
echo ""

# Cleanup
pkill -9 beam.smp 2>/dev/null || true

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All previously failing tests now PASS!${NC}"
    exit 0
else
    echo -e "${RED}$FAILED test(s) still failing${NC}"
    exit 1
fi
