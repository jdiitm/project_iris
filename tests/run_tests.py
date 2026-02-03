#!/usr/bin/env python3
"""
Project Iris - Unified Test Runner

A streamlined test runner following the proven structure from run_all_tests.sh.
Tests are organized into phases based on infrastructure requirements.

Phases:
  Phase 1: Unit tests (no server needed)
  Phase 2: Standalone server tests (pre-started TLS server)
  Phase 3: ClusterManager tests (self-managed cluster per test)
  Phase 4: Docker chaos tests (Docker global cluster)

Usage:
    ./tests/run_tests.py --all                    # Run all tests
    ./tests/run_tests.py --all --skip-docker      # Skip Docker tests (faster)
    ./tests/run_tests.py --suite unit             # Run specific suite
    ./tests/run_tests.py --tier 0                 # CI Tier 0 (unit + integration)
    ./tests/run_tests.py --list                   # List all available tests
"""

import os
os.environ['PYTHONUNBUFFERED'] = '1'

import argparse
import subprocess
import sys
import time
import socket
import shutil
from datetime import datetime
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Optional

# ============================================================================
# Configuration
# ============================================================================

TESTS_ROOT = Path(__file__).parent.absolute()
PROJECT_ROOT = TESTS_ROOT.parent
SUITES_DIR = TESTS_ROOT / "suites"
ARTIFACTS_DIR = TESTS_ROOT / "artifacts"

# Test timeouts (seconds)
TIMEOUTS = {
    "unit": 60,
    "integration": 180,
    "e2e": 180,
    "contract": 180,
    "compatibility": 180,
    "security": 180,
    "resilience": 300,
    "performance_light": 300,
    "stress": 300,
    "chaos_controlled": 300,
    "chaos_dist": 300,
}

# CI Tiers
TIER_0_SUITES = ["unit", "integration"]
TIER_1_SUITES = ["e2e", "contract", "compatibility", "security", "resilience"]
TIER_2_SUITES = ["performance_light", "stress", "chaos_controlled"]

# Tests that use "with ClusterManager(...)" - they manage their own cluster
CLUSTER_MANAGER_TESTS = [
    # Resilience
    "test_resilience",
    # Performance
    "benchmark_memory",
    "measure_dials",
    "test_cpu_utilization",
    # Stress
    "stress_geo_scale",
    "stress_global_fan_in",
    "stress_hotspot",
    "stress_presence",
    "test_backpressure_collapse",
    "test_churn",
    "test_connection_scale",
    "test_fanout",
    "test_hot_shard",
    "test_limits",
    # Chaos controlled
    "chaos_combined",
    "ultimate_chaos",
]

# Standalone stress tests (need pre-started server)
STANDALONE_STRESS = [
    "stress_offline_delete",
    "test_flow_controller_scale",
    "test_group_fanout",
    "test_soak_memory",
]

# Standalone performance tests (need pre-started server)
STANDALONE_PERF = [
    "benchmark_e2ee_latency",
    "benchmark_throughput",
    "benchmark_unit_cost",
]

# Standalone resilience tests
STANDALONE_RESILIENCE = [
    "test_clock_skew",
    "test_hard_kill",
]

# ============================================================================
# Colors
# ============================================================================

class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    BOLD = '\033[1m'
    END = '\033[0m'

def colored(text: str, color: str) -> str:
    if sys.stdout.isatty():
        return f"{color}{text}{Colors.END}"
    return text

def log(msg: str):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)

def log_header(msg: str):
    print(f"\n{'='*60}\n{msg}\n{'='*60}", flush=True)

def log_pass(msg: str):
    print(colored(f"  {msg}", Colors.GREEN), flush=True)

def log_fail(msg: str):
    print(colored(f"  {msg}", Colors.RED), flush=True)

def log_warn(msg: str):
    print(colored(f"  {msg}", Colors.YELLOW), flush=True)

def log_info(msg: str):
    print(f"  {msg}", flush=True)

# ============================================================================
# Data Classes
# ============================================================================

@dataclass
class TestResult:
    name: str
    suite: str
    passed: bool
    duration: float
    skipped: bool = False
    timeout: bool = False

@dataclass
class RunSummary:
    total_pass: int = 0
    total_fail: int = 0
    total_skip: int = 0
    failed_tests: List[str] = field(default_factory=list)
    results: List[TestResult] = field(default_factory=list)

# ============================================================================
# Cleanup Functions
# ============================================================================

def cleanup():
    """Stop all processes and clean up state."""
    log("Cleanup: stopping all processes...")
    
    # Kill Erlang processes
    subprocess.run(["pkill", "-9", "beam.smp"], capture_output=True)
    subprocess.run(["pkill", "-9", "epmd"], capture_output=True)
    
    # Stop Docker cluster
    docker_compose = PROJECT_ROOT / "docker" / "global-cluster" / "docker-compose.yml"
    if docker_compose.exists():
        subprocess.run(
            ["docker", "compose", "-f", str(docker_compose), "down", "-v"],
            capture_output=True, timeout=60
        )
    
    # Clean Mnesia directories
    for pattern in ["Mnesia.*"]:
        for path in PROJECT_ROOT.glob(pattern):
            shutil.rmtree(path, ignore_errors=True)
        for path in Path("/tmp").glob(pattern):
            shutil.rmtree(path, ignore_errors=True)
    
    # Clean log files
    for log_file in ["core.log", "edge1.log", "edge2.log", "erl_crash.dump"]:
        (PROJECT_ROOT / log_file).unlink(missing_ok=True)
    
    time.sleep(2)

def wait_for_port(port: int, timeout: int = 30) -> bool:
    """Wait for a port to be available."""
    start = time.time()
    while time.time() - start < timeout:
        try:
            with socket.create_connection(("localhost", port), timeout=1):
                return True
        except (OSError, ConnectionRefusedError):
            time.sleep(1)
    return False

# ============================================================================
# Server Management
# ============================================================================

def start_server(log_dir: Path) -> bool:
    """Start local TLS server."""
    log("Starting local TLS server...")
    
    log_file = log_dir / "server_start.log"
    with open(log_file, "w") as f:
        subprocess.run(
            ["make", "start"],
            cwd=str(PROJECT_ROOT),
            env={**os.environ, "CONFIG": "config/test_tls"},
            stdout=f, stderr=f, timeout=60
        )
    
    time.sleep(5)
    
    # Wait for server
    for attempt in range(6):
        if wait_for_port(8085, timeout=5):
            log_pass("Server running on port 8085")
            return True
        log_info(f"Waiting for server (attempt {attempt+1}/6)...")
    
    log_fail("Server failed to start")
    return False

def stop_server():
    """Stop local server."""
    subprocess.run(["pkill", "-9", "beam.smp"], capture_output=True)
    time.sleep(2)

# ============================================================================
# Docker Cluster Management
# ============================================================================

def start_docker_cluster(log_dir: Path) -> bool:
    """Start Docker global cluster."""
    log("Starting Docker global cluster...")
    
    docker_dir = PROJECT_ROOT / "docker" / "global-cluster"
    compose_file = docker_dir / "docker-compose.yml"
    
    if not compose_file.exists():
        log_warn("Docker compose file not found")
        return False
    
    # Stop any existing cluster
    subprocess.run(
        ["docker", "compose", "-f", str(compose_file), "down", "-v"],
        capture_output=True, timeout=60
    )
    
    # Start fresh cluster
    log_file = log_dir / "docker_start.log"
    with open(log_file, "w") as f:
        result = subprocess.run(
            ["docker", "compose", "-f", str(compose_file), "up", "-d"],
            cwd=str(docker_dir),
            stdout=f, stderr=f, timeout=180
        )
    
    if result.returncode != 0:
        log_fail("Docker cluster failed to start")
        return False
    
    log_info("Waiting for cluster to stabilize (60s)...")
    time.sleep(60)
    
    log_pass("Docker cluster started")
    return True

def stop_docker_cluster(log_dir: Path):
    """Stop Docker global cluster."""
    log("Stopping Docker cluster...")
    
    docker_dir = PROJECT_ROOT / "docker" / "global-cluster"
    compose_file = docker_dir / "docker-compose.yml"
    
    log_file = log_dir / "docker_stop.log"
    with open(log_file, "w") as f:
        subprocess.run(
            ["docker", "compose", "-f", str(compose_file), "down", "-v"],
            cwd=str(docker_dir),
            stdout=f, stderr=f, timeout=60
        )

# ============================================================================
# Test Discovery
# ============================================================================

def discover_tests(suite: str) -> List[Dict]:
    """Discover all tests in a suite."""
    tests = []
    suite_dir = SUITES_DIR / suite
    
    if not suite_dir.exists():
        return tests
    
    for test_file in sorted(suite_dir.glob("*.py")):
        # Skip __init__.py, utils.py, helpers.py etc
        if test_file.name.startswith("__"):
            continue
        if test_file.stem in ["utils", "helpers", "conftest", "fixtures"]:
            continue
        tests.append({
            "name": test_file.stem,
            "suite": suite,
            "path": str(test_file),
        })
    
    return tests

def list_all_tests() -> Dict[str, List[Dict]]:
    """List all available tests grouped by suite."""
    all_tests = {}
    for suite_dir in sorted(SUITES_DIR.iterdir()):
        if suite_dir.is_dir():
            tests = discover_tests(suite_dir.name)
            if tests:
                all_tests[suite_dir.name] = tests
    return all_tests

# ============================================================================
# Test Execution
# ============================================================================

def run_test(test: Dict, log_dir: Path, timeout: int = 180) -> TestResult:
    """Run a single test."""
    name = test["name"]
    suite = test["suite"]
    path = test["path"]
    
    start = time.time()
    result = TestResult(name=name, suite=suite, passed=False, duration=0)
    
    # Print test name (no newline yet)
    print(f"  {name:<50}", end="", flush=True)
    
    log_file = log_dir / f"{name}.log"
    
    try:
        env = os.environ.copy()
        env["PYTHONUNBUFFERED"] = "1"
        env["IRIS_TEST_RUNNER"] = "1"
        
        with open(log_file, "w") as f:
            proc = subprocess.run(
                ["python3", "-u", path],
                cwd=str(PROJECT_ROOT),
                stdout=f, stderr=subprocess.STDOUT,
                timeout=timeout,
                env=env
            )
        
        if proc.returncode == 0:
            result.passed = True
            print(colored("PASS", Colors.GREEN), flush=True)
        elif proc.returncode == 2:
            result.passed = True
            result.skipped = True
            print(colored("SKIP", Colors.YELLOW), flush=True)
        else:
            print(colored("FAIL", Colors.RED), flush=True)
            
    except subprocess.TimeoutExpired:
        result.timeout = True
        print(colored("TIMEOUT", Colors.RED), flush=True)
    except Exception as e:
        print(colored(f"ERROR: {e}", Colors.RED), flush=True)
    
    result.duration = time.time() - start
    return result

def run_tests_batch(tests: List[Dict], log_dir: Path, summary: RunSummary, 
                    timeout: int = 180, cleanup_between: bool = False):
    """Run a batch of tests."""
    for test in tests:
        if cleanup_between:
            stop_server()
            time.sleep(2)
        
        result = run_test(test, log_dir, timeout)
        summary.results.append(result)
        
        if result.skipped:
            summary.total_skip += 1
        elif result.passed:
            summary.total_pass += 1
        else:
            summary.total_fail += 1
            summary.failed_tests.append(f"{test['name']}" + (" (timeout)" if result.timeout else ""))

# ============================================================================
# Phase Execution
# ============================================================================

def run_phase_unit(log_dir: Path, summary: RunSummary):
    """Phase 1: Unit tests (no server needed)."""
    log_header("Phase 1: Unit Tests")
    
    # Compile first
    log("Compiling...")
    result = subprocess.run(
        ["make", "all"],
        cwd=str(PROJECT_ROOT),
        capture_output=True, timeout=300
    )
    if result.returncode != 0:
        log_fail("Compilation failed!")
        return
    log_pass("Compilation successful")
    
    tests = discover_tests("unit")
    if tests:
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["unit"])

def run_phase_standalone(log_dir: Path, summary: RunSummary, suites: List[str]):
    """Phase 2: Standalone server tests."""
    log_header("Phase 2: Standalone Server Tests")
    
    if not start_server(log_dir):
        log_fail("Cannot run standalone tests without server")
        return
    
    # Integration tests
    if "integration" in suites:
        print("\n--- Integration ---", flush=True)
        tests = discover_tests("integration")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["integration"])
    
    # E2E tests
    if "e2e" in suites:
        print("\n--- E2E ---", flush=True)
        tests = discover_tests("e2e")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["e2e"])
    
    # Contract tests
    if "contract" in suites:
        print("\n--- Contract ---", flush=True)
        tests = discover_tests("contract")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["contract"])
    
    # Compatibility tests
    if "compatibility" in suites:
        print("\n--- Compatibility ---", flush=True)
        tests = discover_tests("compatibility")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["compatibility"])
    
    # Security tests
    if "security" in suites:
        print("\n--- Security ---", flush=True)
        tests = discover_tests("security")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["security"])
    
    # Standalone resilience tests
    if "resilience" in suites:
        print("\n--- Resilience (standalone) ---", flush=True)
        tests = [t for t in discover_tests("resilience") if t["name"] in STANDALONE_RESILIENCE]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["resilience"])
    
    # Standalone performance tests
    if "performance_light" in suites:
        print("\n--- Performance (standalone) ---", flush=True)
        tests = [t for t in discover_tests("performance_light") if t["name"] in STANDALONE_PERF]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["performance_light"])
    
    # Standalone stress tests
    if "stress" in suites:
        print("\n--- Stress (standalone) ---", flush=True)
        tests = [t for t in discover_tests("stress") if t["name"] in STANDALONE_STRESS]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["stress"])
    
    stop_server()

def run_phase_cluster_manager(log_dir: Path, summary: RunSummary, suites: List[str]):
    """Phase 3: ClusterManager tests (self-managed)."""
    log_header("Phase 3: ClusterManager Tests")
    
    # Resilience tests using ClusterManager
    if "resilience" in suites:
        print("\n--- Resilience (ClusterManager) ---", flush=True)
        tests = [t for t in discover_tests("resilience") if t["name"] not in STANDALONE_RESILIENCE]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["resilience"], cleanup_between=True)
    
    # Performance tests using ClusterManager
    if "performance_light" in suites:
        print("\n--- Performance (ClusterManager) ---", flush=True)
        tests = [t for t in discover_tests("performance_light") if t["name"] not in STANDALONE_PERF]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["performance_light"], cleanup_between=True)
    
    # Stress tests using ClusterManager
    if "stress" in suites:
        print("\n--- Stress (ClusterManager) ---", flush=True)
        tests = [t for t in discover_tests("stress") if t["name"] not in STANDALONE_STRESS]
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["stress"], cleanup_between=True)
    
    # Chaos controlled tests
    if "chaos_controlled" in suites:
        print("\n--- Chaos Controlled ---", flush=True)
        tests = discover_tests("chaos_controlled")
        run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["chaos_controlled"], cleanup_between=True)
    
    stop_server()

def run_phase_docker(log_dir: Path, summary: RunSummary):
    """Phase 4: Docker chaos tests."""
    log_header("Phase 4: Docker Chaos Tests")
    
    stop_server()
    
    if not start_docker_cluster(log_dir):
        log_warn("Docker cluster not available - skipping chaos_dist tests")
        return
    
    tests = discover_tests("chaos_dist")
    run_tests_batch(tests, log_dir, summary, timeout=TIMEOUTS["chaos_dist"])
    
    stop_docker_cluster(log_dir)

# ============================================================================
# Main
# ============================================================================

def print_summary(summary: RunSummary, log_dir: Path):
    """Print final summary."""
    print(f"\n{'='*60}", flush=True)
    print("                    FINAL RESULTS", flush=True)
    print(f"{'='*60}", flush=True)
    print(colored(f"  PASSED:  {summary.total_pass}", Colors.GREEN), flush=True)
    print(colored(f"  FAILED:  {summary.total_fail}", Colors.RED), flush=True)
    print(colored(f"  SKIPPED: {summary.total_skip}", Colors.YELLOW), flush=True)
    print(f"\n  TOTAL:   {summary.total_pass + summary.total_fail + summary.total_skip} tests", flush=True)
    
    if summary.failed_tests:
        print(f"\n  Failed tests:", flush=True)
        for t in summary.failed_tests:
            print(colored(f"    ✗ {t}", Colors.RED), flush=True)
    
    print(f"\n  Log directory: {log_dir}", flush=True)
    print(f"{'='*60}", flush=True)
    
    if summary.total_fail == 0:
        print(colored("  ✅ ALL TESTS PASSED", Colors.GREEN), flush=True)
    else:
        print(colored("  ❌ SOME TESTS FAILED", Colors.RED), flush=True)

def main():
    parser = argparse.ArgumentParser(description="Project Iris Test Runner")
    parser.add_argument("--suite", type=str, help="Run specific suite")
    parser.add_argument("--tier", type=int, choices=[0, 1, 2], help="Run CI tier")
    parser.add_argument("--all", action="store_true", help="Run all tests")
    parser.add_argument("--list", action="store_true", help="List all tests")
    parser.add_argument("--skip-docker", action="store_true", help="Skip Docker tests")
    parser.add_argument("--with-cluster", action="store_true", help="Include Docker cluster tests")
    parser.add_argument("--no-cluster", action="store_true", help="Don't manage cluster")
    parser.add_argument("--nuke", action="store_true", help="Kill all processes and exit")
    
    args = parser.parse_args()
    
    # Handle --nuke
    if args.nuke:
        cleanup()
        print("All processes killed.", flush=True)
        return 0
    
    # Handle --list
    if args.list:
        all_tests = list_all_tests()
        print("\nAvailable Tests:", flush=True)
        print("=" * 60, flush=True)
        total = 0
        for suite, tests in sorted(all_tests.items()):
            print(f"\n{Colors.BOLD}{suite}{Colors.END} ({len(tests)} tests)", flush=True)
            for test in tests:
                print(f"  - {test['name']}", flush=True)
            total += len(tests)
        print(f"\n{'='*60}", flush=True)
        print(f"TOTAL: {total} tests", flush=True)
        return 0
    
    # Determine suites to run
    suites = []
    if args.suite:
        suites = [args.suite]
    elif args.tier == 0:
        suites = TIER_0_SUITES
    elif args.tier == 1:
        suites = TIER_0_SUITES + TIER_1_SUITES
    elif args.tier == 2:
        suites = TIER_0_SUITES + TIER_1_SUITES + TIER_2_SUITES
    elif args.all:
        all_tests = list_all_tests()
        suites = list(all_tests.keys())
    else:
        parser.print_help()
        return 1
    
    # Create log directory
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_dir = ARTIFACTS_DIR / f"run_{timestamp}"
    log_dir.mkdir(parents=True, exist_ok=True)
    
    # Print header
    print("=" * 60, flush=True)
    print("                 IRIS TEST RUNNER", flush=True)
    print("=" * 60, flush=True)
    print(f"Log directory: {log_dir}", flush=True)
    print(f"Suites: {', '.join(suites)}", flush=True)
    print()
    
    summary = RunSummary()
    
    # Initial cleanup
    cleanup()
    
    # Phase 1: Unit tests
    if "unit" in suites:
        run_phase_unit(log_dir, summary)
    
    # Phase 2: Standalone server tests
    standalone_suites = [s for s in suites if s in 
                        ["integration", "e2e", "contract", "compatibility", "security",
                         "resilience", "performance_light", "stress"]]
    if standalone_suites:
        run_phase_standalone(log_dir, summary, standalone_suites)
    
    # Phase 3: ClusterManager tests
    cm_suites = [s for s in suites if s in 
                 ["resilience", "performance_light", "stress", "chaos_controlled"]]
    if cm_suites:
        run_phase_cluster_manager(log_dir, summary, cm_suites)
    
    # Phase 4: Docker tests
    if "chaos_dist" in suites and not args.skip_docker:
        run_phase_docker(log_dir, summary)
    elif "chaos_dist" in suites:
        print(colored("\n[Phase 4] Docker tests SKIPPED (--skip-docker)", Colors.YELLOW), flush=True)
    
    # Final cleanup
    cleanup()
    
    # Print summary
    print_summary(summary, log_dir)
    
    return 1 if summary.total_fail > 0 else 0

if __name__ == "__main__":
    sys.exit(main())
