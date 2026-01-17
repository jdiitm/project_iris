#!/usr/bin/env python3
"""
Project Iris - Unified Test Runner

A lightweight, resource-aware test orchestrator designed for constrained infrastructure.
No heavy dependencies - uses only Python stdlib plus PyYAML.

Usage:
    ./tests/run_tests.py --suite unit              # Run unit tests only
    ./tests/run_tests.py --suite integration       # Run integration tests
    ./tests/run_tests.py --tier 0                  # CI Tier 0 (required)
    ./tests/run_tests.py --tier 1                  # CI Tier 1 (optional/nightly)
    ./tests/run_tests.py --list                    # List all available tests
    ./tests/run_tests.py --all                     # Run all tests sequentially
"""

import argparse
import json
import os
import subprocess
import sys
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Optional, Any
import shutil

# ============================================================================
# Configuration
# ============================================================================

TESTS_ROOT = Path(__file__).parent.absolute()
PROJECT_ROOT = TESTS_ROOT.parent
ARTIFACTS_DIR = TESTS_ROOT / "artifacts"
SUITES_DIR = TESTS_ROOT / "suites"

# Test tier definitions
TIER_0_SUITES = ["unit", "integration"]  # Required on every merge
TIER_1_SUITES = ["resilience", "performance_light", "chaos_controlled"]  # Nightly/manual

# ============================================================================
# Data Classes
# ============================================================================

@dataclass
class TestResult:
    name: str
    suite: str
    passed: bool
    duration_seconds: float
    output: str = ""
    error: str = ""
    artifacts: List[str] = field(default_factory=list)

@dataclass
class SuiteResult:
    name: str
    tests_run: int
    tests_passed: int
    tests_failed: int
    duration_seconds: float
    results: List[TestResult] = field(default_factory=list)

@dataclass 
class ResourceSnapshot:
    timestamp: str
    cpu_percent: float = 0.0
    memory_mb: float = 0.0
    disk_free_gb: float = 0.0

# ============================================================================
# Logging & Output
# ============================================================================

class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    END = '\033[0m'

def log(msg: str, color: str = ""):
    timestamp = datetime.now().strftime("%H:%M:%S")
    if color and sys.stdout.isatty():
        print(f"{color}[{timestamp}] {msg}{Colors.END}")
    else:
        print(f"[{timestamp}] {msg}")

def log_header(msg: str):
    log(f"\n{'='*60}\n {msg}\n{'='*60}", Colors.HEADER)

def log_pass(msg: str):
    log(f"✓ {msg}", Colors.GREEN)

def log_fail(msg: str):
    log(f"✗ {msg}", Colors.RED)

def log_info(msg: str):
    log(msg, Colors.CYAN)

def log_warn(msg: str):
    log(f"⚠ {msg}", Colors.YELLOW)

# ============================================================================
# Resource Monitoring
# ============================================================================

def get_resource_snapshot() -> ResourceSnapshot:
    """Capture current system resource usage."""
    snapshot = ResourceSnapshot(timestamp=datetime.now().isoformat())
    
    try:
        # CPU - use ps on macOS/Linux
        result = subprocess.run(
            ["ps", "-A", "-o", "%cpu"],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            cpu_values = [float(x) for x in result.stdout.strip().split('\n')[1:] if x.strip()]
            snapshot.cpu_percent = sum(cpu_values)
    except Exception:
        pass
    
    try:
        # Memory - platform specific
        if sys.platform == "darwin":
            result = subprocess.run(
                ["vm_stat"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                lines = result.stdout.split('\n')
                page_size = 4096  # Default macOS page size
                free_pages = 0
                for line in lines:
                    if "Pages free" in line:
                        free_pages = int(line.split(':')[1].strip().rstrip('.'))
                        break
                snapshot.memory_mb = (free_pages * page_size) / (1024 * 1024)
        else:
            with open('/proc/meminfo', 'r') as f:
                for line in f:
                    if line.startswith('MemAvailable:'):
                        snapshot.memory_mb = int(line.split()[1]) / 1024
                        break
    except Exception:
        pass
    
    try:
        # Disk
        statvfs = os.statvfs(str(PROJECT_ROOT))
        snapshot.disk_free_gb = (statvfs.f_frsize * statvfs.f_bavail) / (1024**3)
    except Exception:
        pass
    
    return snapshot

# ============================================================================
# Test Discovery
# ============================================================================

def discover_tests(suite: str) -> List[Dict[str, Any]]:
    """Discover all tests in a suite directory."""
    tests = []
    suite_dir = SUITES_DIR / suite
    
    if not suite_dir.exists():
        return tests
    
    # Track already-added files to avoid duplicates
    added_files = set()
    
    # Python tests - pick up ALL .py files (they are all tests)
    for test_file in suite_dir.glob("*.py"):
        if test_file.name.startswith("__"):  # Skip __init__.py etc
            continue
        if test_file in added_files:
            continue
        added_files.add(test_file)
        tests.append({
            "name": test_file.stem,
            "suite": suite,
            "type": "python",
            "path": str(test_file),
            "command": f"python3 {test_file}"
        })
    
    # Erlang tests (EUnit modules) from suite dir
    for test_file in suite_dir.glob("*_tests.erl"):
        module = test_file.stem
        tests.append({
            "name": module,
            "suite": suite,
            "type": "erlang",
            "path": str(test_file),
            "command": f"/usr/bin/erl -pa {PROJECT_ROOT}/ebin -pa {suite_dir} -noshell -eval \"eunit:test({module}, []), init:stop().\""
        })
    
    # P0-1 FIX: Also discover EUnit tests from test_utils directory for 'unit' suite
    if suite == "unit":
        test_utils_dir = PROJECT_ROOT / "test_utils"
        if test_utils_dir.exists():
            for test_file in test_utils_dir.glob("*_tests.erl"):
                module = test_file.stem
                # Skip if already in ebin (make test will handle them)
                tests.append({
                    "name": f"test_utils/{module}",
                    "suite": suite,
                    "type": "erlang",
                    "path": str(test_file),
                    "command": f"/usr/bin/erl -pa {PROJECT_ROOT}/ebin -pa {test_utils_dir} -noshell -eval \"eunit:test({module}, []), init:stop().\""
                })
    
    return tests

def list_all_tests() -> Dict[str, List[Dict]]:
    """List all available tests grouped by suite."""
    all_tests = {}
    for suite_dir in SUITES_DIR.iterdir():
        if suite_dir.is_dir():
            tests = discover_tests(suite_dir.name)
            if tests:
                all_tests[suite_dir.name] = tests
    return all_tests

# ============================================================================
# Test Execution
# ============================================================================

def mesh_cluster(suffix: str):
    """Force mesh the cluster nodes."""
    log_info("Meshing cluster nodes...")
    try:
        # Match Makefile's $(shell hostname -s)
        hostname = subprocess.check_output(["hostname", "-s"], text=True).strip()
        core = f"iris_core{suffix}@{hostname}"
        edge = f"iris_edge1{suffix}@{hostname}"
        
        # RPC call to force mesh
        cmd = [
            "erl", "-noshell", "-sname", f"mesher_{int(time.time())}", 
            "-setcookie", "iris_secret",
            "-eval", f"io:format('Ping: ~p~n', [rpc:call('{edge}', net_adm, ping, ['{core}'])]), init:stop()."
        ]
        
        res = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
        if "pong" not in res.stdout:
            log_warn(f"Mesh might have failed: {res.stdout}")
        else:
            log_info("Cluster meshed successfully")
            
    except Exception as e:
        log_warn(f"Mesh error: {e}")

def ensure_cluster_running() -> bool:
    """Ensure the Iris cluster is running."""
    log_info("Ensuring cluster is running...")
    
    try:
        # Get suffix for this run
        suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
        make_args = [f"NODE_SUFFIX={suffix}"] if suffix else []
        
        # Force stop any existing cluster
        stop_cluster()
        time.sleep(2)

        # Build first (can take time on CI)
        log_info("Building project...")
        result = subprocess.run(
            ["make", "all"] + make_args,
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=300  # 5 minutes for compilation
        )
        if result.returncode != 0:
            log_warn(f"Build failed: {result.stderr}")
            return False

        # Try to start cluster (Core)
        result = subprocess.run(
            ["make", "start_core"] + make_args,
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=60
        )
        time.sleep(3)  # Give Mnesia time to initialize
        
        # Start Edge
        result = subprocess.run(
            ["make", "start_edge1"] + make_args,
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=60
        )
        
        # Wait for port 8085 (Edge Node)
        log_info("Waiting for Edge Node (8085)...")
        if not wait_for_port(8085, timeout=30):
            log_warn("Edge node port 8085 did not open in time.")
            return False
            
        # Mesh the cluster
        mesh_cluster(suffix)
            
        return True
    except Exception as e:
        log_warn(f"Could not start cluster: {e}")
        return False

def wait_for_port(port: int, timeout: int = 30) -> bool:
    """Wait for a TCP port to open."""
    import socket
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            with socket.create_connection(("localhost", port), timeout=1):
                return True
        except (OSError, ConnectionRefusedError):
            time.sleep(0.5)
    return False

def stop_cluster():
    """Stop the Iris cluster."""
    log_info("Stopping cluster...")
    try:
        subprocess.run(
            ["make", "stop"],
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            timeout=30
        )
        subprocess.run(
            ["killall", "beam.smp"],
            capture_output=True,
            timeout=10
        )
    except Exception:
        pass

def run_test(test: Dict, run_dir: Path, timeout: int = 120) -> TestResult:
    """Run a single test and return the result."""
    start_time = time.time()
    
    result = TestResult(
        name=test["name"],
        suite=test["suite"],
        passed=False,
        duration_seconds=0.0
    )
    
    # Create test-specific artifact directory
    test_artifact_dir = run_dir / test["suite"] / test["name"]
    test_artifact_dir.mkdir(parents=True, exist_ok=True)
    
    log_file = test_artifact_dir / "output.log"
    
    try:
        log_info(f"Running: {test['name']} ({test['type']})")
        
        # Set up environment
        env = os.environ.copy()
        env["IRIS_TEST_ARTIFACTS"] = str(test_artifact_dir)
        env["IRIS_PROJECT_ROOT"] = str(PROJECT_ROOT)
        # Add project root to PYTHONPATH so tests can import from tests.framework
        existing_pythonpath = env.get("PYTHONPATH", "")
        env["PYTHONPATH"] = f"{PROJECT_ROOT}:{existing_pythonpath}" if existing_pythonpath else str(PROJECT_ROOT)
        
        # Run the test
        proc = subprocess.run(
            test["command"],
            shell=True,
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=timeout,
            env=env
        )
        
        result.output = proc.stdout
        result.error = proc.stderr
        result.passed = proc.returncode == 0
        
        # Write log
        with open(log_file, "w") as f:
            f.write(f"=== STDOUT ===\n{proc.stdout}\n")
            f.write(f"=== STDERR ===\n{proc.stderr}\n")
            f.write(f"=== EXIT CODE: {proc.returncode} ===\n")
        
        result.artifacts.append(str(log_file))
        
    except subprocess.TimeoutExpired:
        result.error = f"Test timed out after {timeout}s"
        result.passed = False
        log_fail(f"TIMEOUT: {test['name']}")
        
    except Exception as e:
        result.error = str(e)
        result.passed = False
        log_fail(f"ERROR: {test['name']} - {e}")
    
    result.duration_seconds = time.time() - start_time
    
    if result.passed:
        log_pass(f"PASS: {test['name']} ({result.duration_seconds:.1f}s)")
    else:
        log_fail(f"FAIL: {test['name']} ({result.duration_seconds:.1f}s)")
    
    return result

def run_suite(suite_name: str, run_dir: Path, require_cluster: bool = True) -> SuiteResult:
    """Run all tests in a suite."""
    log_header(f"Suite: {suite_name}")
    
    start_time = time.time()
    tests = discover_tests(suite_name)
    
    if not tests:
        log_warn(f"No tests found in suite: {suite_name}")
        return SuiteResult(
            name=suite_name,
            tests_run=0,
            tests_passed=0,
            tests_failed=0,
            duration_seconds=0.0
        )
    
    log_info(f"Found {len(tests)} tests")
    
    # Start cluster for integration/stress tests
    if require_cluster and suite_name not in ["unit"]:
        ensure_cluster_running()
    
    # Suite-specific timeouts (seconds)
    suite_timeouts = {
        "integration": 120,
        "resilience": 600,       # 10 min for resilience
        "chaos_controlled": 600, # 10 min for chaos
        "stress": 600,           # 10 min for stress
        "performance_light": 300,# 5 min for performance
        "unit": 60,
    }
    timeout = suite_timeouts.get(suite_name, 300)  # Default 5 min

    
    results = []
    for test in tests:
        result = run_test(test, run_dir, timeout=timeout)
        results.append(result)
    
    duration = time.time() - start_time
    passed = sum(1 for r in results if r.passed)
    failed = len(results) - passed
    
    return SuiteResult(
        name=suite_name,
        tests_run=len(results),
        tests_passed=passed,
        tests_failed=failed,
        duration_seconds=duration,
        results=results
    )

# ============================================================================
# Artifact Management
# ============================================================================

def create_run_directory() -> Path:
    """Create a new run directory with timestamp."""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    run_dir = ARTIFACTS_DIR / "runs" / timestamp
    run_dir.mkdir(parents=True, exist_ok=True)
    return run_dir

def write_summary(run_dir: Path, suite_results: List[SuiteResult], 
                  start_snapshot: ResourceSnapshot, end_snapshot: ResourceSnapshot):
    """Write a JSON summary of the test run."""
    summary = {
        "timestamp": datetime.now().isoformat(),
        "run_directory": str(run_dir),
        "resource_snapshots": {
            "start": asdict(start_snapshot),
            "end": asdict(end_snapshot)
        },
        "suites": [],
        "totals": {
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "duration_seconds": 0.0
        }
    }
    
    for sr in suite_results:
        suite_summary = {
            "name": sr.name,
            "tests_run": sr.tests_run,
            "tests_passed": sr.tests_passed,
            "tests_failed": sr.tests_failed,
            "duration_seconds": sr.duration_seconds,
            "tests": [asdict(r) for r in sr.results]
        }
        summary["suites"].append(suite_summary)
        summary["totals"]["tests_run"] += sr.tests_run
        summary["totals"]["tests_passed"] += sr.tests_passed
        summary["totals"]["tests_failed"] += sr.tests_failed
        summary["totals"]["duration_seconds"] += sr.duration_seconds
    
    summary_file = run_dir / "summary.json"
    with open(summary_file, "w") as f:
        json.dump(summary, f, indent=2)
    
    log_info(f"Summary written to: {summary_file}")
    return summary

def print_final_summary(summary: Dict):
    """Print a human-readable summary."""
    log_header("TEST RUN COMPLETE")
    
    totals = summary["totals"]
    passed = totals["tests_passed"]
    failed = totals["tests_failed"]
    total = totals["tests_run"]
    duration = totals["duration_seconds"]
    
    print(f"\n{'Suite':<25} {'Passed':<10} {'Failed':<10} {'Duration':<10}")
    print("-" * 55)
    
    for suite in summary["suites"]:
        print(f"{suite['name']:<25} {suite['tests_passed']:<10} {suite['tests_failed']:<10} {suite['duration_seconds']:.1f}s")
    
    print("-" * 55)
    print(f"{'TOTAL':<25} {passed:<10} {failed:<10} {duration:.1f}s")
    print()
    
    if failed == 0:
        log_pass(f"All {total} tests passed!")
    else:
        log_fail(f"{failed}/{total} tests failed")

# ============================================================================
# Main Entry Point
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Project Iris - Unified Test Runner",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --suite unit              Run unit tests only
  %(prog)s --suite integration       Run integration tests
  %(prog)s --tier 0                  Run CI Tier 0 (required)
  %(prog)s --tier 1                  Run CI Tier 1 (nightly)
  %(prog)s --list                    List all available tests
  %(prog)s --all                     Run all tests
        """
    )
    
    parser.add_argument("--suite", type=str, help="Run specific suite")
    parser.add_argument("--tier", type=int, choices=[0, 1], help="Run CI tier")
    parser.add_argument("--all", action="store_true", help="Run all tests")
    parser.add_argument("--list", action="store_true", help="List all tests")
    parser.add_argument("--ci", action="store_true", help="CI mode (stricter failure handling)")
    parser.add_argument("--no-cluster", action="store_true", help="Don't manage cluster lifecycle")
    parser.add_argument("--timeout", type=int, default=120, help="Per-test timeout in seconds")
    
    args = parser.parse_args()
    
    # Handle --list
    if args.list:
        all_tests = list_all_tests()
        print("\nAvailable Tests:")
        print("=" * 60)
        for suite, tests in sorted(all_tests.items()):
            print(f"\n{Colors.BOLD}{suite}{Colors.END} ({len(tests)} tests)")
            for test in tests:
                print(f"  - {test['name']} ({test['type']})")
        return 0
    
    # Determine which suites to run
    suites_to_run = []
    
    if args.suite:
        suites_to_run = [args.suite]
    elif args.tier == 0:
        suites_to_run = TIER_0_SUITES
    elif args.tier == 1:
        suites_to_run = TIER_1_SUITES
    elif args.all:
        all_tests = list_all_tests()
        suites_to_run = list(all_tests.keys())
    else:
        parser.print_help()
        return 1
    
    # Create run directory
    run_dir = create_run_directory()
    log_header("Project Iris Test Runner")
    log_info(f"Run directory: {run_dir}")
    log_info(f"Suites: {', '.join(suites_to_run)}")
    
    # Capture start resources
    start_snapshot = get_resource_snapshot()
    log_info(f"Initial resources - CPU: {start_snapshot.cpu_percent:.1f}%, Disk: {start_snapshot.disk_free_gb:.1f}GB")
    
    # Generate unique suffix for this run to avoid zombie process conflicts
    run_suffix = f"_{int(time.time())}"
    os.environ["IRIS_NODE_SUFFIX"] = run_suffix
    log_info(f"Using node suffix: {run_suffix}")
    
    # Run suites
    suite_results = []
    for suite in suites_to_run:
        result = run_suite(suite, run_dir, require_cluster=not args.no_cluster)
        suite_results.append(result)
    
    # Cleanup cluster
    if not args.no_cluster:
        stop_cluster()
    
    # Capture end resources
    end_snapshot = get_resource_snapshot()
    
    # Write summary
    summary = write_summary(run_dir, suite_results, start_snapshot, end_snapshot)
    print_final_summary(summary)
    
    # Return appropriate exit code
    total_failed = sum(sr.tests_failed for sr in suite_results)
    if total_failed > 0:
        if args.ci:
            log_fail("CI mode: Exiting with failure status")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
