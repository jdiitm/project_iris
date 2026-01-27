#!/usr/bin/env python3
"""
Project Iris - Unified Test Runner

A lightweight, resource-aware test orchestrator designed for constrained infrastructure.
No heavy dependencies - uses only Python stdlib plus PyYAML.

Features:
- Deterministic test ordering (lexicographic by suite, then by test name)
- Seeded randomness for reproducible test execution
- Comprehensive cleanup between suites
- Real-time output streaming with heartbeat indicators

Usage:
    ./tests/run_tests.py --suite unit              # Run unit tests only
    ./tests/run_tests.py --suite integration       # Run integration tests
    ./tests/run_tests.py --tier 0                  # CI Tier 0 (required)
    ./tests/run_tests.py --tier 1                  # CI Tier 1 (optional/nightly)
    ./tests/run_tests.py --list                    # List all available tests
    ./tests/run_tests.py --all                     # Run all tests sequentially

Environment Variables:
    TEST_SEED: Master seed for deterministic random (default: 42)
    CI: When set, enables CI-specific behaviors
"""

# Force unbuffered output for real-time visibility
import os
os.environ['PYTHONUNBUFFERED'] = '1'

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
import glob as glob_module

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

# Tests that require TLS-enabled cluster (use config/test_tls.config)
TLS_REQUIRED_TESTS = ["test_tls_mandatory"]

# Known failing tests - excluded from determinism requirements (Phase 2)
KNOWN_FAILING_TESTS = []  # All tests should pass after hardening

# Suites that require Docker global cluster
DOCKER_REQUIRED_SUITES = ["chaos_dist"]
# Tests in other suites that require Docker
DOCKER_REQUIRED_TESTS = ["test_failover_time", "test_multimaster_durability"]
# Tests that kill/restart containers or modify network and may corrupt cluster state
# After these tests, cluster needs FULL restart (with volume removal) to clear Mnesia state
CLUSTER_CORRUPTING_TESTS = [
    "test_ack_durability",       # Kills core-east-1
    "test_cascade_failure",      # Kills core-eu-2
    "test_dist_failover",        # Kills multiple containers
    "test_failover_time",        # Kills core-east-1
    "test_multimaster_durability",  # Kills core-east-1 with SIGKILL
    "test_split_brain",          # Disconnects/reconnects network
]

# Docker cluster paths
DOCKER_CLUSTER_DIR = PROJECT_ROOT / "docker" / "global-cluster"
DOCKER_COMPOSE_FILE = DOCKER_CLUSTER_DIR / "docker-compose.yml"
DOCKER_INIT_SCRIPT = DOCKER_CLUSTER_DIR / "init_cluster.sh"

# ============================================================================
# Determinism Configuration
# ============================================================================

# Import determinism utilities if available
try:
    from tests.conftest import (
        get_determinism_info,
        reset_all_determinism,
        MASTER_SEED,
        TEST_RUN_ID,
    )
    DETERMINISM_AVAILABLE = True
except ImportError:
    # Fallback if conftest not available
    DETERMINISM_AVAILABLE = False
    MASTER_SEED = int(os.environ.get("TEST_SEED", "42"))
    TEST_RUN_ID = f"run_{MASTER_SEED}"
    def get_determinism_info():
        return {"seed": MASTER_SEED, "run_id": TEST_RUN_ID}
    def reset_all_determinism():
        pass

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
    skipped: bool = False  # Per TEST_CONTRACT.md: exit(2) = SKIP
    skip_reason: str = ""

@dataclass
class SuiteResult:
    name: str
    tests_run: int
    tests_passed: int
    tests_failed: int
    tests_skipped: int = 0  # Per TEST_CONTRACT.md: exit(2) = SKIP
    duration_seconds: float = 0.0
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
        print(f"{color}[{timestamp}] {msg}{Colors.END}", flush=True)
    else:
        print(f"[{timestamp}] {msg}", flush=True)

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
# Streaming Subprocess Execution
# ============================================================================

def run_with_streaming(cmd, cwd, timeout, prefix="", shell=False):
    """Run command with real-time output streaming.
    
    Args:
        cmd: Command to run (list or string)
        cwd: Working directory
        timeout: Timeout in seconds
        prefix: Prefix for each output line
        shell: Whether to use shell execution
        
    Returns:
        tuple: (return_code, output_string)
    """
    proc = subprocess.Popen(
        cmd,
        shell=shell or isinstance(cmd, str),
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1  # Line-buffered
    )
    
    output_lines = []
    start = time.time()
    
    while True:
        if proc.poll() is not None:
            break
        if time.time() - start > timeout:
            proc.kill()
            proc.wait()
            raise subprocess.TimeoutExpired(cmd, timeout)
        
        # Non-blocking read with select
        import select
        if select.select([proc.stdout], [], [], 0.1)[0]:
            line = proc.stdout.readline()
            if line:
                output_lines.append(line)
                print(f"{prefix}{line}", end='', flush=True)
    
    # Drain remaining output
    for line in proc.stdout:
        output_lines.append(line)
        print(f"{prefix}{line}", end='', flush=True)
    
    return proc.returncode, ''.join(output_lines)


def run_with_heartbeat(cmd, cwd, timeout, heartbeat_interval=10, shell=False):
    """Run command with periodic 'still running...' heartbeat.
    
    Args:
        cmd: Command to run (list or string)
        cwd: Working directory
        timeout: Timeout in seconds
        heartbeat_interval: Seconds between heartbeat messages
        shell: Whether to use shell execution
        
    Returns:
        tuple: (return_code, stdout, stderr)
    """
    proc = subprocess.Popen(
        cmd,
        shell=shell or isinstance(cmd, str),
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    start = time.time()
    last_heartbeat = start
    
    while proc.poll() is None:
        time.sleep(0.5)
        elapsed = time.time() - start
        
        if elapsed > timeout:
            proc.kill()
            proc.wait()
            raise subprocess.TimeoutExpired(cmd, timeout)
        
        if time.time() - last_heartbeat > heartbeat_interval:
            print(f"  ... still running ({elapsed:.0f}s)", flush=True)
            last_heartbeat = time.time()
    
    stdout, stderr = proc.communicate()
    return proc.returncode, stdout, stderr

# ============================================================================
# Pre-Suite Cleanup (Determinism)
# ============================================================================

def cleanup_before_suite():
    """
    Clean up all state before running a test suite.
    
    This ensures deterministic execution by removing:
    - Orphaned Erlang processes
    - Stale Mnesia directories
    - Log files from previous runs
    """
    log_info("Performing pre-suite cleanup...")
    
    # Kill all beam.smp processes
    try:
        subprocess.run(
            ["pkill", "-9", "beam.smp"],
            capture_output=True,
            timeout=10
        )
    except Exception:
        pass
    
    # Kill epmd (Erlang Port Mapper Daemon)
    try:
        subprocess.run(
            ["pkill", "-9", "epmd"],
            capture_output=True,
            timeout=5
        )
    except Exception:
        pass
    
    # Remove Mnesia directories from project root
    for mnesia_dir in PROJECT_ROOT.glob("Mnesia.*"):
        try:
            shutil.rmtree(mnesia_dir, ignore_errors=True)
        except Exception:
            pass
    
    # Remove Mnesia directories from /tmp
    for mnesia_dir in Path("/tmp").glob("Mnesia.*"):
        try:
            shutil.rmtree(mnesia_dir, ignore_errors=True)
        except Exception:
            pass
    
    # Remove log files from project root
    for log_file in PROJECT_ROOT.glob("*.log"):
        try:
            log_file.unlink()
        except Exception:
            pass
    
    # Wait for ports to be released
    time.sleep(2)
    
    log_info("Cleanup complete")


# ============================================================================
# Docker Cluster Management
# ============================================================================

_docker_cluster_running = False

def is_docker_available() -> bool:
    """Check if Docker is available and running."""
    try:
        result = subprocess.run(
            ["docker", "info"],
            capture_output=True,
            timeout=10
        )
        return result.returncode == 0
    except Exception:
        return False


def wait_for_mnesia_cluster(timeout: int = 120) -> bool:
    """
    Wait for Mnesia cluster to form with all nodes joined.
    
    This checks that db_nodes on core-east-1 includes multiple nodes,
    indicating the cluster has formed.
    """
    import random
    
    start_time = time.time()
    attempt = 0
    
    while time.time() - start_time < timeout:
        attempt += 1
        try:
            # Query Mnesia db_nodes via RPC to the actual running node
            probe_name = f"probe_{random.randint(10000, 99999)}"
            result = subprocess.run(
                ["docker", "exec", "core-east-1", "sh", "-c",
                 f"erl -noshell -sname {probe_name} -setcookie iris_secret -eval '"
                 "case net_adm:ping(core_east_1@coreeast1) of "
                 "pong -> "
                 "  DbNodes = rpc:call(core_east_1@coreeast1, mnesia, system_info, [db_nodes], 5000), "
                 "  case DbNodes of "
                 "    L when is_list(L), length(L) >= 2 -> io:format(\"READY:~p\", [length(L)]); "
                 "    _ -> io:format(\"WAITING\") "
                 "  end; "
                 "pang -> io:format(\"NOCONN\") "
                 "end, halt().'"],
                capture_output=True,
                text=True,
                timeout=15
            )
            
            output = result.stdout.strip()
            
            if "READY:" in output:
                # Extract node count
                try:
                    count = int(output.split("READY:")[1])
                    log_info(f"  Mnesia cluster has {count} nodes")
                    return True
                except:
                    pass
            
            if attempt % 5 == 0:
                log_info(f"  Still waiting for Mnesia cluster... ({int(time.time() - start_time)}s)")
            
        except subprocess.TimeoutExpired:
            pass
        except Exception as e:
            if attempt % 10 == 0:
                log_warn(f"  Error checking Mnesia: {e}")
        
        time.sleep(3)
    
    return False

def start_docker_cluster() -> bool:
    """
    Start the Docker global cluster for cross-region tests.
    
    Returns True if cluster started successfully, False otherwise.
    """
    global _docker_cluster_running
    
    if not is_docker_available():
        log_warn("Docker not available - Docker-dependent tests will skip")
        return False
    
    if not DOCKER_COMPOSE_FILE.exists():
        log_warn(f"Docker compose file not found: {DOCKER_COMPOSE_FILE}")
        return False
    
    log_info("Starting Docker global cluster...")
    
    try:
        # Stop any existing cluster first
        log_info("  Phase 1: Stopping existing Docker cluster...")
        subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "--remove-orphans", "-v"],
            cwd=str(DOCKER_CLUSTER_DIR),
            capture_output=True,
            timeout=120
        )
        
        # Start fresh cluster
        log_info("  Phase 2: Starting Docker cluster (this may take 1-2 minutes)...")
        result = subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "up", "-d"],
            cwd=str(DOCKER_CLUSTER_DIR),
            capture_output=True,
            text=True,
            timeout=300
        )
        
        if result.returncode != 0:
            log_warn(f"Docker cluster start failed: {result.stderr}")
            return False
        
        # Wait for services to be healthy - increased for 6-node cluster
        log_info("  Phase 3: Waiting for containers to start (30s)...")
        time.sleep(30)
        
        # Phase 4: Wait for Mnesia cluster to form (all nodes join)
        log_info("  Phase 4: Waiting for Mnesia cluster to form...")
        mnesia_ready = wait_for_mnesia_cluster(timeout=120)
        if not mnesia_ready:
            log_warn("  Mnesia cluster did not form completely - continuing anyway")
        else:
            log_info("  Mnesia cluster formed successfully")
        
        # Initialize Mnesia replication with retry
        # The init_cluster.sh script now has its own retry logic and verification
        if DOCKER_INIT_SCRIPT.exists():
            log_info("  Phase 5: Initializing cross-region replication...")
            result = subprocess.run(
                ["bash", str(DOCKER_INIT_SCRIPT)],
                cwd=str(DOCKER_CLUSTER_DIR),
                capture_output=True,
                text=True,
                timeout=300  # Increased timeout - init script now does more verification
            )
            
            if result.returncode != 0:
                log_warn(f"Mnesia replication init returned non-zero: {result.returncode}")
                # Log last few lines of output for debugging
                output_lines = (result.stdout + result.stderr).strip().split('\n')
                for line in output_lines[-10:]:
                    log_warn(f"    {line}")
                # Continue anyway - tests will skip if replication not working
            else:
                log_info("  Replication initialization completed successfully")
                # Log success output
                for line in result.stdout.strip().split('\n')[-5:]:
                    log_info(f"    {line}")
        
        # Wait for replication to settle after init
        log_info("  Phase 6: Waiting for replication to settle (15s)...")
        time.sleep(15)
        
        # Phase 7: Run verification script for comprehensive check
        verify_script = PROJECT_ROOT / "scripts" / "verify_cluster_ready.py"
        if verify_script.exists():
            log_info("  Phase 7: Running cluster verification script...")
            # Try verification with retry
            for verify_attempt in range(2):
                verify_result = subprocess.run(
                    ["python3", str(verify_script), "--quick"],
                    capture_output=True,
                    text=True,
                    timeout=90
                )
                if verify_result.returncode == 0:
                    log_info("  Cluster verification: PASSED")
                    break
                else:
                    if verify_attempt == 0:
                        log_warn("  Cluster verification: FAILED - retrying in 10s...")
                        time.sleep(10)
                    else:
                        log_warn("  Cluster verification: WARNINGS (see output)")
                        # Print last few lines of verification output
                        lines = verify_result.stdout.strip().split('\n')
                        for line in lines[-5:]:
                            log_warn(f"    {line}")
        
        _docker_cluster_running = True
        log_info("Docker cluster started successfully")
        return True
        
    except subprocess.TimeoutExpired:
        log_warn("Docker cluster startup timed out")
        return False
    except Exception as e:
        log_warn(f"Docker cluster startup failed: {e}")
        return False

def stop_docker_cluster(remove_volumes: bool = False):
    """Stop the Docker global cluster.
    
    Args:
        remove_volumes: If True, also remove Docker volumes (Mnesia data).
                       Use this after tests that corrupt cluster state.
    """
    global _docker_cluster_running
    
    if not _docker_cluster_running:
        return
    
    log_info("Stopping Docker cluster...")
    
    try:
        cmd = ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "--remove-orphans"]
        if remove_volumes:
            cmd.append("-v")  # Remove volumes to clear Mnesia state
            log_info("  (removing volumes to clear Mnesia state)")
        
        subprocess.run(
            cmd,
            cwd=str(DOCKER_CLUSTER_DIR),
            capture_output=True,
            timeout=120  # Increased timeout for volume removal
        )
        _docker_cluster_running = False
        log_info("Docker cluster stopped")
    except Exception as e:
        log_warn(f"Error stopping Docker cluster: {e}")

def suite_requires_docker(suite_name: str) -> bool:
    """Check if a suite requires Docker cluster."""
    return suite_name in DOCKER_REQUIRED_SUITES

def test_requires_docker(test_name: str) -> bool:
    """Check if a specific test requires Docker cluster."""
    return test_name in DOCKER_REQUIRED_TESTS


# ============================================================================
# Test Discovery
# ============================================================================

def discover_tests(suite: str) -> List[Dict[str, Any]]:
    """
    Discover all tests in a suite directory.
    
    Tests are returned in LEXICOGRAPHIC ORDER by name for deterministic execution.
    """
    tests = []
    suite_dir = SUITES_DIR / suite
    
    if not suite_dir.exists():
        return tests
    
    # Track already-added files to avoid duplicates
    added_files = set()
    
    # Python tests - pick up ALL .py files (they are all tests)
    # Sort by filename for deterministic ordering
    python_files = sorted(suite_dir.glob("*.py"), key=lambda p: p.name)
    for test_file in python_files:
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
    # Find erl binary - don't hardcode path
    erl_binary = shutil.which("erl") or "erl"
    
    # Sort Erlang test files for deterministic ordering
    erlang_files = sorted(suite_dir.glob("*_tests.erl"), key=lambda p: p.name)
    for test_file in erlang_files:
        module = test_file.stem
        tests.append({
            "name": module,
            "suite": suite,
            "type": "erlang",
            "path": str(test_file),
            "command": f"{erl_binary} -pa {PROJECT_ROOT}/ebin -pa {suite_dir} -noshell -eval \"eunit:test({module}, []), init:stop().\""
        })
    
    # P0-1 FIX: Also discover EUnit tests from test_utils directory for 'unit' suite
    if suite == "unit":
        test_utils_dir = PROJECT_ROOT / "test_utils"
        if test_utils_dir.exists():
            # Sort for deterministic ordering
            test_utils_files = sorted(test_utils_dir.glob("*_tests.erl"), key=lambda p: p.name)
            for test_file in test_utils_files:
                module = test_file.stem
                # Skip if already added from suite_dir
                if any(t["name"] == module for t in tests):
                    continue
                tests.append({
                    "name": f"test_utils/{module}",
                    "suite": suite,
                    "type": "erlang",
                    "path": str(test_file),
                    "command": f"{erl_binary} -pa {PROJECT_ROOT}/ebin -pa {test_utils_dir} -noshell -eval \"eunit:test({module}, []), init:stop().\""
                })
    
    # Final sort by name for deterministic ordering
    tests.sort(key=lambda t: t["name"])
    
    return tests

def list_all_tests() -> Dict[str, List[Dict]]:
    """
    List all available tests grouped by suite.
    
    Suites are returned in LEXICOGRAPHIC ORDER for deterministic execution.
    """
    all_tests = {}
    # Sort suite directories by name for deterministic ordering
    suite_dirs = sorted(SUITES_DIR.iterdir(), key=lambda p: p.name)
    for suite_dir in suite_dirs:
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

def ensure_cluster_running(config: str = "config/test") -> bool:
    """Ensure the Iris cluster is running with specified config.
    
    Args:
        config: Config file path without .config extension (default: config/test)
    """
    log_info(f"Ensuring cluster is running (config={config})...")
    
    try:
        # Get suffix for this run
        suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
        make_args = [f"NODE_SUFFIX={suffix}", f"CONFIG={config}"] if suffix else [f"CONFIG={config}"]
        
        # Phase 1: Stop existing cluster
        log_info("Phase 1/5: Stopping existing cluster...")
        stop_cluster()
        time.sleep(2)

        # Phase 2: Build project (can take time on CI)
        log_info("Phase 2/5: Building project (this may take a few minutes)...")
        try:
            returncode, output = run_with_streaming(
                ["make", "all"] + make_args,
                cwd=str(PROJECT_ROOT),
                timeout=300,  # 5 minutes for compilation
                prefix="  [build] "
            )
            if returncode != 0:
                log_warn(f"Build failed with exit code {returncode}")
                return False
        except subprocess.TimeoutExpired:
            log_warn("Build timed out after 5 minutes")
            return False

        # Phase 3: Start Core node
        log_info("Phase 3/5: Starting Core node...")
        try:
            returncode, stdout, stderr = run_with_heartbeat(
                ["make", "start_core"] + make_args,
                cwd=str(PROJECT_ROOT),
                timeout=60,
                heartbeat_interval=10
            )
        except subprocess.TimeoutExpired:
            log_warn("Core node startup timed out")
            return False
        time.sleep(3)  # Give Mnesia time to initialize
        
        # Phase 4: Start Edge node
        log_info("Phase 4/5: Starting Edge node...")
        try:
            returncode, stdout, stderr = run_with_heartbeat(
                ["make", "start_edge1"] + make_args,
                cwd=str(PROJECT_ROOT),
                timeout=60,
                heartbeat_interval=10
            )
        except subprocess.TimeoutExpired:
            log_warn("Edge node startup timed out")
            return False
        
        # Phase 5: Wait for Edge port
        log_info("Phase 5/5: Waiting for Edge Node (8085)...")
        if not wait_for_port(8085, timeout=30):
            log_warn("Edge node port 8085 did not open in time.")
            return False
            
        # Mesh the cluster
        mesh_cluster(suffix)
        log_info("Cluster is ready!")
            
        return True
    except Exception as e:
        log_warn(f"Could not start cluster: {e}")
        return False


# Track what config the cluster is currently running with
_current_cluster_config = None

def ensure_cluster_with_config(config: str = "config/test") -> bool:
    """Ensure cluster is running with the specified config, restarting if needed."""
    global _current_cluster_config
    
    if _current_cluster_config == config:
        # Verify cluster is still actually running
        if wait_for_port(8085, timeout=2):
            log_info(f"Cluster already running with {config}")
            return True
        else:
            log_warn("Cluster config matches but port 8085 not responding - restarting")
            _current_cluster_config = None
    
    if _current_cluster_config is not None:
        log_info(f"Switching cluster config from {_current_cluster_config} to {config}")
    
    result = ensure_cluster_running(config=config)
    if result:
        _current_cluster_config = config
    return result

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

def run_test(test: Dict, run_dir: Path, timeout: int = 120, stream_output: bool = True) -> TestResult:
    """Run a single test and return the result.
    
    Args:
        test: Test configuration dict
        run_dir: Directory for artifacts
        timeout: Timeout in seconds
        stream_output: If True, stream output in real-time; if False, capture silently
    """
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
        env["PYTHONUNBUFFERED"] = "1"  # Force unbuffered output for child processes
        env["IRIS_TEST_RUNNER"] = "1"  # Signal that test runner is managing cluster
        
        # Pass determinism configuration to child processes
        env["TEST_SEED"] = str(MASTER_SEED)
        env["TEST_RUN_ID"] = TEST_RUN_ID
        
        # Pass TEST_PROFILE for proper scaling (no CI-based tricks)
        if "TEST_PROFILE" not in env:
            env["TEST_PROFILE"] = os.environ.get("TEST_PROFILE", "smoke")
        
        # Add project root to PYTHONPATH so tests can import from tests.framework
        existing_pythonpath = env.get("PYTHONPATH", "")
        env["PYTHONPATH"] = f"{PROJECT_ROOT}:{existing_pythonpath}" if existing_pythonpath else str(PROJECT_ROOT)
        
        if stream_output:
            # Stream output in real-time with heartbeat for long tests
            proc = subprocess.Popen(
                test["command"],
                shell=True,
                cwd=str(PROJECT_ROOT),
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,
                env=env
            )
            
            output_lines = []
            last_heartbeat = time.time()
            heartbeat_interval = 15  # Show heartbeat every 15s if no output
            
            while True:
                if proc.poll() is not None:
                    break
                elapsed = time.time() - start_time
                if elapsed > timeout:
                    proc.kill()
                    proc.wait()
                    raise subprocess.TimeoutExpired(test["command"], timeout)
                
                # Non-blocking read
                import select
                if select.select([proc.stdout], [], [], 0.5)[0]:
                    line = proc.stdout.readline()
                    if line:
                        output_lines.append(line)
                        print(f"  | {line}", end='', flush=True)
                        last_heartbeat = time.time()
                elif time.time() - last_heartbeat > heartbeat_interval:
                    print(f"  ... running ({elapsed:.0f}s)", flush=True)
                    last_heartbeat = time.time()
            
            # Drain remaining output
            for line in proc.stdout:
                output_lines.append(line)
                print(f"  | {line}", end='', flush=True)
            
            result.output = ''.join(output_lines)
            result.error = ""
            # Per TEST_CONTRACT.md: exit(0)=pass, exit(1)=fail, exit(2)=skip
            if proc.returncode == 0:
                result.passed = True
            elif proc.returncode == 2:
                result.passed = True  # Skips don't count as failures
                result.skipped = True
                # Extract skip reason from output (look for "SKIP:" prefix)
                for line in output_lines:
                    if "SKIP:" in line:
                        result.skip_reason = line.strip()
                        break
            else:
                result.passed = False
        else:
            # Original behavior: capture silently
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
            # Per TEST_CONTRACT.md: exit(0)=pass, exit(1)=fail, exit(2)=skip
            if proc.returncode == 0:
                result.passed = True
            elif proc.returncode == 2:
                result.passed = True  # Skips don't count as failures
                result.skipped = True
                for line in proc.stdout.split('\n'):
                    if "SKIP:" in line:
                        result.skip_reason = line.strip()
                        break
            else:
                result.passed = False
        
        # Write log
        with open(log_file, "w") as f:
            f.write(f"=== STDOUT ===\n{result.output}\n")
            f.write(f"=== STDERR ===\n{result.error}\n")
            f.write(f"=== EXIT CODE: {proc.returncode if hasattr(proc, 'returncode') else 'N/A'} ===\n")
        
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
    
    if result.skipped:
        log_warn(f"SKIP: {test['name']} ({result.duration_seconds:.1f}s) - {result.skip_reason}")
    elif result.passed:
        log_pass(f"PASS: {test['name']} ({result.duration_seconds:.1f}s)")
    else:
        log_fail(f"FAIL: {test['name']} ({result.duration_seconds:.1f}s)")
    
    return result

def run_suite(suite_name: str, run_dir: Path, require_cluster: bool = True) -> SuiteResult:
    """
    Run all tests in a suite.
    
    Tests are executed in deterministic (lexicographic) order.
    Pre-suite cleanup ensures no state pollution from previous runs.
    """
    global _current_cluster_config
    
    log_header(f"Suite: {suite_name}")
    
    # Reset determinism state for this suite
    reset_all_determinism()
    
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
    
    log_info(f"Found {len(tests)} tests (sorted lexicographically)")
    
    # Suite-specific timeouts (seconds)
    suite_timeouts = {
        "integration": 120,
        "resilience": 600,       # 10 min for resilience
        "chaos_controlled": 600, # 10 min for chaos
        "stress": 600,           # 10 min for stress
        "performance_light": 300,# 5 min for performance
        "unit": 60,
        "security": 120,
    }
    timeout = suite_timeouts.get(suite_name, 300)  # Default 5 min

    # Separate tests by config requirement
    normal_tests = []
    tls_tests = []
    for test in tests:
        if test["name"] in TLS_REQUIRED_TESTS:
            tls_tests.append(test)
        else:
            normal_tests.append(test)
    
    results = []
    
    # Check if this suite requires Docker cluster
    needs_docker = suite_requires_docker(suite_name)
    docker_started = False
    
    # Reorder tests: put cluster-corrupting tests LAST to minimize restarts
    if needs_docker:
        def test_sort_key(test):
            # Tests are dictionaries with "name" key
            test_name = test.get("name", "") if isinstance(test, dict) else str(test)
            # Tests that corrupt cluster go last (return 1), others first (return 0)
            is_corrupting = test_name in CLUSTER_CORRUPTING_TESTS
            return (1 if is_corrupting else 0, test_name)  # Secondary sort by name for stability
        
        original_order = [t.get("name", "?") for t in normal_tests]
        normal_tests = sorted(normal_tests, key=test_sort_key)
        new_order = [t.get("name", "?") for t in normal_tests]
        log_info(f"Reordered tests: {original_order} -> {new_order}")
    
    if needs_docker:
        # Stop local cluster before starting Docker
        log_info("Suite requires Docker cluster - stopping local cluster...")
        stop_cluster()
        
        # Start Docker cluster
        docker_started = start_docker_cluster()
        if not docker_started:
            log_warn(f"Docker cluster unavailable - tests in {suite_name} may skip")
    else:
        # Run normal tests first (with default config)
        if normal_tests and require_cluster and suite_name not in ["unit"]:
            ensure_cluster_with_config("config/test")
    
    for test in normal_tests:
        result = run_test(test, run_dir, timeout=timeout)
        results.append(result)
        
        # If this test corrupts the cluster, do a FULL restart (with volume removal)
        # This ensures clean Mnesia state for subsequent tests
        test_name = test.get("name", "") if isinstance(test, dict) else str(test)
        if needs_docker and test_name in CLUSTER_CORRUPTING_TESTS:
            log_info(f"Test {test_name} corrupted cluster - full restart with volume cleanup...")
            stop_docker_cluster(remove_volumes=True)  # CRITICAL: remove volumes!
            time.sleep(10)  # Allow time for cleanup
            if not start_docker_cluster():
                log_warn("Failed to restart Docker cluster after corrupting test")
    
    # Run TLS-required tests with TLS config
    if tls_tests and require_cluster:
        log_info("Switching to TLS-enforcing config for TLS tests...")
        ensure_cluster_with_config("config/test_tls")
        
        for test in tls_tests:
            result = run_test(test, run_dir, timeout=timeout)
            results.append(result)
        
        # Switch back to normal config for subsequent suites
        _current_cluster_config = None  # Force restart on next suite
    
    # Clean up Docker cluster if we started it
    if docker_started:
        stop_docker_cluster()
        # Reset cluster config so next suite will restart local cluster
        _current_cluster_config = None
    
    duration = time.time() - start_time
    skipped = sum(1 for r in results if r.skipped)
    passed = sum(1 for r in results if r.passed and not r.skipped)
    failed = sum(1 for r in results if not r.passed)
    
    return SuiteResult(
        name=suite_name,
        tests_run=len(results),
        tests_skipped=skipped,
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
        "determinism": get_determinism_info(),
        "resource_snapshots": {
            "start": asdict(start_snapshot),
            "end": asdict(end_snapshot)
        },
        "suites": [],
        "totals": {
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "tests_skipped": 0,
            "duration_seconds": 0.0
        }
    }
    
    for sr in suite_results:
        suite_summary = {
            "name": sr.name,
            "tests_run": sr.tests_run,
            "tests_passed": sr.tests_passed,
            "tests_failed": sr.tests_failed,
            "tests_skipped": sr.tests_skipped,
            "duration_seconds": sr.duration_seconds,
            "tests": [asdict(r) for r in sr.results]
        }
        summary["suites"].append(suite_summary)
        summary["totals"]["tests_run"] += sr.tests_run
        summary["totals"]["tests_passed"] += sr.tests_passed
        summary["totals"]["tests_failed"] += sr.tests_failed
        summary["totals"]["tests_skipped"] += sr.tests_skipped
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
    skipped = totals.get("tests_skipped", 0)
    total = totals["tests_run"]
    duration = totals["duration_seconds"]
    
    print(f"\n{'Suite':<25} {'Passed':<10} {'Failed':<10} {'Skipped':<10} {'Duration':<10}", flush=True)
    print("-" * 65, flush=True)
    
    for suite in summary["suites"]:
        skipped_count = suite.get("tests_skipped", 0)
        print(f"{suite['name']:<25} {suite['tests_passed']:<10} {suite['tests_failed']:<10} {skipped_count:<10} {suite['duration_seconds']:.1f}s", flush=True)
    
    print("-" * 65, flush=True)
    print(f"{'TOTAL':<25} {passed:<10} {failed:<10} {skipped:<10} {duration:.1f}s", flush=True)
    print(flush=True)
    
    if failed == 0:
        if skipped > 0:
            log_warn(f"All {total} tests completed: {passed} passed, {skipped} skipped")
        else:
            log_pass(f"All {total} tests passed!")
    else:
        log_fail(f"{failed}/{total} tests failed ({skipped} skipped)")

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
  %(prog)s --all --with-cluster      Run all tests including cross-region (starts Docker)
  %(prog)s --suite chaos_dist        Run chaos_dist tests (starts Docker automatically)
        """
    )
    
    parser.add_argument("--suite", type=str, help="Run specific suite")
    parser.add_argument("--tier", type=int, choices=[0, 1], help="Run CI tier")
    parser.add_argument("--all", action="store_true", help="Run all tests")
    parser.add_argument("--list", action="store_true", help="List all tests")
    parser.add_argument("--ci", action="store_true", help="CI mode (stricter failure handling)")
    parser.add_argument("--no-cluster", action="store_true", help="Don't manage cluster lifecycle")
    parser.add_argument("--with-cluster", action="store_true", 
                        help="Use Docker global cluster for cross-region tests (auto-starts if needed)")
    parser.add_argument("--timeout", type=int, default=120, help="Per-test timeout in seconds")
    
    args = parser.parse_args()
    
    # Handle --list
    if args.list:
        all_tests = list_all_tests()
        print("\nAvailable Tests:", flush=True)
        print("=" * 60, flush=True)
        for suite, tests in sorted(all_tests.items()):
            print(f"\n{Colors.BOLD}{suite}{Colors.END} ({len(tests)} tests)", flush=True)
            for test in tests:
                print(f"  - {test['name']} ({test['type']})", flush=True)
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
    log_info(f"Suites: {', '.join(sorted(suites_to_run))}")  # Sort for determinism
    
    # Log determinism information
    det_info = get_determinism_info()
    log_info(f"Determinism: seed={det_info['seed']}, run_id={det_info['run_id']}")
    if DETERMINISM_AVAILABLE:
        log_info("Determinism utilities: LOADED")
    else:
        log_warn("Determinism utilities: FALLBACK (conftest.py not found)")
    
    # Capture start resources
    start_snapshot = get_resource_snapshot()
    log_info(f"Initial resources - CPU: {start_snapshot.cpu_percent:.1f}%, Disk: {start_snapshot.disk_free_gb:.1f}GB")
    
    # Generate unique suffix for this run based on seed for reproducibility
    # Use seed + timestamp to ensure uniqueness while maintaining reproducibility
    run_suffix = f"_{MASTER_SEED}_{int(time.time())}"
    os.environ["IRIS_NODE_SUFFIX"] = run_suffix
    log_info(f"Using node suffix: {run_suffix}")
    
    # Pass seed to child processes
    os.environ["TEST_SEED"] = str(MASTER_SEED)
    os.environ["TEST_RUN_ID"] = TEST_RUN_ID
    
    # Perform initial cleanup
    cleanup_before_suite()
    
    # Determine cluster mode
    # --with-cluster: Explicitly use Docker cluster for cross-region tests
    # --no-cluster: Don't manage any cluster (tests handle their own)
    use_docker_cluster = args.with_cluster
    manage_cluster = not args.no_cluster
    
    if use_docker_cluster:
        log_info("Docker cluster mode: ENABLED (--with-cluster)")
    
    # Run suites
    suite_results = []
    docker_cluster_started = False
    
    for suite in suites_to_run:
        # Check if this suite needs Docker cluster
        needs_docker = suite_requires_docker(suite) or (use_docker_cluster and suite in TIER_1_SUITES)
        
        # Start Docker cluster if needed and not already running
        if needs_docker and use_docker_cluster and not docker_cluster_started:
            log_info("Starting Docker global cluster for cross-region tests...")
            docker_cluster_started = start_docker_cluster()
            if not docker_cluster_started:
                log_warn("Docker cluster unavailable - some tests may skip")
        
        result = run_suite(suite, run_dir, require_cluster=manage_cluster)
        suite_results.append(result)
    
    # Cleanup Docker cluster if we started it
    if docker_cluster_started:
        log_info("Stopping Docker global cluster...")
        stop_docker_cluster()
    
    # Cleanup local cluster
    if manage_cluster and not use_docker_cluster:
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
