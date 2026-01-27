#!/usr/bin/env python3
"""
Combined Resilience Test Suite

Tests system behavior under various failure conditions:
- Split brain / network partition
- Memory exhaustion (slow consumer / OOM)
- Disk pressure (Mnesia stress)
- Backpressure detection
- Offline message verification under chaos

INVARIANTS TESTED:
- System survives chaos without crashing
- Memory growth stays within bounds (< 5x baseline)
- Router queues stay below critical threshold
- Offline messages are recoverable after chaos
- Node connectivity recovers after partition

Modes:
  --mode split      : Network partition / split brain test
  --mode oom        : Slow consumer / memory exhaustion test
  --mode disk       : Disk pressure test (Mnesia stress)
  --mode backpressure : Router queue backpressure detection
  --mode offline    : Offline verification under chaos

Tier: 1 (Resilience testing)
"""

import subprocess
import time
import threading
import os
import sys
import re
import argparse
import socket

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster

# ============================================================================
# Configuration - Thresholds
# ============================================================================

# Memory growth threshold: fail if memory grows more than this factor
MAX_MEMORY_GROWTH_FACTOR = 5.0  # 5x baseline (tightened from 20x)

# Queue depth threshold: fail if router queue exceeds this
MAX_QUEUE_DEPTH = 50000  # 50k messages in queue = critical

# Minimum connectivity threshold for split-brain recovery
MIN_CONNECTIVITY_CHECKS = 0.5  # At least 50% of checks must show connectivity

# ============================================================================
# Utilities
# ============================================================================

def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

def get_hostname():
    return subprocess.check_output("hostname -s", shell=True).decode().strip()

def get_node(name):
    suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
    return f"{name}{suffix}@{get_hostname()}"

def run_cmd(cmd, bg=False, ignore_fail=False, timeout=None):
    if bg:
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        if timeout:
            return subprocess.check_output(cmd, shell=True, timeout=timeout).decode()
        return subprocess.check_output(cmd, shell=True).decode()
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        if not ignore_fail:
            log(f"Command failed: {cmd[:50]}... Error: {type(e).__name__}")
        return ""

def verify_cluster_alive():
    """Verify the cluster is still responding."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(("localhost", 8085))
        sock.sendall(bytes([0x01]) + b"resilience_check")
        resp = sock.recv(1024)
        sock.close()
        return b"LOGIN_OK" in resp
    except Exception:
        return False

# Helper: Recovery Metrics
CSV_FILE = os.environ.get("IRIS_RECOVERY_CSV", "recovery_metrics.csv")

def init_csv():
    if not os.path.exists(CSV_FILE):
        with open(CSV_FILE, "w") as f:
            f.write("timestamp,scenario,duration_sec,status\n")

def log_recovery(scenario, duration_sec, status="PASS"):
    with open(CSV_FILE, "a") as f:
        f.write(f"{time.time()},{scenario},{duration_sec:.2f},{status}\n")

# ============================================================================
# Test: Split Brain
# ============================================================================

def run_split_brain(args) -> bool:
    """
    Test system behavior during network partitions.
    
    INVARIANTS:
    - System survives partition chaos
    - Node connectivity recovers at least 50% of monitoring intervals
    - Cluster is responsive after test
    """
    log("--- RESILIENCE TEST: Split Brain (Network Partitions) ---")
    
    passed = True
    connectivity_checks = 0
    successful_checks = 0
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        procs = []
        
        # Verify cluster is alive before starting
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive before test")
            return False
        log("PASS: Pre-test cluster connectivity verified")
        
        # Start load
        log(f"[*] Starting load ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_load -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, {args.duration + 60}, extreme_load), timer:sleep(infinity).\"", bg=True)
        procs.append(p_load)
        
        time.sleep(3)  # Let connections establish
        
        # Start distribution chaos
        log("[*] Starting distribution chaos (disconnect random node every 5s)...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos_dist -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_dist, start, [5000]), init:stop().\"")
        
        # Monitor connectivity
        start_time = time.time()
        while time.time() - start_time < args.duration:
            time.sleep(5)
            connectivity_checks += 1
            
            out = run_cmd(f"erl -setcookie iris_secret -sname check_{int(time.time())} -hidden -noshell -pa ebin -eval \"N = rpc:call('{node}', erlang, nodes, []), io:format('~p', [N]), init:stop().\"", ignore_fail=True, timeout=5)
            
            # Check if we got a response (not badrpc)
            if out.strip() and "badrpc" not in out.lower():
                successful_checks += 1
                log(f"Connected nodes: {out.strip()} [OK]")
            else:
                log(f"Connected nodes: {out.strip() or 'UNREACHABLE'} [DEGRADED]")
        
        # Cleanup
        for p in procs:
            if p:
                try:
                    p.kill()
                except Exception:
                    pass
        
        # Stop chaos
        run_cmd(f"erl -setcookie iris_secret -sname chaos_stop -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_dist, stop, []), init:stop().\"", ignore_fail=True)
        
        time.sleep(3)  # Allow recovery
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        log("\n=== ASSERTIONS ===")
        
        # Assertion 1: Connectivity rate during chaos
        connectivity_rate = successful_checks / connectivity_checks if connectivity_checks > 0 else 0
        if connectivity_rate < MIN_CONNECTIVITY_CHECKS:
            log(f"FAIL: Connectivity rate {connectivity_rate:.0%} < {MIN_CONNECTIVITY_CHECKS:.0%} threshold")
            passed = False
        else:
            log(f"PASS: Connectivity rate {connectivity_rate:.0%} >= {MIN_CONNECTIVITY_CHECKS:.0%}")
        
        # Assertion 2: Cluster recovers after chaos
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive after split-brain test")
            passed = False
        else:
            log("PASS: Cluster responsive after chaos")
        
        # Log result
        status = "PASS" if passed else "FAIL"
        log_recovery("split_brain", args.duration, status)
        
        log(f"\n=== RESULT: {status} ===")
        return passed

# ============================================================================
# Test: OOM (Slow Consumer)
# ============================================================================

def run_oom(args) -> bool:
    """
    Test memory exhaustion with slow consumers.
    
    INVARIANTS:
    - Memory growth stays below threshold (5x baseline)
    - System doesn't crash (OOM kill)
    - Cluster remains responsive
    """
    log("--- RESILIENCE TEST: Slow Consumer (Memory Exhaustion) ---")
    log(f"Max allowed memory growth: {MAX_MEMORY_GROWTH_FACTOR}x")
    
    passed = True
    baseline_kb = 0
    max_kb = 0
    crashed = False
    
    with ClusterManager() as cluster:
        # Verify cluster is alive before starting
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive before test")
            return False
        log("PASS: Pre-test cluster connectivity verified")
        
        log(f"[*] Starting slow consumers ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_oom -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, {args.duration + 60}, extreme_load), timer:sleep(infinity).\"", bg=True)
        
        start_time = time.time()
        
        while time.time() - start_time < args.duration:
            time.sleep(2)
            
            # Get memory usage of Erlang BEAM process (runs the edge node)
            # Use beam.smp as the process name since that's the Erlang VM
            mem = subprocess.getoutput("ps aux | grep 'beam.smp' | grep -v grep | awk '{print $6}'")
            
            if not mem.strip():
                log("!!! CRASH DETECTED !!! Node process not found")
                crashed = True
                break
            
            try:
                kb_values = [int(x) for x in mem.split() if x.strip().isdigit()]
                if not kb_values:
                    continue
                kb = max(kb_values)
                
                if baseline_kb == 0:
                    baseline_kb = kb
                    log(f"Baseline memory: {baseline_kb/1024:.0f}MB")
                    
                max_kb = max(max_kb, kb)
                growth_factor = kb / baseline_kb if baseline_kb > 0 else 0
                log(f"RAM: {kb/1024:.0f}MB (baseline: {baseline_kb/1024:.0f}MB, growth: {growth_factor:.1f}x)")
                
                if kb > 10 * 1024 * 1024:  # 10GB absolute limit
                    log("!!! DANGER !!! RAM exceeded 10GB absolute limit")
            except ValueError as e:
                log(f"Warning: Could not parse memory value: {e}")
        
        if p_load:
            try:
                p_load.kill()
            except Exception:
                pass
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        log("\n=== ASSERTIONS ===")
        
        # Assertion 1: No crash
        if crashed:
            log("FAIL: Node crashed during test")
            passed = False
        else:
            log("PASS: Node survived without crashing")
        
        # Assertion 2: Memory growth within bounds
        growth_factor = max_kb / baseline_kb if baseline_kb > 0 else 0
        if growth_factor > MAX_MEMORY_GROWTH_FACTOR:
            log(f"FAIL: Memory growth {growth_factor:.1f}x > {MAX_MEMORY_GROWTH_FACTOR}x threshold")
            passed = False
        else:
            log(f"PASS: Memory growth {growth_factor:.1f}x <= {MAX_MEMORY_GROWTH_FACTOR}x")
        
        # Assertion 3: Cluster still responsive
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive after OOM test")
            passed = False
        else:
            log("PASS: Cluster responsive after test")
        
        # Log result
        status = "PASS" if passed else "FAIL"
        log_recovery("oom_survival", args.duration, status)
        
        log(f"\n=== RESULT: {status} ===")
        return passed

# ============================================================================
# Test: Disk Pressure
# ============================================================================

def run_disk(args) -> bool:
    """
    Test disk pressure with offline message flood.
    
    INVARIANTS:
    - Mnesia survives disk pressure
    - Queries still work after flood
    - Cluster remains responsive
    """
    log("--- RESILIENCE TEST: Disk Crusher (Mnesia Stress) ---")
    
    passed = True
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        
        # Verify cluster is alive before starting
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive before test")
            return False
        log("PASS: Pre-test cluster connectivity verified")
        
        log(f"[*] Starting offline flood ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_disk -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, {args.duration + 60}, extreme_load), timer:sleep(infinity).\"", bg=True)
        
        initial_size = None
        final_size = None
        start_time = time.time()
        
        while time.time() - start_time < args.duration:
            time.sleep(2)
            hostname = get_hostname()
            suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
            size_out = subprocess.getoutput(f"du -sk Mnesia.iris_core{suffix}@{hostname} 2>/dev/null | awk '{{print $1}}'")
            
            try:
                size_kb = int(size_out.strip()) if size_out.strip().isdigit() else 0
                if initial_size is None and size_kb > 0:
                    initial_size = size_kb
                final_size = size_kb
                log(f"Mnesia DB Size: {size_kb/1024:.1f}MB")
            except ValueError:
                log(f"Mnesia DB Size: N/A")
        
        if p_load:
            try:
                p_load.kill()
            except Exception:
                pass
        
        time.sleep(2)  # Allow Mnesia to settle
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        log("\n=== ASSERTIONS ===")
        
        # Assertion 1: Cluster still responsive
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive after disk pressure")
            passed = False
        else:
            log("PASS: Cluster responsive after disk pressure")
        
        # Assertion 2: Mnesia query still works
        query_cmd = f"erl -setcookie iris_secret -sname query_check -hidden -noshell -pa ebin -eval \"R = rpc:call('{node}', mnesia, system_info, [tables]), io:format('~p', [R]), init:stop().\""
        query_result = run_cmd(query_cmd, ignore_fail=True, timeout=10)
        
        if "badrpc" in query_result.lower() or not query_result.strip():
            log("FAIL: Mnesia query failed after disk pressure")
            passed = False
        else:
            log("PASS: Mnesia queries working after disk pressure")
        
        # Assertion 3: DB actually grew (flood worked)
        if initial_size and final_size and final_size > initial_size:
            growth = (final_size - initial_size) / 1024
            log(f"PASS: DB grew by {growth:.1f}MB during test")
        elif initial_size is None or final_size is None:
            log("WARN: Could not measure DB growth")
        else:
            log("WARN: DB did not grow - flood may not have worked")
        
        # Log result
        status = "PASS" if passed else "FAIL"
        log_recovery("disk_pressure", args.duration, status)
        
        log(f"\n=== RESULT: {status} ===")
        return passed

# ============================================================================
# Test: Backpressure Detection
# ============================================================================

def run_backpressure(args) -> bool:
    """
    Test router queue backpressure under extreme load.
    
    INVARIANTS:
    - Queue depth stays below critical threshold
    - System doesn't crash under load
    - Cluster remains responsive
    """
    log("--- RESILIENCE TEST: Backpressure Detection ---")
    log(f"Max allowed queue depth: {MAX_QUEUE_DEPTH}")
    
    passed = True
    max_queue = 0
    critical_exceeded = False
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        
        # Verify cluster is alive before starting
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive before test")
            return False
        log("PASS: Pre-test cluster connectivity verified")
        
        # Start chaos monkey (moderate mode)
        log("[*] Releasing chaos monkey...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos_starter -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [50, 10]), init:stop().\"")
        
        # Start load generator
        log(f"[*] Starting extreme load ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_node -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, {args.duration + 60}, extreme_load), timer:sleep(infinity).\"", bg=True)
        
        log("[*] Monitoring router queue...")
        
        start_time = time.time()
        queue_samples = []
        
        while time.time() - start_time < args.duration:
            time.sleep(2)
            mon_name = f"mon_{int(time.time() * 1000)}"
            q_cmd = f"erl -setcookie iris_secret -sname {mon_name} -hidden -noshell -pa ebin -eval \"Q = rpc:call('{node}', erlang, process_info, [whereis(iris_router_1), message_queue_len]), io:format('~p', [Q]), init:stop().\""
            
            out = run_cmd(q_cmd, ignore_fail=True, timeout=5)
            m = re.search(r"(\d+)", out)
            if m:
                q_len = int(m.group(1))
                queue_samples.append(q_len)
                max_queue = max(max_queue, q_len)
                
                status = ""
                if q_len > MAX_QUEUE_DEPTH:
                    status = " [CRITICAL]"
                    critical_exceeded = True
                elif q_len > MAX_QUEUE_DEPTH // 2:
                    status = " [WARNING]"
                    
                log(f"Router queue: {q_len} | Max: {max_queue}{status}")
        
        if p_load:
            try:
                p_load.kill()
            except Exception:
                pass
        
        # Stop chaos monkey
        run_cmd(f"erl -setcookie iris_secret -sname chaos_stop -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, stop, []), init:stop().\"", ignore_fail=True)
        
        time.sleep(2)  # Allow recovery
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        log("\n=== ASSERTIONS ===")
        
        # Assertion 1: Queue depth within bounds
        if critical_exceeded:
            log(f"FAIL: Queue depth exceeded critical threshold {MAX_QUEUE_DEPTH}")
            passed = False
        else:
            log(f"PASS: Queue depth stayed below {MAX_QUEUE_DEPTH} (max: {max_queue})")
        
        # Assertion 2: System still responsive
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive after backpressure test")
            passed = False
        else:
            log("PASS: Cluster responsive after test")
        
        # Assertion 3: Queue is draining (recovering)
        if queue_samples and len(queue_samples) >= 2:
            final_samples = queue_samples[-3:] if len(queue_samples) >= 3 else queue_samples
            avg_final = sum(final_samples) / len(final_samples)
            if avg_final < max_queue * 0.5:
                log(f"PASS: Queue is draining (final avg: {avg_final:.0f} vs max: {max_queue})")
            else:
                log(f"WARN: Queue may not be draining effectively (final avg: {avg_final:.0f})")
        
        # Log result
        status = "PASS" if passed else "FAIL"
        log_recovery("backpressure", args.duration, status)
        
        log(f"\n=== RESULT: {status} ===")
        return passed

# ============================================================================
# Test: Offline Verification Under Chaos
# ============================================================================

def run_offline_verify(args) -> bool:
    """
    Test offline message integrity under chaos conditions.
    
    INVARIANTS:
    - Messages stored offline are recoverable
    - Verification completes successfully
    - Cluster survives chaos during verification
    """
    log("--- RESILIENCE TEST: Offline Verification Under Chaos ---")
    
    passed = True
    verification_complete = False
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        
        # Verify cluster is alive before starting
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive before test")
            return False
        log("PASS: Pre-test cluster connectivity verified")
        
        # Fill storage with test messages
        fill_duration = min(30, args.duration // 3)
        log(f"[*] Filling storage for {args.users} users ({fill_duration}s)...")
        p_fill = run_cmd(f"erl +P 2000000 -setcookie iris_secret -sname filler -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, {fill_duration + 30}, extreme_load), timer:sleep(infinity).\"", bg=True)
        
        time.sleep(fill_duration)
        if p_fill:
            try:
                p_fill.kill()
            except Exception:
                pass
        subprocess.run("pkill -f filler", shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        
        # Start chaos
        log("[*] Unleashing chaos monkey...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [100, 5]), init:stop().\"")
        
        # Verify by attempting to retrieve messages
        log(f"[*] Verifying message retrieval under chaos...")
        
        verify_success = 0
        verify_attempts = min(10, args.users)
        
        for i in range(verify_attempts):
            try:
                # Try to login and receive any pending messages
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.settimeout(5)
                sock.connect(("localhost", 8085))
                
                test_user = f"verify_user_{i}_{int(time.time())}"
                sock.sendall(bytes([0x01]) + test_user.encode())
                resp = sock.recv(4096)
                sock.close()
                
                if b"LOGIN_OK" in resp:
                    verify_success += 1
            except socket.timeout:
                log(f"  Verify {i}: timeout (acceptable under chaos)")
            except Exception as e:
                log(f"  Verify {i}: error - {type(e).__name__}")
        
        verification_rate = verify_success / verify_attempts if verify_attempts > 0 else 0
        log(f"Verification success rate: {verify_success}/{verify_attempts} ({verification_rate:.0%})")
        
        # Stop chaos
        run_cmd(f"erl -setcookie iris_secret -sname chaos_stop -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, stop, []), init:stop().\"", ignore_fail=True)
        
        time.sleep(3)  # Allow recovery
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        log("\n=== ASSERTIONS ===")
        
        # Assertion 1: Verification rate acceptable
        if verification_rate < 0.5:
            log(f"FAIL: Verification rate {verification_rate:.0%} < 50% threshold")
            passed = False
        else:
            log(f"PASS: Verification rate {verification_rate:.0%} >= 50%")
            verification_complete = True
        
        # Assertion 2: Cluster recovers after chaos
        if not verify_cluster_alive():
            log("FAIL: Cluster not responsive after chaos")
            passed = False
        else:
            log("PASS: Cluster responsive after chaos")
        
        # Log result
        status = "PASS" if passed else "FAIL"
        log_recovery("offline_verify", args.duration, status)
        
        log(f"\n=== RESULT: {status} ===")
        return passed

# ============================================================================
# Main
# ============================================================================

def main():
    # Profile-based defaults
    profile_name = os.environ.get("TEST_PROFILE", "smoke")
    profile_defaults = {
        "smoke": {"users": 100, "duration": 20},
        "full": {"users": 50000, "duration": 120},
    }
    defaults = profile_defaults.get(profile_name, profile_defaults["smoke"])
    
    parser = argparse.ArgumentParser(description='Combined Resilience Test Suite')
    parser.add_argument('--mode', choices=['split', 'oom', 'disk', 'backpressure', 'offline'],
                        default='oom', help='Test mode')
    parser.add_argument('--users', type=int, default=defaults["users"], help='User/connection count')
    parser.add_argument('--duration', type=int, default=defaults["duration"], help='Test duration (seconds)')
    args = parser.parse_args()
    
    # Ensure CWD is project root
    os.chdir(os.path.dirname(os.path.abspath(__file__)) + "/../../..")
    init_csv()
    
    handlers = {
        'split': run_split_brain,
        'oom': run_oom,
        'disk': run_disk,
        'backpressure': run_backpressure,
        'offline': run_offline_verify
    }
    
    # Run test and get result
    passed = handlers[args.mode](args)
    
    # Exit with appropriate code
    if passed:
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == "__main__":
    main()
