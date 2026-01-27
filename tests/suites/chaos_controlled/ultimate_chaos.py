#!/usr/bin/env python3
"""
Ultimate Chaos Test

Extreme chaos testing with:
- High connection count
- Protocol corruption (garbage messages)
- System process killing (router, workers)
- Concurrent load generation

INVARIANTS TESTED:
1. System remains responsive after extreme chaos
2. Process count healthy (no crash, no leak)
3. Memory within bounds
4. Post-chaos connectivity works

Profiles:
  smoke: 100 users, 30s duration
  full:  1M users, 5 min duration

Tier: 2 (Manual/nightly)
"""

import subprocess
import time
import os
import sys
import socket

# --- Configuration ---
CORE_NODE = "iris_core"
EDGE_NODE = "iris_edge1"

# Profile-based configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
PROFILES = {
    "smoke": {
        "user_count": 100,
        "duration": 30,
        "ramp_time": 5,
        "chaos_time": 10,
        "recovery_time": 10,
        "max_memory_mb": 500,
    },
    "full": {
        "user_count": 1000000,
        "duration": 300,
        "ramp_time": 60,
        "chaos_time": 120,
        "recovery_time": 60,
        "max_memory_mb": 8000,
    },
}

CONFIG = PROFILES.get(TEST_PROFILE, PROFILES["smoke"])

# ============================================================================
# Utilities
# ============================================================================

def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

def get_hostname():
    return subprocess.check_output("hostname -s", shell=True).decode().strip()

def get_node_name(short_name):
    suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
    return f"{short_name}{suffix}@{get_hostname()}"

EDGE_FULL = get_node_name(EDGE_NODE)
HOSTNAME = get_hostname()

def run_cmd(cmd, async_run=False, ignore_fail=False):
    if async_run:
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(cmd, shell=True, stderr=subprocess.DEVNULL).decode()
    except subprocess.CalledProcessError as e:
        if not ignore_fail:
            log(f"Command failed: {cmd[:50]}... (exit code {e.returncode})")
        return ""
    except Exception as e:
        if not ignore_fail:
            log(f"Command error: {cmd[:50]}... ({e})")
        return ""

def setup_ip_aliases():
    """Set up IP aliases (optional, requires sudo)."""
    log("[INIT] Attempting IP aliases setup...")
    result = subprocess.run("sudo -n true", shell=True, capture_output=True)
    if result.returncode != 0:
        log("[INIT] Skipping IP aliases (sudo not available)")
        return
    
    for i in range(1, 21):
        run_cmd(f"sudo -n ifconfig lo:{i} 127.0.0.{i} up 2>/dev/null", ignore_fail=True)

def cleanup_ip_aliases():
    """Clean up IP aliases."""
    result = subprocess.run("sudo -n true", shell=True, capture_output=True)
    if result.returncode != 0:
        return
    
    log("[CLEAN] Cleaning IP aliases...")
    for i in range(1, 21):
        run_cmd(f"sudo -n ifconfig lo:{i} down 2>/dev/null", ignore_fail=True)

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}", flush=True)

# ============================================================================
# Metrics Collection
# ============================================================================

class Metrics:
    """Track test metrics."""
    def __init__(self):
        self.baseline_procs = None
        self.peak_procs = 0
        self.peak_mem = 0
        self.total_sent = 0
        self.total_recv = 0

metrics = Metrics()

def get_load_stats():
    """Get message stats from the load generator via RPC."""
    loader_node = f"loader@{HOSTNAME}"
    cmd = f"erl -setcookie iris_secret -sname stats_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"case rpc:call('{loader_node}', iris_extreme_gen, get_stats, [], 3000) of {{ok, S, R}} -> io:format('~p,~p', [S, R]); _ -> io:format('0,0') end, init:stop().\""
    result = run_cmd(cmd, ignore_fail=True).strip()
    try:
        parts = result.split(',')
        if len(parts) == 2:
            return int(parts[0]), int(parts[1])
    except:
        pass
    return 0, 0

def get_process_count():
    """Get process count from edge node."""
    cmd = f"erl -setcookie iris_secret -sname probe_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, system_info, [process_count])]), init:stop().\""
    result = run_cmd(cmd, ignore_fail=True).strip()
    try:
        return int(result)
    except:
        return result

def get_memory_mb():
    """Get memory usage from edge node in MB."""
    cmd = f"erl -setcookie iris_secret -sname probe_m_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, memory, [total])]), init:stop().\""
    result = run_cmd(cmd, ignore_fail=True).strip()
    try:
        return int(result) / 1024 / 1024
    except:
        return 0

def monitor_system(duration, tag, interval=3):
    """Monitor system for duration, sampling at interval."""
    start = time.time()
    while time.time() - start < duration:
        time.sleep(interval)
        try:
            proc_count = get_process_count()
            mem_mb = get_memory_mb()
            sent, recv = get_load_stats()
            
            metrics.total_sent = sent
            metrics.total_recv = recv
            
            # Track peaks
            if isinstance(proc_count, int):
                if metrics.baseline_procs is None:
                    metrics.baseline_procs = proc_count
                metrics.peak_procs = max(metrics.peak_procs, proc_count)
            metrics.peak_mem = max(metrics.peak_mem, mem_mb)
            
            # Calculate delta
            delta = ""
            if isinstance(proc_count, int) and metrics.baseline_procs:
                d = proc_count - metrics.baseline_procs
                delta = f" ({'+' if d >= 0 else ''}{d})"
            
            elapsed = time.time() - start
            log(f"[{tag}] {elapsed:.0f}s | Procs: {proc_count}{delta} | Mem: {mem_mb:.0f}MB | Msgs: {sent} sent, {recv} recv")
            
        except Exception as e:
            log(f"[{tag}] Monitoring error: {e}")

def wait_for_node(node_name, timeout=30):
    """Wait for an Erlang node to be reachable."""
    start = time.time()
    while time.time() - start < timeout:
        cmd = f"erl -setcookie iris_secret -sname check_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"case net_adm:ping('{node_name}') of pong -> io:format('OK'), init:stop(0); _ -> init:stop(1) end.\""
        result = run_cmd(cmd, ignore_fail=True)
        if "OK" in result:
            return True
        time.sleep(1)
    return False

# ============================================================================
# Main Test
# ============================================================================

def main():
    # Check if running under test runner (cluster already managed)
    running_under_test_runner = os.environ.get("IRIS_TEST_RUNNER") == "1"
    manage_cluster = not running_under_test_runner
    
    print_section(f"PROJECT IRIS: ULTIMATE CHAOS ({CONFIG['user_count']} USERS)")
    log(f"Profile: {TEST_PROFILE}")
    log(f"Duration: {CONFIG['duration']}s")
    log(f"Max memory: {CONFIG['max_memory_mb']}MB")
    if not manage_cluster:
        log("Cluster management: External (test runner)")
    
    # 0. Prep
    setup_ip_aliases()
    
    if manage_cluster:
        subprocess.run("pkill -9 beam.smp", shell=True, capture_output=True)
        subprocess.run("pkill -9 epmd", shell=True, capture_output=True)
        time.sleep(2)
        
        # Build
        erl_path = "/usr/bin/erl" if os.path.exists("/usr/bin/erl") else "erl"
        suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
        make_cmd = f"PATH=/usr/bin:$PATH NODE_SUFFIX={suffix} make ERL={erl_path}"
        
        log("Building project...")
        run_cmd(f"{make_cmd} all")
        subprocess.run("erlc -o ebin test_utils/chaos_resources.erl test_utils/chaos_monkey.erl test_utils/iris_extreme_gen.erl", 
                      shell=True, capture_output=True)
        
        # Start nodes
        log("Starting core node...")
        run_cmd(f"{make_cmd} start_core")
        time.sleep(3)
        
        log("Starting edge node...")
        run_cmd(f"{make_cmd} start_edge1")
        time.sleep(3)
        
        # Wait for edge to be ready
        log("Waiting for nodes to be ready...")
        edge_name = get_node_name("iris_edge1")
        if not wait_for_node(edge_name, timeout=20):
            log(f"ERROR: Edge node {edge_name} not reachable")
            # Try socket check
            try:
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.settimeout(5)
                sock.connect(("localhost", 8085))
                sock.close()
                log("Socket check passed, continuing...")
            except Exception as e:
                log(f"FATAL: Cannot connect to edge: {e}")
                sys.exit(1)
        else:
            log(f"Nodes ready: {edge_name}")
    else:
        # Running under test runner - just verify connectivity
        log("Verifying cluster connectivity...")
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect(("localhost", 8085))
            sock.sendall(bytes([0x01]) + b"ultimate_precheck")
            resp = sock.recv(1024)
            sock.close()
            if b"LOGIN_OK" not in resp:
                log("FATAL: Cluster not accepting connections")
                sys.exit(1)
            log("Cluster connectivity: OK")
        except Exception as e:
            log(f"FATAL: Cannot connect to cluster: {e}")
            sys.exit(1)
    
    processes = []
    
    try:
        # ================================================================
        # PHASE 1: Ramp Up with extreme_load mode
        # ================================================================
        print_section("PHASE 1: RAMP UP")
        log(f"Starting {CONFIG['user_count']} users with extreme_load mode")
        
        load_cmd = f"/usr/bin/erl +P 2000000 -setcookie iris_secret -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({CONFIG['user_count']}, {CONFIG['duration'] + 60}, extreme_load), timer:sleep(infinity).\""
        processes.append(run_cmd(load_cmd, async_run=True))
        
        time.sleep(2)
        monitor_system(CONFIG['ramp_time'], "RAMP-UP")
        
        # ================================================================
        # PHASE 2: Protocol Corruption
        # ================================================================
        print_section("PHASE 2: PROTOCOL CORRUPTION")
        
        # Start chaos with corrupt_pids mode
        log("[CHAOS] Starting PID Corruption (garbage messages)...")
        run_cmd(f"erl -setcookie iris_secret -sname monkey_corrupt -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [100, corrupt_pids]), init:stop().\"")
        
        # Kill system processes periodically
        log("[CHAOS] Starting System Sniper (kill router)...")
        run_cmd(f"erl -setcookie iris_secret -sname monkey_sniper -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, kill_system, [5000, [iris_router, iris_router_worker]]), init:stop().\"")
        
        monitor_system(CONFIG['chaos_time'], "CHAOS")
        
        # ================================================================
        # PHASE 3: Recovery
        # ================================================================
        print_section("PHASE 3: RECOVERY")
        
        log("[CHAOS] Stopping chaos monkey...")
        run_cmd(f"erl -setcookie iris_secret -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, stop, []), init:stop().\"")
        
        monitor_system(CONFIG['recovery_time'], "RECOVERY")
        
        # ================================================================
        # METRICS SUMMARY
        # ================================================================
        print_section("METRICS SUMMARY")
        
        log(f"Baseline processes: {metrics.baseline_procs}")
        log(f"Peak processes: {metrics.peak_procs}")
        log(f"Peak memory: {metrics.peak_mem:.1f} MB")
        log(f"Total messages sent: {metrics.total_sent}")
        log(f"Total messages received: {metrics.total_recv}")
        
        proc_delta = metrics.peak_procs - (metrics.baseline_procs or metrics.peak_procs)
        log(f"Process growth: +{proc_delta}")
        
        # ================================================================
        # ASSERTIONS
        # ================================================================
        print_section("ASSERTIONS")
        
        passed = True
        
        # Assertion 1: System still responsive
        final_procs = get_process_count()
        if not isinstance(final_procs, int) or final_procs == 0:
            log(f"FAIL: System unresponsive (got: {final_procs})")
            passed = False
        else:
            log(f"PASS: System responsive (procs: {final_procs})")
        
        # Assertion 2: Process count healthy (no crash, no major leak)
        # Note: A minimal Erlang node has ~30-50 processes, so < 20 indicates a crash
        if isinstance(final_procs, int):
            if final_procs < 20:
                log(f"FAIL: Process count too low ({final_procs}) - system may have crashed")
                passed = False
            elif final_procs > 500000:
                log(f"FAIL: Process count too high ({final_procs}) - leak?")
                passed = False
            else:
                log(f"PASS: Process count normal ({final_procs})")
        
        # Assertion 3: Load generator sent messages
        if metrics.total_sent == 0:
            log("FAIL: No messages sent - load generator failed!")
            passed = False
        else:
            log(f"PASS: Sent {metrics.total_sent} messages")
        
        # Assertion 4: Message receipt
        if metrics.total_sent > 0:
            if metrics.total_recv == 0:
                log(f"WARN: Sent {metrics.total_sent} but received 0")
            else:
                rate = (metrics.total_recv / metrics.total_sent) * 100
                log(f"PASS: Received {metrics.total_recv} messages ({rate:.1f}%)")
        
        # Assertion 5: Post-chaos connectivity
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect(("localhost", 8085))
            
            test_user = f"chaos_verify_{int(time.time())}"
            sock.sendall(bytes([0x01]) + test_user.encode())
            resp = sock.recv(1024)
            sock.close()
            
            if b"LOGIN_OK" in resp:
                log("PASS: Post-chaos login successful")
            else:
                log("FAIL: Post-chaos login failed")
                passed = False
        except Exception as e:
            log(f"FAIL: Post-chaos connection failed: {e}")
            passed = False
        
        # Assertion 6: Memory within bounds
        final_mem = get_memory_mb()
        if final_mem > CONFIG['max_memory_mb']:
            log(f"FAIL: Memory {final_mem:.0f}MB > {CONFIG['max_memory_mb']}MB")
            passed = False
        else:
            log(f"PASS: Memory {final_mem:.0f}MB within bounds")
        
        # Assertion 7: Process growth indicates load
        # Zero or negative growth with 100+ users is a test failure - load wasn't applied
        if proc_delta <= 0 and CONFIG['user_count'] >= 100:
            log(f"FAIL: Process count did not grow (delta={proc_delta}) - load generator failed")
            passed = False
        elif proc_delta < 10 and CONFIG['user_count'] >= 100:
            log(f"WARN: Process growth only +{proc_delta}")
        else:
            log(f"PASS: Process growth (+{proc_delta}) indicates load")
        
        # ================================================================
        # RESULT
        # ================================================================
        print_section("RESULT")
        
        if passed:
            log("ALL ASSERTIONS PASSED")
        else:
            log("SOME ASSERTIONS FAILED")
        
    finally:
        # Cleanup
        log("Cleaning up...")
        for p in processes:
            if p:
                try:
                    p.kill()
                except Exception:
                    pass  # Cleanup - OK to ignore
        
        if manage_cluster:
            subprocess.run("pkill -9 beam.smp", shell=True, capture_output=True)
            subprocess.run("pkill -9 epmd", shell=True, capture_output=True)
        cleanup_ip_aliases()
    
    if not passed:
        sys.exit(1)
    sys.exit(0)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        cleanup_ip_aliases()
        if os.environ.get("IRIS_TEST_RUNNER") != "1":
            subprocess.run("pkill -9 beam.smp", shell=True, capture_output=True)
            subprocess.run("pkill -9 epmd", shell=True, capture_output=True)
        sys.exit(1)
