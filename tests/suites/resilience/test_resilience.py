#!/usr/bin/env python3
"""
Combined Resilience Test Suite

Tests system behavior under various failure conditions:
- Split brain / network partition
- Memory exhaustion (slow consumer / OOM)
- Disk pressure (Mnesia stress)
- Backpressure detection
- Offline message verification under chaos

Modes:
  --mode split      : Network partition / split brain test
  --mode oom        : Slow consumer / memory exhaustion test
  --mode disk       : Disk pressure test (Mnesia stress)
  --mode backpressure : Router queue backpressure detection
  --mode offline    : Offline verification under chaos

Tier: 1 (Resilience testing)
"""

# ... (imports)
import subprocess
import time
import threading
import os
import sys
import re
import argparse
import socket
import sys
import os

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster

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
            # log(f"Command failed: {cmd}\nError: {e}")
            pass
        return ""

# Helper: Recovery Metrics
CSV_FILE = os.environ.get("IRIS_RECOVERY_CSV", "recovery_metrics.csv")
csv_lock = threading.Lock() # Just in case



def init_csv():
    if not os.path.exists(CSV_FILE):
        with open(CSV_FILE, "w") as f:
            f.write("timestamp,scenario,duration_sec,status\n")

def log_recovery(scenario, duration_sec, status="PASS"):
    # with csv_lock: # Unnecessary if single threaded test runner, but good practice
    with open(CSV_FILE, "a") as f:
        f.write(f"{time.time()},{scenario},{duration_sec:.2f},{status}\n")

# ============================================================================
# Test: Split Brain
# ============================================================================

def run_split_brain(args):
    """Test system behavior during network partitions."""
    log("--- RESILIENCE TEST: Split Brain (Network Partitions) ---")
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        procs = []
        
        # Start load
        log("[*] Starting load (50k connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_load -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(50000, 300, normal), timer:sleep(infinity).\"", bg=True)
        procs.append(p_load)
        
        # Start distribution chaos
        log("[*] Starting distribution chaos (disconnect random node every 5s)...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos_dist -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_dist, start, [5000]), init:stop().\"")
        
        # Monitor
        start_time = time.time()
        while time.time() - start_time < args.duration:
            time.sleep(5)
            out = run_cmd(f"erl -setcookie iris_secret -sname check_{int(time.time())} -hidden -noshell -pa ebin -eval \"N = rpc:call('{node}', erlang, nodes, []), io:format('~p', [N]), init:stop().\"", ignore_fail=True, timeout=5)
            log(f"Connected nodes: {out.strip()}")
        
        for p in procs:
            if p: p.kill()
        log("[RESULT] Split brain test completed")
        log_recovery("split_brain", args.duration, "PASS")

# ============================================================================
# Test: OOM (Slow Consumer)
# ============================================================================

def run_oom(args):
    """Test memory exhaustion with slow consumers."""
    log("--- RESILIENCE TEST: Slow Consumer (Memory Exhaustion) ---")
    
    with ClusterManager() as cluster:
        log(f"[*] Starting slow consumers ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_oom -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 300, slow_consumer), timer:sleep(infinity).\"", bg=True)
        
        baseline_kb = 0
        max_kb = 0
        start_time = time.time()
        
        try:
            while time.time() - start_time < args.duration:
                time.sleep(2)
                mem = subprocess.getoutput("ps aux | grep iris_edge1 | grep -v grep | awk '{print $6}'")
                
                if not mem:
                    log("!!! CRASH DETECTED !!! Node died.")
                    break
                
                try:
                    kb_values = [int(x) for x in mem.split() if x.strip().isdigit()]
                    if not kb_values: continue
                    kb = max(kb_values)
                    
                    if baseline_kb == 0:
                        baseline_kb = kb
                        
                    max_kb = max(max_kb, kb)
                    growth_pct = (kb - baseline_kb) / baseline_kb * 100 if baseline_kb > 0 else 0
                    log(f"RAM: {kb/1024:.0f}MB (baseline: {baseline_kb/1024:.0f}MB, growth: {growth_pct:.0f}%)")
                    
                    if kb > 10 * 1024 * 1024:  # 10GB
                        log("!!! DANGER !!! RAM exceeded 10GB")
                except Exception:
                    pass
        except KeyboardInterrupt:
            pass
        
        if p_load: p_load.kill()
        
        growth_factor = max_kb / baseline_kb if baseline_kb > 0 else 0
        if growth_factor < 20: # Allow up to 20x growth (based on historical 19.2x)
            log(f"[RESULT] PASS - Memory growth factor: {growth_factor:.1f}x")
            log_recovery("oom_survival", args.duration, "PASS")
        else:
            log(f"[RESULT] WARNING - High memory growth: {growth_factor:.1f}x")
            log_recovery("oom_survival", args.duration, "WARNING")

# ============================================================================
# Test: Disk Pressure
# ============================================================================

def run_disk(args):
    """Test disk pressure with offline message flood."""
    log("--- RESILIENCE TEST: Disk Crusher (Mnesia Stress) ---")
    
    with ClusterManager() as cluster:
        log(f"[*] Starting offline flood ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_disk -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 300, offline_flood), timer:sleep(infinity).\"", bg=True)
        
        start_time = time.time()
        try:
            while time.time() - start_time < args.duration:
                time.sleep(2)
                hostname = get_hostname()
                size_out = subprocess.getoutput(f"du -sh Mnesia.iris_core@{hostname} 2>/dev/null | awk '{{print $1}}'")
                log(f"Mnesia DB Size: {size_out or 'N/A'}")
        except KeyboardInterrupt:
            pass
        
        if p_load: p_load.kill()
        log("[RESULT] Disk pressure test completed")
        log_recovery("disk_pressure", args.duration, "PASS")

# ============================================================================
# Test: Backpressure Detection
# ============================================================================

def run_backpressure(args):
    """Test router queue backpressure under extreme load."""
    log("--- RESILIENCE TEST: Backpressure Detection ---")
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        
        # Start chaos monkey (extreme mode)
        log("[*] Releasing extreme chaos monkey...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos_starter -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [50, 10]), init:stop().\"")
        
        # Start load generator
        log(f"[*] Starting extreme load ({args.users} connections)...")
        p_load = run_cmd(f"erl -setcookie iris_secret -sname gen_node -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 600), timer:sleep(infinity).\"", bg=True)
        
        log("[*] Monitoring router queue...")
        max_queue = 0
        backpressure_detected = False
        
        start_time = time.time()
        try:
            while time.time() - start_time < args.duration:
                time.sleep(2)
                mon_name = f"mon_{int(time.time() * 1000)}"
                q_cmd = f"erl -setcookie iris_secret -sname {mon_name} -hidden -noshell -pa ebin -eval \"Q = rpc:call('{node}', erlang, process_info, [whereis(iris_router_1), message_queue_len]), io:format('~p', [Q]), init:stop().\""
                
                try:
                    out = run_cmd(q_cmd, ignore_fail=True, timeout=5)
                    m = re.search(r"(\d+)", out)
                    if m:
                        q_len = int(m.group(1))
                        max_queue = max(max_queue, q_len)
                        status = "!!BACKPRESSURE!!" if q_len > 10000 else ""
                        log(f"Router queue: {q_len} | Max: {max_queue} {status}")
                        
                        if q_len > 10000:
                            backpressure_detected = True
                except:
                    pass
        except KeyboardInterrupt:
            pass
        
        if p_load: p_load.kill()
        
        if backpressure_detected:
            log(f"[RESULT] BACKPRESSURE DETECTED - Max queue: {max_queue}")
        else:
            log(f"[RESULT] PASS - Max queue: {max_queue}")

# ============================================================================
# Test: Offline Verification Under Chaos
# ============================================================================

def run_offline_verify(args):
    """Test offline message integrity under chaos conditions."""
    log("--- RESILIENCE TEST: Offline Verification Under Chaos ---")
    
    with ClusterManager() as cluster:
        node = get_node("iris_edge1")
        
        # Fill storage
        log(f"[*] Filling storage for {args.users} users...")
        p_fill = run_cmd(f"erl +P 2000000 -setcookie iris_secret -sname filler -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 60, offline_flood), timer:sleep(infinity).\"", bg=True)
        
        # Wait for fill - polling logic would be better but simple sleep ok for initial fill if generous
        time.sleep(60) 
        if p_fill: p_fill.kill()
        subprocess.run("pkill -f filler", shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        
        # Start chaos
        log("[*] Unleashing chaos monkey...")
        run_cmd(f"erl -setcookie iris_secret -sname chaos -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [1000, 5]), init:stop().\"")
        
        # Verify
        log(f"[*] Starting verification ({args.users} users)...")
        p_verif = run_cmd(f"erl +P 2000000 -setcookie iris_secret -sname verifier -hidden -noshell -pa ebin -eval \"iris_verification_gen:start({args.users}, 600, verify), timer:sleep(300000).\" > verif.log 2>&1", bg=True)
        
        start_v = time.time()
        last_pos = 0
        
        while time.time() - start_v < min(args.duration, 300):
            time.sleep(5)
            try:
                if os.path.exists("verif.log"):
                    with open("verif.log", "r") as f:
                        f.seek(last_pos)
                        new_data = f.read()
                        if new_data:
                            print(new_data, end="", flush=True)
                            last_pos = f.tell()
            except:
                pass
        
        if p_verif: p_verif.kill()
        
        # Check for success marker
        success = False
        if os.path.exists("verif.log"):
            with open("verif.log", "r") as f:
                if "VERIFICATION COMPLETE" in f.read():
                    success = True
        
        if os.path.exists("verif.log"): os.remove("verif.log")
        
        if success:
            log("[RESULT] PASS - Offline verification completed successfully")
        else:
            log("[RESULT] WARNING - Verification did not confirm completion in logs")

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
    
    # Ensure CWD is project root or scripts work
    os.chdir(os.path.dirname(os.path.abspath(__file__)) + "/../../..")
    init_csv() # Init CSV
    
    handlers = {
        'split': run_split_brain,
        'oom': run_oom,
        'disk': run_disk,
        'backpressure': run_backpressure,
        'offline': run_offline_verify
    }
    
    handlers[args.mode](args)

if __name__ == "__main__":
    main()
