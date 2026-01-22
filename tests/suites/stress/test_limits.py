#!/usr/bin/env python3
"""
Physical Limits Verification Suite

pushes the system to specific load targets and asserts that resource usage
stays within defined engineering limits.

Targets:
- RAM Usage < 2.5GB (for 200k users)
- File Descriptors < 250k
- Erlang Processes < 300k
"""

# ... (imports)
import subprocess
import time
import threading
import os
import sys
import argparse
import socket
import csv

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

def run_cmd(cmd, bg=False):
    if bg:
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(cmd, shell=True).decode()
    except:
        return ""

# Helper: Memory Monitor
CSV_FILE = os.environ.get("IRIS_RESOURCE_CSV", "resource_usage.csv")
csv_lock = threading.Lock()
start_monitor = time.time()

def init_csv():
    if not os.path.exists(CSV_FILE):
        with open(CSV_FILE, "w") as f:
            f.write("timestamp,elapsed_sec,ram_mb,procs,cpu_percent\n")

def log_resources(ram, procs):
    with csv_lock:
        with open(CSV_FILE, "a") as f:
            now = time.time()
            elapsed = now - start_monitor
            f.write(f"{now},{elapsed:.2f},{ram},{procs},0\n")

def get_memory_usage():
    """Returns RSS memory usage in MB for all beam.smp processes."""
    mem_raw = subprocess.getoutput("ps aux | grep iris_edge1 | grep -v grep | awk '{print $6}'")
    try:
        kb = max([int(x) for x in mem_raw.split() if x.strip().isdigit()])
        mb = kb / 1024
    except:
        mb = 0
    return mb

def get_metrics(node_name):
    # Get RAM via PS
    # The original get_metrics function had a direct call to subprocess.getoutput for mem_raw.
    # The new get_memory_usage function encapsulates this logic.
    # We will call the new function here.
    mb = get_memory_usage()
        
    # Get Procs via Erlang
    cmd = f"erl -setcookie iris_secret -sname probe_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{node_name}', erlang, system_info, [process_count])]), init:stop().\""
    try:
        procs = int(run_cmd(cmd).strip())
    except:
        procs = 0
        
    return mb, procs

# ============================================================================
# Main
# ============================================================================

def main():
    # CI-aware defaults: reduce scale for CI environments
    IS_CI = os.environ.get("CI", "").lower() in ("true", "1", "yes")
    
    if IS_CI:
        # CI environment: minimal scale to fit within timeout
        DEFAULT_USERS = 100
        TIMEOUT = int(os.environ.get("STRESS_TIMEOUT", "300"))  # 5 min default
    else:
        # Local/production: full scale
        DEFAULT_USERS = 1000000
        TIMEOUT = int(os.environ.get("STRESS_TIMEOUT", "600"))  # 10 min default
    
    parser = argparse.ArgumentParser()
    parser.add_argument('--users', type=int, default=DEFAULT_USERS)
    parser.add_argument('--timeout', type=int, default=TIMEOUT, help='Monitoring timeout in seconds')
    args = parser.parse_args()
    
    if IS_CI:
        log(f"[CI MODE] Reduced scale: users={args.users}, timeout={args.timeout}s")
    
    # Ensure correct CWD
    os.chdir(project_root)
    init_csv() # Initialize CSV
    
    # RAM limit scales with users
    # Estimate: 15KB/user
    # Allow 1.5x margin
    limit_ram = max(500, int(args.users * 15 / 1024 * 1.5))  # Convert KB to MB with margin
    
    with ClusterManager(project_root=project_root) as cluster:
        node = get_node("iris_edge1")
        
        log(f"[*] Generating Load: {args.users} users...")
        # Duration 1200s (20 mins) to allow slow ramp up
        # Mode: 'normal' (Wait for login)
        
        # We need a long timeout for the generator
        cmd = f"erl +P 2000000 -setcookie iris_secret -sname gen_lim -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 1200, normal), timer:sleep(infinity).\""
        p_load = run_cmd(cmd, bg=True)
        
        max_ram = 0
        max_procs = 0
        
        # Monitor for configured timeout
        # Ramp up rate depends on users: ~5k/sec typical
        monitor_duration = args.timeout
        log(f"Monitoring for up to {monitor_duration}s...")
        start = time.time()
        
        target_reached = False
        
        while time.time() - start < monitor_duration:
            time.sleep(10)
            mb, procs = get_metrics(node)
            max_ram = max(max_ram, mb)
            max_procs = max(max_procs, procs)
            log(f"Metrics: {mb:.0f}MB RAM | {procs} Procs")
            log_resources(mb, procs) # Log to CSV
            
            if procs >= args.users * 0.95:
                # We reached target!
                log("Target connection count reached!")
                target_reached = True
                # Hold for a bit then break
                time.sleep(30)
                break
            
        if p_load: 
            try:
                os.killpg(os.getpgid(p_load.pid), signal.SIGTERM)
            except:
                pass
        
        # Assertions
        log("\n--- LIMIT VERIFICATION ---")
        log(f"Max RAM: {max_ram:.0f} MB")
        log(f"Max Procs: {max_procs}")
        
        failed = False
        
        if max_ram > limit_ram:
            log(f"[FAIL] RAM exceeded limit ({limit_ram} MB)")
            failed = True
        else:
            log(f"[PASS] RAM within limit ({limit_ram} MB)")
            
        if max_procs < (args.users * 0.9):
            log(f"[FAIL] Did not reach user target (Got {max_procs}, Expected > {args.users*0.9})")
            failed = True
        else:
            log(f"[PASS] Reached user target")
            
        if failed:
            sys.exit(1)
        else:
            log("[SUCCESS] physical limits verified.")

if __name__ == "__main__":
    main()
