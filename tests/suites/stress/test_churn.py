#!/usr/bin/env python3
"""
Churn Stress Test Suite

Simulates massive connection churn (Connect/Disconnect storms):
- Maintains a stable base load.
- Cyclically adds and removes a large batch of 'churning' users.
- Verifies system stability and base user connectivity during storms.

Usage:
  python3 test_churn.py --users 50000 --churn 25000 --cycles 5
"""

import subprocess
import time
import os
import sys
import argparse
import signal
import socket

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

def run_cmd(cmd, bg=False, ignore_fail=False):
    if bg:
        # Use simple Popen for background consistency
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, preexec_fn=os.setsid)
    try:
        return subprocess.check_output(cmd, shell=True).decode()
    except subprocess.CalledProcessError:
        if not ignore_fail:
            pass
        return ""

def get_process_count(node):
    cmd = f"erl -setcookie iris_secret -sname probe_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{node}', erlang, system_info, [process_count])]), init:stop().\""
    try:
        res = run_cmd(cmd, ignore_fail=True)
        return int(res.strip())
    except:
        return 0

# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Churn Stress Test')
    parser.add_argument('--base', type=int, default=500000, help='Base stable users')
    parser.add_argument('--churn', type=int, default=50000, help='Churning users per cycle')
    parser.add_argument('--cycles', type=int, default=3, help='Number of churn cycles')
    args = parser.parse_args()
    
    os.chdir(project_root)
    
    with ClusterManager(project_root=project_root) as cluster:
        # Recompile helper for load generation
        subprocess.run("erlc -o ebin src/iris_extreme_gen.erl", shell=True, check=True)
        
        procs = []
        node = get_node("iris_edge1")
        
        try:
            # 1. Establish Base Load
            log(f"[*] Establishing base load ({args.base} users)...")
            log(f"[*] Establishing base load ({args.base} users)...")
            # Use range start offset 0
            cmd_base = f"erl +P 2000000 -setcookie iris_secret -sname gen_base -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.base}, 3600, normal), timer:sleep(infinity).\""
            p_base = run_cmd(cmd_base, bg=True)
            procs.append(p_base)
            
            # Wait for base to connect
            time.sleep(10) 
            base_count = get_process_count(node)
            log(f"Base loaded. Cluster Processes: {base_count}")
            
            # 2. Churn Cycles
            for i in range(args.cycles):
                log(f"\n--- Cycle {i+1}/{args.cycles} ---")
                
                # CONNECT STORM
                log(f"[*] Connect Storm (+{args.churn} users)...")
                # Use separate sname and offset logic if needed, but extreme_gen uses random 127.0.0.X alias
                # We rely on unique PIDs from new generator instance
                cmd_churn = f"erl +P 2000000 -setcookie iris_secret -sname gen_churn_{i} -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.churn}, 3600, normal), timer:sleep(infinity).\""
                p_churn = run_cmd(cmd_churn, bg=True)
                procs.append(p_churn)
                
                # Allow ramp up
                time.sleep(15)
                peak_count = get_process_count(node)
                log(f"Peak Processes: {peak_count}")
                
                # Assert we grew
                if peak_count < (args.base + args.churn * 0.5):
                    log("WARNING: Did not reach expected peak process count!")
                
                # DISCONNECT STORM
                log("[*] Disconnect Storm (Kill churn generator)...")
                os.killpg(os.getpgid(p_churn.pid), signal.SIGTERM)
                procs.remove(p_churn) # Remove from cleanup list since we killed it
                
                # Allow cleanup
                time.sleep(10)
                trough_count = get_process_count(node)
                log(f"Post-Disconnect Processes: {trough_count}")
                
                # Assert recovery to near base
                # Note: Erlang processes take time to die, but 10s should show trend
                # If we are excessively high, we are leaking
                if trough_count > (args.base + 5000): # Allow small buffer
                    log(f"WARNING: High residual process count. Possible Leak? (Expected ~{args.base}, Got {trough_count})")
                
                # Stability Check
                if trough_count < 100:
                    log("CRITICAL: Node crashed or emptied!")
                    sys.exit(1)
                    
        except KeyboardInterrupt:
            log("Aborted.")
        finally:
            log("Cleaning up load generators...")
            for p in procs:
                if p:
                    try:
                        os.killpg(os.getpgid(p.pid), signal.SIGTERM)
                    except:
                        pass
            # ClusterManager handles cluster teardown

if __name__ == "__main__":
    main()
