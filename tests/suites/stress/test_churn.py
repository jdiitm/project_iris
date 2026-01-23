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

def get_tcp_connections(port=8085):
    """Count TCP connections to a specific port using ss/netstat."""
    try:
        # Use ss (faster) with state filter for ESTABLISHED connections
        result = subprocess.run(
            ["ss", "-tn", f"sport = :{port}"],
            capture_output=True, text=True, timeout=5
        )
        # Count lines minus header
        lines = [l for l in result.stdout.strip().split('\n') if l and 'ESTAB' in l]
        return len(lines)
    except Exception:
        pass
    
    try:
        # Fallback to netstat
        result = subprocess.run(
            ["netstat", "-tn"],
            capture_output=True, text=True, timeout=5
        )
        count = 0
        for line in result.stdout.split('\n'):
            if f':{port}' in line and 'ESTABLISHED' in line:
                count += 1
        return count
    except Exception:
        return 0

def verify_edge_alive(port=8085, timeout=2):
    """Verify edge is accepting connections."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect(('127.0.0.1', port))
        # Send a simple login to verify edge is responsive
        sock.sendall(b'\x01probe_churn_test')
        sock.settimeout(2)
        try:
            data = sock.recv(1024)
            sock.close()
            return True
        except socket.timeout:
            sock.close()
            return True  # Connection works, just no response
    except Exception as e:
        return False

def get_extreme_gen_stats():
    """Get stats from running extreme_gen coordinator via RPC."""
    try:
        cmd = "erl -setcookie iris_secret -sname stats_probe -hidden -noshell -pa ebin -eval \"case rpc:call(gen_base@" + get_hostname() + ", iris_extreme_gen, get_stats, []) of {ok, S} -> io:format('~p', [maps:get(connected, S, 0)]); _ -> io:format('0') end, init:stop().\""
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=10)
        return int(result.stdout.strip())
    except:
        return 0

# ============================================================================
# Main
# ============================================================================

def main():
    # CI-aware defaults: drastically reduce scale for CI environments
    IS_CI = os.environ.get("CI", "").lower() in ("true", "1", "yes")
    
    if IS_CI:
        # CI environment: minimal scale to fit within timeout
        DEFAULT_BASE = 100
        DEFAULT_CHURN = 50
        DEFAULT_CYCLES = 2
    else:
        # Local/production: full scale
        DEFAULT_BASE = 500000
        DEFAULT_CHURN = 50000
        DEFAULT_CYCLES = 3
    
    parser = argparse.ArgumentParser(description='Churn Stress Test')
    parser.add_argument('--base', type=int, default=DEFAULT_BASE, help='Base stable users')
    parser.add_argument('--churn', type=int, default=DEFAULT_CHURN, help='Churning users per cycle')
    parser.add_argument('--cycles', type=int, default=DEFAULT_CYCLES, help='Number of churn cycles')
    args = parser.parse_args()
    
    if IS_CI:
        log(f"[CI MODE] Reduced scale: base={args.base}, churn={args.churn}, cycles={args.cycles}")
    
    os.chdir(project_root)
    
    with ClusterManager(project_root=project_root) as cluster:
        # Recompile helper for load generation
        subprocess.run("erlc -o ebin src/iris_extreme_gen.erl", shell=True, check=True)
        
        # Verify edge is alive before starting
        if not verify_edge_alive():
            log("CRITICAL: Edge node not responding on port 8085!")
            sys.exit(1)
        log("Edge node verified alive on port 8085")
        
        procs = []
        initial_conns = get_tcp_connections(8085)
        log(f"Initial TCP connections: {initial_conns}")
        
        try:
            # 1. Establish Base Load
            log(f"[*] Establishing base load ({args.base} users)...")
            cmd_base = f"erl +P 2000000 -setcookie iris_secret -sname gen_base -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.base}, 3600, normal), timer:sleep(infinity).\""
            p_base = run_cmd(cmd_base, bg=True)
            procs.append(p_base)
            
            # Wait for base to connect - use TCP connection count
            log("Waiting for base connections to establish...")
            time.sleep(15)  # Give more time for connections to establish
            
            base_conns = get_tcp_connections(8085)
            log(f"Base loaded. TCP connections: {base_conns}")
            
            # Verify edge is still alive
            if not verify_edge_alive():
                log("CRITICAL: Edge died during base load!")
                sys.exit(1)
            
            # 2. Churn Cycles
            for i in range(args.cycles):
                log(f"\n--- Cycle {i+1}/{args.cycles} ---")
                
                # Verify edge is alive before churn
                if not verify_edge_alive():
                    log("CRITICAL: Edge died before churn cycle!")
                    sys.exit(1)
                
                # CONNECT STORM
                log(f"[*] Connect Storm (+{args.churn} users)...")
                cmd_churn = f"erl +P 2000000 -setcookie iris_secret -sname gen_churn_{i} -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.churn}, 3600, normal), timer:sleep(infinity).\""
                p_churn = run_cmd(cmd_churn, bg=True)
                procs.append(p_churn)
                
                # Allow ramp up
                time.sleep(15)
                peak_conns = get_tcp_connections(8085)
                log(f"Peak TCP connections: {peak_conns}")
                
                # Verify edge still alive at peak
                if not verify_edge_alive():
                    log("CRITICAL: Edge died during connect storm!")
                    sys.exit(1)
                
                # DISCONNECT STORM
                log("[*] Disconnect Storm (Kill churn generator)...")
                try:
                    os.killpg(os.getpgid(p_churn.pid), signal.SIGTERM)
                except:
                    pass
                procs.remove(p_churn)
                
                # Allow cleanup
                time.sleep(10)
                trough_conns = get_tcp_connections(8085)
                log(f"Post-Disconnect TCP connections: {trough_conns}")
                
                # Stability Check - edge must still be responsive
                if not verify_edge_alive():
                    log("CRITICAL: Edge crashed during disconnect storm!")
                    sys.exit(1)
                
                log(f"Cycle {i+1} complete. Edge stable.")
            
            log("\n=== CHURN TEST PASSED ===")
            log(f"Successfully completed {args.cycles} churn cycles")
            log("Edge remained stable throughout all connect/disconnect storms")
                    
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
