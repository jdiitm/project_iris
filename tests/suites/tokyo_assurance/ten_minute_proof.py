#!/usr/bin/env python3
"""
10-Minute Global Proof Run (Senior Principal Engineer Assurance)
==============================================================

Orchestrates a 10-minute continuous assurance run against the Live Tokyo Edge.
- 50k Simulated Users (via iris_extreme_gen)
- Metrics Collection
- Manual Failover Prompts

Usage:
    python3 tests/suites/tokyo_assurance/ten_minute_proof.py --edge-node iris_edge1@100.82.212.50
"""

import sys
import os
import time
import argparse
import subprocess
import csv
import signal
from datetime import datetime

# ============================================================================
# Configuration
# ============================================================================

DURATION = 600  # 10 Minutes
USERS = 50000   # 50k Concurrency
METRICS_CSV = "tokyo_proof_metrics.csv"
LOG_FILE = "proof_run.log"

def log(msg, level="INFO"):
    ts = datetime.now().strftime("%H:%M:%S")
    formatted = f"[{ts}] [{level}] {msg}"
    print(formatted, flush=True)
    with open(LOG_FILE, "a") as f:
        f.write(formatted + "\n")

def run_erl_cmd(cmd, bg=False):
    # Use a unique name for the controller to avoid conflicts
    full_cmd = f"erl -name proof_ctrl_{int(time.time()*1000)}@127.0.0.1 -setcookie iris_secret -hidden -noshell -pa ebin -eval \"{cmd}\""
    if bg:
        return subprocess.Popen(full_cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(full_cmd, shell=True).decode()
    except subprocess.CalledProcessError:
        return ""

class MetricsCollector:
    def __init__(self, filename):
        self.filename = filename
        self.start_time = time.time()
        with open(self.filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['Timestamp', 'Elapsed', 'Event', 'Status', 'Sent', 'Recv', 'LatencyAvg', 'Throughput'])

    def record(self, event, status="OK", sent=0, recv=0, latency=0, throughput=0):
        current = time.time()
        elapsed = round(current - self.start_time, 1)
        ts = datetime.now().isoformat()
        with open(self.filename, 'a', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([ts, elapsed, event, status, sent, recv, latency, throughput])

def main():
    parser = argparse.ArgumentParser(description='10-Minute Proof Run')
    parser.add_argument('--edge-node', type=str, required=True, help='Edge Node Name (e.g. iris_edge1@IP)')
    args = parser.parse_args()

    collector = MetricsCollector(METRICS_CSV)
    
    log("================================================================", "MAIN")
    log("       TOKYO EDGE ASSURANCE: 10-MINUTE PROOF RUN               ", "MAIN")
    log("================================================================", "MAIN")
    log(f"Target: {args.edge_node}", "CONFIG")
    log(f"Load: {USERS} Users", "CONFIG")
    log(f"Duration: {DURATION}s", "CONFIG")
    
    # 1. Start Evidence Collection
    log("Starting background load generator...", "SETUP")
    
    # Parse IP from args.edge_node (e.g. iris_edge1@100.82.212.50)
    try:
        edge_ip_str = args.edge_node.split('@')[1]
        # Convert to Erlang tuple {100.82.212.50}
        ip_parts = edge_ip_str.split('.')
        erl_ip = f"{{{ip_parts[0]},{ip_parts[1]},{ip_parts[2]},{ip_parts[3]}}}"
    except:
        erl_ip = "{127,0,0,1}" # Fallback
        
    edge_port = 8080

    # Switch to 'extreme_load' mode for 100M/day scale with bursts
    # usage: start(Count, Duration, Mode, Host, Port)
    load_cmd = f"iris_extreme_gen:start({USERS}, {DURATION + 60}, extreme_load, {erl_ip}, {edge_port}), timer:sleep(infinity)."
    
    # Fixed Node Name for RPC - CHANGED TO DYNAMIC TO AVOID COLLISIONS
    loadgen_node = f"loadgen_{int(time.time())}@127.0.0.1"
    
    # Redirect output to file for debugging
    log_file = open("loadgen.log", "w")
    
    # We run a local heavy node that peers with the Edge.
    load_proc = subprocess.Popen(
        f"erl -name {loadgen_node} -setcookie iris_secret -pa ebin -hidden -noshell -eval \"net_adm:ping('{args.edge_node}'), {load_cmd}\"",
        shell=True, stdout=log_file, stderr=log_file
    )
    
    log(f"Load Generator Started: {loadgen_node} -> {args.edge_node}", "LOAD")
    collector.record("LoadStart")
    
    start_time = time.time()
    next_announcement = 60
    
    try:
        while time.time() - start_time < DURATION:
            elapsed = time.time() - start_time
            remaining = DURATION - elapsed
            
            # Check edge node liveness AND connectivity AND stats via RPC
            # 1. Check Connectivity (Hidden Node aware)
            # 2. Check Stats from LoadGen
            
            cmd = (
                f"Nodes = rpc:call('{args.edge_node}', erlang, nodes, [connected]), "
                f"Count = case is_list(Nodes) of true -> length(Nodes); false -> 0 end, "
                f"Stats = rpc:call('{loadgen_node}', iris_extreme_gen, get_stats, []), "
                f"io:format('~p', [{{net_adm:ping('{args.edge_node}'), Count, Stats}}]), "
                f"init:stop()."
            )
            res = run_erl_cmd(cmd)
            
            # Default values
            is_alive = False
            node_count = 0
            sent = 0
            recv = 0
            
            # Parsing logic for: {pong, 1, {ok, 500, 500}}
            if "pong" in res:
                is_alive = True
                try:
                    # Remove outer braces roughly
                    clean = res.strip().strip("'").strip('"')
                    
                    if "{ok," in clean:
                        # Extract Stats: {ok, S, R}
                        # Find the stats tuple
                        stats_part = clean.split("{ok,")[1].split("}")[0]
                        s_str, r_str = stats_part.split(',')
                        sent = int(s_str.strip())
                        recv = int(r_str.strip())
                        
                    # Extract Node Count
                    # structure is {pong, Count, ...}
                    # split by comma, index 1
                    parts = clean.split(',')
                    node_count = int(parts[1].strip())
                    
                except Exception as e:
                    # Keep defaults if parsing fails
                    pass
            
            status = "Alive"
            level = "INFO"
            
            if not is_alive:
                status = "DOWN"
                level = "FATAL"
                log(f"Edge Node Unreachable! {res}", "FATAL")
                collector.record("HealthCheck", "FAIL", sent, recv)
            elif node_count == 0:
                status = "ISOLATED"
                level = "FATAL"
                log(f"Edge Node ISOLATED! (No peers visible)", "FATAL")
                collector.record("HealthCheck", "ISOLATED", sent, recv)
            else:
                 collector.record("HealthCheck", "OK", sent, recv)
            
            # Timeline Events
            if 120 < elapsed < 130:
                log(">>> ACTION REQUIRED: MANUALLY TERMINATE IRIS_CORE1 NOW! <<<", "ACTION")
                log(">>> (You have 60 seconds) <<<", "ACTION")
                collector.record("FailoverPrompt", "OK", sent, recv)
                
            if 300 < elapsed < 310:
                 log(">>> ACTION REQUIRED: BRING CORE1 BACK UP (RECOVERY) <<<", "ACTION")
                 collector.record("RecoveryPrompt", "OK", sent, recv)

            if elapsed > next_announcement:
                log(f"Time Remaining: {int(remaining)}s - System Status: {status} - Traffic: {sent}/{recv}", level)
                next_announcement += 60
            
            time.sleep(5)
            
    except KeyboardInterrupt:
        log("Run Aborted by User", "WARN")
    finally:
        log("Stopping Load Generator...", "CLEANUP")
        load_proc.terminate()
        
    log("PROOF RUN COMPLETE. Generating Verdict...", "MAIN")
    
    # Quick Verdict Analysis
    with open(METRICS_CSV, 'r') as f:
        rows = list(csv.reader(f))
        headers = rows[0]
        data = rows[1:]
        
        # Check for outages
        failures = [r for r in data if "FAIL" in r or "ISOLATED" in r]
        
        # Check traffic
        traffic_ok = True
        try:
            sent_idx = headers.index('Sent')
            recv_idx = headers.index('Recv')
            last_row = data[-1]
            total_sent = int(last_row[sent_idx])
            total_recv = int(last_row[recv_idx])
            
            if total_sent == 0:
                log("WARNING: No traffic sent during run!", "WARN")
                traffic_ok = False
            elif total_recv < total_sent * 0.9: # Allow 10% loss
                log(f"WARNING: High packet loss! Sent={total_sent}, Recv={total_recv}", "WARN")
                traffic_ok = False
        except:
             log("WARNING: Could not verify traffic stats.", "WARN")
    
    if not failures and traffic_ok:
        log("VERDICT: SUCCESS (No outages, Traffic Verified)", "VERDICT")
        log(f"Evidence saved to {METRICS_CSV}", "VERDICT")
    else:
        log(f"VERDICT: FAIL ({len(failures)} failures, TrafficOK={traffic_ok})", "VERDICT")

if __name__ == "__main__":
    main()
