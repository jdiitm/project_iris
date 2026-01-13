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
            writer.writerow(['Timestamp', 'Elapsed', 'Event', 'Status', 'LatencyAvg', 'Throughput'])

    def record(self, event, status="OK", latency=0, throughput=0):
        elapsed = time.time() - self.start_time
        with open(self.filename, 'a', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([datetime.now().isoformat(), f"{elapsed:.1f}", event, status, latency, throughput])

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
    
    # We use iris_extreme_gen for load. We assume it's compiled.
    # We run it locally to generate traffic towards the Edge via Erlang Distribution (simulating heavy RPC load)
    # OR we assume the edge is remote and we are just generating traffic via python clients?
    # The requirement says "Reuse existing chaos primitives".
    # iris_extreme_gen usually runs ON the cluster or CONNECTED to it.
    # We will run a local node that connects to the Edge and pushes load.
    
    # Switch to 'extreme_load' mode for 100M/day scale with bursts
    load_cmd = f"iris_extreme_gen:start({USERS}, {DURATION + 60}, extreme_load), timer:sleep(infinity)."
    
    # We run a local heavy node that peers with the Edge.
    load_proc = subprocess.Popen(
        f"erl -name loadgen_{int(time.time())}@127.0.0.1 -setcookie iris_secret -pa ebin -hidden -noshell -eval \"net_adm:ping('{args.edge_node}'), {load_cmd}\"",
        shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    
    log("Load Generator Started: 50k Users / 100M-per-day Scale (Burst)", "LOAD")
    collector.record("LoadStart")
    
    start_time = time.time()
    next_announcement = 60
    
    try:
        while time.time() - start_time < DURATION:
            elapsed = time.time() - start_time
            remaining = DURATION - elapsed
            
            # periodic status check via RPC
            # Check edge node liveness AND connectivity
            # We want to know if the Edge is CONNECTED to the cluster, not just if the VM is running.
            res = run_erl_cmd(f"nodes = nodes(), io:format('~p', [{{net_adm:ping('{args.edge_node}'), length(rpc:call('{args.edge_node}', erlang, nodes, []))}}]), init:stop().")
            
            # Output format expecting: {pong, NodeCount}
            # If rpc fails (badrpc), it might show up differently.
            
            is_alive = "pong" in res
            node_count = 0
            
            # Parse Node Count roughly
            if "pong" in res:
                try:
                    # extract number after comma
                    # e.g. {pong, 2}
                    part = res.split(',')[-1].strip().strip('}').strip('])')
                    node_count = int(part)
                except:
                    node_count = 0
            
            status = "Alive"
            level = "INFO"
            
            if not is_alive:
                status = "DOWN"
                level = "FATAL"
                log(f"Edge Node Unreachable! {res}", "FATAL")
                collector.record("HealthCheck", "FAIL")
            elif node_count == 0:
                status = "ISOLATED"
                level = "FATAL"
                log(f"Edge Node ISOLATED! (No peers visible)", "FATAL")
                collector.record("HealthCheck", "ISOLATED")
            else:
                 collector.record("HealthCheck", "OK")

            # Timeline Events
            if 120 < elapsed < 130:
                log(">>> ACTION REQUIRED: MANUALLY TERMINATE IRIS_CORE1 NOW! <<<", "ACTION")
                log(">>> (You have 60 seconds) <<<", "ACTION")
                collector.record("FailoverPrompt")
                
            if 300 < elapsed < 310:
                 log(">>> ACTION REQUIRED: BRING CORE1 BACK UP (RECOVERY) <<<", "ACTION")
                 collector.record("RecoveryPrompt")

            if elapsed > next_announcement:
                log(f"Time Remaining: {int(remaining)}s - System Status: {status}", level)
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
        failures = [r for r in rows if r[3] == "FAIL" or r[3] == "DOWN"]
    
    if not failures:
        log("VERDICT: SUCCESS (No outages detected during run)", "VERDICT")
        log(f"Evidence saved to {METRICS_CSV}", "VERDICT")
    else:
        log(f"VERDICT: FAIL ({len(failures)} failures detected)", "VERDICT")

if __name__ == "__main__":
    main()
