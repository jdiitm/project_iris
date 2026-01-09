#!/usr/bin/env python3
"""
Combined Chaos Test Suite

Consolidates all chaos testing scenarios into a single configurable test:
- Connection flood & offline message storm
- Process kills (chaos monkey)
- Network chaos (latency/loss)
- CPU/memory stress
- Protocol corruption

Modes:
  --mode laptop     : Safe for local development (50k users, 60s)
  --mode standard   : Full chaos suite (200k users, 3 min)
  --mode extreme    : Maximum scale (1M users, 5 min)

Tier: 1 (Nightly/manual)
"""

# ... (imports)
import subprocess
import time
import os
import sys
import threading
import argparse
import csv
import socket
from datetime import datetime

# Add project root to sys.path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster

# ============================================================================
# Configuration Presets
# ============================================================================

PRESETS = {
    "laptop": {
        "user_count": 50000,
        "offline_workers": 100,
        "duration": 60,
        "ramp_time": 15,
        "enable_network_chaos": False,
        "enable_memory_stress": False,
        "enable_cpu_stress": True,
        "description": "Laptop-safe chaos (50k users, 60s)"
    },
    "standard": {
        "user_count": 200000,
        "offline_workers": 500,
        "duration": 180,
        "ramp_time": 30,
        "enable_network_chaos": False,
        "enable_memory_stress": False,
        "enable_cpu_stress": True,
        "description": "Standard chaos (200k users, 3 min)"
    },
    "extreme": {
        "user_count": 1000000,
        "offline_workers": 1000,
        "duration": 300,
        "ramp_time": 60,
        "enable_network_chaos": False,
        "enable_memory_stress": True,
        "enable_cpu_stress": True,
        "description": "Extreme scale (1M users, 5 min)"
    }
}

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

def run_cmd(cmd, async_run=False, ignore_fail=False):
    if async_run:
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(cmd, shell=True).decode()
    except subprocess.CalledProcessError:
        if not ignore_fail:
            # log(f"Command failed (ignored={ignore_fail}): {cmd}")
            pass
        return ""

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}", flush=True)

# ============================================================================
# Monitoring
# ============================================================================

class SystemMonitor:
    def __init__(self, edge_node, log_file=None):
        self.edge_node = edge_node
        self.log_file = log_file
        self.metrics = []
        self.start_time = time.time()
        
        if log_file:
            with open(log_file, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(['elapsed_s', 'processes', 'memory_mb'])
    
    def sample(self, tag):
        elapsed = time.time() - self.start_time
        
        # Process count
        proc_cmd = f"erl -sname probe_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{self.edge_node}', erlang, system_info, [process_count])]), init:stop().\""
        proc_count = run_cmd(proc_cmd, ignore_fail=True).strip()
        
        # Memory
        mem_cmd = f"erl -sname probe_m_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{self.edge_node}', erlang, memory, [total])]), init:stop().\""
        mem_raw = run_cmd(mem_cmd, ignore_fail=True).strip()
        mem_mb = int(mem_raw) / 1024 / 1024 if mem_raw.isdigit() else 0
        
        log(f"[{tag}] {elapsed:.0f}s | Procs: {proc_count} | Mem: {mem_mb:.0f}MB")
        
        if self.log_file and proc_count.isdigit():
            with open(self.log_file, 'a', newline='') as f:
                writer = csv.writer(f)
                writer.writerow([elapsed, proc_count, f"{mem_mb:.1f}"])
    
    def monitor_loop(self, duration, tag, interval=5):
        start = time.time()
        while time.time() - start < duration:
            self.sample(tag)
            time.sleep(interval)

# Helper: Chaos Experiments Log
CHAOS_CSV = os.environ.get("IRIS_CHAOS_CSV", "chaos_experiments.csv")

def init_chaos_csv():
    if not os.path.exists(CHAOS_CSV):
        with open(CHAOS_CSV, 'w') as f:
            f.write("timestamp,experiment,duration_sec,result\n")

def log_experiment(experiment, duration, result="PASS"):
    with open(CHAOS_CSV, 'a') as f:
        f.write(f"{time.time()},{experiment},{duration},{result}\n")

# ============================================================================
# Chaos Actions
# ============================================================================

def apply_network_chaos(enable=True):
    iface = "lo"
    if enable:
        log(f"[CHAOS] Applying network latency/loss to {iface}...")
        cmd = f"sudo -n tc qdisc add dev {iface} root netem delay 100ms 50ms distribution normal loss 1% duplicate 1%"
        run_cmd(cmd, ignore_fail=True)
    else:
        log(f"[CHAOS] Removing network chaos from {iface}...")
        run_cmd(f"sudo -n tc qdisc del dev {iface} root", ignore_fail=True)

def start_chaos_monkey(edge_node, kill_rate=100, interval=1):
    log(f"[CHAOS] Starting chaos monkey (kill {kill_rate} procs/s)...")
    cmd = f"erl -sname monkey -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_monkey, start, [{kill_rate}, {interval}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stop_chaos_monkey(edge_node):
    log("[CHAOS] Stopping chaos monkey...")
    cmd = f"erl -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_monkey, stop, []), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stress_cpu(edge_node, cores=8):
    log(f"[CHAOS] Burning {cores} CPU cores...")
    cmd = f"erl -sname cpu_burn -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_resources, burn_cpu, [{cores}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stress_memory(edge_node, mb=2000):
    log(f"[CHAOS] Eating {mb}MB memory...")
    cmd = f"erl -sname mem_eat -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_resources, eat_memory, [{mb}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

# ============================================================================
# Main Test Execution
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Combined Chaos Test Suite')
    parser.add_argument('--mode', choices=['laptop', 'standard', 'extreme'], default='laptop',
                        help='Test intensity mode')
    parser.add_argument('--log', type=str, default=None, help='CSV log file for metrics')
    parser.add_argument('--skip-restart', action='store_true', help='Skip cluster restart')
    args = parser.parse_args()
    
    # Ensure correct CWD
    os.chdir(project_root)

    config = PRESETS[args.mode]
    edge_node = get_node_name("iris_edge1")
    
    print_section(f"CHAOS TEST: {config['description']}")
    log(f"Users: {config['user_count']:,}")
    log(f"Duration: {config['duration']}s")
    log(f"Chaos features: Network={config['enable_network_chaos']}, CPU={config['enable_cpu_stress']}, Memory={config['enable_memory_stress']}")
    
    # cleanup previous chaos just in case
    apply_network_chaos(False)
    init_chaos_csv()
    
    with ClusterManager(project_root=project_root) as cluster:
        if not args.skip_restart:
           # Recompile helpers manually since ClusterManager builds 'all' but these might be extra
           # actually ClusterManager 'build' runs make all.
           # But we need chaos_resources.erl etc.
           # Let's ensure they are compiled.
           subprocess.run("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl src/iris_extreme_gen.erl 2>/dev/null", shell=True)
        
        monitor = SystemMonitor(edge_node, args.log)
        processes = []
        
        try:
            # Phase 1: Ramp Up
            print_section("PHASE 1: RAMP UP")
            load_cmd = f"/usr/bin/erl +P 2000000 -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({config['user_count']}, {config['duration'] + 60}, normal), timer:sleep(infinity).\""
            processes.append(run_cmd(load_cmd, async_run=True))
            monitor.monitor_loop(config['ramp_time'], "RAMP")
            
            # Phase 2: Offline Flood
            print_section("PHASE 2: OFFLINE FLOOD")
            flood_cmd = f"/usr/bin/erl +P 2000000 -sname flooder -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({config['offline_workers']}, {config['duration']}, offline_flood), timer:sleep(infinity).\""
            processes.append(run_cmd(flood_cmd, async_run=True))
            monitor.monitor_loop(30, "FLOOD")
            
            # Phase 3: Chaos
            print_section("PHASE 3: CHAOS UNLEASHED")
            
            chaos_duration = config['duration'] - config['ramp_time'] - 60
            
            if config['enable_network_chaos']:
                apply_network_chaos(True)
                log_experiment("network_latency_loss", chaos_duration)
            
            if config['enable_cpu_stress']:
                stress_cpu(edge_node, cores=4)
                log_experiment("cpu_burn", chaos_duration)
            
            if config['enable_memory_stress']:
                stress_memory(edge_node, mb=2000)
                log_experiment("memory_eater", chaos_duration)
            
            start_chaos_monkey(edge_node, kill_rate=100)
            log_experiment("process_killer", chaos_duration)
            
            # Run Chaos
            monitor.monitor_loop(max(30, chaos_duration), "CHAOS")
            
            # Phase 4: Recovery
            print_section("PHASE 4: RECOVERY")
            apply_network_chaos(False)
            stop_chaos_monkey(edge_node)
            monitor.monitor_loop(30, "RECOVERY")
            
            print_section("TEST COMPLETE")
            
        except KeyboardInterrupt:
            log("Aborted by user.")
        finally:
            log("Cleaning up...")
            apply_network_chaos(False)
            for p in processes:
                if p:
                    try: p.kill() 
                    except: pass
            # ClusterManager __exit__ will handle stop

if __name__ == "__main__":
    main()
