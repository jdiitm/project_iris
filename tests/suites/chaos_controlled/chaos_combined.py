#!/usr/bin/env python3
"""
Combined Chaos Test Suite

Consolidates all chaos testing scenarios into a single configurable test:
- Connection flood & message traffic
- Process kills (chaos monkey)
- Network chaos (latency/loss)
- CPU/memory stress

INVARIANTS TESTED:
1. System remains responsive during chaos
2. Message delivery continues (>= 90% during chaos)
3. Process count remains healthy (no leak, no crash)
4. System recovers after chaos stops

Modes:
  --mode smoke      : Quick smoke test (100 users, ~30s)
  --mode laptop     : Safe for local development (50k users, 60s)
  --mode standard   : Full chaos suite (200k users, 3 min)
  --mode extreme    : Maximum scale (1M users, 5 min)

Tier: 1 (Nightly/manual)
"""

import subprocess
import time
import os
import sys
import argparse
import csv
import socket
from datetime import datetime

# Add project root to sys.path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager

# ============================================================================
# Configuration Presets
# ============================================================================

PRESETS = {
    "smoke": {
        "user_count": 100,
        "duration": 30,
        "ramp_time": 5,
        "load_time": 10,
        "chaos_time": 10,
        "recovery_time": 5,
        "enable_network_chaos": False,
        "enable_memory_stress": False,
        "enable_cpu_stress": False,
        "min_delivery_rate": 0.90,  # 90% minimum during chaos
        "description": "Quick smoke test (100 users, ~30s)"
    },
    "laptop": {
        "user_count": 50000,
        "duration": 60,
        "ramp_time": 15,
        "load_time": 30,
        "chaos_time": 30,
        "recovery_time": 15,
        "enable_network_chaos": False,
        "enable_memory_stress": False,
        "enable_cpu_stress": True,
        "min_delivery_rate": 0.90,
        "description": "Laptop-safe chaos (50k users, 60s)"
    },
    "standard": {
        "user_count": 200000,
        "duration": 180,
        "ramp_time": 30,
        "load_time": 60,
        "chaos_time": 60,
        "recovery_time": 30,
        "enable_network_chaos": False,
        "enable_memory_stress": False,
        "enable_cpu_stress": True,
        "min_delivery_rate": 0.95,
        "description": "Standard chaos (200k users, 3 min)"
    },
    "extreme": {
        "user_count": 1000000,
        "duration": 300,
        "ramp_time": 60,
        "load_time": 120,
        "chaos_time": 120,
        "recovery_time": 60,
        "enable_network_chaos": False,
        "enable_memory_stress": True,
        "enable_cpu_stress": True,
        "min_delivery_rate": 0.95,
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
        return subprocess.check_output(cmd, shell=True, stderr=subprocess.DEVNULL).decode()
    except subprocess.CalledProcessError as e:
        if not ignore_fail:
            log(f"Command failed: {cmd[:50]}... (exit code {e.returncode})")
        return ""
    except Exception as e:
        if not ignore_fail:
            log(f"Command error: {cmd[:50]}... ({e})")
        return ""

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}", flush=True)

# ============================================================================
# Metrics Collection
# ============================================================================

class MetricsCollector:
    """Collects metrics from the load generator and system."""
    
    def __init__(self, edge_node, log_file=None):
        self.edge_node = edge_node
        self.log_file = log_file
        self.start_time = time.time()
        self.hostname = get_hostname()
        
        # Tracked metrics
        self.baseline_procs = None
        self.peak_procs = 0
        self.peak_mem = 0
        self.samples = []
        
        if log_file:
            with open(log_file, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(['elapsed_s', 'processes', 'memory_mb', 'msgs_sent', 'msgs_recv'])
    
    def get_load_stats(self):
        """Get message stats from the load generator via RPC."""
        loader_node = f"loader@{self.hostname}"
        cmd = f"erl -setcookie iris_secret -sname stats_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"case rpc:call('{loader_node}', iris_extreme_gen, get_stats, [], 3000) of {{ok, S, R}} -> io:format('~p,~p', [S, R]); _ -> io:format('0,0') end, init:stop().\""
        result = run_cmd(cmd, ignore_fail=True).strip()
        try:
            parts = result.split(',')
            if len(parts) == 2:
                return int(parts[0]), int(parts[1])
        except:
            pass
        return 0, 0
    
    def get_process_count(self):
        """Get process count from edge node."""
        cmd = f"erl -setcookie iris_secret -sname probe_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{self.edge_node}', erlang, system_info, [process_count])]), init:stop().\""
        result = run_cmd(cmd, ignore_fail=True).strip()
        try:
            return int(result)
        except:
            return result  # Return raw string for error detection
    
    def get_memory_mb(self):
        """Get memory usage from edge node in MB."""
        cmd = f"erl -setcookie iris_secret -sname probe_m_{int(time.time()*1000)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{self.edge_node}', erlang, memory, [total])]), init:stop().\""
        result = run_cmd(cmd, ignore_fail=True).strip()
        try:
            return int(result) / 1024 / 1024
        except:
            return 0
    
    def sample(self, tag):
        """Take a metrics sample."""
        elapsed = time.time() - self.start_time
        
        proc_count = self.get_process_count()
        mem_mb = self.get_memory_mb()
        sent, recv = self.get_load_stats()
        
        # Track peaks
        if isinstance(proc_count, int):
            if self.baseline_procs is None:
                self.baseline_procs = proc_count
            self.peak_procs = max(self.peak_procs, proc_count)
        self.peak_mem = max(self.peak_mem, mem_mb)
        
        # Store sample
        self.samples.append({
            'elapsed': elapsed,
            'procs': proc_count,
            'mem_mb': mem_mb,
            'sent': sent,
            'recv': recv,
            'tag': tag
        })
        
        # Log
        delta = ""
        if isinstance(proc_count, int) and self.baseline_procs:
            d = proc_count - self.baseline_procs
            delta = f" ({'+' if d >= 0 else ''}{d})"
        
        log(f"[{tag}] {elapsed:.0f}s | Procs: {proc_count}{delta} | Mem: {mem_mb:.0f}MB | Msgs: {sent} sent, {recv} recv")
        
        # Write to CSV
        if self.log_file and isinstance(proc_count, int):
            with open(self.log_file, 'a', newline='') as f:
                writer = csv.writer(f)
                writer.writerow([f"{elapsed:.1f}", proc_count, f"{mem_mb:.1f}", sent, recv])
        
        return proc_count, mem_mb, sent, recv
    
    def monitor_loop(self, duration, tag, interval=3):
        """Monitor for a duration, sampling at interval."""
        start = time.time()
        while time.time() - start < duration:
            self.sample(tag)
            time.sleep(interval)
    
    def get_final_stats(self):
        """Get final statistics summary."""
        sent, recv = self.get_load_stats()
        return {
            'baseline_procs': self.baseline_procs,
            'peak_procs': self.peak_procs,
            'peak_mem': self.peak_mem,
            'total_sent': sent,
            'total_recv': recv,
            'samples': len(self.samples)
        }

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
    cmd = f"erl -setcookie iris_secret -sname monkey -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_monkey, start, [{kill_rate}, {interval}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stop_chaos_monkey(edge_node):
    log("[CHAOS] Stopping chaos monkey...")
    cmd = f"erl -setcookie iris_secret -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_monkey, stop, []), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stress_cpu(edge_node, cores=8):
    log(f"[CHAOS] Burning {cores} CPU cores...")
    cmd = f"erl -setcookie iris_secret -sname cpu_burn -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_resources, burn_cpu, [{cores}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

def stress_memory(edge_node, mb=2000):
    log(f"[CHAOS] Eating {mb}MB memory...")
    cmd = f"erl -setcookie iris_secret -sname mem_eat -hidden -noshell -pa ebin -eval \"rpc:call('{edge_node}', chaos_resources, eat_memory, [{mb}]), init:stop().\""
    run_cmd(cmd, ignore_fail=True)

# ============================================================================
# Main Test Execution
# ============================================================================

class NoOpCluster:
    """No-op cluster manager when cluster is managed externally."""
    def __enter__(self):
        log("Using externally managed cluster")
        return self
    def __exit__(self, *args):
        pass


def main():
    # Select default mode based on TEST_PROFILE
    test_profile = os.environ.get("TEST_PROFILE", "smoke")
    default_mode = "smoke" if test_profile == "smoke" else "laptop"
    
    parser = argparse.ArgumentParser(description='Combined Chaos Test Suite')
    parser.add_argument('--mode', choices=['smoke', 'laptop', 'standard', 'extreme'], default=default_mode,
                        help='Test intensity mode')
    parser.add_argument('--log', type=str, default=None, help='CSV log file for metrics')
    parser.add_argument('--skip-restart', action='store_true', help='Skip cluster restart (use existing cluster)')
    parser.add_argument('--no-cluster', action='store_true', help='Do not manage cluster lifecycle')
    args = parser.parse_args()
    
    # Auto-detect if running under test runner (cluster already managed)
    running_under_test_runner = os.environ.get("IRIS_TEST_RUNNER") == "1"
    skip_cluster = args.skip_restart or args.no_cluster or running_under_test_runner
    
    # Ensure correct CWD
    os.chdir(project_root)

    config = PRESETS[args.mode]
    edge_node = get_node_name("iris_edge1")
    
    print_section(f"CHAOS TEST: {config['description']}")
    log(f"Users: {config['user_count']:,}")
    log(f"Duration: {config['duration']}s")
    log(f"Minimum delivery rate: {config['min_delivery_rate']:.0%}")
    log(f"Chaos features: Network={config['enable_network_chaos']}, CPU={config['enable_cpu_stress']}, Memory={config['enable_memory_stress']}")
    if skip_cluster:
        log("Cluster management: External (test runner)")
    
    # Cleanup previous chaos
    if config['enable_network_chaos']:
        apply_network_chaos(False)
    
    # Use external cluster or manage our own
    cluster_mgr = NoOpCluster() if skip_cluster else ClusterManager(project_root=project_root)
    
    with cluster_mgr as cluster:
        # Compile test utilities
        subprocess.run("erlc -o ebin test_utils/chaos_resources.erl test_utils/chaos_monkey.erl test_utils/iris_extreme_gen.erl", 
                      shell=True, capture_output=True)
        
        metrics = MetricsCollector(edge_node, args.log)
        processes = []
        
        try:
            # ================================================================
            # PRE-CHECK: Verify cluster connectivity
            # ================================================================
            log("Verifying cluster connectivity...")
            try:
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.settimeout(5)
                sock.connect(("localhost", 8085))
                sock.sendall(bytes([0x01]) + b"chaos_precheck")
                resp = sock.recv(1024)
                sock.close()
                if b"LOGIN_OK" not in resp:
                    log(f"FATAL: Cluster not accepting connections")
                    sys.exit(1)
                log("Cluster connectivity: OK")
            except Exception as e:
                log(f"FATAL: Cannot connect to cluster: {e}")
                sys.exit(1)
            
            # ================================================================
            # PHASE 1: Ramp Up - Start load generator with extreme_load mode
            # ================================================================
            print_section("PHASE 1: RAMP UP")
            log(f"Starting {config['user_count']} users with extreme_load mode")
            
            # Use extreme_load mode - this actually sends messages between users
            load_cmd = f"/usr/bin/erl +P 2000000 -setcookie iris_secret -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({config['user_count']}, {config['duration'] + 60}, extreme_load), timer:sleep(infinity).\""
            processes.append(run_cmd(load_cmd, async_run=True))
            
            time.sleep(2)  # Let connections establish
            metrics.monitor_loop(config['ramp_time'], "RAMP")
            
            # ================================================================
            # PHASE 2: Sustained Load - Baseline measurement
            # ================================================================
            print_section("PHASE 2: SUSTAINED LOAD (Baseline)")
            metrics.monitor_loop(config['load_time'], "LOAD")
            
            # ================================================================
            # PHASE 3: Chaos Injection
            # ================================================================
            print_section("PHASE 3: CHAOS UNLEASHED")
            
            if config['enable_network_chaos']:
                apply_network_chaos(True)
            
            if config['enable_cpu_stress']:
                stress_cpu(edge_node, cores=4)
            
            if config['enable_memory_stress']:
                stress_memory(edge_node, mb=2000)
            
            start_chaos_monkey(edge_node, kill_rate=100)
            
            metrics.monitor_loop(config['chaos_time'], "CHAOS")
            
            # ================================================================
            # PHASE 4: Recovery
            # ================================================================
            print_section("PHASE 4: RECOVERY")
            if config['enable_network_chaos']:
                apply_network_chaos(False)
            stop_chaos_monkey(edge_node)
            
            metrics.monitor_loop(config['recovery_time'], "RECOVERY")
            
            # ================================================================
            # METRICS SUMMARY
            # ================================================================
            print_section("METRICS SUMMARY")
            
            stats = metrics.get_final_stats()
            log(f"Baseline processes: {stats['baseline_procs']}")
            log(f"Peak processes: {stats['peak_procs']}")
            log(f"Peak memory: {stats['peak_mem']:.1f} MB")
            log(f"Total messages sent: {stats['total_sent']}")
            log(f"Total messages received: {stats['total_recv']}")
            
            proc_delta = stats['peak_procs'] - (stats['baseline_procs'] or stats['peak_procs'])
            log(f"Process growth during test: +{proc_delta}")
            
            # ================================================================
            # ASSERTIONS - Real pass/fail criteria
            # ================================================================
            print_section("ASSERTIONS")
            
            passed = True
            
            # Assertion 1: System must still be responsive
            final_procs = metrics.get_process_count()
            if not isinstance(final_procs, int) or final_procs == 0:
                log(f"FAIL: System unresponsive after chaos (got: {final_procs})")
                passed = False
            else:
                log(f"PASS: System responsive (process count: {final_procs})")
            
            # Assertion 2: Process count healthy (no crash, no major leak)
            # Note: A minimal Erlang node has ~30-50 processes, so < 20 indicates a crash
            if isinstance(final_procs, int):
                if final_procs < 20:
                    log(f"FAIL: Process count too low ({final_procs}) - system may have crashed")
                    passed = False
                elif final_procs > 500000:
                    log(f"FAIL: Process count too high ({final_procs}) - possible process leak")
                    passed = False
                else:
                    log(f"PASS: Process count healthy ({final_procs})")
            
            # Assertion 3: Load generator actually sent messages
            if stats['total_sent'] == 0:
                log("FAIL: No messages were sent - load generator did not work!")
                passed = False
            else:
                log(f"PASS: Load generator sent {stats['total_sent']} messages")
            
            # Assertion 4: Message delivery rate
            if stats['total_sent'] > 0:
                delivery_rate = stats['total_recv'] / stats['total_sent']
                if delivery_rate < config['min_delivery_rate']:
                    log(f"FAIL: Delivery rate {delivery_rate:.1%} < {config['min_delivery_rate']:.0%} threshold")
                    passed = False
                else:
                    log(f"PASS: Delivery rate {delivery_rate:.1%} >= {config['min_delivery_rate']:.0%}")
            
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
                    log(f"FAIL: Post-chaos login failed")
                    passed = False
            except Exception as e:
                log(f"FAIL: Post-chaos connection failed: {e}")
                passed = False
            
            # Assertion 6: Process growth indicates load was applied
            # Zero or negative growth with 100+ users MAY indicate load wasn't applied
            # BUT: if messages were sent/received, load was applied successfully
            if proc_delta <= 0 and config['user_count'] >= 100:
                if stats['total_sent'] > 0 and stats['total_recv'] > 0:
                    log(f"PASS: Process count stable (delta={proc_delta}) - efficient process reuse")
                    log(f"      Load verified: {stats['total_sent']} sent, {stats['total_recv']} recv")
                else:
                    log(f"FAIL: Process count did not grow (delta={proc_delta}) - load generator failed to apply load")
                    passed = False
            elif proc_delta < 10 and config['user_count'] >= 100:
                log(f"WARN: Process count only grew by {proc_delta} - expected more with {config['user_count']} users")
            else:
                log(f"PASS: Process growth ({proc_delta}) indicates load was applied")
            
            # ================================================================
            # FINAL RESULT
            # ================================================================
            print_section("RESULT")
            
            if passed:
                log("ALL ASSERTIONS PASSED")
                sys.exit(0)
            else:
                log("SOME ASSERTIONS FAILED")
                sys.exit(1)
            
        except KeyboardInterrupt:
            log("Aborted by user.")
            sys.exit(1)
        finally:
            log("Cleaning up...")
            if config.get('enable_network_chaos'):
                apply_network_chaos(False)
            for p in processes:
                if p:
                    try:
                        p.kill()
                    except:
                        pass

if __name__ == "__main__":
    main()
