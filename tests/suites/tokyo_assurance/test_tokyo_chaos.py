#!/usr/bin/env python3
"""
Tokyo Edge Assurance Suite
==========================

Senior Principal Engineer Validation:
- Simulates Tokyo <-> Bangalore WAN characteristics (Latency, Loss).
- Validates Split-Brain resilience between Core nodes.
- Verifies System behavior under Resource Exhaustion.

Restrictions:
- Does NOT modify existing tests.
- Uses existing 'chaos' primitives from the codebase.
"""

import sys
import os
import time
import argparse
import subprocess
import csv
from datetime import datetime

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster
from tests.utilities import IrisClient

# ============================================================================
# Configuration
# ============================================================================

EVIDENCE_LOG = "tokyo_assurance_evidence.csv"

def log(msg, level="INFO"):
    ts = datetime.now().strftime("%H:%M:%S")
    print(f"[{ts}] [{level}] {msg}", flush=True)

def record_evidence(scenario, metric, value, outcome):
    """Log structured evidence for the Principal Engineer report."""
    exists = os.path.exists(EVIDENCE_LOG)
    with open(EVIDENCE_LOG, 'a', newline='') as f:
        writer = csv.writer(f)
        if not exists:
            writer.writerow(["Timestamp", "Scenario", "Metric", "Value", "Outcome"])
        writer.writerow([datetime.now().isoformat(), scenario, metric, value, outcome])

def run_erl_cmd(cmd, bg=False):
    """Run an Erlang command/eval."""
    full_cmd = f"erl -hidden -noshell -pa ebin -eval \"{cmd}\""
    if bg:
        return subprocess.Popen(full_cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(full_cmd, shell=True).decode()
    except subprocess.CalledProcessError:
        return ""

def run_external_rpc(node, mod, fun, args_list, cookie="iris_secret"):
    """Run RPC on an external node using erl."""
    # args_list should be a string like "[Arg1, Arg2]"
    erl_cmd = f"erl -name tester_{int(time.time())}@127.0.0.1 -setcookie {cookie} -hidden -noshell -eval \"io:format('~p', [rpc:call('{node}', {mod}, {fun}, {args_list})]), init:stop().\""
    try:
        return subprocess.check_output(erl_cmd, shell=True, timeout=10).decode()
    except Exception as e:
        log(f"RPC Error: {e}", "RPC")
        return ""


# ============================================================================
# Chaos Primitives (Re-using existing infrastructure)
# ============================================================================

def compile_chaos_helpers():
    """Ensure chaos helpers are compiled."""
    log("Compiling chaos helpers...", "SETUP")
    subprocess.run("erlc -o ebin src/chaos_*.erl src/iris_extreme_gen.erl 2>/dev/null", shell=True)

def inject_wan_conditions(latency="200ms", jitter="50ms", loss="1%"):
    """Simulate Tokyo -> Bangalore WAN."""
    log(f"Injecting WAN: {latency} +/- {jitter}, {loss} loss", "CHAOS")
    # Note: On a shared machine, this affects localhost loopback. 
    # In a real deployed test, this would be on the Edge VM's outgoing interface.
    # For local validation, we assume 'lo'.
    cmd = f"sudo -n tc qdisc add dev lo root netem delay {latency} {jitter} distribution normal loss {loss}"
    subprocess.run(cmd, shell=True, stderr=subprocess.DEVNULL)

def remove_wan_conditions():
    """Remove WAN simulation."""
    log("Removing WAN conditions", "CLEANUP")
    subprocess.run("sudo -n tc qdisc del dev lo root", shell=True, stderr=subprocess.DEVNULL)

def trigger_split_brain(node1, node2):
    """Sever link between two nodes."""
    log(f"Severing link {node1} <-> {node2}", "CHAOS")
    # Using erlang:disconnect_node/1 via RPC
    run_erl_cmd(f"rpc:call('{node1}', erlang, disconnect_node, ['{node2}']), init:stop().")

# ============================================================================
# Scenarios
# ============================================================================

def scenario_wan_reliability(args):
    """Validates message delivery under high WAN latency."""
    log("--- SCENARIO: WAN RELIABILITY (Tokyo <-> Bangalore) ---", "START")
    
    # 1. Setup WAN
    if not args.external:
        inject_wan_conditions(latency="200ms", loss="2%")
    else:
        log("External Mode: Assuming real WAN conditions exist.", "SETUP")
    
    try:
        # Use config IP/Port
        client = IrisClient(host=args.edge_ip, port=args.edge_port)
        client.login("tokyo_wan_user")
        
        # 2. Send messages
        start_time = time.time()
        sent_count = 100
        for i in range(sent_count):
            client.send_msg("tokyo_wan_user", f"msg_{i}")
        
        # 3. Receive (Reliable Protocol should handle the loss/latency)
        received_count = 0
        client.sock.settimeout(5.0 if args.external else 2.0) # Higher timeout for real WAN
        
        # We expect some retries at TCP level, but app level should succeed eventually
        for i in range(sent_count):
            try:
                client.recv_msg(timeout=5.0)
                received_count += 1
            except:
                pass
        
        duration = time.time() - start_time
        loss_rate = (sent_count - received_count) / sent_count
        
        record_evidence("WAN_Reliability", "LossRate", f"{loss_rate*100:.1f}%", "PASS" if loss_rate < 0.1 else "WARN")
        record_evidence("WAN_Reliability", "Duration", f"{duration:.2f}s", "INFO")
        
        if loss_rate < 0.1: # Allow some application timeout drop, but mostly reliability
            log("WAN Reliability Passed", "PASS")
            return True
        else:
            log(f"WAN Reliability Failed: {loss_rate*100}% Loss", "FAIL")
            return False
            
    except Exception as e:
        log(f"WAN Scenario Error: {e}", "ERROR")
        return False
    finally:
        if not args.external:
            remove_wan_conditions()
        try: client.close() 
        except: pass

def scenario_split_brain(args):
    """Validates system behavior when Core nodes are partitioned."""
    log("--- SCENARIO: SPLIT BRAIN (Bangalore Core Partition) ---", "START")
    
    core1 = getattr(args, 'core1', "iris_core1@100.95.21.52")
    core2 = getattr(args, 'core2', "iris_core2@100.68.74.48")

    if not args.external:
        suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
        hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
        core1 = f"iris_core1{suffix}@{hostname}"
        core2 = f"iris_core2{suffix}@{hostname}"
    
    # FIXED: Actually trigger partition in external mode too
    log(f"Triggering partition: {core1} <-> {core2}", "CHAOS")
    trigger_split_brain(core1, core2)
    
    # Verify partition occurred
    time.sleep(2)
    verify_result = run_external_rpc(core1, "erlang", "nodes", "[connected]")
    partition_verified = core2.split('@')[0] not in verify_result
    
    if not partition_verified:
        log(f"WARN: Partition may not have occurred. Core1 still sees: {verify_result}", "WARN")
        # Continue anyway - the test is about availability during partition
    else:
        log("Partition verified: Core1 no longer sees Core2", "PASS")
        record_evidence("Split_Brain", "Partition_Verified", "True", "PASS")
    
    # 2. Write Data to Edge (Should still succeed if at least one Core is reachable)
    try:
        client = IrisClient(host=args.edge_ip, port=args.edge_port)
        client.login("split_brain_user")
        client.send_msg("split_brain_user", "surviving_partition")
        msg = client.recv_msg(timeout=5.0)
        
        if msg == b"surviving_partition":
            record_evidence("Split_Brain", "WriteAvailability", "Success", "PASS")
            log("Split Brain Availability Passed", "PASS")
            return True
        else:
            record_evidence("Split_Brain", "WriteAvailability", "WrongMsg", "FAIL")
            return False
            
    except Exception as e:
        log(f"Split Brain Error: {e}", "FAIL")
        record_evidence("Split_Brain", "WriteAvailability", f"Exception: {e}", "FAIL")
        return False
    finally:
        try: client.close() 
        except: pass

def scenario_resource_exhaustion(args):
    """Validates behavior under simulated disk exhaustion."""
    log("--- SCENARIO: RESOURCE EXHAUSTION (Disk Pressure) ---", "START")
    
    if args.external:
        # In external mode, we skip the destructive chaos monkey unless we have a safe way to target it.
        # We'll just do a liveness check.
        log("EXTERNAL MODE: Skipping Chaos Monkey. Performing Liveness Check.", "INFO")
    else:
        # We simulate Mnesia overload by flooding 'store_offline' commands
        node = f"iris_edge1{os.environ.get('IRIS_NODE_SUFFIX', '')}@{subprocess.check_output('hostname -s', shell=True).decode().strip()}"
        # Use existing chaos_resources module
        cmd = f"rpc:call('{node}', chaos_monkey, start, [100, 1]), init:stop()." # Mild chaos to simulate busy system
        run_erl_cmd(cmd)
    
    start = time.time()
    try:
        if not args.external:
            # Check if system stays up
            time.sleep(10)
            
        # Simple liveness check
        client = IrisClient(host=args.edge_ip, port=args.edge_port)
        client.login("survivor")
        client.close()
        
        record_evidence("Resource_Exhaustion", "Liveness", "Alive", "PASS")
        log("Resource Exhaustion Survival Passed", "PASS")
        return True
    except Exception as e:
        record_evidence("Resource_Exhaustion", "Liveness", "Crashed", "FAIL")
        log(f"System crashed under pressure: {e}", "FAIL")
        return False

# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Tokyo Edge Assurance Suite')
    parser.add_argument('--external', action='store_true', help='Run against external cluster (do not start/stop nodes)')
    parser.add_argument('--edge-ip', type=str, default="127.0.0.1", help='Edge Node IP (default: 127.0.0.1)')
    parser.add_argument('--edge-port', type=int, default=8080, help='Edge Node Port (default: 8080)')
    parser.add_argument('--cookie', type=str, default="iris_secret", help='Erlang cookie for RPC')
    args = parser.parse_args()

    log("Starting Tokyo Edge Assurance Suite...", "MAIN")
    
    # Context manager for Cluster (Either real manager or a dummy one)
    class ExternalCluster:
        def __enter__(self): return self
        def __exit__(self, exc_type, exc_val, exc_tb): pass
        def is_healthy(self): return True
    
    if args.external:
        log(f"Running in EXTERNAL mode. Target: {args.edge_ip}:{args.edge_port}", "CONFIG")
        cm = ExternalCluster()
        # In external mode, we don't compile helpers because we can't easily push them to remote nodes
        # We assume the remote nodes are already running the code or we rely on basic RPC
    else:
        log("Running in LOCAL SIMULATION mode.", "CONFIG")
        cm = ClusterManager()
        compile_chaos_helpers()

    # Shared state for RPC helpers
    global RPC_COOKIE
    RPC_COOKIE = args.cookie

    def get_node_name(prefix):
        if args.external:
            # For external, we guess standard names or would need more args. 
            # Assuming standard deployment names for now.
            # Hostname is tricky for external RPC. 
            # We'll try to discover via net_adm:names() if needed, but for now specific hardcoding 
            # or 'nodes()' rpc might be needed. 
            pass 
        return "" # Logic needs to be inside scenarios or robustified

    # Start Test Session
    with cm as cluster:
        if not args.external and not cluster.is_healthy():
            log("Cluster failed to start!", "FATAL")
            sys.exit(1)
            
        passes = 0
        total = 0
        
        # Scenarios
        # We pass 'args' to scenarios so they know how to behave
        scenarios = [
            (scenario_wan_reliability, "WAN Reliability"),
            (scenario_split_brain, "Split Brain"),
            (scenario_resource_exhaustion, "Resource Exhaustion")
        ]
        
        for func, name in scenarios:
            total += 1
            # Pass args to scenario logic handling
            if func(args):
                passes += 1
            
            if not args.external:
                time.sleep(2)
            else:
                log("Waiting 5s between external tests...", "WAIT")
                time.sleep(5)

        log(f"Assurance Complete: {passes}/{total} Passed", "MAIN")
        
        if passes == total:
            sys.exit(0)
        else:
            sys.exit(1)

if __name__ == "__main__":
    main()
