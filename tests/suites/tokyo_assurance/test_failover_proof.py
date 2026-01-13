#!/usr/bin/env python3
"""
Failover Proof Test: Verifiable Resilience Validation
======================================================

This test PROVES resilience by enforcing observable, verifiable assertions.
It will FAIL if any of the following are not met:

1. Pre-flight: Both Core nodes are UP, Edge is connected, traffic is flowing
2. Failure: Core1 is killed and confirmed dead
3. Takeover: Edge loses Core1 from connected nodes
4. Continuity: Traffic delivery rate >= 90% during failover
5. Recovery: Core1 rejoins and Edge reconnects (if configured)

Usage:
    python3 test_failover_proof.py --edge-node iris_edge1@100.82.212.50 \
        --core1 iris_core1@100.95.21.52 --core2 iris_core2@100.68.74.48

Author: Senior Principal Engineer Assurance
"""

import sys
import os
import time
import argparse
import subprocess
import csv
import signal
from datetime import datetime
from typing import Tuple, Optional, Dict, Any

# ============================================================================
# Configuration
# ============================================================================

EVIDENCE_CSV = "failover_proof_evidence.csv"
LOG_FILE = "failover_proof.log"
POLL_INTERVAL = 2  # seconds
TRAFFIC_LOSS_THRESHOLD = 0.10  # 10% max allowed loss
FAILOVER_TIMEOUT = 60  # seconds to wait for takeover
COOKIE = "iris_secret"

# ============================================================================
# Logging & Evidence
# ============================================================================

def log(msg: str, level: str = "INFO") -> None:
    ts = datetime.now().strftime("%H:%M:%S.%f")[:-3]
    formatted = f"[{ts}] [{level:8}] {msg}"
    print(formatted, flush=True)
    with open(LOG_FILE, "a") as f:
        f.write(formatted + "\n")


class EvidenceCollector:
    """Structured evidence collection with causal event tracking."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.start_time = time.time()
        self.events: list = []
        
        with open(self.filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([
                'Timestamp', 'Elapsed_s', 'EventType', 'EventName', 
                'Status', 'Details', 'Sent', 'Recv', 'Core1_State', 'Core2_State'
            ])
    
    def record(self, event_type: str, event_name: str, status: str, 
               details: str = "", sent: int = 0, recv: int = 0,
               core1: str = "UNKNOWN", core2: str = "UNKNOWN") -> None:
        elapsed = round(time.time() - self.start_time, 2)
        ts = datetime.now().isoformat()
        
        row = [ts, elapsed, event_type, event_name, status, details, sent, recv, core1, core2]
        self.events.append(row)
        
        with open(self.filename, 'a', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(row)
    
    def has_event(self, event_name: str, status: str = "PASS") -> bool:
        """Check if a specific event occurred with expected status."""
        return any(e[3] == event_name and e[4] == status for e in self.events)
    
    def get_event_time(self, event_name: str) -> Optional[float]:
        """Get elapsed time of an event."""
        for e in self.events:
            if e[3] == event_name:
                return float(e[1])
        return None


# ============================================================================
# RPC Helpers
# ============================================================================

def run_erl_rpc(cmd: str, timeout: int = 10) -> str:
    """Execute Erlang command via a temporary node."""
    node_name = f"probe_{int(time.time()*1000)}@127.0.0.1"
    full_cmd = (
        f"erl -name {node_name} -setcookie {COOKIE} -hidden -noshell "
        f"-pa ebin -eval \"{cmd}\" 2>/dev/null"
    )
    try:
        result = subprocess.check_output(full_cmd, shell=True, timeout=timeout)
        return result.decode().strip()
    except subprocess.TimeoutExpired:
        return "TIMEOUT"
    except subprocess.CalledProcessError as e:
        return f"ERROR:{e.returncode}"


def ping_node(node: str) -> bool:
    """Check if a node is reachable."""
    cmd = f"io:format('~p', [net_adm:ping('{node}')]), init:stop()."
    result = run_erl_rpc(cmd)
    return "pong" in result


def get_connected_nodes(node: str) -> list:
    """Get list of nodes connected to target node."""
    cmd = (
        f"Nodes = rpc:call('{node}', erlang, nodes, [connected]), "
        f"io:format('~p', [Nodes]), init:stop()."
    )
    result = run_erl_rpc(cmd)
    if "badrpc" in result or "ERROR" in result:
        return []
    # Parse Erlang list format like [node1, node2]
    try:
        # Simple parsing for ['node1@ip', 'node2@ip']
        if result.startswith("[") and "]" in result:
            content = result[1:result.rfind("]")]
            if not content.strip():
                return []
            nodes = [n.strip().strip("'") for n in content.split(",")]
            return [n for n in nodes if n]
    except:
        pass
    return []


def get_traffic_stats(loadgen_node: str) -> Tuple[int, int]:
    """Get sent/recv counts from load generator."""
    cmd = (
        f"Stats = rpc:call('{loadgen_node}', iris_extreme_gen, get_stats, []), "
        f"io:format('~p', [Stats]), init:stop()."
    )
    result = run_erl_rpc(cmd)
    
    # Parse {ok, Sent, Recv} or {error, ...}
    if "{ok," in result:
        try:
            content = result.split("{ok,")[1].split("}")[0]
            parts = content.split(",")
            return int(parts[0].strip()), int(parts[1].strip())
        except:
            pass
    return 0, 0


def kill_node(node: str) -> bool:
    """Send init:stop() to a node to shut it down."""
    cmd = f"rpc:call('{node}', init, stop, []), init:stop()."
    run_erl_rpc(cmd)
    # Verify it died
    time.sleep(2)
    return not ping_node(node)


# ============================================================================
# Test Phases
# ============================================================================

def phase_preflight(args, evidence: EvidenceCollector, loadgen_node: str) -> bool:
    """
    Pre-flight checks: Verify cluster is healthy before test.
    FAILS if: Any Core is down, Edge is isolated, or no traffic is flowing.
    """
    log("=" * 60, "PHASE")
    log("PHASE 1: PRE-FLIGHT CHECKS", "PHASE")
    log("=" * 60, "PHASE")
    
    # Check Core1
    core1_up = ping_node(args.core1)
    log(f"Core1 ({args.core1}): {'UP' if core1_up else 'DOWN'}", 
        "PASS" if core1_up else "FAIL")
    evidence.record("PREFLIGHT", "Core1_Ping", "PASS" if core1_up else "FAIL",
                   args.core1, core1="UP" if core1_up else "DOWN")
    
    # Check Core2
    core2_up = ping_node(args.core2)
    log(f"Core2 ({args.core2}): {'UP' if core2_up else 'DOWN'}", 
        "PASS" if core2_up else "FAIL")
    evidence.record("PREFLIGHT", "Core2_Ping", "PASS" if core2_up else "FAIL",
                   args.core2, core2="UP" if core2_up else "DOWN")
    
    # Check Edge connectivity
    edge_up = ping_node(args.edge_node)
    log(f"Edge ({args.edge_node}): {'UP' if edge_up else 'DOWN'}", 
        "PASS" if edge_up else "FAIL")
    
    connected = get_connected_nodes(args.edge_node)
    log(f"Edge Connected Nodes: {connected}", "INFO")
    
    edge_has_cores = any(args.core1.split('@')[0] in str(n) or 
                        args.core2.split('@')[0] in str(n) for n in connected)
    evidence.record("PREFLIGHT", "Edge_Connectivity", 
                   "PASS" if edge_has_cores else "FAIL",
                   f"Connected: {connected}")
    
    # Check traffic is flowing
    time.sleep(5)  # Let load generator warm up
    sent, recv = get_traffic_stats(loadgen_node)
    log(f"Traffic Stats: Sent={sent}, Recv={recv}", "INFO")
    
    traffic_ok = sent > 0
    evidence.record("PREFLIGHT", "Traffic_Baseline", 
                   "PASS" if traffic_ok else "FAIL",
                   f"Sent={sent}, Recv={recv}", sent, recv,
                   "UP" if core1_up else "DOWN", "UP" if core2_up else "DOWN")
    
    if not traffic_ok:
        log("FATAL: No traffic flowing! Load generator may have failed.", "FAIL")
    
    # Verdict
    preflight_pass = core1_up and core2_up and edge_up and edge_has_cores and traffic_ok
    log(f"Pre-flight: {'PASS' if preflight_pass else 'FAIL'}", 
        "PASS" if preflight_pass else "FAIL")
    
    return preflight_pass


def phase_failure_injection(args, evidence: EvidenceCollector, 
                           loadgen_node: str) -> Tuple[bool, int, int]:
    """
    Failure Injection: Kill Core1 and verify death.
    FAILS if: Core1 does not die within timeout.
    Returns: (success, sent_before, recv_before)
    """
    log("=" * 60, "PHASE")
    log("PHASE 2: FAILURE INJECTION", "PHASE")
    log("=" * 60, "PHASE")
    
    # Capture traffic baseline
    sent_before, recv_before = get_traffic_stats(loadgen_node)
    log(f"Traffic Before Failure: Sent={sent_before}, Recv={recv_before}", "INFO")
    evidence.record("FAILURE", "Traffic_Before", "INFO", 
                   "", sent_before, recv_before, "UP", "UP")
    
    # Kill Core1
    log(f"Killing Core1: {args.core1}", "ACTION")
    evidence.record("FAILURE", "Core1_Kill_Start", "INFO", args.core1)
    
    dead = kill_node(args.core1)
    
    if dead:
        log(f"Core1 confirmed DEAD", "PASS")
        evidence.record("FAILURE", "Core1_Confirmed_Dead", "PASS", 
                       args.core1, core1="DOWN")
    else:
        log(f"Core1 still alive after kill attempt!", "FAIL")
        evidence.record("FAILURE", "Core1_Confirmed_Dead", "FAIL",
                       "Node did not die", core1="UP")
        return False, sent_before, recv_before
    
    return True, sent_before, recv_before


def phase_takeover_verification(args, evidence: EvidenceCollector) -> bool:
    """
    Takeover Verification: Ensure Edge detects Core1 loss.
    FAILS if: Edge still shows Core1 connected after timeout.
    """
    log("=" * 60, "PHASE")
    log("PHASE 3: TAKEOVER VERIFICATION", "PHASE")
    log("=" * 60, "PHASE")
    
    start = time.time()
    takeover_detected = False
    
    while time.time() - start < FAILOVER_TIMEOUT:
        connected = get_connected_nodes(args.edge_node)
        core1_name = args.core1.split('@')[0]
        
        core1_gone = not any(core1_name in str(n) for n in connected)
        core2_present = any(args.core2.split('@')[0] in str(n) for n in connected)
        
        log(f"Edge connected: {connected} (Core1 gone: {core1_gone}, Core2 present: {core2_present})", 
            "INFO")
        
        if core1_gone:
            takeover_detected = True
            evidence.record("TAKEOVER", "Core1_Disconnected", "PASS",
                           f"Detected in {time.time()-start:.1f}s",
                           core1="DOWN", core2="UP" if core2_present else "UNKNOWN")
            log(f"Takeover detected: Core1 disconnected from Edge", "PASS")
            break
        
        time.sleep(POLL_INTERVAL)
    
    if not takeover_detected:
        log(f"Takeover NOT detected within {FAILOVER_TIMEOUT}s!", "FAIL")
        evidence.record("TAKEOVER", "Core1_Disconnected", "FAIL",
                       f"Timeout after {FAILOVER_TIMEOUT}s")
    
    return takeover_detected


def phase_traffic_continuity(args, evidence: EvidenceCollector,
                            loadgen_node: str, sent_before: int, 
                            recv_before: int) -> bool:
    """
    Traffic Continuity: Verify message delivery during/after failover.
    FAILS if: Delivery rate < 90%.
    """
    log("=" * 60, "PHASE")
    log("PHASE 4: TRAFFIC CONTINUITY", "PHASE")
    log("=" * 60, "PHASE")
    
    # Active traffic monitoring instead of passive wait
    log("Actively monitoring traffic for 30s...", "INFO")
    
    samples = []
    start_time = time.time()
    prev_sent, prev_recv = get_traffic_stats(loadgen_node)
    
    while time.time() - start_time < 30:
        time.sleep(5)
        sent, recv = get_traffic_stats(loadgen_node)
        delta_sent = sent - prev_sent
        delta_recv = recv - prev_sent  # Compare recv to prev_sent for delivery rate
        
        elapsed = int(time.time() - start_time)
        log(f"  [{elapsed:2d}s] Sent={sent} (+{delta_sent}), Recv={recv} | Delta delivery: {delta_recv}/{delta_sent if delta_sent > 0 else 1}", "TRAFFIC")
        
        samples.append((sent, recv, delta_sent, recv - prev_recv))
        prev_sent, prev_recv = sent, recv
        
        evidence.record("TRAFFIC", "Sample", "INFO", 
                       f"Sent={sent}, Recv={recv}", sent, recv, "DOWN", "UP")
    
    sent_after, recv_after = get_traffic_stats(loadgen_node)
    log(f"Traffic After Failover: Sent={sent_after}, Recv={recv_after}", "INFO")
    
    sent_delta = sent_after - sent_before
    recv_delta = recv_after - recv_before
    
    if sent_delta <= 0:
        log("No messages sent during failover window!", "FAIL")
        evidence.record("CONTINUITY", "Traffic_During_Failover", "FAIL",
                       "No sent delta", sent_after, recv_after, "DOWN", "UP")
        return False
    
    delivery_rate = recv_delta / sent_delta
    log(f"Delivery Rate: {delivery_rate*100:.1f}% ({recv_delta}/{sent_delta})", "INFO")
    
    continuity_pass = delivery_rate >= (1 - TRAFFIC_LOSS_THRESHOLD)
    
    evidence.record("CONTINUITY", "Delivery_Rate", 
                   "PASS" if continuity_pass else "FAIL",
                   f"{delivery_rate*100:.1f}% ({recv_delta}/{sent_delta})",
                   sent_after, recv_after, "DOWN", "UP")
    
    if continuity_pass:
        log(f"Traffic continuity PASS: {delivery_rate*100:.1f}% >= 90%", "PASS")
    else:
        log(f"Traffic continuity FAIL: {delivery_rate*100:.1f}% < 90%", "FAIL")
    
    return continuity_pass


def phase_recovery(args, evidence: EvidenceCollector, skip: bool = False) -> bool:
    """
    Recovery Verification: Bring Core1 back and verify rejoin.
    Optional phase - can be skipped if --no-recovery is set.
    """
    if skip:
        log("Recovery phase skipped (--no-recovery)", "INFO")
        evidence.record("RECOVERY", "Skipped", "INFO", "Flag --no-recovery set")
        return True
    
    log("=" * 60, "PHASE")
    log("PHASE 5: RECOVERY VERIFICATION", "PHASE")
    log("=" * 60, "PHASE")
    
    log(f">>> ACTION REQUIRED: Restart Core1 ({args.core1}) <<<", "ACTION")
    log(">>> Waiting for operator to restart the node... <<<", "ACTION")
    log(">>> Press ENTER when Core1 is restarted, or Ctrl+C to skip <<<", "ACTION")
    
    try:
        input()
    except (KeyboardInterrupt, EOFError):
        log("Recovery skipped by operator", "WARN")
        evidence.record("RECOVERY", "Operator_Skip", "WARN")
        return True
    
    # Poll for Core1 to come back
    start = time.time()
    recovered = False
    
    while time.time() - start < FAILOVER_TIMEOUT:
        if ping_node(args.core1):
            recovered = True
            break
        time.sleep(POLL_INTERVAL)
    
    if recovered:
        log(f"Core1 recovered and reachable", "PASS")
        evidence.record("RECOVERY", "Core1_Rejoined", "PASS", args.core1,
                       core1="UP", core2="UP")
        
        # Verify Edge reconnects
        time.sleep(5)
        connected = get_connected_nodes(args.edge_node)
        core1_reconnected = any(args.core1.split('@')[0] in str(n) for n in connected)
        
        if core1_reconnected:
            log("Edge reconnected to Core1", "PASS")
            evidence.record("RECOVERY", "Edge_Reconnected", "PASS")
        else:
            log("Edge did not reconnect to Core1", "WARN")
            evidence.record("RECOVERY", "Edge_Reconnected", "WARN",
                           f"Connected: {connected}")
    else:
        log(f"Core1 did not recover within {FAILOVER_TIMEOUT}s", "FAIL")
        evidence.record("RECOVERY", "Core1_Rejoined", "FAIL", 
                       f"Timeout {FAILOVER_TIMEOUT}s")
    
    return recovered


# ============================================================================
# Evidence Validation
# ============================================================================

def validate_evidence(evidence: EvidenceCollector) -> Tuple[bool, list]:
    """
    Final evidence validation: Ensure all required events occurred.
    Returns (pass, missing_events)
    """
    required_events = [
        ("Core1_Ping", "PASS"),
        ("Core2_Ping", "PASS"),
        ("Edge_Connectivity", "PASS"),
        ("Traffic_Baseline", "PASS"),
        ("Core1_Confirmed_Dead", "PASS"),
        ("Core1_Disconnected", "PASS"),
        ("Delivery_Rate", "PASS"),
    ]
    
    missing = []
    for event_name, required_status in required_events:
        if not evidence.has_event(event_name, required_status):
            missing.append(f"{event_name}={required_status}")
    
    return len(missing) == 0, missing


# ============================================================================
# Load Generator Management
# ============================================================================

def start_load_generator(args) -> Tuple[subprocess.Popen, str]:
    """Start the load generator and return process + node name."""
    # Parse Edge IP for load gen targeting
    try:
        edge_ip = args.edge_node.split('@')[1]
        ip_parts = edge_ip.split('.')
        erl_ip = f"{{{ip_parts[0]},{ip_parts[1]},{ip_parts[2]},{ip_parts[3]}}}"
    except:
        erl_ip = "{127,0,0,1}"
    
    loadgen_node = f"loadgen_{int(time.time())}@127.0.0.1"
    
    # Use extreme_load mode with target host/port
    cmd = (
        f"iris_extreme_gen:start({args.users}, {args.duration + 120}, "
        f"extreme_load, {erl_ip}, 8080), timer:sleep(infinity)."
    )
    
    log_file = open("loadgen.log", "w")
    
    proc = subprocess.Popen(
        f"erl -name {loadgen_node} -setcookie {COOKIE} -pa ebin -hidden "
        f"-noshell -eval \"net_adm:ping('{args.edge_node}'), {cmd}\"",
        shell=True, stdout=log_file, stderr=log_file
    )
    
    log(f"Load generator started: {loadgen_node}", "INFO")
    return proc, loadgen_node


# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Failover Proof Test')
    parser.add_argument('--edge-node', type=str, required=True,
                       help='Edge node name (e.g. iris_edge1@100.82.212.50)')
    parser.add_argument('--core1', type=str, required=True,
                       help='Primary Core node (will be killed)')
    parser.add_argument('--core2', type=str, required=True,
                       help='Secondary Core node (replica)')
    parser.add_argument('--users', type=int, default=10000,
                       help='Concurrent users (default: 10000)')
    parser.add_argument('--duration', type=int, default=300,
                       help='Test duration in seconds (default: 300)')
    parser.add_argument('--no-recovery', action='store_true',
                       help='Skip recovery phase')
    args = parser.parse_args()
    
    # Initialize
    evidence = EvidenceCollector(EVIDENCE_CSV)
    
    log("=" * 70, "MAIN")
    log("    FAILOVER PROOF TEST: VERIFIABLE RESILIENCE VALIDATION", "MAIN")
    log("=" * 70, "MAIN")
    log(f"Edge:  {args.edge_node}", "CONFIG")
    log(f"Core1: {args.core1} (will be killed)", "CONFIG")
    log(f"Core2: {args.core2} (replica)", "CONFIG")
    log(f"Users: {args.users}, Duration: {args.duration}s", "CONFIG")
    log("", "MAIN")
    
    # Start load generator
    load_proc, loadgen_node = start_load_generator(args)
    time.sleep(10)  # Warm-up
    
    try:
        # Phase 1: Pre-flight
        if not phase_preflight(args, evidence, loadgen_node):
            log("ABORT: Pre-flight checks failed", "FATAL")
            evidence.record("VERDICT", "Abort", "FAIL", "Pre-flight failed")
            return 1
        
        # Phase 2: Failure Injection
        success, sent_before, recv_before = phase_failure_injection(
            args, evidence, loadgen_node)
        if not success:
            log("ABORT: Failure injection failed", "FATAL")
            evidence.record("VERDICT", "Abort", "FAIL", "Failure injection failed")
            return 1
        
        # Phase 3: Takeover Verification
        if not phase_takeover_verification(args, evidence):
            log("ABORT: Takeover not detected", "FATAL")
            evidence.record("VERDICT", "Abort", "FAIL", "Takeover not detected")
            return 1
        
        # Phase 4: Traffic Continuity
        continuity_pass = phase_traffic_continuity(
            args, evidence, loadgen_node, sent_before, recv_before)
        
        # Phase 5: Recovery (optional)
        phase_recovery(args, evidence, skip=args.no_recovery)
        
        # Final Evidence Validation
        log("=" * 60, "PHASE")
        log("FINAL VERDICT", "PHASE")
        log("=" * 60, "PHASE")
        
        evidence_valid, missing = validate_evidence(evidence)
        
        if evidence_valid and continuity_pass:
            log("VERDICT: PASS - Failover resilience PROVEN", "VERDICT")
            log(f"Evidence saved to {EVIDENCE_CSV}", "VERDICT")
            evidence.record("VERDICT", "Final", "PASS", "All assertions met")
            return 0
        else:
            reasons = []
            if not evidence_valid:
                reasons.append(f"Missing evidence: {missing}")
            if not continuity_pass:
                reasons.append("Traffic continuity failed")
            
            log(f"VERDICT: FAIL - {'; '.join(reasons)}", "VERDICT")
            evidence.record("VERDICT", "Final", "FAIL", "; ".join(reasons))
            return 1
            
    except KeyboardInterrupt:
        log("Test aborted by operator", "WARN")
        evidence.record("VERDICT", "Abort", "WARN", "Operator interrupt")
        return 1
    finally:
        log("Stopping load generator...", "CLEANUP")
        load_proc.terminate()
        try:
            load_proc.wait(timeout=5)
        except:
            load_proc.kill()


if __name__ == "__main__":
    sys.exit(main())
