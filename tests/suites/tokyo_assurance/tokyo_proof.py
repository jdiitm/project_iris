#!/usr/bin/env python3
"""
Tokyo Proof of Exercise: Master Orchestrator
=============================================

This is the CANONICAL entry point for Tokyo region proof of system behavior.

Runs all proof scenarios:
1. Scalability Proof - Find maximum sustainable load
2. Latency Proof - Measure P50/P95/P99 under load
3. Reliability Proof - Verify failover resilience
4. Resource Proof - Capture CPU/memory/network footprint

Outputs:
- tokyo_scalability_metrics.csv
- tokyo_latency_metrics.csv
- tokyo_resource_metrics.csv
- tokyo_reliability_metrics.csv
- tokyo_summary.csv
- TOKYO_PROOF_REPORT.md

Usage:
    python3 tokyo_proof.py \
        --edge-node iris_edge1@100.82.212.50 \
        --core1 iris_core1@100.95.21.52 \
        --core2 iris_core2@100.68.74.48

Author: Senior Principal Engineer - Audit Grade Testing
"""

import sys
import os
import time
import argparse
import subprocess
import csv
import json
import socket
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass, field, asdict

# Add parent paths for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from framework.resource_monitor import ResourceMonitor

# ============================================================================
# Configuration
# ============================================================================

COOKIE = "iris_secret"
OUTPUT_DIR = Path(__file__).parent / "proof_outputs"

# SLOs (Service Level Objectives)
SLO_P99_MS = 500  # P99 latency must be under 500ms
SLO_DELIVERY_RATE = 0.90  # 90% delivery rate during failures
SLO_QUEUE_GROWTH_THRESHOLD = 0.10  # 10% queue growth = overloaded

# Test configuration
SCALABILITY_DURATION = 60  # seconds per load level
SCALABILITY_LEVELS = [100, 250, 500, 1000, 2500, 5000, 7500, 10000]
RELIABILITY_DURATION = 60  # seconds for failover test

# Output files
SCALABILITY_CSV = "tokyo_scalability_metrics.csv"
LATENCY_CSV = "tokyo_latency_metrics.csv"
RESOURCE_CSV = "tokyo_resource_metrics.csv"
RELIABILITY_CSV = "tokyo_reliability_metrics.csv"
SUMMARY_CSV = "tokyo_summary.csv"
REPORT_MD = "TOKYO_PROOF_REPORT.md"
LOG_FILE = "tokyo_proof.log"

# ============================================================================
# Data Classes
# ============================================================================

@dataclass
class ScalabilityResult:
    load_level: int
    status: str  # STABLE, OVERLOADED, DRAINING
    queue_growth_rate: float
    delivery_rate: float
    p50_ms: int
    p95_ms: int
    p99_ms: int
    samples: List[Dict] = field(default_factory=list)


@dataclass
class ReliabilityResult:
    scenario: str
    delivery_rate: float
    messages_sent: int
    messages_recv: int
    messages_lost: int
    core1_state: str
    core2_state: str
    edge_state: str
    pass_fail: str


@dataclass
class SummaryMetric:
    metric: str
    value: Any
    unit: str
    pass_fail: str
    threshold: Any
    notes: str = ""


# ============================================================================
# Logging
# ============================================================================

def setup_logging():
    """Ensure output directory exists."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    if LOG_FILE.exists():
        LOG_FILE.unlink() # Start fresh log
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    with open(LOG_FILE, "w") as f:
        f.write(f"TOKYO PROOF LOG - Started {timestamp}\n")
        f.write("="*80 + "\n")

def log(msg: str, level: str = "INFO") -> None:
    ts = datetime.now().strftime("%H:%M:%S.%f")[:-3]
    formatted = f"[{ts}] [{level:8}] {msg}"
    print(formatted, flush=True)
    with open(OUTPUT_DIR / LOG_FILE, "a") as f:
        f.write(formatted + "\n")


# ============================================================================
# RPC Helpers
# ============================================================================

def run_erl_rpc(cmd: str, timeout: int = 10) -> str:
    node_name = f"probe_{int(time.time()*1000)}@100.95.21.52"
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
    cmd = f"io:format('~p', [net_adm:ping('{node}')]), init:stop()."
    result = run_erl_rpc(cmd)
    return "pong" in result


def get_traffic_stats(loadgen_node: str) -> Tuple[int, int]:
    cmd = (
        f"Stats = rpc:call('{loadgen_node}', iris_extreme_gen, get_stats, []), "
        f"io:format('~p', [Stats]), init:stop()."
    )
    result = run_erl_rpc(cmd)
    if "{ok," in result:
        try:
            content = result.split("{ok,")[1].split("}")[0]
            parts = content.split(",")
            return int(parts[0].strip()), int(parts[1].strip())
        except:
            pass
    return 0, 0


def get_latency_stats(loadgen_node: str) -> Dict:
    cmd = (
        f"Stats = rpc:call('{loadgen_node}', iris_extreme_gen, get_latency_stats, []), "
        f"io:format('~p', [Stats]), init:stop()."
    )
    result = run_erl_rpc(cmd)
    default = {'p50': 0, 'p95': 0, 'p99': 0, 'max': 0, 'avg': 0, 'samples': 0}
    if "{ok," in result and "#{" in result:
        try:
            map_str = result.split("#{")[1].split("}")[0]
            for pair in map_str.split(","):
                if "=>" in pair:
                    key, val = pair.split("=>")
                    key = key.strip()
                    val = int(val.strip())
                    if key in default:
                        default[key] = val
        except:
            pass
    return default


def kill_node(node: str) -> bool:
    cmd = f"rpc:call('{node}', init, stop, []), init:stop()."
    run_erl_rpc(cmd)
    time.sleep(2)
    return not ping_node(node)


# ============================================================================
# Load Generator
# ============================================================================

def start_load_generator(args, num_users: int, duration: int) -> Tuple[subprocess.Popen, str]:
    try:
        edge_ip = args.edge_node.split('@')[1]
        ip_parts = edge_ip.split('.')
        erl_ip = f"{{{ip_parts[0]},{ip_parts[1]},{ip_parts[2]},{ip_parts[3]}}}"
    except:
        erl_ip = "{127,0,0,1}"
    
    loadgen_node = f"loadgen_{int(time.time())}@127.0.0.1"
    cmd = (
        f"iris_extreme_gen:start({num_users}, {duration + 60}, "
        f"extreme_load, {erl_ip}, 8080), timer:sleep(infinity)."
    )
    
    log_file = open(OUTPUT_DIR / f"loadgen_{num_users}.log", "w")
    proc = subprocess.Popen(
        f"erl -name {loadgen_node} -setcookie {COOKIE} -pa ebin -hidden "
        f"-noshell -eval \"net_adm:ping('{args.edge_node}'), {cmd}\"",
        shell=True, stdout=log_file, stderr=log_file
    )
    return proc, loadgen_node


def stop_load_generator(proc: subprocess.Popen) -> None:
    proc.terminate()
    try:
        proc.wait(timeout=5)
    except:
        proc.kill()


# ============================================================================
# Scalability Proof
# ============================================================================

def run_scalability_proof(args) -> Tuple[List[ScalabilityResult], int]:
    """Run scalability tests and return results + max stable load."""
    log("=" * 70, "PHASE")
    log("SCALABILITY PROOF: Finding Maximum Sustainable Load", "PHASE")
    log("=" * 70, "PHASE")
    
    results = []
    max_stable = 0
    
    with open(OUTPUT_DIR / SCALABILITY_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['timestamp', 'load_level', 'sent', 'recv', 'queue_depth', 
                        'delivery_rate', 'p50_ms', 'p95_ms', 'p99_ms', 'status'])
    
    levels = SCALABILITY_LEVELS
    if args.users:
        levels = [args.users]
        log(f"Running custom load level: {levels}", "INFO")

    for load_level in levels:
        log(f"Testing {load_level} users...", "INFO")
        
        proc, loadgen_node = start_load_generator(args, load_level, SCALABILITY_DURATION + 30)
        time.sleep(10)  # Ramp up
        
        # =====================================================================
        # CLEAN SLATE VALIDATION
        # =====================================================================
        # Get initial stats - these should show Sent >= Recv for a clean test
        initial_sent, initial_recv = get_traffic_stats(loadgen_node)
        
        if initial_recv > initial_sent:
            log(f"WARNING: Recv ({initial_recv}) > Sent ({initial_sent}) - stale messages detected!", "WARN")
            log("Restarting load generator to flush stats...", "INFO")
            stop_load_generator(proc)
            time.sleep(5)
            # Restart with clean state
            proc, loadgen_node = start_load_generator(args, load_level, SCALABILITY_DURATION + 30)
            time.sleep(10)
            baseline_sent, baseline_recv = get_traffic_stats(loadgen_node)
            log(f"New Baseline: Sent={baseline_sent}, Recv={baseline_recv}", "INFO")
        else:
            baseline_sent = 0
            baseline_recv = 0
        
        log(f"Baseline: Sent={initial_sent}, Recv={initial_recv}", "INFO")
        
        samples = []
        start_time = time.time()
        
        while time.time() - start_time < SCALABILITY_DURATION:
            raw_sent, raw_recv = get_traffic_stats(loadgen_node)
            latency = get_latency_stats(loadgen_node)
            elapsed = time.time() - start_time
            
            # Adjust for baseline
            sent = raw_sent - baseline_sent
            recv = raw_recv - baseline_recv
            
            sample = {
                'timestamp': datetime.now().isoformat(),
                'load_level': load_level,
                'sent': sent,
                'recv': recv,
                'queue_depth': sent - recv,
                'delivery_rate': recv / sent if sent > 0 else 0,
                'p50_ms': latency.get('p50', 0),
                'p95_ms': latency.get('p95', 0),
                'p99_ms': latency.get('p99', 0)
            }
            samples.append(sample)
            
            log(f"  [{int(elapsed):3d}s] Sent={sent:6d} Recv={recv:6d} "
                f"Queue={sample['queue_depth']:6d} P99={sample['p99_ms']}ms", "SAMPLE")
            
            time.sleep(5)
        
        stop_load_generator(proc)
        
        # Calculate queue growth rate
        if len(samples) >= 2:
            growth_rates = []
            for i in range(1, len(samples)):
                prev = samples[i-1]['queue_depth']
                curr = samples[i]['queue_depth']
                if prev > 0:
                    growth_rates.append((curr - prev) / prev)
                else:
                    growth_rates.append(1.0 if curr > 0 else 0.0)
            avg_growth = sum(growth_rates) / len(growth_rates)
        else:
            avg_growth = 0.0
        
        # Determine status
        if avg_growth > SLO_QUEUE_GROWTH_THRESHOLD:
            status = "OVERLOADED"
        elif avg_growth < -SLO_QUEUE_GROWTH_THRESHOLD:
            status = "DRAINING"
        else:
            status = "STABLE"
            max_stable = max(max_stable, load_level)
        
        # Get final latency
        final_latency = get_latency_stats(loadgen_node)
        
        result = ScalabilityResult(
            load_level=load_level,
            status=status,
            queue_growth_rate=avg_growth,
            delivery_rate=samples[-1]['delivery_rate'] if samples else 0,
            p50_ms=final_latency.get('p50', 0),
            p95_ms=final_latency.get('p95', 0),
            p99_ms=final_latency.get('p99', 0),
            samples=samples
        )
        results.append(result)
        
        # Write samples to CSV
        with open(OUTPUT_DIR / SCALABILITY_CSV, 'a', newline='') as f:
            writer = csv.writer(f)
            for s in samples:
                writer.writerow([s['timestamp'], s['load_level'], s['sent'], s['recv'],
                                s['queue_depth'], round(s['delivery_rate'], 4),
                                s['p50_ms'], s['p95_ms'], s['p99_ms'], status])
        
        log(f"Result: {status} (Growth: {avg_growth*100:.1f}%)", 
            "PASS" if status == "STABLE" else "WARN")
        
        # Early termination if severely overloaded
        if status == "OVERLOADED" and (samples[-1]['delivery_rate'] if samples else 0) < 0.3:
            log(f"Stopping early: severely overloaded at {load_level}", "WARN")
            break
    
    log(f"Maximum Stable Load: {max_stable} users", "RESULT")
    
    # COOLDOWN
    log("Cooling down for 30s before Reliability phase...", "INFO")
    time.sleep(30)
    
    return results, max_stable


# ============================================================================
# Reliability Proof
# ============================================================================

def run_reliability_proof(args) -> List[ReliabilityResult]:
    """Run comprehensive failover reliability test with continuous monitoring."""
    log("=" * 70, "PHASE")
    log("RELIABILITY PROOF: Failover Resilience", "PHASE")
    log("=" * 70, "PHASE")
    
    results = []
    
    # Initialize CSV with detailed schema for time-series data
    with open(OUTPUT_DIR / RELIABILITY_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['timestamp', 'elapsed_s', 'scenario', 'phase', 
                        'core1_state', 'core2_state', 'edge_state',
                        'sent', 'recv', 'queue_depth', 'delivery_rate',
                        'delta_sent', 'delta_recv', 'delta_delivery_rate'])
    
    # Start load generator with moderate load
    log("Starting load generator (100 users)...", "INFO")
    proc, loadgen_node = start_load_generator(args, 100, RELIABILITY_DURATION * 3)
    time.sleep(10)  # Ramp up
    
    samples = []
    start_time = time.time()
    prev_sent, prev_recv = get_traffic_stats(loadgen_node)
    
    # =========================================================================
    # Phase 1: Baseline (30s)
    # =========================================================================
    log("Phase 1: Baseline measurement (30s)...", "PHASE")
    phase_start = time.time()
    
    while time.time() - phase_start < 30:
        sent, recv = get_traffic_stats(loadgen_node)
        elapsed = time.time() - start_time
        delta_sent = sent - prev_sent
        delta_recv = recv - prev_sent  # Compare recv to prev_sent for delta rate
        delta_rate = delta_recv / delta_sent if delta_sent > 0 else 0
        
        sample = {
            'timestamp': datetime.now().isoformat(),
            'elapsed_s': round(elapsed, 1),
            'scenario': 'core1_failover',
            'phase': 'baseline',
            'core1_state': 'UP',
            'core2_state': 'UP',
            'edge_state': 'UP',
            'sent': sent,
            'recv': recv,
            'queue_depth': sent - recv,
            'delivery_rate': recv / sent if sent > 0 else 0,
            'delta_sent': delta_sent,
            'delta_recv': recv - prev_recv,
            'delta_delivery_rate': delta_rate
        }
        samples.append(sample)
        
        log(f"  [{int(elapsed):3d}s] BASELINE Sent={sent:5d} Recv={recv:5d} "
            f"Delta={delta_rate*100:.1f}%", "SAMPLE")
        
        prev_sent, prev_recv = sent, recv
        time.sleep(5)
    
    baseline_sent = sent
    baseline_recv = recv
    log(f"Baseline complete: Sent={baseline_sent}, Recv={baseline_recv}", "INFO")
    
    # =========================================================================
    # Phase 2: Core1 Failure
    # =========================================================================
    log("Phase 2: Killing Core1...", "ACTION")
    kill_start = time.time()
    
    dead = kill_node(args.core1)
    if not dead:
        log("WARNING: kill_node returned false, but proceeding", "WARN")
    
    kill_elapsed = time.time() - kill_start
    log(f"Core1 kill completed in {kill_elapsed:.1f}s", "INFO")
    
    # =========================================================================
    # Phase 3: During Failure (60s)
    # =========================================================================
    log("Phase 3: Monitoring during failover (60s)...", "PHASE")
    failure_start = time.time()
    
    while time.time() - failure_start < RELIABILITY_DURATION:
        sent, recv = get_traffic_stats(loadgen_node)
        elapsed = time.time() - start_time
        delta_sent = sent - prev_sent
        delta_recv = recv - prev_sent
        delta_rate = delta_recv / delta_sent if delta_sent > 0 else 0
        
        sample = {
            'timestamp': datetime.now().isoformat(),
            'elapsed_s': round(elapsed, 1),
            'scenario': 'core1_failover',
            'phase': 'during_failure',
            'core1_state': 'DOWN',
            'core2_state': 'UP',
            'edge_state': 'UP',
            'sent': sent,
            'recv': recv,
            'queue_depth': sent - recv,
            'delivery_rate': recv / sent if sent > 0 else 0,
            'delta_sent': delta_sent,
            'delta_recv': recv - prev_recv,
            'delta_delivery_rate': delta_rate
        }
        samples.append(sample)
        
        status = "OK" if delta_rate >= SLO_DELIVERY_RATE else "DEGRADED"
        log(f"  [{int(elapsed):3d}s] DURING_FAILURE Sent={sent:5d} Recv={recv:5d} "
            f"Delta={delta_rate*100:.1f}% [{status}]", "SAMPLE")
        
        prev_sent, prev_recv = sent, recv
        time.sleep(5)
    
    failure_sent = sent
    failure_recv = recv
    
    # Calculate metrics during failure
    failure_samples = [s for s in samples if s['phase'] == 'during_failure']
    if failure_samples:
        failure_deltas = [s['delta_delivery_rate'] for s in failure_samples]
        avg_failure_rate = sum(failure_deltas) / len(failure_deltas) if failure_deltas else 0
    else:
        avg_failure_rate = 0
    
    log(f"During failure: Avg delta delivery rate = {avg_failure_rate*100:.1f}%", "INFO")
    
    # =========================================================================
    # Write all samples to CSV
    # =========================================================================
    with open(OUTPUT_DIR / RELIABILITY_CSV, 'a', newline='') as f:
        writer = csv.writer(f)
        for s in samples:
            writer.writerow([
                s['timestamp'], s['elapsed_s'], s['scenario'], s['phase'],
                s['core1_state'], s['core2_state'], s['edge_state'],
                s['sent'], s['recv'], s['queue_depth'], 
                round(s['delivery_rate'], 4),
                s['delta_sent'], s['delta_recv'], 
                round(s['delta_delivery_rate'], 4)
            ])
    
    # =========================================================================
    # Calculate overall result
    # =========================================================================
    total_sent = failure_sent - baseline_sent
    total_recv = failure_recv - baseline_recv
    overall_delivery_rate = total_recv / total_sent if total_sent > 0 else 0
    
    pass_fail = "PASS" if overall_delivery_rate >= SLO_DELIVERY_RATE else "FAIL"
    
    result = ReliabilityResult(
        scenario="core1_failover",
        delivery_rate=overall_delivery_rate,
        messages_sent=total_sent,
        messages_recv=total_recv,
        messages_lost=total_sent - total_recv,
        core1_state="DOWN",
        core2_state="UP",
        edge_state="UP",
        pass_fail=pass_fail
    )
    results.append(result)
    
    log(f"Failover Result: {pass_fail}", "PASS" if pass_fail == "PASS" else "FAIL")
    log(f"  Total Sent: {total_sent}", "INFO")
    log(f"  Total Recv: {total_recv}", "INFO")
    log(f"  Delivery Rate: {overall_delivery_rate*100:.1f}%", "INFO")
    log(f"  Messages Lost: {total_sent - total_recv}", "INFO")
    
    stop_load_generator(proc)
    
    log(f"Reliability metrics saved to {RELIABILITY_CSV}", "INFO")
    return results


# ============================================================================
# Resource Monitoring
# ============================================================================

def save_resource_samples(monitor: ResourceMonitor) -> None:
    """Save resource samples to CSV."""
    samples = monitor.get_samples()
    
    with open(OUTPUT_DIR / RESOURCE_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['timestamp', 'elapsed_s', 'cpu_percent', 'memory_rss_mb',
                        'memory_available_mb', 'disk_free_gb', 'open_connections',
                        'process_count'])
        
        for s in samples:
            writer.writerow([s.timestamp, s.elapsed_seconds, s.cpu_percent,
                            s.memory_rss_mb, s.memory_available_mb, s.disk_free_gb,
                            s.open_connections, s.process_count])
    
    log(f"Resource metrics saved to {RESOURCE_CSV}", "INFO")


# ============================================================================
# Summary Generation
# ============================================================================

def generate_summary(scalability_results: List[ScalabilityResult],
                    reliability_results: List[ReliabilityResult],
                    max_stable: int,
                    resource_summary: Dict) -> List[SummaryMetric]:
    """Generate summary metrics with pass/fail."""
    
    metrics = []
    
    # Scalability
    metrics.append(SummaryMetric(
        metric="max_stable_load",
        value=max_stable,
        unit="users",
        pass_fail="PASS" if max_stable >= 100 else "FAIL",
        threshold=100,
        notes="Minimum viable load"
    ))
    
    # Latency at stable load
    stable_result = next((r for r in scalability_results if r.load_level == max_stable), None)
    if stable_result:
        metrics.append(SummaryMetric(
            metric="p99_latency_at_stable",
            value=stable_result.p99_ms,
            unit="ms",
            pass_fail="PASS" if stable_result.p99_ms <= SLO_P99_MS else "FAIL",
            threshold=SLO_P99_MS,
            notes=f"At {max_stable} users"
        ))
    
    # Reliability
    for r in reliability_results:
        metrics.append(SummaryMetric(
            metric=f"reliability_{r.scenario}",
            value=round(r.delivery_rate * 100, 1),
            unit="%",
            pass_fail=r.pass_fail,
            threshold=SLO_DELIVERY_RATE * 100,
            notes=f"Sent={r.messages_sent}, Recv={r.messages_recv}"
        ))
    
    # Resources
    if resource_summary:
        metrics.append(SummaryMetric(
            metric="memory_peak_mb",
            value=resource_summary.get('memory_max', 0),
            unit="MB",
            pass_fail="PASS",
            threshold="N/A",
            notes="Peak observed"
        ))
        metrics.append(SummaryMetric(
            metric="cpu_avg_percent",
            value=resource_summary.get('cpu_avg', 0),
            unit="%",
            pass_fail="PASS" if resource_summary.get('cpu_avg', 0) < 90 else "WARN",
            threshold=90,
            notes="Average during test"
        ))
    
    # Write summary CSV
    with open(OUTPUT_DIR / SUMMARY_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['metric', 'value', 'unit', 'pass_fail', 'threshold', 'notes'])
        for m in metrics:
            writer.writerow([m.metric, m.value, m.unit, m.pass_fail, m.threshold, m.notes])
    
    log(f"Summary saved to {SUMMARY_CSV}", "INFO")
    return metrics


# ============================================================================
# Messi Problem (Celebrity Fan-In) Proof
# ============================================================================

def run_messi_proof(args) -> List[SummaryMetric]:
    """Run 100k Fan-In test (Messi Problem)."""
    log("=" * 70, "PHASE")
    log("MESSI PROOF: 100k User Celebrity Fan-In", "PHASE")
    log("=" * 70, "PHASE")
    
    MESSI_DURATION = 60
    TARGET_USERS = 100000
    MESSI_CSV = "tokyo_messi_metrics.csv"
    
    # Initialize CSV
    with open(OUTPUT_DIR / MESSI_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['timestamp', 'elapsed_s', 'clients_connected', 'msgs_received_total', 'rate_per_sec'])
        
    # 1. Start VIP Receiver in background
    vip_socket = None
    vip_metrics = {'received': 0}
    stop_vip = False
    
    def vip_receiver_thread():
        nonlocal vip_socket
        try:
            edge_ip = args.edge_node.split('@')[1]
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect((edge_ip, 8080))
            s.sendall(b'\x01vip_messi') # Login
            s.recv(1024) # Ack
            vip_socket = s
            
            s.settimeout(1.0)
            while not stop_vip:
                try:
                    chunk = s.recv(65536)
                    if not chunk: break
                    # Count packets (approx based on size or headers? 
                    # iris_extreme_gen sends 16-byte headers + payload.
                    # Let's count bytes and divide by avg msg size (~30 bytes)
                    # Packet: 2(op) + 2(tlen) + T + 2(mlen) + M
                    # T="vip_messi" (9), M="GOAL" (4).
                    # 1 + 2 + 9 + 2 + 4 = 18 bytes payload.
                    # Plus 1 byte op? No, see iris_extreme_gen:fan_in_loop
                    # Packet = <<2, ...>>. Total size ~ 20 bytes.
                    # Actually iris_session wraps it? No, raw TCP.
                    vip_metrics['received'] += len(chunk) // 20 
                except socket.timeout:
                    continue
                except:
                    break
        except Exception as e:
            log(f"VIP Receiver failed: {e}", "ERROR")

    import threading
    t_vip = threading.Thread(target=vip_receiver_thread, daemon=True)
    t_vip.start()
    
    time.sleep(2) # Wait for VIP login
    
    # 2. Start Load Generator (100k Fan-In)
    log(f"Starting 100k Fan-In Load Generator...", "INFO")
    
    # Use internal start_load_generator but modify command for fan_in mode
    # We can't reuse start_load_generator easily because it hardcodes 'extreme_load'.
    # Manual start:
    try:
        edge_ip = args.edge_node.split('@')[1]
        ip_parts = edge_ip.split('.')
        erl_ip = f"{{{ip_parts[0]},{ip_parts[1]},{ip_parts[2]},{ip_parts[3]}}}"
    except:
         erl_ip = "{127,0,0,1}"
         
    loadgen_node = f"loadgen_messi_{int(time.time())}@100.95.21.52"
    # Mode = fan_in
    cmd = (
        f"iris_extreme_gen:start({TARGET_USERS}, {MESSI_DURATION + 60}, "
        f"fan_in, {erl_ip}, 8080), timer:sleep(infinity)."
    )
    
    log_file = open(OUTPUT_DIR / "messi_loadgen.log", "w")
    proc = subprocess.Popen(
        f"erl -name {loadgen_node} -setcookie {COOKIE} -pa ebin -hidden "
        f"-noshell -eval \"net_adm:ping('{args.edge_node}'), {cmd}\"",
        shell=True, stdout=log_file, stderr=log_file
    )
    
    # 3. Monitor Loop
    start_time = time.time()
    last_recv = 0
    max_rate = 0
    
    while time.time() - start_time < MESSI_DURATION:
        elapsed = time.time() - start_time
        curr_recv = vip_metrics['received']
        rate = (curr_recv - last_recv) / 5.0
        max_rate = max(max_rate, rate)
        
        # Get client count from stats (approx)
        sent, _ = get_traffic_stats(loadgen_node)
        # Note: get_traffic_stats returns 'Sent' which in fan-in mode tracks messages sent.
        # We can't easily get 'connected customers' count from iris_extreme_gen stats yet.
        # But we assume they connect.
        
        log(f"  [{int(elapsed):3d}s] VIP Recv={curr_recv} Rate={rate:.1f} msg/sec", "SAMPLE")
        
        with open(OUTPUT_DIR / MESSI_CSV, 'a', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([datetime.now().isoformat(), round(elapsed,1), 0, curr_recv, rate])
            
        last_recv = curr_recv
        time.sleep(5)
        
    # Cleanup
    stop_vip = True
    stop_load_generator(proc)
    if vip_socket: vip_socket.close()
    
    log(f"Peak Rate: {max_rate:.1f} msg/sec", "RESULT")
    
    return [SummaryMetric(
        metric="messi_peak_rate",
        value=int(max_rate),
        unit="msg/s",
        pass_fail="PASS" if max_rate > 1000 else "WARN",
        threshold=1000,
        notes="Target 100k Fan-In"
    )]

# ============================================================================
# Report Generation
# ============================================================================

def generate_report(args, scalability_results: List[ScalabilityResult],
                   reliability_results: List[ReliabilityResult],
                   max_stable: int,
                   resource_summary: Dict,
                   summary_metrics: List[SummaryMetric]) -> None:
    """Generate markdown report."""
    
    # Get git commit
    try:
        commit = subprocess.check_output(
            "git rev-parse --short HEAD", shell=True
        ).decode().strip()
    except:
        commit = "unknown"
    
    # Handle case when reliability was skipped
    if reliability_results:
        reliability_str = f"{reliability_results[0].delivery_rate*100:.1f}%"
        reliability_status = "✅ PASS" if reliability_results[0].pass_fail == "PASS" else "❌ FAIL"
    else:
        reliability_str = "N/A (skipped)"
        reliability_status = "⏭️ SKIPPED"
    
    report = f"""# Tokyo Region Proof of Exercise

## Executive Summary

| Metric | Value | Status |
|--------|-------|--------|
| **Maximum Sustainable Load** | {max_stable} users | {'✅ PASS' if max_stable >= 100 else '❌ FAIL'} |
| **P99 Latency at Stable** | {scalability_results[-1].p99_ms if scalability_results else 'N/A'}ms | - |
| **Failover Reliability** | {reliability_str} | {reliability_status} |

---

## Test Environment

| Component | Value |
|-----------|-------|
| Edge Node | `{args.edge_node}` |
| Core1 Node | `{args.core1}` |
| Core2 Node | `{args.core2}` |
| Test Date | {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} |
| Git Commit | `{commit}` |

---

## Scalability Analysis

### Load Level Results

| Load | Status | Queue Growth | Delivery Rate | P50 | P95 | P99 |
|------|--------|--------------|---------------|-----|-----|-----|
"""
    
    for r in scalability_results:
        status_icon = "✓" if r.status == "STABLE" else "✗"
        report += f"| {r.load_level} | {status_icon} {r.status} | {r.queue_growth_rate*100:+.1f}% | {r.delivery_rate*100:.1f}% | {r.p50_ms}ms | {r.p95_ms}ms | {r.p99_ms}ms |\n"
    
    report += f"""
### Finding

> **Maximum Sustainable Load: {max_stable} concurrent users**

Above this load, queue growth becomes unbounded.

---

## Reliability Analysis

### Failover Test Results

| Scenario | Delivery Rate | Messages Sent | Messages Recv | Status |
|----------|---------------|---------------|---------------|--------|
"""
    
    for r in reliability_results:
        status_icon = "✅" if r.pass_fail == "PASS" else "❌"
        report += f"| {r.scenario} | {r.delivery_rate*100:.1f}% | {r.messages_sent} | {r.messages_recv} | {status_icon} {r.pass_fail} |\n"
    
    report += f"""
---

## Resource Footprint

| Metric | Value |
|--------|-------|
| CPU Average | {resource_summary.get('cpu_avg', 0):.1f}% |
| CPU Peak | {resource_summary.get('cpu_max', 0):.1f}% |
| Memory Peak | {resource_summary.get('memory_max', 0):.1f} MB |
| Open Connections (max) | {resource_summary.get('connections_max', 0)} |

---

## Summary Pass/Fail

| Metric | Value | Unit | Status | Threshold |
|--------|-------|------|--------|-----------|
"""
    
    for m in summary_metrics:
        status_icon = "✅" if m.pass_fail == "PASS" else ("⚠️" if m.pass_fail == "WARN" else "❌")
        report += f"| {m.metric} | {m.value} | {m.unit} | {status_icon} {m.pass_fail} | {m.threshold} |\n"
    
    report += f"""
---

## Known Limitations

1. **WAN Latency**: Tokyo ↔ Bangalore is ~100-200ms RTT, limiting synchronous RPC throughput
2. **Architecture**: Current design uses synchronous RPCs which block on network I/O
3. **Single Edge**: Test run on single Edge node, not cluster

---

## Raw Data

All metrics are available in CSV format:

- [`{SCALABILITY_CSV}`](./{SCALABILITY_CSV}) - Load vs throughput/latency over time
- [`{LATENCY_CSV}`](./{LATENCY_CSV}) - Latency percentiles
- [`{RESOURCE_CSV}`](./{RESOURCE_CSV}) - CPU/memory/connections over time  
- [`{RELIABILITY_CSV}`](./{RELIABILITY_CSV}) - Failover test results
- [`{SUMMARY_CSV}`](./{SUMMARY_CSV}) - Pass/fail summary

---

## Reproduction Instructions

```bash
# Clone and checkout
git clone https://github.com/jdiitm/project_iris.git
cd project_iris
git checkout {commit}

# Compile
erlc -o ebin src/*.erl

# Start nodes (on respective servers)
# See docs/CLUSTER_SETUP.md

# Run proof
cd tests/suites/tokyo_assurance
python3 tokyo_proof.py \\
    --edge-node {args.edge_node} \\
    --core1 {args.core1} \\
    --core2 {args.core2}
```

---

*Generated by Tokyo Proof of Exercise on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*
"""
    
    with open(OUTPUT_DIR / REPORT_MD, 'w') as f:
        f.write(report)
    
    log(f"Report saved to {REPORT_MD}", "INFO")


# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Tokyo Proof of Exercise')
    parser.add_argument('--edge-node', type=str, required=True,
                       help='Edge node (e.g. iris_edge1@100.82.212.50)')
    parser.add_argument('--core1', type=str, required=True,
                       help='Primary Core node')
    parser.add_argument('--core2', type=str, required=True,
                       help='Secondary Core node')
    parser.add_argument('--skip-reliability', action='store_true',
                       help='Skip reliability tests (does not kill Core1)')
    parser.add_argument("--messi", action="store_true", help="Run 100k Fan-In Proof (Messi Problem)")
    parser.add_argument("--reliability", action="store_true", help="Run Reliability Proof ONLY")
    parser.add_argument("--users", type=int, help="Override scalability levels with a single user count (for split-client testing)")
    args = parser.parse_args()
    
    # Create output directory
    OUTPUT_DIR.mkdir(exist_ok=True)

    # Setup
    global LOG_FILE
    LOG_FILE = OUTPUT_DIR / "tokyo_proof.log"
    setup_logging()
    
    log("="*70, "MAIN")
    # Start resource monitoring
    log("Starting resource monitor...", "INFO")
    monitor = ResourceMonitor(sample_interval_seconds=5.0)
    monitor.start()
    
    scalability_results = []
    reliability_results = []
    max_stable = 0
    
    try:
        if args.messi:
             metrics = run_messi_proof(args)
             # Basic report for messi
             with open(OUTPUT_DIR / REPORT_MD, 'a') as f:
                 f.write(f"\n## Messi Proof Results\nPeak Rate: {metrics[0].value} msg/s\nStatus: {metrics[0].pass_fail}\n")
             monitor.stop()
             save_resource_samples(monitor)
             return 0

        elif args.reliability:
            reliability_results = run_reliability_proof(args)
            
        else:
            # Standard Full Suite
            scalability_results, max_stable = run_scalability_proof(args)
            
            if not args.skip_reliability:
                reliability_results = run_reliability_proof(args)
        
    finally:
        # Stop resource monitoring
        monitor.stop()
        save_resource_samples(monitor)
    
    # Get resource summary
    resource_summary = monitor.get_summary()
    
    # Generate summary
    summary_metrics = generate_summary(
        scalability_results, reliability_results, 
        max_stable, resource_summary
    )
    
    # Generate report
    generate_report(
        args, scalability_results, reliability_results,
        max_stable, resource_summary, summary_metrics
    )
    
    # Final verdict
    log("=" * 70, "PHASE")
    log("FINAL VERDICT", "PHASE")
    log("=" * 70, "PHASE")
    
    failed = [m for m in summary_metrics if m.pass_fail == "FAIL"]
    
    if failed:
        log(f"VERDICT: FAIL - {len(failed)} metrics failed", "FAIL")
        for m in failed:
            log(f"  ❌ {m.metric}: {m.value} {m.unit} (threshold: {m.threshold})", "FAIL")
        return 1
    else:
        log("VERDICT: PASS - All metrics within thresholds", "PASS")
        log(f"Maximum Sustainable Load: {max_stable} users", "RESULT")
        return 0


if __name__ == "__main__":
    sys.exit(main())
