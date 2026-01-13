#!/usr/bin/env python3
"""
Scalability Test: Find True Sustainable Load
=============================================

This test finds the MAXIMUM SUSTAINABLE LOAD by measuring queue growth rate.

Key Principle: If queue grows unbounded, the system is OVERLOADED at that load.
              Eventual delivery does NOT equal sustainable performance.

Metrics:
- Queue Depth = Sent - Recv (backlog size)
- Queue Growth Rate = d(Queue)/dt over time
- Stable = Growth Rate ≈ 0
- Overloaded = Growth Rate > 0 consistently

Usage:
    python3 test_scalability.py --edge-node iris_edge1@100.82.212.50 \
        --core iris_core1@100.95.21.52

Author: Senior Principal Engineer - Performance Analysis
"""

import sys
import os
import time
import argparse
import subprocess
import csv
from datetime import datetime
from typing import Tuple, List, Dict, Optional
from dataclasses import dataclass

# ============================================================================
# Configuration
# ============================================================================

COOKIE = "iris_secret"
SAMPLE_INTERVAL = 5  # seconds between samples
STEADY_STATE_DURATION = 60  # seconds per load level
RAMP_UP_WAIT = 10  # seconds to let connections establish
QUEUE_GROWTH_THRESHOLD = 0.10  # 10% growth rate = overloaded

# Load levels to test (binary search will narrow down)
LOAD_LEVELS = [100, 250, 500, 1000, 2500, 5000, 7500, 10000]

# Output files
METRICS_CSV = "scalability_metrics.csv"
RESULTS_CSV = "scalability_results.csv"
LOG_FILE = "scalability_test.log"

# ============================================================================
# Data Classes
# ============================================================================

@dataclass
class Sample:
    """Single point-in-time measurement."""
    timestamp: float
    elapsed: float
    load_level: int
    sent: int
    recv: int
    queue_depth: int
    p50_ms: int = 0
    p95_ms: int = 0
    p99_ms: int = 0
    
    @property
    def delivery_rate(self) -> float:
        return self.recv / self.sent if self.sent > 0 else 0.0


@dataclass
class LoadLevelResult:
    """Result of testing a single load level."""
    load_level: int
    samples: List[Sample]
    avg_queue_growth_rate: float
    final_queue_depth: int
    final_delivery_rate: float
    status: str  # STABLE, OVERLOADED, DRAINING
    
    def to_dict(self) -> Dict:
        return {
            "load_level": self.load_level,
            "num_samples": len(self.samples),
            "avg_queue_growth_rate": round(self.avg_queue_growth_rate, 4),
            "final_queue_depth": self.final_queue_depth,
            "final_delivery_rate": round(self.final_delivery_rate, 4),
            "status": self.status
        }


# ============================================================================
# Logging
# ============================================================================

def log(msg: str, level: str = "INFO") -> None:
    ts = datetime.now().strftime("%H:%M:%S.%f")[:-3]
    formatted = f"[{ts}] [{level:8}] {msg}"
    print(formatted, flush=True)
    with open(LOG_FILE, "a") as f:
        f.write(formatted + "\n")


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


def get_traffic_stats(loadgen_node: str) -> Tuple[int, int]:
    """Get sent/recv counts from load generator."""
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
    """Get latency percentiles from load generator."""
    cmd = (
        f"Stats = rpc:call('{loadgen_node}', iris_extreme_gen, get_latency_stats, []), "
        f"io:format('~p', [Stats]), init:stop()."
    )
    result = run_erl_rpc(cmd)
    
    # Parse Erlang map format: {ok,#{p50 => X, p95 => Y, ...}}
    default = {'p50': 0, 'p95': 0, 'p99': 0, 'max': 0, 'avg': 0, 'samples': 0}
    
    if "{ok," in result and "#{" in result:
        try:
            # Extract map content
            map_str = result.split("#{")[1].split("}")[0]
            # Parse key => value pairs
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


# ============================================================================
# Load Generator Management
# ============================================================================

def start_load_generator(args, num_users: int, duration: int) -> Tuple[subprocess.Popen, str]:
    """Start the load generator and return process + node name."""
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
    
    log_file = open(f"loadgen_{num_users}.log", "w")
    
    proc = subprocess.Popen(
        f"erl -name {loadgen_node} -setcookie {COOKIE} -pa ebin -hidden "
        f"-noshell -eval \"net_adm:ping('{args.edge_node}'), {cmd}\"",
        shell=True, stdout=log_file, stderr=log_file
    )
    
    return proc, loadgen_node


def stop_load_generator(proc: subprocess.Popen) -> None:
    """Stop the load generator."""
    proc.terminate()
    try:
        proc.wait(timeout=5)
    except:
        proc.kill()


# ============================================================================
# Metrics Collection
# ============================================================================

def collect_samples(loadgen_node: str, load_level: int, 
                   duration: int, interval: int) -> List[Sample]:
    """Collect samples over the test duration."""
    samples = []
    start_time = time.time()
    
    while time.time() - start_time < duration:
        sent, recv = get_traffic_stats(loadgen_node)
        latency = get_latency_stats(loadgen_node)
        elapsed = time.time() - start_time
        
        sample = Sample(
            timestamp=time.time(),
            elapsed=round(elapsed, 1),
            load_level=load_level,
            sent=sent,
            recv=recv,
            queue_depth=sent - recv,
            p50_ms=latency.get('p50', 0),
            p95_ms=latency.get('p95', 0),
            p99_ms=latency.get('p99', 0)
        )
        samples.append(sample)
        
        log(f"  [{int(elapsed):3d}s] Sent={sent:6d} Recv={recv:6d} Queue={sample.queue_depth:6d} "
            f"P50={sample.p50_ms}ms P95={sample.p95_ms}ms P99={sample.p99_ms}ms", "SAMPLE")
        
        time.sleep(interval)
    
    return samples


def calculate_queue_growth_rate(samples: List[Sample]) -> float:
    """
    Calculate average queue growth rate.
    
    Returns: Rate as fraction per interval (positive = growing, negative = draining)
    """
    if len(samples) < 2:
        return 0.0
    
    growth_rates = []
    for i in range(1, len(samples)):
        prev = samples[i-1]
        curr = samples[i]
        
        if prev.queue_depth > 0:
            rate = (curr.queue_depth - prev.queue_depth) / prev.queue_depth
        else:
            rate = 1.0 if curr.queue_depth > 0 else 0.0
        
        growth_rates.append(rate)
    
    return sum(growth_rates) / len(growth_rates) if growth_rates else 0.0


def determine_status(avg_growth_rate: float) -> str:
    """Determine system status based on queue growth rate."""
    if avg_growth_rate > QUEUE_GROWTH_THRESHOLD:
        return "OVERLOADED"
    elif avg_growth_rate < -QUEUE_GROWTH_THRESHOLD:
        return "DRAINING"
    else:
        return "STABLE"


# ============================================================================
# Test Execution
# ============================================================================

def test_load_level(args, load_level: int) -> LoadLevelResult:
    """Test a single load level and return results."""
    log(f"=" * 60, "PHASE")
    log(f"Testing Load Level: {load_level} users", "PHASE")
    log(f"=" * 60, "PHASE")
    
    # Start load generator
    log(f"Starting load generator with {load_level} users...", "INFO")
    proc, loadgen_node = start_load_generator(args, load_level, STEADY_STATE_DURATION + 30)
    
    # Wait for ramp-up
    log(f"Ramp-up: waiting {RAMP_UP_WAIT}s for connections...", "INFO")
    time.sleep(RAMP_UP_WAIT)
    
    # Collect samples
    log(f"Collecting samples for {STEADY_STATE_DURATION}s...", "INFO")
    samples = collect_samples(loadgen_node, load_level, 
                              STEADY_STATE_DURATION, SAMPLE_INTERVAL)
    
    # Stop load generator
    log("Stopping load generator...", "INFO")
    stop_load_generator(proc)
    
    # Calculate metrics
    avg_growth_rate = calculate_queue_growth_rate(samples)
    final_sample = samples[-1] if samples else Sample(0, 0, 0, 0, 0, 0)
    status = determine_status(avg_growth_rate)
    
    result = LoadLevelResult(
        load_level=load_level,
        samples=samples,
        avg_queue_growth_rate=avg_growth_rate,
        final_queue_depth=final_sample.queue_depth,
        final_delivery_rate=final_sample.delivery_rate,
        status=status
    )
    
    # Log result
    log(f"Result: {status}", "PASS" if status == "STABLE" else "WARN")
    log(f"  Avg Queue Growth Rate: {avg_growth_rate*100:.1f}%", "INFO")
    log(f"  Final Queue Depth: {final_sample.queue_depth}", "INFO")
    log(f"  Final Delivery Rate: {final_sample.delivery_rate*100:.1f}%", "INFO")
    
    return result


def save_metrics(results: List[LoadLevelResult]) -> None:
    """Save all samples to CSV for analysis."""
    with open(METRICS_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['LoadLevel', 'Elapsed', 'Sent', 'Recv', 'QueueDepth', 'DeliveryRate', 'P50ms', 'P95ms', 'P99ms'])
        
        for result in results:
            for sample in result.samples:
                writer.writerow([
                    sample.load_level,
                    sample.elapsed,
                    sample.sent,
                    sample.recv,
                    sample.queue_depth,
                    round(sample.delivery_rate, 4),
                    sample.p50_ms,
                    sample.p95_ms,
                    sample.p99_ms
                ])
    
    log(f"Metrics saved to {METRICS_CSV}", "INFO")


def save_results(results: List[LoadLevelResult]) -> None:
    """Save summary results to CSV."""
    with open(RESULTS_CSV, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=[
            'load_level', 'num_samples', 'avg_queue_growth_rate',
            'final_queue_depth', 'final_delivery_rate', 'status'
        ])
        writer.writeheader()
        for result in results:
            writer.writerow(result.to_dict())
    
    log(f"Results saved to {RESULTS_CSV}", "INFO")


def find_max_stable_load(results: List[LoadLevelResult]) -> int:
    """Find the highest load level that remained stable."""
    stable_loads = [r.load_level for r in results if r.status == "STABLE"]
    return max(stable_loads) if stable_loads else 0


# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Scalability Test - Find True Sustainable Load')
    parser.add_argument('--edge-node', type=str, required=True,
                       help='Edge node name (e.g. iris_edge1@100.82.212.50)')
    parser.add_argument('--core', type=str, required=True,
                       help='Core node to verify is up')
    parser.add_argument('--levels', type=str, default=None,
                       help='Comma-separated load levels to test (default: auto)')
    args = parser.parse_args()
    
    # Parse custom load levels if provided
    if args.levels:
        load_levels = [int(x.strip()) for x in args.levels.split(',')]
    else:
        load_levels = LOAD_LEVELS
    
    # Initialize
    log("=" * 70, "MAIN")
    log("    SCALABILITY TEST: FIND TRUE SUSTAINABLE LOAD", "MAIN")
    log("=" * 70, "MAIN")
    log(f"Edge: {args.edge_node}", "CONFIG")
    log(f"Core: {args.core}", "CONFIG")
    log(f"Load Levels: {load_levels}", "CONFIG")
    log(f"Steady State Duration: {STEADY_STATE_DURATION}s per level", "CONFIG")
    log(f"Queue Growth Threshold: {QUEUE_GROWTH_THRESHOLD*100:.0f}%", "CONFIG")
    log("", "MAIN")
    
    # Pre-flight
    log("Pre-flight: Checking nodes...", "INFO")
    if not ping_node(args.edge_node):
        log(f"FATAL: Edge node {args.edge_node} not reachable", "FAIL")
        return 1
    if not ping_node(args.core):
        log(f"FATAL: Core node {args.core} not reachable", "FAIL")
        return 1
    log("Pre-flight: PASS", "PASS")
    
    # Run tests
    results = []
    for load_level in load_levels:
        try:
            result = test_load_level(args, load_level)
            results.append(result)
            
            # Early termination if severely overloaded
            if result.status == "OVERLOADED" and result.final_delivery_rate < 0.3:
                log(f"Stopping early: system severely overloaded at {load_level}", "WARN")
                break
                
        except KeyboardInterrupt:
            log("Test interrupted by user", "WARN")
            break
        except Exception as e:
            log(f"Error testing {load_level}: {e}", "ERROR")
            continue
    
    # Save data
    save_metrics(results)
    save_results(results)
    
    # Final analysis
    log("=" * 60, "PHASE")
    log("FINAL ANALYSIS", "PHASE")
    log("=" * 60, "PHASE")
    
    max_stable = find_max_stable_load(results)
    
    log("", "INFO")
    log("Load Level Summary:", "INFO")
    for result in results:
        status_marker = "✓" if result.status == "STABLE" else "✗"
        log(f"  {status_marker} {result.load_level:6d} users: {result.status:10s} "
            f"(Queue Growth: {result.avg_queue_growth_rate*100:+.1f}%, "
            f"Delivery: {result.final_delivery_rate*100:.1f}%)", "INFO")
    
    log("", "INFO")
    log(f"MAXIMUM SUSTAINABLE LOAD: {max_stable} concurrent users", 
        "RESULT" if max_stable > 0 else "FAIL")
    
    if max_stable == 0:
        log("WARNING: System could not sustain ANY tested load level", "FAIL")
        log("         This indicates a fundamental scalability issue", "FAIL")
    elif max_stable < load_levels[-1]:
        log(f"System becomes OVERLOADED above {max_stable} users", "WARN")
    else:
        log(f"System is STABLE at all tested levels (up to {max_stable})", "PASS")
    
    return 0 if max_stable > 0 else 1


if __name__ == "__main__":
    sys.exit(main())
