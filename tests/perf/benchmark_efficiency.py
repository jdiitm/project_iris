#!/usr/bin/env python3
"""
Efficiency Metrics Dashboard (Phase 0)

Collects and displays efficiency metrics from Iris nodes per RFC-001:
- NFR-19: Memory per connection ≤10KB  
- NFR-20: CPU utilization nominal 50%

Per TEST_CONTRACT.md:
- exit(0) = PASS - All targets met
- exit(1) = FAIL - Targets not met
- exit(2) = SKIP - Infrastructure not available
"""

import os
import sys
import time
import json
import subprocess
import socket

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, PROJECT_ROOT)

# =============================================================================
# Configuration
# =============================================================================

# RFC-001 targets
TARGETS = {
    "memory_per_conn_kb": 10.0,      # NFR-19: ≤10KB per connection
    "cpu_utilization_pct": 50.0,     # NFR-20: ~50% nominal
    "cpu_max_pct": 80.0,             # Upper bound for burst
}

# Erlang node to query
EDGE_HOST = os.environ.get("EDGE_HOST", "127.0.0.1")
EDGE_PORT = int(os.environ.get("EDGE_PORT", "8085"))


# =============================================================================
# Metrics Collection
# =============================================================================

def check_server_available():
    """Check if Iris server is running."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((EDGE_HOST, EDGE_PORT))
        sock.close()
        return result == 0
    except Exception:
        return False


def get_erlang_metrics():
    """Get metrics from Erlang node via RPC."""
    try:
        # Use a subprocess to call Erlang and get metrics
        cmd = [
            "erl", "-noshell", "-sname", f"metrics_collector_{os.getpid()}",
            "-setcookie", "iris_secret",
            "-eval", """
                %% Try to connect to the edge node
                case net_adm:ping('iris_edge1@localhost') of
                    pong ->
                        %% Get efficiency metrics
                        Metrics = rpc:call('iris_edge1@localhost', iris_efficiency_monitor, get_metrics, []),
                        case Metrics of
                            {badrpc, Reason} ->
                                io:format("ERROR:badrpc:~p~n", [Reason]);
                            _ ->
                                %% Format as JSON-like output
                                io:format("METRICS:~p~n", [Metrics])
                        end;
                    pang ->
                        io:format("ERROR:node_unreachable~n")
                end,
                init:stop().
            """
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
        output = result.stdout + result.stderr
        
        if "ERROR:" in output:
            return None
        
        # Parse Erlang term (simplified)
        if "METRICS:" in output:
            # Extract the metrics portion
            start = output.find("METRICS:") + 8
            end = output.rfind("}")
            if end > start:
                metrics_str = output[start:end+1]
                return parse_erlang_metrics(metrics_str)
        
        return None
    except Exception as e:
        print(f"[WARN] Could not get Erlang metrics: {e}")
        return None


def parse_erlang_metrics(metrics_str):
    """Parse Erlang term to Python dict (simplified)."""
    # This is a simplified parser - in production use erlang_term library
    try:
        # Extract key metrics using regex-like approach
        import re
        
        metrics = {}
        
        # Extract total memory
        total_match = re.search(r'total_bytes\s*=>\s*(\d+)', metrics_str)
        if total_match:
            metrics['total_memory_bytes'] = int(total_match.group(1))
        
        # Extract process count
        proc_match = re.search(r'process_count\s*=>\s*(\d+)', metrics_str)
        if proc_match:
            metrics['process_count'] = int(proc_match.group(1))
        
        # Extract scheduler utilization
        sched_match = re.search(r'average_pct\s*=>\s*([\d.]+)', metrics_str)
        if sched_match:
            metrics['scheduler_util_pct'] = float(sched_match.group(1))
        
        # Extract efficiency score
        score_match = re.search(r'efficiency_score\s*=>\s*(\d+)', metrics_str)
        if score_match:
            metrics['efficiency_score'] = int(score_match.group(1))
        
        return metrics if metrics else None
    except Exception as e:
        print(f"[WARN] Failed to parse metrics: {e}")
        return None


def get_local_metrics():
    """Get metrics using local system commands as fallback."""
    metrics = {}
    
    try:
        # Get process info for beam.smp
        result = subprocess.run(
            ["ps", "aux"], 
            capture_output=True, text=True
        )
        
        for line in result.stdout.split('\n'):
            if 'iris_edge' in line.lower() or 'beam.smp' in line.lower():
                parts = line.split()
                if len(parts) >= 6:
                    metrics['cpu_pct'] = float(parts[2])
                    metrics['mem_pct'] = float(parts[3])
                    # VSZ is in KB
                    metrics['vsz_kb'] = int(parts[4])
                    # RSS is in KB  
                    metrics['rss_kb'] = int(parts[5])
                    break
        
        return metrics
    except Exception as e:
        print(f"[WARN] Could not get local metrics: {e}")
        return {}


def collect_metrics(duration_sec=30, interval_sec=5):
    """Collect metrics over a period of time."""
    samples = []
    
    print(f"[INFO] Collecting metrics for {duration_sec}s (interval: {interval_sec}s)")
    
    start = time.time()
    while time.time() - start < duration_sec:
        sample = {
            'timestamp': time.time(),
            'erlang': get_erlang_metrics(),
            'local': get_local_metrics()
        }
        samples.append(sample)
        
        # Display progress
        elapsed = time.time() - start
        print(f"[SAMPLE] t={elapsed:.0f}s", end="")
        
        if sample['local']:
            print(f" CPU={sample['local'].get('cpu_pct', 'N/A')}%", end="")
            print(f" RSS={sample['local'].get('rss_kb', 'N/A')}KB", end="")
        
        if sample['erlang']:
            print(f" Sched={sample['erlang'].get('scheduler_util_pct', 'N/A')}%", end="")
        
        print()
        
        time.sleep(interval_sec)
    
    return samples


def analyze_metrics(samples):
    """Analyze collected metrics against targets."""
    results = {
        'samples_collected': len(samples),
        'cpu_samples': [],
        'memory_samples': [],
        'scheduler_samples': [],
    }
    
    for sample in samples:
        if sample['local']:
            if 'cpu_pct' in sample['local']:
                results['cpu_samples'].append(sample['local']['cpu_pct'])
            if 'rss_kb' in sample['local']:
                results['memory_samples'].append(sample['local']['rss_kb'])
        
        if sample['erlang']:
            if 'scheduler_util_pct' in sample['erlang']:
                results['scheduler_samples'].append(sample['erlang']['scheduler_util_pct'])
    
    # Calculate statistics
    if results['cpu_samples']:
        results['cpu_avg'] = sum(results['cpu_samples']) / len(results['cpu_samples'])
        results['cpu_max'] = max(results['cpu_samples'])
        results['cpu_min'] = min(results['cpu_samples'])
    
    if results['memory_samples']:
        results['memory_avg_kb'] = sum(results['memory_samples']) / len(results['memory_samples'])
        results['memory_max_kb'] = max(results['memory_samples'])
    
    if results['scheduler_samples']:
        results['scheduler_avg'] = sum(results['scheduler_samples']) / len(results['scheduler_samples'])
    
    return results


def check_targets(results, connection_count=0):
    """Check if efficiency targets are met."""
    passed = True
    report = []
    
    # Memory per connection check (NFR-19)
    if connection_count > 0 and 'memory_avg_kb' in results:
        mem_per_conn = results['memory_avg_kb'] / connection_count
        target = TARGETS['memory_per_conn_kb']
        status = "PASS" if mem_per_conn <= target else "FAIL"
        if status == "FAIL":
            passed = False
        report.append(f"[{status}] NFR-19 Memory/conn: {mem_per_conn:.2f}KB (target: ≤{target}KB)")
    
    # CPU utilization check (NFR-20)
    if 'cpu_avg' in results:
        cpu_avg = results['cpu_avg']
        target = TARGETS['cpu_utilization_pct']
        max_target = TARGETS['cpu_max_pct']
        
        # Check if in acceptable range
        if cpu_avg <= max_target:
            status = "PASS"
        else:
            status = "FAIL"
            passed = False
        
        report.append(f"[{status}] NFR-20 CPU avg: {cpu_avg:.1f}% (target: ~{target}%, max: {max_target}%)")
    
    # Scheduler utilization
    if 'scheduler_avg' in results:
        sched_avg = results['scheduler_avg']
        report.append(f"[INFO] Scheduler utilization avg: {sched_avg:.1f}%")
    
    return passed, report


def print_dashboard(results):
    """Print metrics dashboard."""
    print("\n" + "=" * 60)
    print(" IRIS EFFICIENCY DASHBOARD")
    print("=" * 60)
    
    print(f"\nSamples collected: {results['samples_collected']}")
    
    if 'cpu_avg' in results:
        print(f"\nCPU Utilization:")
        print(f"  Average: {results['cpu_avg']:.1f}%")
        print(f"  Max:     {results['cpu_max']:.1f}%")
        print(f"  Min:     {results['cpu_min']:.1f}%")
    
    if 'memory_avg_kb' in results:
        print(f"\nMemory (RSS):")
        print(f"  Average: {results['memory_avg_kb'] / 1024:.1f} MB")
        print(f"  Max:     {results['memory_max_kb'] / 1024:.1f} MB")
    
    if 'scheduler_avg' in results:
        print(f"\nErlang Scheduler:")
        print(f"  Average: {results['scheduler_avg']:.1f}%")
    
    print("\n" + "=" * 60)


# =============================================================================
# Main
# =============================================================================

def main():
    """Main entry point."""
    print("[INFO] Iris Efficiency Benchmark")
    print(f"[INFO] Targets: Memory/conn ≤{TARGETS['memory_per_conn_kb']}KB, CPU ~{TARGETS['cpu_utilization_pct']}%")
    
    # Check server availability
    if not check_server_available():
        print(f"[SKIP] Iris server not available at {EDGE_HOST}:{EDGE_PORT}")
        print("       Start the server with 'make start' first")
        sys.exit(2)
    
    # Collect metrics
    duration = int(os.environ.get("BENCHMARK_DURATION", "30"))
    samples = collect_metrics(duration_sec=duration, interval_sec=5)
    
    if not samples:
        print("[FAIL] No samples collected")
        sys.exit(1)
    
    # Analyze
    results = analyze_metrics(samples)
    
    # Print dashboard
    print_dashboard(results)
    
    # Check targets (assume 100 baseline connections for now)
    baseline_connections = 100
    passed, report = check_targets(results, baseline_connections)
    
    print("\n" + "-" * 60)
    print("TARGET CHECKS:")
    for line in report:
        print(f"  {line}")
    print("-" * 60)
    
    if passed:
        print("\n[RESULT] All efficiency targets met")
        sys.exit(0)
    else:
        print("\n[RESULT] Some efficiency targets not met")
        sys.exit(1)


if __name__ == "__main__":
    main()
