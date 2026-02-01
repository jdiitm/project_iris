#!/usr/bin/env python3
"""
Global Test Coordinator - runs from j (Bangalore)

Orchestrates comprehensive validation test across all Tailscale nodes.
Collects all data centrally for analysis.

Usage:
    python3 coordinator.py --discover                    # Show available nodes
    python3 coordinator.py --deploy                      # Deploy scripts to all nodes
    python3 coordinator.py --phase latency               # Run latency test
    python3 coordinator.py --phase scale                 # Run scale test
    python3 coordinator.py --run-full-test               # Run everything
    python3 coordinator.py --generate-report             # Generate report from data
"""

import os
import sys
import json
import time
import subprocess
import argparse
import csv
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from concurrent.futures import ThreadPoolExecutor, as_completed

# =============================================================================
# Configuration
# =============================================================================

IRIS_SERVER_IP = "100.95.21.52"  # j
IRIS_PORT = 8085

# Your actual Tailscale network
NODES = {
    # Bangalore (local) - Control Plane
    "j": {
        "ip": "100.95.21.52",
        "user": "j",
        "role": "server",
        "location": "Bangalore",
        "capacity": 0,  # Server doesn't generate load
    },
    "s": {
        "ip": "100.68.74.48",
        "user": "j",
        "role": "generator",
        "location": "Bangalore",
        "capacity": 60000,
    },
    "hmecmb-fvfg32xpq05n": {  # MacBook M1
        "ip": "100.91.106.101",
        "user": "j",  # Adjust if different on Mac
        "role": "generator",
        "location": "Bangalore",
        "capacity": 60000,
    },
    
    # AWS instances (launched and on Tailscale)
    "ip-172-31-18-141": {
        "ip": "100.98.79.95",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-20-113": {
        "ip": "100.75.184.26",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-21-200": {
        "ip": "100.103.138.120",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-27-129": {
        "ip": "100.118.183.108",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-29-249": {
        "ip": "100.89.216.118",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-39-163": {
        "ip": "100.86.53.70",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-39-188": {
        "ip": "100.78.82.22",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
    "ip-172-31-43-76": {
        "ip": "100.69.114.66",
        "user": "ubuntu",
        "role": "probe",
        "location": "AWS",
        "capacity": 5000,
    },
}

# Test configuration
TEST_CONFIG = {
    "latency": {
        "samples": 1000,
    },
    "scale": {
        "levels": [10000, 25000, 50000, 75000, 100000],
        "hold_time_s": 300,  # 5 min per level
    },
    "soak": {
        "connections": 50000,
        "message_rate": 1000,
        "duration_s": 1800,  # 30 min
    },
}


# =============================================================================
# Utilities
# =============================================================================

def log(msg: str):
    """Log with timestamp."""
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"[{ts}] {msg}", flush=True)


def discover_tailscale_nodes() -> Dict[str, str]:
    """Get IPs of all nodes from Tailscale."""
    try:
        result = subprocess.run(
            ["tailscale", "status", "--json"],
            capture_output=True, text=True, timeout=10
        )
        if result.returncode != 0:
            log(f"Tailscale status failed: {result.stderr}")
            return {}
        
        status = json.loads(result.stdout)
        ips = {}
        
        # Self
        if "Self" in status:
            hostname = status["Self"].get("HostName", "self")
            tailscale_ips = status["Self"].get("TailscaleIPs", [])
            if tailscale_ips:
                ips[hostname] = tailscale_ips[0]
        
        # Peers
        for peer_id, peer in status.get("Peer", {}).items():
            hostname = peer.get("HostName", "")
            tailscale_ips = peer.get("TailscaleIPs", [])
            online = peer.get("Online", False)
            if hostname and tailscale_ips:
                ips[hostname] = {
                    "ip": tailscale_ips[0],
                    "online": online,
                }
        
        return ips
    except Exception as e:
        log(f"Error discovering nodes: {e}")
        return {}


def update_node_ips():
    """Update NODES with discovered Tailscale IPs."""
    discovered = discover_tailscale_nodes()
    
    for name, info in discovered.items():
        if isinstance(info, dict):
            ip = info.get("ip")
            online = info.get("online", False)
        else:
            ip = info
            online = True
        
        if name in NODES and ip:
            NODES[name]["ip"] = ip
            NODES[name]["online"] = online


def get_active_nodes() -> List[str]:
    """Get list of nodes that are online and have IPs."""
    return [
        name for name, info in NODES.items()
        if info.get("ip") and info.get("online", True) and info["role"] != "server"
    ]


def ssh_run(node: str, cmd: str, timeout: int = 60) -> Tuple[bool, str]:
    """Run command on remote node via SSH."""
    info = NODES.get(node)
    if not info or not info.get("ip"):
        return False, f"Node {node} not found or no IP"
    
    ssh_cmd = [
        "ssh",
        "-o", "StrictHostKeyChecking=no",
        "-o", "ConnectTimeout=10",
        "-o", "BatchMode=yes",
        f"{info['user']}@{info['ip']}",
        cmd
    ]
    
    try:
        result = subprocess.run(ssh_cmd, capture_output=True, text=True, timeout=timeout)
        return result.returncode == 0, result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return False, "Timeout"
    except Exception as e:
        return False, str(e)


def scp_to(node: str, local_path: str, remote_path: str = "~/") -> bool:
    """Copy file to remote node."""
    info = NODES.get(node)
    if not info or not info.get("ip"):
        return False
    
    scp_cmd = [
        "scp",
        "-o", "StrictHostKeyChecking=no",
        "-o", "ConnectTimeout=10",
        local_path,
        f"{info['user']}@{info['ip']}:{remote_path}"
    ]
    
    result = subprocess.run(scp_cmd, capture_output=True, timeout=60)
    return result.returncode == 0


def scp_from(node: str, remote_path: str, local_path: str) -> bool:
    """Copy file from remote node."""
    info = NODES.get(node)
    if not info or not info.get("ip"):
        return False
    
    scp_cmd = [
        "scp",
        "-o", "StrictHostKeyChecking=no",
        "-o", "ConnectTimeout=10",
        f"{info['user']}@{info['ip']}:{remote_path}",
        local_path
    ]
    
    result = subprocess.run(scp_cmd, capture_output=True, timeout=60)
    return result.returncode == 0


# =============================================================================
# Test Runner
# =============================================================================

class TestRunner:
    def __init__(self, output_dir: Path):
        self.output_dir = output_dir
        self._setup_directories()
    
    def _setup_directories(self):
        """Create output directory structure."""
        self.output_dir.mkdir(parents=True, exist_ok=True)
        (self.output_dir / "config").mkdir(exist_ok=True)
        (self.output_dir / "raw" / "latency").mkdir(parents=True, exist_ok=True)
        (self.output_dir / "raw" / "scale").mkdir(parents=True, exist_ok=True)
        (self.output_dir / "raw" / "soak").mkdir(parents=True, exist_ok=True)
        (self.output_dir / "analysis").mkdir(exist_ok=True)
        
        # Save configuration
        with open(self.output_dir / "config" / "test_config.json", "w") as f:
            json.dump(TEST_CONFIG, f, indent=2)
        
        with open(self.output_dir / "config" / "nodes.json", "w") as f:
            json.dump(NODES, f, indent=2)
    
    def deploy_scripts(self):
        """Deploy test scripts to all nodes."""
        log("Deploying test scripts to all nodes...")
        
        script_dir = Path(__file__).parent
        scripts = ["load_worker.py", "global_probe.py"]
        
        nodes = get_active_nodes()
        log(f"Active nodes: {nodes}")
        
        for node in nodes:
            for script in scripts:
                script_path = script_dir / script
                if script_path.exists():
                    success = scp_to(node, str(script_path), "~/")
                    status = "✓" if success else "✗"
                    log(f"  {status} {script} -> {node}")
                else:
                    log(f"  ! Script not found: {script_path}")
    
    def run_latency_test(self):
        """Run latency probe from all nodes."""
        log("=" * 60)
        log("LATENCY TEST")
        log("=" * 60)
        
        nodes = get_active_nodes()
        samples = TEST_CONFIG["latency"]["samples"]
        
        log(f"Running {samples} RTT samples from {len(nodes)} nodes...")
        
        # Run probes in parallel
        results = {}
        with ThreadPoolExecutor(max_workers=len(nodes)) as executor:
            futures = {}
            for node in nodes:
                cmd = f"EDGE_HOST={IRIS_SERVER_IP} EDGE_PORT={IRIS_PORT} WORKER_ID={node} SAMPLES={samples} python3 ~/global_probe.py"
                futures[executor.submit(ssh_run, node, cmd, timeout=300)] = node
            
            for future in as_completed(futures):
                node = futures[future]
                success, output = future.result()
                status = "✓" if success else "✗"
                log(f"  {status} {node}")
                results[node] = {"success": success, "output": output}
        
        # Collect results
        log("\nCollecting latency data...")
        latency_dir = self.output_dir / "raw" / "latency"
        
        for node in nodes:
            local_path = latency_dir / f"{node}.json"
            if scp_from(node, f"~/latency_{node}.json", str(local_path)):
                log(f"  ✓ Collected from {node}")
            else:
                log(f"  ✗ Failed to collect from {node}")
        
        # Generate matrix
        self._generate_latency_matrix()
    
    def _generate_latency_matrix(self):
        """Generate latency matrix from collected data."""
        matrix = []
        latency_dir = self.output_dir / "raw" / "latency"
        
        for f in latency_dir.glob("*.json"):
            try:
                with open(f) as fp:
                    data = json.load(fp)
                    if "error" in data:
                        continue
                    
                    node_name = data.get("worker_id", f.stem)
                    node_info = NODES.get(node_name, {})
                    
                    matrix.append({
                        "node": node_name,
                        "location": node_info.get("location", "Unknown"),
                        "expected_ms": node_info.get("latency_expected", 0),
                        "samples": data.get("samples", 0),
                        "p50_ms": data.get("latency_ms", {}).get("p50", 0),
                        "p95_ms": data.get("latency_ms", {}).get("p95", 0),
                        "p99_ms": data.get("latency_ms", {}).get("p99", 0),
                        "max_ms": data.get("latency_ms", {}).get("max", 0),
                        "stdev_ms": data.get("latency_ms", {}).get("stdev", 0),
                    })
            except Exception as e:
                log(f"  Warning: Could not parse {f}: {e}")
        
        # Sort by P50
        matrix.sort(key=lambda x: x["p50_ms"])
        
        # Save CSV
        if matrix:
            csv_path = self.output_dir / "analysis" / "latency_matrix.csv"
            with open(csv_path, "w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=matrix[0].keys())
                writer.writeheader()
                writer.writerows(matrix)
        
        # Print summary
        log("\n" + "=" * 80)
        log("LATENCY MATRIX")
        log("=" * 80)
        log(f"{'Node':<25} {'Location':<15} {'P50':>10} {'P95':>10} {'P99':>10} {'Max':>10}")
        log("-" * 80)
        for row in matrix:
            log(f"{row['node']:<25} {row['location']:<15} {row['p50_ms']:>9.1f}ms {row['p95_ms']:>9.1f}ms {row['p99_ms']:>9.1f}ms {row['max_ms']:>9.1f}ms")
    
    def run_scale_test(self):
        """Run connection scale test."""
        log("=" * 60)
        log("SCALE TEST")
        log("=" * 60)
        
        levels = TEST_CONFIG["scale"]["levels"]
        hold_time = TEST_CONFIG["scale"]["hold_time_s"]
        
        for target in levels:
            log(f"\nTesting {target:,} connections...")
            
            # Distribute load
            distribution = self._distribute_load(target)
            log(f"  Distribution: {distribution}")
            
            # Start generators
            self._start_generators(distribution, duration=hold_time + 120)
            
            # Wait
            log(f"  Holding for {hold_time}s...")
            time.sleep(hold_time)
            
            # Stop
            self._stop_generators()
            
            # Cool down
            log(f"  Cooling down...")
            time.sleep(30)
    
    def _distribute_load(self, total: int) -> Dict[str, int]:
        """Distribute connection load across generators."""
        generators = {
            name: info for name, info in NODES.items()
            if info["role"] == "generator" and info.get("ip") and info.get("online", True)
        }
        probes = {
            name: info for name, info in NODES.items()
            if info["role"] == "probe" and info.get("ip") and info.get("online", True)
        }
        
        distribution = {}
        remaining = total
        
        # Generators first (higher capacity)
        gen_total = sum(info["capacity"] for info in generators.values())
        for name, info in generators.items():
            if gen_total > 0:
                alloc = int(min(remaining, total * 0.8) * info["capacity"] / gen_total)
                distribution[name] = alloc
                remaining -= alloc
        
        # Probes get rest
        probe_total = sum(info["capacity"] for info in probes.values())
        for name, info in probes.items():
            if probe_total > 0 and remaining > 0:
                alloc = min(info["capacity"], remaining // len(probes))
                distribution[name] = alloc
                remaining -= alloc
        
        return distribution
    
    def _start_generators(self, distribution: Dict[str, int], duration: int, msg_rate: int = 100):
        """Start load generators."""
        for node, connections in distribution.items():
            if connections <= 0:
                continue
            
            node_rate = max(10, msg_rate // len(distribution))
            cmd = f"nohup python3 ~/load_worker.py --host {IRIS_SERVER_IP} --port {IRIS_PORT} --connections {connections} --rate {node_rate} --duration {duration} > ~/worker.log 2>&1 &"
            
            success, _ = ssh_run(node, cmd, timeout=10)
            status = "✓" if success else "✗"
            log(f"    {status} {node}: {connections:,} connections")
    
    def _stop_generators(self):
        """Stop all generators."""
        nodes = get_active_nodes()
        for node in nodes:
            ssh_run(node, "pkill -f load_worker.py 2>/dev/null || true", timeout=10)
    
    def generate_report(self):
        """Generate validation report."""
        log("=" * 60)
        log("GENERATING REPORT")
        log("=" * 60)
        
        report = f"""# Iris Global Scale Validation Report

**Generated:** {datetime.now().isoformat()}
**Server:** j (100.95.21.52) - Bangalore

## Infrastructure

### Active Nodes

| Node | IP | Location | Role | Status |
|------|----|---------|----|--------|
"""
        
        for name, info in NODES.items():
            if info.get("ip"):
                status = "✅ Online" if info.get("online", True) else "⏸ Offline"
                report += f"| {name} | {info['ip']} | {info.get('location', 'Unknown')} | {info['role']} | {status} |\n"
        
        # Latency results
        latency_csv = self.output_dir / "analysis" / "latency_matrix.csv"
        if latency_csv.exists():
            report += "\n## Latency Results\n\n"
            report += "| Node | Location | P50 | P95 | P99 | Max |\n"
            report += "|------|----------|-----|-----|-----|-----|\n"
            
            with open(latency_csv) as f:
                reader = csv.DictReader(f)
                for row in reader:
                    report += f"| {row['node']} | {row['location']} | {float(row['p50_ms']):.1f}ms | {float(row['p95_ms']):.1f}ms | {float(row['p99_ms']):.1f}ms | {float(row['max_ms']):.1f}ms |\n"
        
        report += f"""
## Methodology

- All nodes connected via Tailscale mesh VPN
- Latency measured using custom RTT probe
- Data collected centrally on j (Bangalore)

---
*Generated by Iris Global Test Coordinator*
"""
        
        report_path = self.output_dir / "VALIDATION_REPORT.md"
        with open(report_path, "w") as f:
            f.write(report)
        
        log(f"Report saved to: {report_path}")


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Iris Global Test Coordinator",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python3 coordinator.py --discover
    python3 coordinator.py --deploy
    python3 coordinator.py --phase latency
    python3 coordinator.py --run-full-test
        """
    )
    parser.add_argument("--discover", action="store_true", help="Discover Tailscale nodes")
    parser.add_argument("--deploy", action="store_true", help="Deploy scripts to all nodes")
    parser.add_argument("--phase", choices=["latency", "scale", "soak"], help="Run specific phase")
    parser.add_argument("--run-full-test", action="store_true", help="Run complete test")
    parser.add_argument("--generate-report", action="store_true", help="Generate report")
    parser.add_argument("--output-dir", default=None, help="Output directory")
    args = parser.parse_args()
    
    # Always discover nodes first
    log("Discovering Tailscale nodes...")
    update_node_ips()
    
    if args.discover:
        log("\nDiscovered nodes:")
        for name, info in NODES.items():
            if info.get("ip"):
                status = "✅" if info.get("online", True) else "⏸"
                log(f"  {status} {name}: {info['ip']} ({info.get('location', 'Unknown')})")
            else:
                log(f"  ❌ {name}: Not on Tailscale")
        return
    
    # Setup output directory
    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        output_dir = Path.home() / f"iris-validation-{timestamp}"
    
    runner = TestRunner(output_dir)
    
    if args.deploy:
        runner.deploy_scripts()
        return
    
    if args.generate_report:
        runner.generate_report()
        return
    
    if args.phase:
        if args.phase == "latency":
            runner.run_latency_test()
        elif args.phase == "scale":
            runner.run_scale_test()
        return
    
    if args.run_full_test:
        log("=" * 60)
        log("IRIS GLOBAL SCALE VALIDATION TEST")
        log("=" * 60)
        log(f"Output: {output_dir}")
        log("")
        
        runner.deploy_scripts()
        runner.run_latency_test()
        runner.run_scale_test()
        runner.generate_report()
        
        log("")
        log("=" * 60)
        log("TEST COMPLETE")
        log("=" * 60)
        log(f"Results: {output_dir}")
        return
    
    parser.print_help()


if __name__ == "__main__":
    main()
