#!/usr/bin/env python3
"""
Performance Baseline Infrastructure

This module provides automated performance baseline tracking for regression detection.
Per plan Section 1.9: Start with file-based JSON and migrate later.

Features:
- Store performance metrics per commit
- Statistical significance testing (avoid false alarms)
- Automated alerting on regression
- Historical trend analysis

Usage:
    # Record baseline for current commit
    python3 tests/perf/baseline.py record --commit abc123 --metrics results.json
    
    # Compare current run against baseline
    python3 tests/perf/baseline.py compare --commit abc123 --metrics results.json
    
    # Show historical trends
    python3 tests/perf/baseline.py trend --metric p99_latency --days 30

Metrics Format (JSON):
{
    "p50_latency_ms": 12.5,
    "p99_latency_ms": 45.2,
    "throughput_msg_sec": 15000,
    "connection_setup_ms": 85,
    "memory_per_conn_kb": 12.3,
    "test_duration_sec": 60
}
"""

import argparse
import json
import os
import sys
import statistics
import subprocess
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, asdict

# Configuration
BASELINE_DIR = Path(os.environ.get("IRIS_BASELINE_DIR", 
                                    Path(__file__).parent.parent.parent / "tests" / "perf" / "baselines"))
BASELINE_FILE = BASELINE_DIR / "baselines.json"

# Regression thresholds (percentage)
THRESHOLDS = {
    "p50_latency_ms": 20,       # 20% regression triggers alert
    "p99_latency_ms": 25,       # 25% regression triggers alert
    "throughput_msg_sec": -15,  # 15% drop triggers alert (negative = lower is worse)
    "connection_setup_ms": 30,  # 30% regression triggers alert
    "memory_per_conn_kb": 25,   # 25% regression triggers alert
}

# Minimum samples for statistical significance
MIN_SAMPLES = 3


@dataclass
class PerfResult:
    """Performance test result."""
    commit: str
    timestamp: str
    branch: str
    metrics: Dict[str, float]
    environment: Dict[str, str]
    
    def to_dict(self) -> dict:
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict) -> 'PerfResult':
        return cls(**data)


class BaselineStore:
    """JSON-based baseline storage."""
    
    def __init__(self, path: Path = BASELINE_FILE):
        self.path = path
        self.data: Dict[str, List[dict]] = {"results": [], "baselines": {}}
        self._load()
    
    def _load(self):
        """Load baselines from file."""
        if self.path.exists():
            try:
                with open(self.path, 'r') as f:
                    self.data = json.load(f)
            except json.JSONDecodeError:
                print(f"Warning: Could not parse {self.path}, starting fresh")
                self.data = {"results": [], "baselines": {}}
    
    def _save(self):
        """Save baselines to file."""
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with open(self.path, 'w') as f:
            json.dump(self.data, f, indent=2)
    
    def add_result(self, result: PerfResult):
        """Add a new performance result."""
        self.data["results"].append(result.to_dict())
        self._update_baseline(result)
        self._save()
    
    def _update_baseline(self, result: PerfResult):
        """Update rolling baseline with new result."""
        branch = result.branch
        if branch not in self.data["baselines"]:
            self.data["baselines"][branch] = {}
        
        baseline = self.data["baselines"][branch]
        
        for metric, value in result.metrics.items():
            if metric not in baseline:
                baseline[metric] = {"values": [], "mean": 0, "stddev": 0}
            
            # Keep last 10 values for rolling average
            baseline[metric]["values"].append(value)
            if len(baseline[metric]["values"]) > 10:
                baseline[metric]["values"] = baseline[metric]["values"][-10:]
            
            # Update statistics
            values = baseline[metric]["values"]
            baseline[metric]["mean"] = statistics.mean(values)
            if len(values) >= 2:
                baseline[metric]["stddev"] = statistics.stdev(values)
            else:
                baseline[metric]["stddev"] = 0
    
    def get_baseline(self, branch: str = "main") -> Dict[str, Dict]:
        """Get current baseline for branch."""
        return self.data["baselines"].get(branch, {})
    
    def get_results(self, branch: str = None, days: int = 30) -> List[PerfResult]:
        """Get historical results."""
        cutoff = datetime.now() - timedelta(days=days)
        cutoff_str = cutoff.isoformat()
        
        results = []
        for r in self.data["results"]:
            if branch and r.get("branch") != branch:
                continue
            if r.get("timestamp", "") >= cutoff_str:
                results.append(PerfResult.from_dict(r))
        
        return results


def get_git_info() -> Tuple[str, str]:
    """Get current git commit and branch."""
    try:
        commit = subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True, check=True
        ).stdout.strip()
        
        branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, check=True
        ).stdout.strip()
        
        return commit, branch
    except subprocess.CalledProcessError:
        return "unknown", "unknown"


def get_environment_info() -> Dict[str, str]:
    """Get environment information for reproducibility."""
    import platform
    
    return {
        "python_version": platform.python_version(),
        "os": platform.system(),
        "os_version": platform.release(),
        "hostname": platform.node(),
        "timestamp": datetime.now().isoformat(),
    }


def check_regression(current: Dict[str, float], baseline: Dict[str, Dict]) -> List[Tuple[str, float, float, str]]:
    """
    Check for performance regressions.
    
    Returns list of (metric, current_value, baseline_mean, status) tuples.
    """
    issues = []
    
    for metric, value in current.items():
        if metric not in baseline:
            continue
        
        base = baseline[metric]
        mean = base.get("mean", value)
        stddev = base.get("stddev", 0)
        
        if mean == 0:
            continue
        
        # Calculate percentage change
        pct_change = ((value - mean) / mean) * 100
        
        # Get threshold for this metric
        threshold = THRESHOLDS.get(metric, 20)
        
        # Check if regression (accounting for direction)
        if threshold < 0:
            # Lower is worse (e.g., throughput)
            is_regression = pct_change < threshold
        else:
            # Higher is worse (e.g., latency)
            is_regression = pct_change > threshold
        
        # Also check if outside 2 standard deviations
        if stddev > 0 and len(base.get("values", [])) >= MIN_SAMPLES:
            z_score = abs(value - mean) / stddev
            is_significant = z_score > 2
        else:
            is_significant = True  # Not enough data, be conservative
        
        if is_regression and is_significant:
            status = "REGRESSION"
        elif is_regression:
            status = "WARNING"
        else:
            status = "OK"
        
        issues.append((metric, value, mean, pct_change, status))
    
    return issues


def print_comparison(issues: List[Tuple], verbose: bool = True):
    """Print comparison results."""
    print("\n" + "=" * 70)
    print("Performance Baseline Comparison")
    print("=" * 70 + "\n")
    
    has_regression = False
    has_warning = False
    
    for metric, current, baseline, pct_change, status in issues:
        if status == "REGRESSION":
            icon = "✗"
            has_regression = True
        elif status == "WARNING":
            icon = "⚠"
            has_warning = True
        else:
            icon = "✓"
        
        direction = "+" if pct_change > 0 else ""
        print(f"  {icon} {metric}:")
        print(f"      Current:  {current:.2f}")
        print(f"      Baseline: {baseline:.2f}")
        print(f"      Change:   {direction}{pct_change:.1f}% ({status})")
        print()
    
    print("=" * 70)
    
    if has_regression:
        print("RESULT: REGRESSION DETECTED")
        print("  One or more metrics exceeded regression threshold.")
        return False
    elif has_warning:
        print("RESULT: WARNINGS (no blocking regression)")
        return True
    else:
        print("RESULT: ALL METRICS WITHIN BASELINE")
        return True


def cmd_record(args):
    """Record a new baseline."""
    # Load metrics
    with open(args.metrics, 'r') as f:
        metrics = json.load(f)
    
    # Get git info
    commit = args.commit
    branch = args.branch
    if not commit or not branch:
        git_commit, git_branch = get_git_info()
        commit = commit or git_commit
        branch = branch or git_branch
    
    # Create result
    result = PerfResult(
        commit=commit,
        timestamp=datetime.now().isoformat(),
        branch=branch,
        metrics=metrics,
        environment=get_environment_info()
    )
    
    # Store
    store = BaselineStore()
    store.add_result(result)
    
    print(f"Recorded baseline for {commit} on {branch}")
    print(f"Metrics: {json.dumps(metrics, indent=2)}")


def cmd_compare(args):
    """Compare current metrics against baseline."""
    # Load metrics
    with open(args.metrics, 'r') as f:
        metrics = json.load(f)
    
    # Get baseline
    branch = args.branch or get_git_info()[1]
    store = BaselineStore()
    baseline = store.get_baseline(branch)
    
    if not baseline:
        print(f"No baseline found for branch '{branch}'")
        print("Recording this as the first baseline...")
        args.commit = args.commit or get_git_info()[0]
        args.branch = branch
        cmd_record(args)
        return 0
    
    # Check regression
    issues = check_regression(metrics, baseline)
    
    # Print results
    passed = print_comparison(issues)
    
    # Optionally record this result
    if args.record:
        args.commit = args.commit or get_git_info()[0]
        args.branch = branch
        cmd_record(args)
    
    return 0 if passed else 1


def cmd_trend(args):
    """Show historical trends."""
    store = BaselineStore()
    results = store.get_results(args.branch, args.days)
    
    if not results:
        print(f"No results found for last {args.days} days")
        return
    
    metric = args.metric
    
    print(f"\n{'='*70}")
    print(f"Trend: {metric} (last {args.days} days)")
    print(f"{'='*70}\n")
    
    values = []
    for r in sorted(results, key=lambda x: x.timestamp):
        if metric in r.metrics:
            values.append((r.timestamp[:10], r.commit, r.metrics[metric]))
    
    if not values:
        print(f"No data for metric '{metric}'")
        return
    
    # Simple ASCII chart
    min_val = min(v[2] for v in values)
    max_val = max(v[2] for v in values)
    range_val = max_val - min_val if max_val != min_val else 1
    
    chart_width = 40
    
    for date, commit, value in values:
        bar_len = int((value - min_val) / range_val * chart_width)
        bar = "█" * bar_len + "░" * (chart_width - bar_len)
        print(f"  {date} {commit[:7]} │{bar}│ {value:.2f}")
    
    print()
    print(f"  Min: {min_val:.2f}  Max: {max_val:.2f}  Latest: {values[-1][2]:.2f}")


def cmd_export(args):
    """Export baseline data to CSV."""
    store = BaselineStore()
    results = store.get_results(args.branch, args.days)
    
    if not results:
        print("No results to export")
        return
    
    # Get all metric names
    all_metrics = set()
    for r in results:
        all_metrics.update(r.metrics.keys())
    
    # Write CSV
    output = args.output or f"baseline_export_{datetime.now().strftime('%Y%m%d')}.csv"
    
    with open(output, 'w') as f:
        # Header
        headers = ["timestamp", "commit", "branch"] + sorted(all_metrics)
        f.write(",".join(headers) + "\n")
        
        # Data
        for r in sorted(results, key=lambda x: x.timestamp):
            row = [r.timestamp, r.commit, r.branch]
            for m in sorted(all_metrics):
                row.append(str(r.metrics.get(m, "")))
            f.write(",".join(row) + "\n")
    
    print(f"Exported {len(results)} results to {output}")


def main():
    parser = argparse.ArgumentParser(description="Performance Baseline Management")
    subparsers = parser.add_subparsers(dest="command", help="Commands")
    
    # Record command
    record_parser = subparsers.add_parser("record", help="Record new baseline")
    record_parser.add_argument("--metrics", required=True, help="Path to metrics JSON file")
    record_parser.add_argument("--commit", help="Git commit (auto-detected if not specified)")
    record_parser.add_argument("--branch", help="Git branch (auto-detected if not specified)")
    
    # Compare command
    compare_parser = subparsers.add_parser("compare", help="Compare against baseline")
    compare_parser.add_argument("--metrics", required=True, help="Path to metrics JSON file")
    compare_parser.add_argument("--commit", help="Git commit")
    compare_parser.add_argument("--branch", help="Git branch to compare against")
    compare_parser.add_argument("--record", action="store_true", help="Also record this result")
    
    # Trend command
    trend_parser = subparsers.add_parser("trend", help="Show historical trend")
    trend_parser.add_argument("--metric", required=True, help="Metric to analyze")
    trend_parser.add_argument("--branch", help="Branch to analyze")
    trend_parser.add_argument("--days", type=int, default=30, help="Number of days")
    
    # Export command
    export_parser = subparsers.add_parser("export", help="Export to CSV")
    export_parser.add_argument("--branch", help="Branch to export")
    export_parser.add_argument("--days", type=int, default=90, help="Number of days")
    export_parser.add_argument("--output", help="Output file path")
    
    args = parser.parse_args()
    
    if args.command == "record":
        return cmd_record(args)
    elif args.command == "compare":
        return cmd_compare(args)
    elif args.command == "trend":
        return cmd_trend(args)
    elif args.command == "export":
        return cmd_export(args)
    else:
        parser.print_help()
        return 1


if __name__ == "__main__":
    sys.exit(main() or 0)
