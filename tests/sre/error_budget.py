#!/usr/bin/env python3
"""
Per-Region Error Budget Monitor

Implements SLO tracking per Plan Section 5 and Error Budget Breakdown.

Error Budget Allocation (99.99% availability = 4.32 min/month downtime):
- US-East:       52 seconds/month (1.7 sec/day)
- US-West:       52 seconds/month (1.7 sec/day)
- EU-Frankfurt:  52 seconds/month (1.7 sec/day)
- Sydney:        52 seconds/month (1.7 sec/day)
- Sao Paulo:     52 seconds/month (1.7 sec/day)
- Total:         4.32 minutes/month (8.6 sec/day)

Metrics Tracked:
- Uptime percentage
- P99 latency compliance (<=100ms in-region, <=500ms cross-region)
- Message delivery rate (>=99.999%)
- Error rate

Usage:
    # Check current error budget status
    python3 tests/sre/error_budget.py status
    
    # Record an incident
    python3 tests/sre/error_budget.py record --region us-east --duration 30 --reason "Core node crash"
    
    # Check if budget allows deployment
    python3 tests/sre/error_budget.py can-deploy --region us-west
    
    # Generate monthly report
    python3 tests/sre/error_budget.py report --month 2026-01
"""

import argparse
import json
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional
from dataclasses import dataclass, asdict

# Configuration
DATA_DIR = Path(os.environ.get("IRIS_SRE_DIR", 
                                Path(__file__).parent.parent.parent / "tests" / "sre" / "data"))
ERROR_BUDGET_FILE = DATA_DIR / "error_budget.json"

# SLO Targets (from RFC and Plan)
REGIONS = ["us-east", "us-west", "eu-frankfurt", "sydney", "sao-paulo"]

MONTHLY_BUDGET_SECONDS = 52  # Per region
DAILY_BUDGET_SECONDS = 1.7   # Per region

SLO_TARGETS = {
    "availability": 99.99,           # %
    "p99_latency_in_region_ms": 100,
    "p99_latency_cross_region_ms": 500,
    "message_delivery_rate": 99.999,  # %
    "error_rate_max": 0.01,           # %
}

# Deployment freeze threshold (% of budget remaining)
DEPLOYMENT_FREEZE_THRESHOLD = 10  # Freeze if <10% budget remaining


@dataclass
class Incident:
    """An incident that consumed error budget."""
    timestamp: str
    region: str
    duration_seconds: float
    reason: str
    severity: str  # minor, major, critical
    resolved: bool
    
    def to_dict(self) -> dict:
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict) -> 'Incident':
        return cls(**data)


@dataclass
class RegionBudget:
    """Error budget status for a region."""
    region: str
    monthly_budget_seconds: float
    consumed_seconds: float
    remaining_seconds: float
    remaining_percentage: float
    incidents_this_month: int
    status: str  # healthy, warning, critical, exhausted
    
    def to_dict(self) -> dict:
        return asdict(self)


class ErrorBudgetStore:
    """JSON-based error budget storage."""
    
    def __init__(self, path: Path = ERROR_BUDGET_FILE):
        self.path = path
        self.data: Dict = {
            "incidents": [],
            "monthly_summaries": {},
        }
        self._load()
    
    def _load(self):
        """Load data from file."""
        if self.path.exists():
            try:
                with open(self.path, 'r') as f:
                    self.data = json.load(f)
            except json.JSONDecodeError:
                print(f"Warning: Could not parse {self.path}, starting fresh")
    
    def _save(self):
        """Save data to file."""
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with open(self.path, 'w') as f:
            json.dump(self.data, f, indent=2)
    
    def record_incident(self, incident: Incident):
        """Record a new incident."""
        self.data["incidents"].append(incident.to_dict())
        self._save()
    
    def get_incidents(self, region: str = None, 
                      start_date: str = None, 
                      end_date: str = None) -> List[Incident]:
        """Get incidents with optional filters."""
        incidents = []
        
        for i in self.data["incidents"]:
            if region and i["region"] != region:
                continue
            if start_date and i["timestamp"] < start_date:
                continue
            if end_date and i["timestamp"] > end_date:
                continue
            incidents.append(Incident.from_dict(i))
        
        return incidents
    
    def get_budget_status(self, region: str, month: str = None) -> RegionBudget:
        """Get current budget status for a region."""
        if month is None:
            month = datetime.now().strftime("%Y-%m")
        
        start = f"{month}-01T00:00:00"
        end = f"{month}-31T23:59:59"
        
        incidents = self.get_incidents(region=region, start_date=start, end_date=end)
        consumed = sum(i.duration_seconds for i in incidents)
        remaining = max(0, MONTHLY_BUDGET_SECONDS - consumed)
        remaining_pct = (remaining / MONTHLY_BUDGET_SECONDS) * 100
        
        if remaining_pct <= 0:
            status = "exhausted"
        elif remaining_pct <= 10:
            status = "critical"
        elif remaining_pct <= 30:
            status = "warning"
        else:
            status = "healthy"
        
        return RegionBudget(
            region=region,
            monthly_budget_seconds=MONTHLY_BUDGET_SECONDS,
            consumed_seconds=consumed,
            remaining_seconds=remaining,
            remaining_percentage=remaining_pct,
            incidents_this_month=len(incidents),
            status=status,
        )


def cmd_status(args):
    """Show current error budget status for all regions."""
    store = ErrorBudgetStore()
    month = args.month or datetime.now().strftime("%Y-%m")
    
    print(f"\n{'='*70}")
    print(f"Error Budget Status - {month}")
    print(f"{'='*70}\n")
    
    print(f"{'Region':<15} {'Consumed':<12} {'Remaining':<12} {'%':<8} {'Status':<10}")
    print("-" * 60)
    
    total_consumed = 0
    total_budget = 0
    
    for region in REGIONS:
        budget = store.get_budget_status(region, month)
        total_consumed += budget.consumed_seconds
        total_budget += budget.monthly_budget_seconds
        
        status_icon = {
            "healthy": "✓",
            "warning": "⚠",
            "critical": "!",
            "exhausted": "✗"
        }.get(budget.status, "?")
        
        print(f"{budget.region:<15} {budget.consumed_seconds:>8.1f}s   "
              f"{budget.remaining_seconds:>8.1f}s   "
              f"{budget.remaining_percentage:>5.1f}%   "
              f"{status_icon} {budget.status}")
    
    print("-" * 60)
    total_remaining = total_budget - total_consumed
    total_pct = (total_remaining / total_budget) * 100 if total_budget > 0 else 0
    print(f"{'TOTAL':<15} {total_consumed:>8.1f}s   {total_remaining:>8.1f}s   {total_pct:>5.1f}%")
    
    print(f"\n{'='*70}")
    print(f"SLO Target: 99.99% availability = {total_budget:.0f}s downtime/month")
    print(f"{'='*70}\n")


def cmd_record(args):
    """Record a new incident."""
    store = ErrorBudgetStore()
    
    incident = Incident(
        timestamp=args.timestamp or datetime.now().isoformat(),
        region=args.region,
        duration_seconds=args.duration,
        reason=args.reason,
        severity=args.severity,
        resolved=not args.ongoing,
    )
    
    store.record_incident(incident)
    
    print(f"Recorded incident for {args.region}:")
    print(f"  Duration: {args.duration}s")
    print(f"  Reason: {args.reason}")
    print(f"  Severity: {args.severity}")
    
    # Show updated budget
    budget = store.get_budget_status(args.region)
    print(f"\nUpdated budget for {args.region}:")
    print(f"  Consumed: {budget.consumed_seconds:.1f}s")
    print(f"  Remaining: {budget.remaining_seconds:.1f}s ({budget.remaining_percentage:.1f}%)")
    print(f"  Status: {budget.status}")


def cmd_can_deploy(args):
    """Check if deployment is allowed based on error budget."""
    store = ErrorBudgetStore()
    
    region = args.region
    budget = store.get_budget_status(region)
    
    can_deploy = budget.remaining_percentage > DEPLOYMENT_FREEZE_THRESHOLD
    
    print(f"\nDeployment Check for {region}")
    print(f"  Budget remaining: {budget.remaining_percentage:.1f}%")
    print(f"  Freeze threshold: {DEPLOYMENT_FREEZE_THRESHOLD}%")
    
    if can_deploy:
        print(f"\n✓ DEPLOYMENT ALLOWED")
        print(f"  Budget is healthy ({budget.status})")
        return 0
    else:
        print(f"\n✗ DEPLOYMENT BLOCKED")
        print(f"  Error budget is {budget.status}")
        print(f"  Wait for budget reset or get approval for override")
        return 1


def cmd_report(args):
    """Generate monthly report."""
    store = ErrorBudgetStore()
    month = args.month or datetime.now().strftime("%Y-%m")
    
    print(f"\n{'='*70}")
    print(f"Monthly Error Budget Report - {month}")
    print(f"{'='*70}\n")
    
    # Summary by region
    print("REGIONAL SUMMARY")
    print("-" * 50)
    
    all_healthy = True
    
    for region in REGIONS:
        budget = store.get_budget_status(region, month)
        incidents = store.get_incidents(region=region, 
                                        start_date=f"{month}-01",
                                        end_date=f"{month}-31")
        
        if budget.status != "healthy":
            all_healthy = False
        
        print(f"\n{region.upper()}")
        print(f"  Status: {budget.status}")
        print(f"  Consumed: {budget.consumed_seconds:.1f}s / {budget.monthly_budget_seconds:.1f}s")
        print(f"  Incidents: {len(incidents)}")
        
        if incidents:
            print(f"  Recent incidents:")
            for i in sorted(incidents, key=lambda x: x.timestamp, reverse=True)[:3]:
                print(f"    - {i.timestamp[:10]}: {i.reason} ({i.duration_seconds}s, {i.severity})")
    
    # Overall assessment
    print(f"\n{'='*70}")
    print("OVERALL ASSESSMENT")
    print("-" * 50)
    
    if all_healthy:
        print("✓ All regions within error budget")
        print("  Recommendation: Continue normal operations")
    else:
        print("⚠ One or more regions exceeded warning threshold")
        print("  Recommendation: Review incident patterns and remediation")
    
    print(f"\n{'='*70}")


def cmd_simulate(args):
    """Simulate error budget consumption for testing."""
    store = ErrorBudgetStore()
    
    print("Simulating incidents for error budget testing...")
    
    # Simulate various scenarios
    scenarios = [
        ("us-east", 10, "Minor network blip", "minor"),
        ("us-west", 25, "Core node failover", "major"),
        ("eu-frankfurt", 5, "DNS resolution delay", "minor"),
        ("sydney", 45, "Cross-region routing issue", "major"),
    ]
    
    for region, duration, reason, severity in scenarios:
        incident = Incident(
            timestamp=datetime.now().isoformat(),
            region=region,
            duration_seconds=duration,
            reason=reason,
            severity=severity,
            resolved=True,
        )
        store.record_incident(incident)
        print(f"  Recorded: {region} - {reason} ({duration}s)")
    
    print("\nSimulation complete. Run 'status' to see updated budgets.")


def main():
    parser = argparse.ArgumentParser(description="Error Budget Monitor")
    subparsers = parser.add_subparsers(dest="command", help="Commands")
    
    # Status command
    status_parser = subparsers.add_parser("status", help="Show current status")
    status_parser.add_argument("--month", help="Month (YYYY-MM)")
    
    # Record command
    record_parser = subparsers.add_parser("record", help="Record incident")
    record_parser.add_argument("--region", required=True, choices=REGIONS)
    record_parser.add_argument("--duration", type=float, required=True, help="Duration in seconds")
    record_parser.add_argument("--reason", required=True, help="Incident reason")
    record_parser.add_argument("--severity", default="minor", choices=["minor", "major", "critical"])
    record_parser.add_argument("--timestamp", help="ISO timestamp (default: now)")
    record_parser.add_argument("--ongoing", action="store_true", help="Incident not yet resolved")
    
    # Can-deploy command
    deploy_parser = subparsers.add_parser("can-deploy", help="Check if deployment allowed")
    deploy_parser.add_argument("--region", required=True, choices=REGIONS)
    
    # Report command
    report_parser = subparsers.add_parser("report", help="Generate monthly report")
    report_parser.add_argument("--month", help="Month (YYYY-MM)")
    
    # Simulate command (for testing)
    simulate_parser = subparsers.add_parser("simulate", help="Simulate incidents for testing")
    
    args = parser.parse_args()
    
    if args.command == "status":
        return cmd_status(args)
    elif args.command == "record":
        return cmd_record(args)
    elif args.command == "can-deploy":
        return cmd_can_deploy(args)
    elif args.command == "report":
        return cmd_report(args)
    elif args.command == "simulate":
        return cmd_simulate(args)
    else:
        parser.print_help()
        return 1


if __name__ == "__main__":
    sys.exit(main() or 0)
