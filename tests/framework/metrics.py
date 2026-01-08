"""
Metrics Post-Processing

Derives metrics from structured JSON logs after test completion.
Produces summary statistics suitable for CI reporting.
"""

import json
import statistics
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, field, asdict
from datetime import datetime


@dataclass
class DerivedMetrics:
    """Derived metrics from log analysis."""
    test_id: str
    timestamp: str
    duration_seconds: float = 0.0
    
    # Message metrics
    messages_sent: int = 0
    messages_received: int = 0
    messages_lost: int = 0
    messages_duplicated: int = 0
    
    # Latency metrics (milliseconds)
    latency_p50_ms: float = 0.0
    latency_p90_ms: float = 0.0
    latency_p99_ms: float = 0.0
    latency_min_ms: float = 0.0
    latency_max_ms: float = 0.0
    latency_avg_ms: float = 0.0
    
    # Throughput
    throughput_msgs_per_sec: float = 0.0
    
    # Error metrics
    error_count: int = 0
    error_rate_pct: float = 0.0
    
    # Connection metrics
    connections_opened: int = 0
    connections_closed: int = 0
    
    # Custom metrics
    extra: Dict[str, Any] = field(default_factory=dict)


def parse_log_file(log_path: Path) -> List[Dict[str, Any]]:
    """Parse a JSON Lines log file."""
    events = []
    with open(log_path, 'r') as f:
        for line in f:
            line = line.strip()
            if line:
                try:
                    events.append(json.loads(line))
                except json.JSONDecodeError:
                    pass
    return events


def percentile(values: List[float], p: float) -> float:
    """Calculate percentile of a list of values."""
    if not values:
        return 0.0
    sorted_values = sorted(values)
    idx = int(len(sorted_values) * p / 100)
    idx = min(idx, len(sorted_values) - 1)
    return sorted_values[idx]


def derive_metrics(log_path: Path) -> DerivedMetrics:
    """Derive metrics from a log file."""
    events = parse_log_file(log_path)
    
    if not events:
        return DerivedMetrics(
            test_id="unknown",
            timestamp=datetime.utcnow().isoformat() + "Z"
        )
    
    # Extract test_id from first event
    test_id = events[0].get("test_id", "unknown")
    
    # Initialize counters
    messages_sent = 0
    messages_received = 0
    message_ids_sent = set()
    message_ids_received = set()
    latencies = []
    errors = 0
    connections_opened = 0
    connections_closed = 0
    
    start_time = None
    end_time = None
    
    for event in events:
        event_type = event.get("event_type", "")
        
        # Track time range
        if "wall_time" in event:
            try:
                ts = datetime.fromisoformat(event["wall_time"].rstrip("Z"))
                if start_time is None:
                    start_time = ts
                end_time = ts
            except Exception:
                pass
        
        # Message sent
        if event_type == "message_sent":
            messages_sent += 1
            msg_id = event.get("message_id")
            if msg_id:
                message_ids_sent.add(msg_id)
        
        # Message received
        elif event_type == "message_received":
            messages_received += 1
            msg_id = event.get("message_id")
            if msg_id:
                message_ids_received.add(msg_id)
            
            # Track latency
            latency = event.get("latency_ms")
            if latency is not None:
                latencies.append(latency)
        
        # Message acknowledged
        elif event_type == "message_acked":
            latency = event.get("latency_ms")
            if latency is not None:
                latencies.append(latency)
        
        # Errors
        elif event.get("error_type"):
            errors += 1
        
        # Connections
        elif event_type == "connection_login":
            connections_opened += 1
        elif event_type == "connection_disconnect":
            connections_closed += 1
    
    # Calculate derived values
    lost = len(message_ids_sent - message_ids_received)
    
    # Find duplicates (messages received multiple times)
    receive_counts: Dict[str, int] = {}
    for event in events:
        if event.get("event_type") == "message_received":
            msg_id = event.get("message_id")
            if msg_id:
                receive_counts[msg_id] = receive_counts.get(msg_id, 0) + 1
    duplicates = sum(1 for count in receive_counts.values() if count > 1)
    
    # Duration
    duration = 0.0
    if start_time and end_time:
        duration = (end_time - start_time).total_seconds()
    
    # Throughput
    throughput = messages_sent / duration if duration > 0 else 0.0
    
    # Error rate
    total_ops = messages_sent + messages_received
    error_rate = (errors / total_ops * 100) if total_ops > 0 else 0.0
    
    return DerivedMetrics(
        test_id=test_id,
        timestamp=datetime.utcnow().isoformat() + "Z",
        duration_seconds=duration,
        messages_sent=messages_sent,
        messages_received=messages_received,
        messages_lost=lost,
        messages_duplicated=duplicates,
        latency_p50_ms=percentile(latencies, 50),
        latency_p90_ms=percentile(latencies, 90),
        latency_p99_ms=percentile(latencies, 99),
        latency_min_ms=min(latencies) if latencies else 0.0,
        latency_max_ms=max(latencies) if latencies else 0.0,
        latency_avg_ms=statistics.mean(latencies) if latencies else 0.0,
        throughput_msgs_per_sec=throughput,
        error_count=errors,
        error_rate_pct=error_rate,
        connections_opened=connections_opened,
        connections_closed=connections_closed
    )


def process_run_directory(run_dir: Path) -> Dict[str, DerivedMetrics]:
    """Process all log files in a test run directory."""
    results = {}
    
    for log_file in run_dir.rglob("*.jsonl"):
        try:
            metrics = derive_metrics(log_file)
            results[str(log_file.relative_to(run_dir))] = metrics
        except Exception as e:
            print(f"Error processing {log_file}: {e}")
    
    return results


def save_metrics(metrics: DerivedMetrics, output_path: Path):
    """Save derived metrics to JSON file."""
    with open(output_path, 'w') as f:
        json.dump(asdict(metrics), f, indent=2)


def aggregate_metrics(metrics_list: List[DerivedMetrics]) -> Dict[str, Any]:
    """Aggregate metrics across multiple tests."""
    if not metrics_list:
        return {}
    
    total_sent = sum(m.messages_sent for m in metrics_list)
    total_received = sum(m.messages_received for m in metrics_list)
    total_lost = sum(m.messages_lost for m in metrics_list)
    total_errors = sum(m.error_count for m in metrics_list)
    total_duration = sum(m.duration_seconds for m in metrics_list)
    
    all_latencies_p99 = [m.latency_p99_ms for m in metrics_list if m.latency_p99_ms > 0]
    
    return {
        "tests_count": len(metrics_list),
        "total_messages_sent": total_sent,
        "total_messages_received": total_received,
        "total_messages_lost": total_lost,
        "loss_rate_pct": (total_lost / total_sent * 100) if total_sent > 0 else 0,
        "total_errors": total_errors,
        "total_duration_seconds": total_duration,
        "latency_p99_max_ms": max(all_latencies_p99) if all_latencies_p99 else 0,
        "latency_p99_avg_ms": statistics.mean(all_latencies_p99) if all_latencies_p99 else 0,
        "throughput_total_msgs_per_sec": total_sent / total_duration if total_duration > 0 else 0
    }


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python metrics.py <log_file_or_directory>")
        sys.exit(1)
    
    path = Path(sys.argv[1])
    
    if path.is_file():
        metrics = derive_metrics(path)
        print(json.dumps(asdict(metrics), indent=2))
    elif path.is_dir():
        results = process_run_directory(path)
        for log_path, metrics in results.items():
            print(f"\n=== {log_path} ===")
            print(json.dumps(asdict(metrics), indent=2))
    else:
        print(f"Path not found: {path}")
        sys.exit(1)
