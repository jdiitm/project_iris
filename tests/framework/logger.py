"""
Structured JSON Logging for Tests

Provides deterministic, parseable logging for post-processing metrics derivation.
No external dependencies - pure Python.
"""

import json
import os
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any
from dataclasses import dataclass, field, asdict
import threading


@dataclass
class LogEvent:
    """Structured log event."""
    wall_time: str
    monotonic_ns: int
    node_id: str
    test_id: str
    suite: str
    event_type: str
    message: str = ""
    message_id: Optional[str] = None
    target_user: Optional[str] = None
    source_user: Optional[str] = None
    latency_ms: Optional[float] = None
    error_type: Optional[str] = None
    extra: Dict[str, Any] = field(default_factory=dict)


class TestLogger:
    """
    Structured JSON logger for test observability.
    
    Writes JSON Lines format to file with optional console output.
    Thread-safe.
    """
    
    def __init__(
        self,
        test_id: str,
        suite: str,
        node_id: str = "local",
        output_dir: Optional[Path] = None,
        console_output: bool = True
    ):
        self.test_id = test_id
        self.suite = suite
        self.node_id = node_id
        self.console_output = console_output
        self._lock = threading.Lock()
        self._start_time = time.monotonic_ns()
        
        # Determine output directory
        if output_dir:
            self.output_dir = Path(output_dir)
        else:
            artifacts_env = os.environ.get("IRIS_TEST_ARTIFACTS")
            if artifacts_env:
                self.output_dir = Path(artifacts_env)
            else:
                self.output_dir = Path(__file__).parent.parent / "artifacts" / "logs"
        
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create log file
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.log_file = self.output_dir / f"{test_id}_{timestamp}.jsonl"
        self._file_handle = open(self.log_file, "w")
        
        # Log initialization
        self.info("test_start", f"Test {test_id} started")
    
    def _create_event(
        self,
        event_type: str,
        message: str = "",
        **kwargs
    ) -> LogEvent:
        """Create a structured log event."""
        return LogEvent(
            wall_time=datetime.utcnow().isoformat() + "Z",
            monotonic_ns=time.monotonic_ns(),
            node_id=self.node_id,
            test_id=self.test_id,
            suite=self.suite,
            event_type=event_type,
            message=message,
            **kwargs
        )
    
    def _write(self, event: LogEvent):
        """Write event to log file and optionally console."""
        event_dict = asdict(event)
        # Remove None values for cleaner output
        event_dict = {k: v for k, v in event_dict.items() if v is not None}
        
        json_line = json.dumps(event_dict, separators=(',', ':'))
        
        with self._lock:
            self._file_handle.write(json_line + "\n")
            self._file_handle.flush()
            
            if self.console_output:
                # Simplified console output
                level = "ERROR" if event.error_type else "INFO"
                print(f"[{event.wall_time[11:19]}] [{level}] {event.event_type}: {event.message}")
    
    def info(self, event_type: str, message: str = "", **kwargs):
        """Log an info event."""
        event = self._create_event(event_type, message, **kwargs)
        self._write(event)
    
    def error(self, event_type: str, message: str = "", error_type: str = "error", **kwargs):
        """Log an error event."""
        event = self._create_event(event_type, message, error_type=error_type, **kwargs)
        self._write(event)
    
    def message_sent(
        self,
        message_id: str,
        target_user: str,
        source_user: Optional[str] = None,
        **kwargs
    ):
        """Log a message send event."""
        event = self._create_event(
            "message_sent",
            f"Message {message_id} sent to {target_user}",
            message_id=message_id,
            target_user=target_user,
            source_user=source_user,
            **kwargs
        )
        self._write(event)
    
    def message_received(
        self,
        message_id: str,
        latency_ms: float,
        source_user: Optional[str] = None,
        **kwargs
    ):
        """Log a message receive event with latency."""
        event = self._create_event(
            "message_received",
            f"Message {message_id} received ({latency_ms:.2f}ms)",
            message_id=message_id,
            latency_ms=latency_ms,
            source_user=source_user,
            **kwargs
        )
        self._write(event)
    
    def message_acked(self, message_id: str, latency_ms: float, **kwargs):
        """Log a message acknowledgment event."""
        event = self._create_event(
            "message_acked",
            f"Message {message_id} acknowledged ({latency_ms:.2f}ms)",
            message_id=message_id,
            latency_ms=latency_ms,
            **kwargs
        )
        self._write(event)
    
    def message_lost(self, message_id: str, **kwargs):
        """Log a message loss event."""
        event = self._create_event(
            "message_lost",
            f"Message {message_id} not delivered",
            message_id=message_id,
            error_type="message_lost",
            **kwargs
        )
        self._write(event)
    
    def duplicate_detected(self, message_id: str, count: int, **kwargs):
        """Log a duplicate message detection event."""
        event = self._create_event(
            "duplicate_detected",
            f"Message {message_id} received {count} times",
            message_id=message_id,
            error_type="duplicate",
            extra={"duplicate_count": count},
            **kwargs
        )
        self._write(event)
    
    def connection_event(self, event_subtype: str, user: str, **kwargs):
        """Log a connection event (login, disconnect, etc)."""
        event = self._create_event(
            f"connection_{event_subtype}",
            f"User {user} {event_subtype}",
            source_user=user,
            **kwargs
        )
        self._write(event)
    
    def cluster_event(self, event_subtype: str, message: str = "", **kwargs):
        """Log a cluster event."""
        event = self._create_event(
            f"cluster_{event_subtype}",
            message,
            **kwargs
        )
        self._write(event)
    
    def metric(self, name: str, value: float, unit: str = "", **kwargs):
        """Log a metric event."""
        event = self._create_event(
            "metric",
            f"{name}: {value} {unit}",
            extra={"metric_name": name, "metric_value": value, "metric_unit": unit, **kwargs}
        )
        self._write(event)
    
    def close(self):
        """Close the logger and finalize the log file."""
        elapsed_ns = time.monotonic_ns() - self._start_time
        elapsed_ms = elapsed_ns / 1_000_000
        
        self.info("test_end", f"Test {self.test_id} completed in {elapsed_ms:.2f}ms")
        
        with self._lock:
            self._file_handle.close()
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type:
            self.error("test_error", str(exc_val), error_type=exc_type.__name__)
        self.close()
        return False


# Global logger registry
_loggers: Dict[str, TestLogger] = {}
_logger_lock = threading.Lock()


def get_logger(
    test_id: str,
    suite: str = "default",
    node_id: str = "local",
    **kwargs
) -> TestLogger:
    """
    Get or create a logger for the given test.
    
    Thread-safe singleton per test_id.
    """
    with _logger_lock:
        if test_id not in _loggers:
            _loggers[test_id] = TestLogger(test_id, suite, node_id, **kwargs)
        return _loggers[test_id]


def close_all_loggers():
    """Close all active loggers."""
    with _logger_lock:
        for logger in _loggers.values():
            logger.close()
        _loggers.clear()
