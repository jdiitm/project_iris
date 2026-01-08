"""
Resource Monitor for Tests

Captures CPU, memory, and disk snapshots during test execution.
Designed for post-processing, not real-time monitoring.
"""

import os
import sys
import time
import subprocess
import threading
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Dict, Any
import json


@dataclass
class ResourceSample:
    """Single resource measurement sample."""
    timestamp: str
    elapsed_seconds: float
    cpu_percent: float = 0.0
    memory_rss_mb: float = 0.0
    memory_available_mb: float = 0.0
    disk_free_gb: float = 0.0
    process_count: int = 0
    open_connections: int = 0
    extra: Dict[str, Any] = field(default_factory=dict)


class ResourceMonitor:
    """
    Periodic resource sampler for test execution.
    
    Captures system resources at configurable intervals.
    Thread-safe, designed for background sampling.
    """
    
    def __init__(
        self,
        sample_interval_seconds: float = 1.0,
        project_root: Optional[Path] = None
    ):
        self.sample_interval = sample_interval_seconds
        self.project_root = project_root or Path.cwd()
        self.samples: List[ResourceSample] = []
        self._running = False
        self._thread: Optional[threading.Thread] = None
        self._start_time: float = 0
        self._lock = threading.Lock()
    
    def _get_cpu_percent(self) -> float:
        """Get current CPU utilization percentage."""
        try:
            if sys.platform == "darwin":
                # macOS: use top command
                result = subprocess.run(
                    ["top", "-l", "1", "-n", "0"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                for line in result.stdout.split('\n'):
                    if 'CPU usage' in line:
                        # Parse "CPU usage: 5.0% user, 3.0% sys, 92.0% idle"
                        parts = line.split(',')
                        user = float(parts[0].split(':')[1].strip().rstrip('%'))
                        sys_cpu = float(parts[1].strip().split('%')[0])
                        return user + sys_cpu
            else:
                # Linux: read /proc/stat
                with open('/proc/stat', 'r') as f:
                    line = f.readline()
                    parts = line.split()
                    idle = int(parts[4])
                    total = sum(int(p) for p in parts[1:])
                    return 100 * (1 - idle / total) if total > 0 else 0
        except Exception:
            pass
        return 0.0
    
    def _get_memory_info(self) -> tuple:
        """Get memory RSS and available in MB."""
        rss_mb = 0.0
        available_mb = 0.0
        
        try:
            if sys.platform == "darwin":
                # Get available memory from vm_stat
                result = subprocess.run(
                    ["vm_stat"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0:
                    page_size = 4096
                    for line in result.stdout.split('\n'):
                        if "Pages free" in line:
                            pages = int(line.split(':')[1].strip().rstrip('.'))
                            available_mb = (pages * page_size) / (1024 * 1024)
                            break
                
                # Get beam.smp RSS
                result = subprocess.run(
                    ["ps", "-o", "rss=", "-C", "beam.smp"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0:
                    rss_values = [int(x) for x in result.stdout.strip().split() if x.strip().isdigit()]
                    rss_mb = sum(rss_values) / 1024 if rss_values else 0
            else:
                # Linux
                with open('/proc/meminfo', 'r') as f:
                    for line in f:
                        if line.startswith('MemAvailable:'):
                            available_mb = int(line.split()[1]) / 1024
                            break
                
                # Get beam.smp RSS from /proc
                result = subprocess.run(
                    ["pgrep", "-f", "beam.smp"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0:
                    pids = result.stdout.strip().split()
                    for pid in pids:
                        try:
                            with open(f'/proc/{pid}/status', 'r') as f:
                                for line in f:
                                    if line.startswith('VmRSS:'):
                                        rss_mb += int(line.split()[1]) / 1024
                                        break
                        except Exception:
                            pass
        except Exception:
            pass
        
        return rss_mb, available_mb
    
    def _get_disk_free_gb(self) -> float:
        """Get free disk space in GB."""
        try:
            statvfs = os.statvfs(str(self.project_root))
            return (statvfs.f_frsize * statvfs.f_bavail) / (1024**3)
        except Exception:
            return 0.0
    
    def _get_process_count(self) -> int:
        """Get count of beam.smp processes."""
        try:
            result = subprocess.run(
                ["pgrep", "-fc", "beam.smp"],
                capture_output=True,
                text=True,
                timeout=5
            )
            return int(result.stdout.strip()) if result.returncode == 0 else 0
        except Exception:
            return 0
    
    def _get_open_connections(self, port: int = 8085) -> int:
        """Get count of open TCP connections to a port."""
        try:
            if sys.platform == "darwin":
                result = subprocess.run(
                    f"lsof -i :{port} | wc -l",
                    shell=True,
                    capture_output=True,
                    text=True,
                    timeout=5
                )
            else:
                result = subprocess.run(
                    f"ss -tn state established 'sport = :{port}' | wc -l",
                    shell=True,
                    capture_output=True,
                    text=True,
                    timeout=5
                )
            return max(0, int(result.stdout.strip()) - 1)  # Subtract header
        except Exception:
            return 0
    
    def _take_sample(self) -> ResourceSample:
        """Take a single resource sample."""
        rss_mb, available_mb = self._get_memory_info()
        
        return ResourceSample(
            timestamp=datetime.utcnow().isoformat() + "Z",
            elapsed_seconds=time.monotonic() - self._start_time,
            cpu_percent=self._get_cpu_percent(),
            memory_rss_mb=rss_mb,
            memory_available_mb=available_mb,
            disk_free_gb=self._get_disk_free_gb(),
            process_count=self._get_process_count(),
            open_connections=self._get_open_connections()
        )
    
    def _sampling_loop(self):
        """Background sampling loop."""
        while self._running:
            sample = self._take_sample()
            with self._lock:
                self.samples.append(sample)
            time.sleep(self.sample_interval)
    
    def start(self):
        """Start background sampling."""
        if self._running:
            return
        
        self._start_time = time.monotonic()
        self._running = True
        self._thread = threading.Thread(target=self._sampling_loop, daemon=True)
        self._thread.start()
    
    def stop(self):
        """Stop background sampling."""
        self._running = False
        if self._thread:
            self._thread.join(timeout=2.0)
            self._thread = None
    
    def sample_now(self) -> ResourceSample:
        """Take an immediate sample (synchronous)."""
        sample = self._take_sample()
        with self._lock:
            self.samples.append(sample)
        return sample
    
    def get_samples(self) -> List[ResourceSample]:
        """Get all collected samples."""
        with self._lock:
            return list(self.samples)
    
    def get_summary(self) -> Dict[str, Any]:
        """Get summary statistics from samples."""
        samples = self.get_samples()
        if not samples:
            return {}
        
        cpu_values = [s.cpu_percent for s in samples]
        mem_values = [s.memory_rss_mb for s in samples]
        conn_values = [s.open_connections for s in samples]
        
        return {
            "sample_count": len(samples),
            "duration_seconds": samples[-1].elapsed_seconds if samples else 0,
            "cpu": {
                "min": min(cpu_values),
                "max": max(cpu_values),
                "avg": sum(cpu_values) / len(cpu_values)
            },
            "memory_rss_mb": {
                "min": min(mem_values),
                "max": max(mem_values),
                "avg": sum(mem_values) / len(mem_values)
            },
            "connections": {
                "min": min(conn_values),
                "max": max(conn_values),
                "avg": sum(conn_values) / len(conn_values)
            }
        }
    
    def save_to_file(self, filepath: Path):
        """Save samples to JSON file."""
        samples = self.get_samples()
        data = {
            "summary": self.get_summary(),
            "samples": [asdict(s) for s in samples]
        }
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
    
    def __enter__(self):
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()
        return False
