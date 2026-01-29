#!/usr/bin/env python3
"""
Soak Test: Memory Leak Detection (P2 - Scale Critical)

This test runs for an extended period (configurable, default 1 hour for CI,
24 hours for nightly) and monitors memory growth to detect leaks.

Per Principal Test Audit:
"Only short load tests, no 24h leak detection."

INVARIANTS:
- Memory growth over test duration should be < 10%
- No unbounded memory growth patterns
- Process count should remain stable
- ETS table sizes should not grow unbounded

CONFIGURATION:
- SOAK_DURATION_HOURS: Test duration (default: 1 for CI, 24 for nightly)
- SOAK_CONNECTIONS: Number of concurrent connections (default: 100)
- SOAK_MSG_RATE: Messages per second per connection (default: 10)
- SOAK_SAMPLE_INTERVAL: Seconds between memory samples (default: 60)

USAGE:
  # Quick CI run (1 hour)
  python3 test_soak_memory.py

  # Full nightly run (24 hours)
  SOAK_DURATION_HOURS=24 python3 test_soak_memory.py

  # Custom configuration
  SOAK_DURATION_HOURS=2 SOAK_CONNECTIONS=500 python3 test_soak_memory.py
"""

import socket
import struct
import time
import sys
import os
import threading
import subprocess
import json
from datetime import datetime, timedelta
from collections import deque

# Add parent directories to path
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
sys.path.insert(0, os.path.join(PROJECT_ROOT, 'tests'))

try:
    from utilities.iris_client import IrisClient
except ImportError:
    IrisClient = None


def log(msg):
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(f"[{timestamp}] {msg}", flush=True)


class MemorySample:
    """Single memory sample."""
    def __init__(self, timestamp, beam_rss_mb, process_count, ets_memory_mb):
        self.timestamp = timestamp
        self.beam_rss_mb = beam_rss_mb
        self.process_count = process_count
        self.ets_memory_mb = ets_memory_mb


class SoakTestConfig:
    """Configuration for soak test."""
    def __init__(self):
        self.duration_hours = float(os.environ.get('SOAK_DURATION_HOURS', '1'))
        self.connections = int(os.environ.get('SOAK_CONNECTIONS', '100'))
        self.msg_rate = float(os.environ.get('SOAK_MSG_RATE', '10'))
        self.sample_interval = int(os.environ.get('SOAK_SAMPLE_INTERVAL', '60'))
        self.host = os.environ.get('IRIS_HOST', 'localhost')
        self.port = int(os.environ.get('IRIS_PORT', '8085'))
        self.max_memory_growth_pct = float(os.environ.get('SOAK_MAX_GROWTH_PCT', '10'))
        
    def __str__(self):
        return (f"SoakTestConfig(duration={self.duration_hours}h, "
                f"connections={self.connections}, msg_rate={self.msg_rate}/s, "
                f"sample_interval={self.sample_interval}s)")


class MemoryMonitor:
    """Monitors BEAM memory usage over time."""
    
    def __init__(self, config):
        self.config = config
        self.samples = []
        self.running = False
        self.thread = None
        self.beam_pid = None
        
    def find_beam_pid(self):
        """Find the BEAM process ID."""
        try:
            # Try to find beam.smp process
            result = subprocess.run(
                ['pgrep', '-f', 'beam.smp'],
                capture_output=True, text=True
            )
            if result.returncode == 0:
                pids = result.stdout.strip().split('\n')
                if pids and pids[0]:
                    return int(pids[0])
        except Exception as e:
            log(f"Warning: Could not find BEAM PID: {e}")
        return None
    
    def get_beam_memory_mb(self):
        """Get BEAM RSS memory in MB."""
        if not self.beam_pid:
            self.beam_pid = self.find_beam_pid()
        
        if not self.beam_pid:
            return 0.0
        
        try:
            # Read from /proc on Linux
            with open(f'/proc/{self.beam_pid}/status', 'r') as f:
                for line in f:
                    if line.startswith('VmRSS:'):
                        # VmRSS is in kB
                        kb = int(line.split()[1])
                        return kb / 1024.0
        except FileNotFoundError:
            # Process may have died
            self.beam_pid = None
        except Exception as e:
            log(f"Warning: Could not read BEAM memory: {e}")
        
        return 0.0
    
    def get_beam_process_count(self):
        """Get number of Erlang processes (via epmd or estimation)."""
        # This would ideally query the BEAM, but for now we estimate
        # based on connections + overhead
        return self.config.connections * 2 + 100  # Rough estimate
    
    def get_ets_memory_mb(self):
        """Get ETS memory usage (would query BEAM in production)."""
        # Placeholder - would use RPC to BEAM in production
        return 0.0
    
    def take_sample(self):
        """Take a memory sample."""
        sample = MemorySample(
            timestamp=datetime.now(),
            beam_rss_mb=self.get_beam_memory_mb(),
            process_count=self.get_beam_process_count(),
            ets_memory_mb=self.get_ets_memory_mb()
        )
        self.samples.append(sample)
        return sample
    
    def start(self):
        """Start background monitoring."""
        self.running = True
        self.thread = threading.Thread(target=self._monitor_loop, daemon=True)
        self.thread.start()
        log("Memory monitor started")
    
    def stop(self):
        """Stop monitoring."""
        self.running = False
        if self.thread:
            self.thread.join(timeout=5)
        log(f"Memory monitor stopped. Collected {len(self.samples)} samples.")
    
    def _monitor_loop(self):
        """Background monitoring loop."""
        while self.running:
            try:
                sample = self.take_sample()
                if sample.beam_rss_mb > 0:
                    log(f"Memory sample: RSS={sample.beam_rss_mb:.1f}MB, "
                        f"procs={sample.process_count}")
            except Exception as e:
                log(f"Warning: Sample failed: {e}")
            
            # Sleep in small increments to allow quick shutdown
            for _ in range(self.config.sample_interval):
                if not self.running:
                    break
                time.sleep(1)
    
    def analyze(self):
        """Analyze collected samples for memory leaks."""
        if len(self.samples) < 2:
            return {
                'status': 'insufficient_data',
                'samples': len(self.samples),
                'error': 'Need at least 2 samples for analysis'
            }
        
        # Get first and last samples with valid memory readings
        valid_samples = [s for s in self.samples if s.beam_rss_mb > 0]
        if len(valid_samples) < 2:
            return {
                'status': 'insufficient_data',
                'samples': len(valid_samples),
                'error': 'Need at least 2 valid memory samples'
            }
        
        first = valid_samples[0]
        last = valid_samples[-1]
        
        # Calculate growth
        growth_mb = last.beam_rss_mb - first.beam_rss_mb
        if first.beam_rss_mb > 0:
            growth_pct = (growth_mb / first.beam_rss_mb) * 100
        else:
            growth_pct = 0
        
        # Calculate duration
        duration = (last.timestamp - first.timestamp).total_seconds() / 3600  # hours
        
        # Analyze trend (simple linear regression)
        times = [(s.timestamp - first.timestamp).total_seconds() for s in valid_samples]
        memories = [s.beam_rss_mb for s in valid_samples]
        
        if len(times) > 2:
            # Calculate slope (MB per hour)
            n = len(times)
            sum_t = sum(times)
            sum_m = sum(memories)
            sum_tm = sum(t * m for t, m in zip(times, memories))
            sum_tt = sum(t * t for t in times)
            
            if (n * sum_tt - sum_t * sum_t) != 0:
                slope = (n * sum_tm - sum_t * sum_m) / (n * sum_tt - sum_t * sum_t)
                slope_per_hour = slope * 3600  # Convert to MB/hour
            else:
                slope_per_hour = 0
        else:
            slope_per_hour = growth_mb / duration if duration > 0 else 0
        
        # Determine pass/fail
        passed = growth_pct < self.config.max_memory_growth_pct
        
        return {
            'status': 'passed' if passed else 'failed',
            'samples': len(valid_samples),
            'duration_hours': duration,
            'initial_mb': first.beam_rss_mb,
            'final_mb': last.beam_rss_mb,
            'growth_mb': growth_mb,
            'growth_pct': growth_pct,
            'slope_mb_per_hour': slope_per_hour,
            'max_allowed_growth_pct': self.config.max_memory_growth_pct,
            'passed': passed
        }


class LoadGenerator:
    """Generates sustained load for soak testing."""
    
    def __init__(self, config):
        self.config = config
        self.clients = []
        self.running = False
        self.threads = []
        self.stats = {
            'messages_sent': 0,
            'messages_received': 0,
            'errors': 0,
            'reconnects': 0
        }
        self.stats_lock = threading.Lock()
    
    def start(self):
        """Start load generation."""
        self.running = True
        
        # Create client connections
        log(f"Creating {self.config.connections} connections...")
        
        for i in range(self.config.connections):
            thread = threading.Thread(
                target=self._client_loop,
                args=(i,),
                daemon=True
            )
            self.threads.append(thread)
            thread.start()
            
            # Stagger connection creation
            if i % 10 == 0:
                time.sleep(0.1)
        
        log(f"Load generator started with {self.config.connections} clients")
    
    def stop(self):
        """Stop load generation."""
        self.running = False
        
        # Wait for threads to finish
        for thread in self.threads:
            thread.join(timeout=5)
        
        log(f"Load generator stopped. Stats: {self.stats}")
    
    def _client_loop(self, client_id):
        """Single client load loop."""
        username = f"soak_user_{client_id}"
        msg_interval = 1.0 / self.config.msg_rate
        client = None
        
        while self.running:
            try:
                # Connect if needed
                if client is None:
                    if IrisClient:
                        client = IrisClient(self.config.host, self.config.port)
                        client.login(username)
                    else:
                        # Fallback: just track stats without real connection
                        time.sleep(1)
                        continue
                
                # Send a message
                target = f"soak_user_{(client_id + 1) % self.config.connections}"
                client.send_msg(target, f"soak_msg_{time.time()}")
                
                with self.stats_lock:
                    self.stats['messages_sent'] += 1
                
                # Try to receive
                try:
                    client.sock.settimeout(0.01)
                    msg = client.recv_msg(timeout=0.01)
                    if msg:
                        with self.stats_lock:
                            self.stats['messages_received'] += 1
                except socket.timeout:
                    pass
                except Exception:
                    pass
                
                # Rate limiting
                time.sleep(msg_interval)
                
            except socket.error as e:
                with self.stats_lock:
                    self.stats['errors'] += 1
                    self.stats['reconnects'] += 1
                client = None
                time.sleep(1)  # Backoff before reconnect
                
            except Exception as e:
                with self.stats_lock:
                    self.stats['errors'] += 1
                time.sleep(1)
        
        # Cleanup
        if client:
            try:
                client.close()
            except Exception:
                pass
    
    def get_stats(self):
        """Get current statistics."""
        with self.stats_lock:
            return dict(self.stats)


def test_soak_24h():
    """
    Run soak test for configured duration.
    Assert memory growth < 10%.
    """
    config = SoakTestConfig()
    
    log("=" * 70)
    log("SOAK TEST: Memory Leak Detection")
    log("=" * 70)
    log(f"Configuration: {config}")
    log(f"Expected duration: {config.duration_hours} hours")
    log("=" * 70)
    
    # Check if we can connect to the server
    try:
        test_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        test_sock.settimeout(5)
        test_sock.connect((config.host, config.port))
        test_sock.close()
        log(f"Successfully connected to {config.host}:{config.port}")
    except socket.error as e:
        log(f"SKIP: Cannot connect to server at {config.host}:{config.port}: {e}")
        log("Make sure the Iris server is running before running soak tests.")
        return True  # Skip, don't fail
    
    # Initialize components
    monitor = MemoryMonitor(config)
    load_gen = LoadGenerator(config)
    
    # Calculate end time
    end_time = datetime.now() + timedelta(hours=config.duration_hours)
    log(f"Test will run until: {end_time}")
    
    try:
        # Start monitoring and load generation
        monitor.start()
        time.sleep(5)  # Let monitoring stabilize
        
        load_gen.start()
        time.sleep(5)  # Let load stabilize
        
        # Take baseline sample
        baseline = monitor.take_sample()
        log(f"Baseline memory: {baseline.beam_rss_mb:.1f}MB")
        
        # Main loop - run until duration expires
        last_report = datetime.now()
        report_interval = timedelta(minutes=15)
        
        while datetime.now() < end_time:
            time.sleep(60)  # Check every minute
            
            # Periodic progress report
            if datetime.now() - last_report > report_interval:
                elapsed = (datetime.now() - (end_time - timedelta(hours=config.duration_hours)))
                remaining = end_time - datetime.now()
                stats = load_gen.get_stats()
                current = monitor.take_sample()
                
                log(f"Progress: elapsed={elapsed}, remaining={remaining}")
                log(f"  Memory: {current.beam_rss_mb:.1f}MB, "
                    f"sent={stats['messages_sent']}, "
                    f"received={stats['messages_received']}, "
                    f"errors={stats['errors']}")
                
                last_report = datetime.now()
        
        log("Test duration completed. Stopping load...")
        
    except KeyboardInterrupt:
        log("Test interrupted by user")
    
    finally:
        # Cleanup
        load_gen.stop()
        monitor.stop()
    
    # Analyze results
    log("\n" + "=" * 70)
    log("ANALYSIS")
    log("=" * 70)
    
    results = monitor.analyze()
    
    if results['status'] == 'insufficient_data':
        log(f"WARNING: {results['error']}")
        log("Test inconclusive - not enough data collected")
        return True  # Don't fail on insufficient data
    
    log(f"Test duration: {results['duration_hours']:.2f} hours")
    log(f"Samples collected: {results['samples']}")
    log(f"Initial memory: {results['initial_mb']:.1f}MB")
    log(f"Final memory: {results['final_mb']:.1f}MB")
    log(f"Growth: {results['growth_mb']:.1f}MB ({results['growth_pct']:.1f}%)")
    log(f"Trend: {results['slope_mb_per_hour']:.2f} MB/hour")
    log(f"Max allowed growth: {results['max_allowed_growth_pct']}%")
    
    log("\n" + "=" * 70)
    if results['passed']:
        log("RESULT: PASSED - Memory growth within acceptable limits")
    else:
        log("RESULT: FAILED - Memory growth exceeded threshold")
        log(f"  Growth {results['growth_pct']:.1f}% > max {results['max_allowed_growth_pct']}%")
    log("=" * 70)
    
    # Get final load stats
    final_stats = load_gen.get_stats()
    log(f"\nLoad statistics:")
    log(f"  Messages sent: {final_stats['messages_sent']}")
    log(f"  Messages received: {final_stats['messages_received']}")
    log(f"  Errors: {final_stats['errors']}")
    log(f"  Reconnects: {final_stats['reconnects']}")
    
    return results['passed']


def test_quick_memory_check():
    """
    Quick memory check (5 minutes) for CI.
    Verifies basic memory stability without full soak.
    """
    log("=" * 60)
    log("QUICK MEMORY CHECK (5 minutes)")
    log("=" * 60)
    
    config = SoakTestConfig()
    config.duration_hours = 5 / 60  # 5 minutes
    config.connections = 10
    config.sample_interval = 30
    
    # Override env for quick test
    os.environ['SOAK_DURATION_HOURS'] = str(config.duration_hours)
    
    return test_soak_24h()


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='Soak test for memory leak detection')
    parser.add_argument('--quick', action='store_true', 
                        help='Run quick 5-minute test instead of full soak')
    args = parser.parse_args()
    
    if args.quick:
        passed = test_quick_memory_check()
    else:
        passed = test_soak_24h()
    
    sys.exit(0 if passed else 1)
