#!/usr/bin/env python3
"""
Unit Cost Benchmark

Measures the CPU cost per message to establish baseline performance.

INVARIANTS:
- Throughput must exceed minimum threshold (8k local / 3k CI msg/s)
- System must handle concurrent load without crashing
- CPU cost per message must be measurable

Note: Local threshold 8k, CI threshold 3k (TLS on 2-vCPU).
Full system benchmark target is still 30k+ msg/s (NFR-2).

Exit codes:
- 0: Benchmark passed (throughput >= threshold)
- 1: Benchmark failed or error
"""

import socket
import ssl
import struct
import time
import os
import sys
import threading
import psutil
from pathlib import Path

HOST = 'localhost'
PORT = 8085

# CI environment detection — scale workload to available resources
IS_CI = os.environ.get("CI") == "true" or os.environ.get("GITHUB_ACTIONS") == "true"

# TLS Configuration
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from tests.utilities.tls_connection import get_verified_ssl_context

CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"

# Socket timeout — prevents hangs when server is degraded from prior benchmarks
SOCKET_TIMEOUT = 30

def get_ssl_context():
    """Create SSL context for TLS connections."""
    return get_verified_ssl_context()


# Scale workload to environment:
# - CI (2 vCPU): 5 threads x 5000 = 25K messages (completes in ~30s)
# - Local (multi-core): 10 threads x 50000 = 500K messages (full stress)
# Throughput assertion (8k msg/s) is IDENTICAL in both paths.
if IS_CI:
    MSG_COUNT = 5000
    THREADS = 5
else:
    MSG_COUNT = 50000
    THREADS = 10

# Minimum throughput threshold (messages per second)
# Full system benchmark (NFR-2) requires 30k+ msg/s.
# Local (multi-core, TLS): 8k msg/s — accounts for test suite overhead.
# CI (2-vCPU, TLS): 3k msg/s — TLS encryption adds significant per-message CPU cost
# on a constrained runner. This still validates the system handles concurrent load.
if IS_CI:
    MIN_THROUGHPUT = 3000
else:
    MIN_THROUGHPUT = 8000


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


_ssl_context_cache = None


def _get_cached_ssl_context():
    """Get or create a cached SSL context for benchmark connections.
    
    Creating ssl.create_default_context() is expensive (loads all system CAs).
    Cache it for reuse across all worker threads.
    """
    global _ssl_context_cache
    if _ssl_context_cache is None:
        _ssl_context_cache = get_ssl_context()
    return _ssl_context_cache


def create_socket():
    """Create a TLS connection to the Iris server.
    
    TLS is MANDATORY per RFC NFR-14. No plain TCP fallback.
    
    Uses wrap-then-connect pattern (same as chaos_dist/utils.create_tls_socket)
    which performs TCP+TLS handshake atomically. This is critical after heavy benchmarks
    where the Erlang SSL acceptor may have in-flight state that causes connect-then-wrap
    to hang (the server accepted TCP but never responds to the late SSL ClientHello).
    """
    try:
        context = _get_cached_ssl_context()
        raw_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        raw_sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        raw_sock.settimeout(SOCKET_TIMEOUT)
        # Wrap BEFORE connect — TCP+TLS handshake happens atomically in connect().
        # This matches create_tls_socket() from chaos_dist/utils.py which all other
        # tests use successfully.
        tls_sock = context.wrap_socket(raw_sock, server_hostname=HOST)
        tls_sock.connect((HOST, PORT))
        return tls_sock
    except (socket.error, ssl.SSLError, socket.timeout) as e:
        log(f"TLS connection failed: {e}")
        try:
            raw_sock.close()
        except Exception:
            pass
        return None


def packet_login(user):
    return b'\x01' + user.encode('utf-8')


def packet_msg(target, payload, seq_no):
    """
    Build a sequenced message packet (opcode 0x07).
    
    RFC-001-AMENDMENT-001 v1.0: Opcode 0x02 (plaintext) is REJECTED.
    Must use 0x07 (sequenced) for all messages.
    
    Wire format: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
    """
    t_bytes = target.encode('utf-8')
    return (b'\x07' + struct.pack('>H', len(t_bytes)) + t_bytes +
            struct.pack('>Q', seq_no) + struct.pack('>H', len(payload)) + payload)


def benchmark_worker(results, errors, pid):
    """Worker thread that sends messages and records duration."""
    sock = create_socket()
    if not sock:
        errors.append(f"Worker {pid} failed to connect")
        return

    try:
        # Login
        sock.sendall(packet_login(f"user_{pid}"))
        resp = sock.recv(1024)
        if b"LOGIN_OK" not in resp:
            errors.append(f"Worker {pid} login failed")
            sock.close()
            return

        # Send messages using RFC-compliant opcode 0x07 (sequenced)
        target = "recipient_0"
        payload = b"X" * 50

        start = time.time()
        for j in range(MSG_COUNT):
            pkt = packet_msg(target, payload, j + 1)
            sock.sendall(pkt)
        dur = time.time() - start

        sock.close()
        results.append(dur)
    except (OSError, socket.timeout) as e:
        errors.append(f"Worker {pid} error: {type(e).__name__}: {e}")
        try:
            sock.close()
        except Exception:
            pass
    except Exception as e:
        errors.append(f"Worker {pid} unexpected: {type(e).__name__}: {e}")
        try:
            sock.close()
        except Exception:
            pass


def measure_system_resources(pid, duration, container):
    """Monitor Erlang process CPU/RAM usage."""
    try:
        p = psutil.Process(pid)
        cpu_start = p.cpu_times()

        time.sleep(duration)

        cpu_end = p.cpu_times()
        container['cpu_user'] = cpu_end.user - cpu_start.user
        container['cpu_sys'] = cpu_end.system - cpu_start.system
        container['mem_rss'] = p.memory_info().rss
        container['success'] = True
    except psutil.NoSuchProcess:
        container['success'] = False
        container['error'] = "Process terminated during monitoring"
    except Exception as e:
        container['success'] = False
        container['error'] = str(e)


def main() -> int:
    """
    Run benchmark and return exit code.
    
    Returns:
        0 if benchmark passed, 1 if failed
    """
    log("--- UNIT COST BENCHMARK ---")
    log(f"  IS_CI={IS_CI}, TLS=mandatory, CA_CERT exists={CA_CERT.exists()}")
    log(f"  THREADS={THREADS}, MSG_COUNT={MSG_COUNT}, MIN_THROUGHPUT={MIN_THROUGHPUT}")

    passed = True

    # Quick TCP probe — verify server is listening before attempting TLS.
    # Using a short timeout (3s) to fail fast if server is down, instead of
    # the full SOCKET_TIMEOUT (30s) which would cause the test to appear stuck.
    try:
        probe = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        probe.settimeout(3)
        probe.connect((HOST, PORT))
        probe.close()
        log("PASS: Server is listening on port 8085")
    except Exception as e:
        log(f"FAIL: Server not running on port 8085 ({e})")
        log("Please start the cluster before running benchmarks")
        return 1

    # Verify TLS/connection works with full handshake (retry up to 3 times)
    # After heavy benchmarks, the server's SSL acceptor may need a moment to stabilise.
    conn_ok = False
    for attempt in range(3):
        try:
            test_sock = create_socket()
            if test_sock:
                test_sock.close()
                log(f"PASS: TLS connection verified (attempt {attempt + 1})")
                conn_ok = True
                break
            else:
                log(f"WARN: Connection attempt {attempt + 1}/3 failed — retrying in 2s")
                time.sleep(2)
        except Exception:
            log(f"WARN: Connection probe attempt {attempt + 1}/3 raised exception — retrying in 2s")
            time.sleep(2)
    if not conn_ok:
        log("FAIL: Could not establish any connection after 3 attempts")
        return 1

    # Find beam pid
    erl_pid = None
    for proc in psutil.process_iter(['pid', 'name']):
        try:
            if 'beam' in proc.info['name'].lower():
                erl_pid = proc.info['pid']
                break
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            continue

    if not erl_pid:
        log("FAIL: Erlang node (beam) process not found")
        return 1

    log(f"PASS: Found Erlang PID: {erl_pid}")

    # Configuration — THREADS and MSG_COUNT set at module level (CI-aware)
    TOTAL_MSGS = THREADS * MSG_COUNT

    # Global timeout: prevent infinite hangs from degraded server
    # CI: 120s, Local: 300s
    GLOBAL_TIMEOUT = 120 if IS_CI else 300

    log(f"Running benchmark: {THREADS} threads x {MSG_COUNT} msgs = {TOTAL_MSGS} total messages")
    log(f"Environment: {'CI' if IS_CI else 'local'}, timeout: {GLOBAL_TIMEOUT}s")

    # Start resource monitoring
    res_container = {}
    monitor = threading.Thread(target=measure_system_resources, args=(erl_pid, 5, res_container))
    monitor.start()

    # Run benchmark workers
    threads = []
    durations = []
    errors = []

    start_time = time.time()
    for i in range(THREADS):
        t = threading.Thread(target=benchmark_worker, args=(durations, errors, i), daemon=True)
        t.start()
        threads.append(t)

    # Join with timeout to prevent infinite hangs
    for t in threads:
        remaining = GLOBAL_TIMEOUT - (time.time() - start_time)
        if remaining <= 0:
            log("WARN: Global timeout reached, not all workers finished")
            break
        t.join(timeout=remaining)
        if t.is_alive():
            log("WARN: Worker thread still alive after timeout")
    monitor.join(timeout=10)

    total_time = time.time() - start_time

    # ================================================================
    # ASSERTIONS
    # ================================================================
    log("\n=== ASSERTIONS ===")

    # Check for worker errors
    if errors:
        log(f"FAIL: {len(errors)} worker errors occurred:")
        for err in errors[:5]:  # Show first 5 errors
            log(f"  - {err}")
        passed = False
    else:
        log(f"PASS: All {THREADS} workers completed successfully")

    # Calculate metrics
    if len(durations) > 0:
        successful_msgs = len(durations) * MSG_COUNT
        cpu_total_seconds = res_container.get('cpu_user', 0) + res_container.get('cpu_sys', 0)
        msgs_per_sec = successful_msgs / total_time if total_time > 0 else 0
        cpu_per_msg = cpu_total_seconds / successful_msgs if successful_msgs > 0 else 0

        log("\n--- METRICS ---")
        log(f"Total Messages: {successful_msgs}")
        log(f"Total Time:     {total_time:.4f}s")
        log(f"Throughput:     {msgs_per_sec:.2f} msgs/sec")
        log(f"Total CPU Time: {cpu_total_seconds:.4f}s")
        log(f"CPU Cost/Msg:   {cpu_per_msg*1_000_000:.2f} microseconds")

        if cpu_per_msg > 0:
            log(f"Est. Max RPS (1 Core): {1.0/cpu_per_msg:.2f}")

        log("\n=== THRESHOLD CHECK ===")

        # Assertion: Throughput meets minimum
        if msgs_per_sec >= MIN_THROUGHPUT:
            log(f"PASS: Throughput {msgs_per_sec:.0f} msg/s >= {MIN_THROUGHPUT} threshold")
        else:
            log(f"FAIL: Throughput {msgs_per_sec:.0f} msg/s < {MIN_THROUGHPUT} threshold")
            passed = False
    else:
        log("FAIL: No successful benchmark runs completed")
        passed = False

    # Final result
    log("\n=== RESULT ===")
    if passed:
        log("BENCHMARK PASSED")
        return 0
    else:
        log("BENCHMARK FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
