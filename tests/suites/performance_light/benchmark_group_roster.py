#!/usr/bin/env python3
"""
Benchmark: Group Roster Query Latency (NFR-29)
RFC Reference: RFC-001 v4.0 NFR-29

Validates that group roster query latency is ≤50ms at P99.

Test approach:
1. Create a group with a moderate number of members
2. Issue repeated roster queries
3. Measure P99 latency

Target: P99 ≤ 50ms for group roster queries

Tier: 1 (Performance validation)
Safe for laptop: Yes
Expected duration: <60s
"""

import os
import sys
import time
import socket
import ssl
import struct
import statistics
import uuid
from pathlib import Path

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient

# Configuration
HOST = os.environ.get("IRIS_HOST", "localhost")
PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"

# RFC NFR-29 target
ROSTER_P99_TARGET_MS = 50
NUM_ROSTER_QUERIES = 100
GROUP_MEMBER_COUNT = 20  # Moderate size for benchmark

results = []


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def log_result(name, passed, value, unit, target):
    status = "PASS" if passed else "FAIL"
    log(f"  {status}: {name}")
    log(f"         Measured: {value:.2f} {unit}")
    log(f"         Target:   <= {target:.2f} {unit}")
    results.append((name, passed, value, unit, target))


def unique_user(prefix="roster"):
    return f"{prefix}_{int(time.time() * 1000)}_{uuid.uuid4().hex[:6]}"


def get_tls_socket():
    context = ssl.create_default_context()
    if CA_CERT.exists():
        context.load_verify_locations(str(CA_CERT))
    else:
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(10.0)
    s = context.wrap_socket(raw, server_hostname=HOST)
    s.connect((HOST, PORT))
    return s


def do_login(sock, username):
    sock.sendall(b'\x01' + username.encode('utf-8'))
    sock.settimeout(5.0)
    resp = sock.recv(4096)
    if b"LOGIN_OK" not in resp:
        raise Exception(f"Login failed: {resp!r}")
    return resp


def send_group_create(sock, group_id):
    """Opcode 0x30: Create group."""
    gid_bytes = group_id.encode('utf-8')
    sock.sendall(b'\x30' + struct.pack('!H', len(gid_bytes)) + gid_bytes)


def send_group_join(sock, group_id, user_id):
    """Opcode 0x31: Join group."""
    gid_bytes = group_id.encode('utf-8')
    uid_bytes = user_id.encode('utf-8')
    sock.sendall(
        b'\x31' +
        struct.pack('!H', len(gid_bytes)) + gid_bytes +
        struct.pack('!H', len(uid_bytes)) + uid_bytes
    )


def send_roster_request(sock, group_id):
    """Opcode 0x35: Request group roster."""
    gid_bytes = group_id.encode('utf-8')
    sock.sendall(b'\x35' + struct.pack('!H', len(gid_bytes)) + gid_bytes)


def drain(sock, timeout=1.0):
    """Drain available responses."""
    data = b''
    sock.settimeout(timeout)
    while True:
        try:
            chunk = sock.recv(4096)
            if not chunk:
                break
            data += chunk
        except socket.timeout:
            break
        except Exception:
            break
    return data


def test_roster_query_latency():
    """NFR-29: Group roster query P99 <= 50ms."""
    log("\n=== Benchmark: Group Roster Query Latency ===")
    log(f"  Target: P99 <= {ROSTER_P99_TARGET_MS}ms")
    log(f"  Queries: {NUM_ROSTER_QUERIES}")

    group_id = f"roster_bench_{uuid.uuid4().hex[:8]}"
    admin_name = unique_user("admin")

    try:
        # Step 1: Create group
        admin_sock = get_tls_socket()
        do_login(admin_sock, admin_name)
        send_group_create(admin_sock, group_id)
        drain(admin_sock, timeout=1.0)
        log(f"  Created group '{group_id}'")

        # Step 2: Add members
        member_socks = []
        for i in range(GROUP_MEMBER_COUNT):
            member_name = unique_user(f"member_{i}")
            try:
                msock = get_tls_socket()
                do_login(msock, member_name)
                send_group_join(msock, group_id, member_name)
                drain(msock, timeout=0.5)
                member_socks.append(msock)
            except Exception as e:
                log(f"  WARNING: Failed to add member {i}: {e}")

        log(f"  Added {len(member_socks)} members")
        time.sleep(1)  # Allow membership to propagate

        # Step 3: Benchmark roster queries
        # Measure time-to-first-byte (not drain time) for accurate latency
        latencies = []
        for i in range(NUM_ROSTER_QUERIES):
            start = time.monotonic()
            send_roster_request(admin_sock, group_id)
            admin_sock.settimeout(2.0)
            try:
                resp = admin_sock.recv(4096)
                elapsed_ms = (time.monotonic() - start) * 1000
            except socket.timeout:
                elapsed_ms = (time.monotonic() - start) * 1000
            latencies.append(elapsed_ms)

        # Step 4: Calculate statistics
        if latencies:
            latencies.sort()
            p50 = latencies[len(latencies) // 2]
            p99_idx = int(len(latencies) * 0.99)
            p99 = latencies[min(p99_idx, len(latencies) - 1)]
            mean = statistics.mean(latencies)

            log(f"  P50:  {p50:.2f}ms")
            log(f"  P99:  {p99:.2f}ms")
            log(f"  Mean: {mean:.2f}ms")

            log_result(
                "roster_query_p99",
                p99 <= ROSTER_P99_TARGET_MS,
                p99, "ms",
                ROSTER_P99_TARGET_MS
            )
        else:
            log("  FAIL: No latency samples collected")
            results.append(("roster_query_p99", False, 0, "ms", ROSTER_P99_TARGET_MS))

        # Cleanup sockets
        for s in member_socks:
            try:
                s.close()
            except Exception:
                pass
        admin_sock.close()

    except Exception as e:
        log(f"  FAIL: {e}")
        results.append(("roster_query_p99", False, 0, "ms", ROSTER_P99_TARGET_MS))


if __name__ == "__main__":
    log("=" * 60)
    log("Group Roster Query Latency Benchmark (NFR-29)")
    log("RFC Reference: RFC-001 v4.0 NFR-29")
    log(f"Target: P99 <= {ROSTER_P99_TARGET_MS}ms")
    log("=" * 60)

    test_roster_query_latency()

    log("")
    log("=" * 60)
    total = len(results)
    passed = sum(1 for r in results if r[1])
    log(f"RESULTS: {passed}/{total} benchmarks passed")
    log("=" * 60)

    sys.exit(0 if passed == total else 1)
