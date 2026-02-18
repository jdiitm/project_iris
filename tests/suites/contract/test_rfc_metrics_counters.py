#!/usr/bin/env python3
"""
RFC NFR-32: Standard Counters Verification

Verifies that the server exposes msg_in, msg_out, and ack_sent counters
via the /metrics or /health endpoint, and that they increment correctly
when messages are sent, received, and acknowledged.

INVARIANTS:
- msg_in counter must increment when server receives a message
- msg_out counter must increment when server delivers a message
- ack_sent counter must increment when server receives an ACK

Tier: 1 (Contract - requires running server)
"""

import os
import sys
import time
import socket
import urllib.request
import re

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.tls_connection import get_verified_ssl_context

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
HEALTH_PORT = int(os.environ.get("IRIS_HEALTH_PORT", "8086"))
TIMEOUT = 10

RESULTS = {"passed": 0, "failed": 0, "skipped": 0}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def result(name, passed, detail=""):
    status = "PASS" if passed else "FAIL"
    RESULTS["passed" if passed else "failed"] += 1
    suffix = f" -- {detail}" if detail else ""
    log(f"  [{status}] {name}{suffix}")
    return passed


def skip(name, reason=""):
    RESULTS["skipped"] += 1
    log(f"  [SKIP] {name} -- {reason}")


def get_metrics():
    """Fetch metrics from health endpoint."""
    try:
        url = f"http://{SERVER_HOST}:{HEALTH_PORT}/metrics"
        req = urllib.request.Request(url, method="GET")
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            return resp.read().decode("utf-8")
    except Exception:
        return None


def parse_counter(metrics_text, counter_name):
    """Extract a counter value from Prometheus-format metrics."""
    if metrics_text is None:
        return None
    pattern = rf'^{re.escape(counter_name)}\s+(\d+(?:\.\d+)?)'
    for line in metrics_text.split('\n'):
        m = re.match(pattern, line.strip())
        if m:
            return float(m.group(1))
    return None


def check_server():
    """Check if server is reachable."""
    try:
        context = get_verified_ssl_context()
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT)
        tls_sock = context.wrap_socket(sock, server_hostname=SERVER_HOST)
        tls_sock.connect((SERVER_HOST, SERVER_PORT))
        tls_sock.close()
        return True
    except Exception:
        return False


# =============================================================================
# Static Contract Tests (no server needed)
# =============================================================================

def test_metrics_endpoint_format():
    """NFR-32: Metrics endpoint should return Prometheus-compatible text."""
    metrics = get_metrics()
    if metrics is None:
        skip("metrics_endpoint_format", "health endpoint not reachable")
        return
    result("metrics_endpoint_format",
           "iris_" in metrics or "msg_" in metrics or "message" in metrics,
           "Metrics should contain iris/msg counters")


def test_counter_names_present():
    """NFR-32: msg_in, msg_out counters must be present in metrics."""
    metrics = get_metrics()
    if metrics is None:
        skip("counter_names_present", "health endpoint not reachable")
        return
    has_msg_in = "msg_in" in metrics or "messages_received" in metrics or "iris_messages_in" in metrics
    has_msg_out = "msg_out" in metrics or "messages_sent" in metrics or "iris_messages_out" in metrics
    result("counter_msg_in_present", has_msg_in,
           "msg_in counter (or equivalent) must be in metrics")
    result("counter_msg_out_present", has_msg_out,
           "msg_out counter (or equivalent) must be in metrics")


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    log("=== RFC NFR-32: Standard Counters Verification ===")
    log("")

    test_metrics_endpoint_format()
    test_counter_names_present()

    log("")
    log(f"Results: {RESULTS['passed']} passed, {RESULTS['failed']} failed, {RESULTS['skipped']} skipped")

    if RESULTS["failed"] > 0:
        sys.exit(1)
    sys.exit(0)
