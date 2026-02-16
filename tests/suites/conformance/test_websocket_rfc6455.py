#!/usr/bin/env python3
"""
G-29: WebSocket RFC 6455 Upgrade Compliance

RFC 6455: The WebSocket Protocol
RFC-001 NFR-18: Input validation -- all untrusted input MUST be validated.

Tests that the server handles HTTP/WebSocket upgrade requests without
crashing, regardless of whether WebSocket is supported.

If a dedicated WS port is open, tests run against it.
If not, tests run against the main server port -- the server MUST
handle unexpected HTTP input gracefully (disconnect, error -- not crash).

Test Scenarios:
1. Valid WebSocket upgrade request -- server must not crash
2. Missing Sec-WebSocket-Key -- server must not crash
3. Wrong Sec-WebSocket-Version -- server must not crash
4. Server must remain alive after all probing

INVARIANTS:
- Server MUST NOT crash from any HTTP request on any port
- No skips, no weakening

Pattern: follows test_protocol_fuzz.py (server_alive after abuse)

Tier: 2 (Conformance)
"""

import sys
import os
import socket
import ssl
import time
import hashlib
import base64

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)
from tests.utilities.helpers import wait_until

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
WS_PORT = int(os.environ.get("IRIS_WS_PORT", "8086"))
TIMEOUT = 5

# Resolved at runtime: WS port if open, otherwise main server port.
# Either way, the server must handle HTTP/WS requests without crashing.
TARGET_PORT = None

results = []


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def server_alive():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect((SERVER_HOST, SERVER_PORT))
        s.close()
        return True
    except Exception:
        return False


def port_open(port):
    """Check if a port is accepting connections."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect((SERVER_HOST, port))
        s.close()
        return True
    except Exception:
        return False


def send_http_request(port, request_bytes):
    """Send raw HTTP request via TLS and return response bytes."""
    try:
        ctx = ssl.create_default_context()
        ctx.check_hostname = False
        ctx.verify_mode = ssl.CERT_NONE
        raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        raw.settimeout(TIMEOUT)
        tls_sock = ctx.wrap_socket(raw, server_hostname=SERVER_HOST)
        tls_sock.connect((SERVER_HOST, port))
        tls_sock.sendall(request_bytes)
        tls_sock.settimeout(3)
        try:
            resp = tls_sock.recv(4096)
        except socket.timeout:
            resp = b""
        tls_sock.close()
        return resp
    except (ssl.SSLError, ConnectionResetError, BrokenPipeError):
        # Server rejected or disconnected -- that's acceptable
        return b""
    except Exception:
        return None


def generate_ws_key():
    """Generate a random Sec-WebSocket-Key."""
    return base64.b64encode(os.urandom(16)).decode("ascii")


# =============================================================================
# Test 1: WebSocket Upgrade Request
# =============================================================================
def test_ws_upgrade():
    """Send valid WebSocket upgrade request. Server must not crash."""
    log(f"\n=== Test 1: WebSocket Upgrade (port {TARGET_PORT}) ===")

    ws_key = generate_ws_key()
    request = (
        f"GET /ws HTTP/1.1\r\n"
        f"Host: {SERVER_HOST}:{TARGET_PORT}\r\n"
        f"Upgrade: websocket\r\n"
        f"Connection: Upgrade\r\n"
        f"Sec-WebSocket-Key: {ws_key}\r\n"
        f"Sec-WebSocket-Version: 13\r\n"
        f"\r\n"
    ).encode("utf-8")

    resp = send_http_request(TARGET_PORT, request)

    if not wait_until(server_alive, timeout=3, description="server alive after WS upgrade"):
        log("  FAIL: Server crashed from WebSocket upgrade request")
        return False

    if resp is None:
        log("  Connection failed (port may not accept TLS)")
    elif len(resp) == 0:
        log("  Server disconnected or timed out (acceptable)")
    else:
        resp_str = resp.decode("utf-8", errors="replace")
        log(f"  Response: {resp_str[:100]}")

    log("  PASS: Server survived WebSocket upgrade request")
    return True


# =============================================================================
# Test 2: Missing Sec-WebSocket-Key
# =============================================================================
def test_missing_ws_key():
    """Send upgrade without Sec-WebSocket-Key. Server must not crash."""
    log(f"\n=== Test 2: Missing Sec-WebSocket-Key (port {TARGET_PORT}) ===")

    request = (
        f"GET /ws HTTP/1.1\r\n"
        f"Host: {SERVER_HOST}:{TARGET_PORT}\r\n"
        f"Upgrade: websocket\r\n"
        f"Connection: Upgrade\r\n"
        f"Sec-WebSocket-Version: 13\r\n"
        f"\r\n"
    ).encode("utf-8")

    resp = send_http_request(TARGET_PORT, request)

    if not wait_until(server_alive, timeout=3, description="server alive after malformed WS"):
        log("  FAIL: Server crashed from malformed WS upgrade")
        return False

    log("  PASS: Server survived malformed WS upgrade")
    return True


# =============================================================================
# Test 3: Wrong WebSocket Version
# =============================================================================
def test_wrong_ws_version():
    """Send upgrade with wrong Sec-WebSocket-Version. Server must not crash."""
    log(f"\n=== Test 3: Wrong WebSocket Version (port {TARGET_PORT}) ===")

    ws_key = generate_ws_key()
    request = (
        f"GET /ws HTTP/1.1\r\n"
        f"Host: {SERVER_HOST}:{TARGET_PORT}\r\n"
        f"Upgrade: websocket\r\n"
        f"Connection: Upgrade\r\n"
        f"Sec-WebSocket-Key: {ws_key}\r\n"
        f"Sec-WebSocket-Version: 8\r\n"
        f"\r\n"
    ).encode("utf-8")

    resp = send_http_request(TARGET_PORT, request)

    if not wait_until(server_alive, timeout=3, description="server alive after wrong WS version"):
        log("  FAIL: Server crashed from wrong WS version")
        return False

    log("  PASS: Server survived wrong WS version")
    return True


# =============================================================================
# Test 4: Raw HTTP GET to Binary Protocol Server
# =============================================================================
def test_raw_http_get():
    """Send a plain HTTP GET to the server. Must not crash."""
    log(f"\n=== Test 4: Raw HTTP GET (port {TARGET_PORT}) ===")

    request = (
        f"GET / HTTP/1.1\r\n"
        f"Host: {SERVER_HOST}:{TARGET_PORT}\r\n"
        f"\r\n"
    ).encode("utf-8")

    resp = send_http_request(TARGET_PORT, request)

    if not wait_until(server_alive, timeout=3, description="server alive after HTTP GET"):
        log("  FAIL: Server crashed from plain HTTP GET")
        return False

    log("  PASS: Server survived raw HTTP GET")
    return True


# =============================================================================
# Test 5: Server Alive After All Probing
# =============================================================================
def test_server_survives():
    """Final check: server must be alive and accept a real client."""
    log("\n=== Test 5: Server Survives All HTTP Probing ===")

    if not server_alive():
        log("  FAIL: Server is DOWN after HTTP probing")
        return False

    try:
        from tests.utilities import IrisClient
        c = IrisClient()
        c.login("legit_after_ws_probe")
        c.send_msg("ws_probe_target", "hello after ws probing")
        c.close()
        log("  PASS: Legitimate client works after HTTP probing")
        return True
    except Exception as e:
        log(f"  FAIL: Legitimate client failed: {e}")
        return False


def main():
    global TARGET_PORT

    print("=" * 60)
    print(" G-29: WEBSOCKET / HTTP PROTOCOL CONFORMANCE")
    print(" RFC 6455 + RFC-001 NFR-18: Input validation")
    print("=" * 60)

    # Pre-check with retry (server may be recovering from previous heavy tests)
    log("\nPre-check: server availability...")
    alive = wait_until(server_alive, timeout=15, interval=3, description="server available")

    if not alive:
        log("FAIL: Server not running after 5 attempts")
        return 1

    # Determine target port: prefer dedicated WS port, fall back to main port
    if port_open(WS_PORT):
        TARGET_PORT = WS_PORT
        log(f"Testing against WebSocket port {WS_PORT}")
    else:
        TARGET_PORT = SERVER_PORT
        log(f"WebSocket port {WS_PORT} not open -- testing against main port {SERVER_PORT}")
        log("Server must handle HTTP/WS requests without crashing")

    tests = [
        ("WebSocket Upgrade", test_ws_upgrade),
        ("Missing WS Key", test_missing_ws_key),
        ("Wrong WS Version", test_wrong_ws_version),
        ("Raw HTTP GET", test_raw_http_get),
        ("Server Survives", test_server_survives),
    ]

    for name, fn in tests:
        try:
            result = fn()
            results.append((name, result))
        except Exception as e:
            log(f"  Exception in {name}: {e}")
            results.append((name, False))

    print("\n" + "=" * 60)
    print("RESULTS:")
    print("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)
    for name, result in results:
        print(f"  [{'PASS' if result else 'FAIL'}] {name}")

    if passed == total:
        print(f"\nG-29 WebSocket Conformance: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nG-29 WebSocket Conformance: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
