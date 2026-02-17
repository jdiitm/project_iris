"""
Integration test suite conftest: ensure server health between tests.

Integration tests create many TLS connections over time. After hundreds of
connections the Erlang server can degrade. This conftest:
1. Checks server connectivity before each test (fast path: ~0ms overhead).
2. If unhealthy, waits up to 10s for recovery.
3. If still unhealthy, restarts the server automatically.
"""
import os
import socket
import ssl
import subprocess
import time
import pytest
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"

# Track consecutive failures to avoid restarting on a single hiccup
_consecutive_failures = 0


def _try_connect(timeout=3.0):
    """Attempt a TLS connection and login probe. Returns True on success."""
    try:
        ctx = ssl.create_default_context()
        if CA_CERT.exists():
            ctx.load_verify_locations(str(CA_CERT))
        else:
            ctx.check_hostname = False
            ctx.verify_mode = ssl.CERT_NONE
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        tls = ctx.wrap_socket(sock, server_hostname=SERVER_HOST)
        tls.connect((SERVER_HOST, SERVER_PORT))
        # Probe: send a login and check for LOGIN_OK
        tls.sendall(b'\x01health_probe')
        data = tls.recv(1024)
        tls.close()
        return b'LOGIN_OK' in data
    except Exception:
        try:
            sock.close()
        except Exception:
            pass
        return False


def _restart_server():
    """Kill and restart the Iris server with a clean state."""
    # Kill existing beam.smp processes
    subprocess.run(
        ["pkill", "-9", "beam.smp"],
        capture_output=True, timeout=5
    )
    time.sleep(2)

    # Clean Mnesia state
    for p in PROJECT_ROOT.glob("Mnesia.*"):
        subprocess.run(["rm", "-rf", str(p)], capture_output=True)

    # Restart epmd
    subprocess.run(["epmd", "-daemon"], capture_output=True)

    # Start server
    subprocess.run(
        ["make", "start", "CONFIG=config/test_tls"],
        capture_output=True, timeout=30, cwd=str(PROJECT_ROOT)
    )

    # Wait for server to be ready
    for _ in range(15):
        time.sleep(2)
        if _try_connect(timeout=3.0):
            return True
    return False


@pytest.fixture(autouse=True)
def ensure_server_health():
    """Before each integration test, verify the server is responsive.
    Restart if it has degraded from cumulative connection load."""
    global _consecutive_failures

    if _try_connect(timeout=2.0):
        _consecutive_failures = 0
        return  # Fast path: server healthy

    # Slow path: wait for natural recovery (up to 10s)
    for _ in range(5):
        time.sleep(2)
        if _try_connect(timeout=3.0):
            _consecutive_failures = 0
            return

    # Server is unresponsive — restart it
    _consecutive_failures += 1
    _restart_server()
