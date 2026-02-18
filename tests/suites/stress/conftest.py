"""
Stress test suite conftest: ensure server connectivity between test modules.

Stress tests (reconnect storms, fanouts, etc.) can temporarily exhaust
server connection pools. This conftest ensures the server accepts connections
before each test, preventing cascading SSL EOF failures.
"""
import os
import sys
import socket
import ssl
import time
import pytest
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from tests.utilities.tls_connection import get_unverified_ssl_context
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"


def _try_connect(timeout=3.0):
    """Attempt a single TLS connection. Returns True on success."""
    try:
        ctx = get_unverified_ssl_context()  # Unverified: testing rejection/attack scenario
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        tls = ctx.wrap_socket(sock, server_hostname=SERVER_HOST)
        tls.connect((SERVER_HOST, SERVER_PORT))
        tls.close()
        return True
    except Exception:
        return False


@pytest.fixture(autouse=True)
def wait_for_server_ready():
    """Before each stress test, verify the server is accepting connections.
    If not, poll for up to 30 seconds for recovery."""
    if _try_connect(timeout=2.0):
        return  # Fast path: server ready

    # Slow path: wait for server recovery
    for _ in range(15):
        time.sleep(2)
        if _try_connect(timeout=3.0):
            return
