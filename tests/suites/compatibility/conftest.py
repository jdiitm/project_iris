"""
Compatibility test suite conftest: ensure server connectivity between tests.

Reuses the same server health-check pattern as integration/security suites.
"""
import os
import socket
import ssl
import time
import pytest
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"


def _try_connect(timeout=3.0):
    """Attempt a single TLS connection. Returns True on success."""
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
        tls.close()
        return True
    except Exception:
        return False


@pytest.fixture(autouse=True)
def wait_for_server_ready():
    """Before each compatibility test, verify the server is accepting connections."""
    if _try_connect(timeout=2.0):
        return

    for _ in range(15):
        time.sleep(2)
        if _try_connect(timeout=3.0):
            return
