"""
TLS Connection Utility for Tests

Provides TLS-aware connection functions for connecting to edge nodes.
Test certificates are self-signed; the CA cert is loaded for verification.

IMPORTANT: Use get_verified_ssl_context() by default. Only use
get_unverified_ssl_context() in tests that explicitly test rejection
scenarios (e.g., wrong cert, expired cert, plaintext rejection).
"""

import socket
import ssl
import os
from pathlib import Path

# Project root for locating certificates
PROJECT_ROOT = Path(__file__).parent.parent.parent

# Default certificate paths
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"
CLIENT_CERT = PROJECT_ROOT / "certs" / "test-client.pem"
CLIENT_KEY = PROJECT_ROOT / "certs" / "test-client.key"

# Default timeout
DEFAULT_TIMEOUT = 10


def get_verified_ssl_context(client_cert: bool = False) -> ssl.SSLContext:
    """
    Create an SSL context that verifies the server certificate against the test CA.

    This is the DEFAULT for all tests. It ensures tests actually validate TLS.
    Hostname checking is disabled because tests connect to localhost with
    certs issued to edge-east-1 etc.

    Raises:
        FileNotFoundError: If certs/ca.pem is missing.
    """
    if not CA_CERT.exists():
        raise FileNotFoundError(
            f"CA certificate not found at {CA_CERT}. "
            "Run 'make certs' or 'cd certs && bash generate_certs.sh' to generate test certificates."
        )
    # Use bare SSLContext to avoid loading system CAs — self-signed test CA
    # must be the ONLY trusted root, otherwise OpenSSL rejects the chain.
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.load_verify_locations(str(CA_CERT))
    context.check_hostname = False

    if client_cert and CLIENT_CERT.exists() and CLIENT_KEY.exists():
        context.load_cert_chain(str(CLIENT_CERT), str(CLIENT_KEY))

    return context


def get_unverified_ssl_context() -> ssl.SSLContext:
    """
    Create an SSL context that does NOT verify the server certificate.

    ONLY use this in tests that explicitly test rejection scenarios:
    - Plaintext rejection tests
    - Wrong/expired certificate tests
    - Protocol fuzzing against TLS listener
    """
    context = ssl.create_default_context()
    context.check_hostname = False
    context.verify_mode = ssl.CERT_NONE
    return context


def get_ssl_context(verify: bool = True, client_cert: bool = False) -> ssl.SSLContext:
    """
    Create an SSL context for TLS connections.

    Prefer get_verified_ssl_context() or get_unverified_ssl_context() for clarity.
    This function is kept for backward compatibility.
    """
    if verify and CA_CERT.exists():
        return get_verified_ssl_context(client_cert=client_cert)
    else:
        ctx = get_unverified_ssl_context()
        if client_cert and CLIENT_CERT.exists() and CLIENT_KEY.exists():
            ctx.load_cert_chain(str(CLIENT_CERT), str(CLIENT_KEY))
        return ctx


def connect_tls(host: str, port: int, timeout: int = DEFAULT_TIMEOUT,
                verify: bool = True) -> ssl.SSLSocket:
    """
    Create a TLS connection to an edge node.
    
    Args:
        host: Hostname or IP address
        port: Port number
        timeout: Connection timeout in seconds
        verify: Whether to verify server certificate
    
    Returns:
        ssl.SSLSocket ready for communication
    
    Raises:
        ConnectionError: If connection fails
        ssl.SSLError: If TLS handshake fails
    """
    context = get_ssl_context(verify=verify)

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(timeout)

    tls_sock = context.wrap_socket(sock, server_hostname=host)
    tls_sock.connect((host, port))

    return tls_sock


def connect_auto(host: str, port: int, timeout: int = DEFAULT_TIMEOUT,
                 prefer_tls: bool = True) -> socket.socket:
    """
    Connect to an edge node, automatically using TLS if available.
    
    First tries TLS connection, falls back to plaintext if TLS fails.
    This allows tests to work with both TLS and non-TLS clusters.
    
    Args:
        host: Hostname or IP address
        port: Port number
        timeout: Connection timeout in seconds
        prefer_tls: If True, try TLS first (default: True)
    
    Returns:
        socket.socket (may be ssl.SSLSocket if TLS succeeded)
    """
    if prefer_tls:
        try:
            return connect_tls(host, port, timeout, verify=False)
        except (ssl.SSLError, ConnectionRefusedError, OSError):
            # TLS failed, try plaintext
            pass

    # Plaintext connection
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(timeout)
    sock.connect((host, port))
    return sock


def is_tls_enabled(host: str, port: int, timeout: int = 5) -> bool:
    """
    Check if a port is using TLS.
    
    Args:
        host: Hostname or IP address
        port: Port number
        timeout: Connection timeout in seconds
    
    Returns:
        True if TLS is enabled, False otherwise
    """
    try:
        sock = connect_tls(host, port, timeout, verify=False)
        sock.close()
        return True
    except (ssl.SSLError, ConnectionRefusedError, OSError):
        return False


# Environment variable to force TLS mode
def should_use_tls() -> bool:
    """Check if TLS should be used based on environment."""
    # If running against Docker cluster, TLS is typically enabled
    if os.environ.get('IRIS_DOCKER_CLUSTER', '').lower() in ('true', '1', 'yes'):
        return True
    # Check explicit TLS setting
    if os.environ.get('IRIS_TLS_ENABLED', '').lower() in ('true', '1', 'yes'):
        return True
    return False
