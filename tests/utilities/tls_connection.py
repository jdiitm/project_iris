"""
TLS Connection Utility for Chaos Tests

Provides TLS-aware connection functions for connecting to edge nodes
in the Docker cluster where TLS is enabled by default.
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


def get_ssl_context(verify: bool = True, client_cert: bool = False) -> ssl.SSLContext:
    """
    Create an SSL context for TLS connections.
    
    Args:
        verify: Whether to verify server certificate (default: True)
        client_cert: Whether to use client certificate for mTLS (default: False)
    
    Returns:
        ssl.SSLContext configured for TLS
    """
    context = ssl.create_default_context()
    
    if verify and CA_CERT.exists():
        context.load_verify_locations(str(CA_CERT))
    else:
        # Don't verify for testing (not recommended for production)
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
    
    if client_cert and CLIENT_CERT.exists() and CLIENT_KEY.exists():
        context.load_cert_chain(str(CLIENT_CERT), str(CLIENT_KEY))
    
    return context


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
