"""
Chaos Dist Test Utilities

Provides TLS-aware connection functions for chaos_dist tests.
All tests in this suite connect to the Docker cluster which has TLS enabled.
"""

import socket
import ssl
import struct
from pathlib import Path
from typing import Optional, Tuple

# Project root for locating certificates
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"

# Default timeout for connections
DEFAULT_TIMEOUT = 10


def get_tls_context() -> ssl.SSLContext:
    """
    Create an SSL context for TLS connections to the Docker cluster.
    
    Returns:
        ssl.SSLContext configured for TLS with CA verification
    """
    context = ssl.create_default_context()
    
    if CA_CERT.exists():
        context.load_verify_locations(str(CA_CERT))
    else:
        # Fallback: don't verify if CA cert not found (for local testing)
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
    
    return context


def create_tls_socket(host: str, port: int, timeout: int = DEFAULT_TIMEOUT) -> ssl.SSLSocket:
    """
    Create a TLS-wrapped socket connected to an edge node.
    
    Args:
        host: Hostname or IP address (usually 'localhost')
        port: Port number (8085-8094 for edge nodes)
        timeout: Connection timeout in seconds
    
    Returns:
        ssl.SSLSocket ready for communication
    
    Raises:
        ssl.SSLError: If TLS handshake fails
        socket.error: If connection fails
    """
    context = get_tls_context()
    
    raw_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw_sock.settimeout(timeout)
    
    tls_sock = context.wrap_socket(raw_sock, server_hostname=host)
    tls_sock.connect((host, port))
    
    return tls_sock


def tls_connect_and_login(host: str, port: int, username: str, 
                          timeout: int = DEFAULT_TIMEOUT) -> Optional[ssl.SSLSocket]:
    """
    Connect via TLS and perform login handshake.
    
    This is a convenience function that combines TLS connection with
    the login protocol (opcode 0x01 + username).
    
    Args:
        host: Hostname or IP address
        port: Port number
        username: Username for login
        timeout: Connection timeout in seconds
    
    Returns:
        ssl.SSLSocket if login successful, None otherwise
    """
    try:
        sock = create_tls_socket(host, port, timeout)
        
        # Send login packet: opcode 0x01 + username
        packet = bytes([0x01]) + username.encode('utf-8')
        sock.sendall(packet)
        
        # Wait for response (with timeout)
        sock.settimeout(timeout)
        response = sock.recv(1024)
        
        if b"LOGIN_OK" in response:
            # Small delay to ensure server-side registration completes
            # This prevents race conditions where messages are sent before
            # the recipient is fully registered in the presence table
            import time
            time.sleep(0.05)
            return sock
        else:
            # Login failed - log the response for debugging
            print(f"  Login failed for {username}: {response!r}")
            sock.close()
            return None
            
    except ssl.SSLError as e:
        print(f"  TLS error for {username}: {e}")
        return None
    except socket.timeout:
        print(f"  Connection timeout for {username} to {host}:{port}")
        return None
    except socket.error as e:
        print(f"  Socket error for {username}: {e}")
        return None
    except Exception as e:
        print(f"  Unexpected error connecting {username}: {e}")
        return None


# Sequence counter for RFC-compliant messaging
_tls_seq_counter = [0]

def tls_send_message(sock: ssl.SSLSocket, target: str, message: str) -> Tuple[bool, float]:
    """
    Send a message over a TLS socket.
    
    RFC-001-AMENDMENT-001 v1.0 COMPLIANT: Uses opcode 0x07 (sequenced message)
    instead of deprecated opcode 0x02 (plaintext) which is now rejected.
    
    Args:
        sock: TLS socket from tls_connect_and_login()
        target: Target username
        message: Message content
    
    Returns:
        Tuple of (success: bool, latency_ms: float)
    """
    import time
    start = time.time()
    
    try:
        target_bytes = target.encode('utf-8')
        msg_bytes = message.encode('utf-8')
        
        # Increment sequence counter
        _tls_seq_counter[0] += 1
        seq_no = _tls_seq_counter[0]
        
        # Message packet: 0x07 | target_len(2) | target | seq_no(8) | msg_len(2) | msg
        packet = (
            bytes([0x07]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>Q', seq_no) +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        sock.sendall(packet)
        latency = (time.time() - start) * 1000
        return True, latency
        
    except Exception as e:
        latency = (time.time() - start) * 1000
        return False, latency


def close_socket(sock: Optional[ssl.SSLSocket]) -> None:
    """Safely close a socket."""
    if sock:
        try:
            sock.close()
        except Exception:
            pass


def tls_connect_and_login_with_retry(host: str, port: int, username: str,
                                      timeout: int = DEFAULT_TIMEOUT,
                                      max_retries: int = 3,
                                      retry_delay: float = 2.0) -> Optional[ssl.SSLSocket]:
    """
    Connect via TLS and perform login handshake with retry logic.
    
    After network partition heals, edge nodes may take time to re-establish
    connectivity. This function retries login failures to handle transient
    infrastructure issues.
    
    Args:
        host: Hostname or IP address
        port: Port number
        username: Username for login
        timeout: Connection timeout in seconds per attempt
        max_retries: Maximum number of retry attempts (default 3)
        retry_delay: Delay in seconds between retries (default 2.0)
    
    Returns:
        ssl.SSLSocket if login successful, None if all retries exhausted
    """
    import time
    
    last_error = None
    for attempt in range(max_retries + 1):
        if attempt > 0:
            print(f"  Retry {attempt}/{max_retries} for {username}...")
            time.sleep(retry_delay)
        
        sock = tls_connect_and_login(host, port, username, timeout)
        if sock:
            return sock
    
    print(f"  All {max_retries + 1} login attempts failed for {username}")
    return None
