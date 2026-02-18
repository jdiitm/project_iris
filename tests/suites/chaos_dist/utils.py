"""
Chaos Dist Test Utilities

Provides TLS-aware connection functions and Docker cluster management
primitives for chaos_dist tests.
All tests in this suite connect to the Docker cluster which has TLS enabled.
"""

import sys
import socket
import ssl
import struct
import subprocess
import time
from pathlib import Path
from typing import Optional, Tuple, List

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from tests.utilities.tls_connection import get_verified_ssl_context
CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"

# Default timeout for connections
DEFAULT_TIMEOUT = 10


_tls_context_cache: Optional[ssl.SSLContext] = None

def get_tls_context() -> ssl.SSLContext:
    """
    Get a cached SSL context for TLS connections.
    
    The context is created once and reused for all connections. This avoids
    the overhead of loading system CAs + test CA for every connection, which
    is critical for stress tests creating thousands of connections.
    
    Returns:
        ssl.SSLContext configured for TLS with CA verification
    """
    global _tls_context_cache
    if _tls_context_cache is not None:
        return _tls_context_cache

    context = get_verified_ssl_context()

    _tls_context_cache = context
    return context


def wait_for_edge_tls(host: str = "localhost", ports: List[int] = None,
                      max_wait: int = 30) -> bool:
    """
    Wait until at least one edge port accepts a TLS connection.

    Call this at the start of a test to avoid 0-second failures when edge
    nodes are still booting after cluster init.

    Args:
        host: Hostname (usually 'localhost')
        ports: List of edge ports to try (default: 8085, 8087, 8089)
        max_wait: Maximum seconds to wait

    Returns:
        True if at least one edge responded, False on timeout.
    """
    if ports is None:
        ports = [8085, 8087, 8089]

    deadline = time.time() + max_wait
    while time.time() < deadline:
        for port in ports:
            try:
                sock = create_tls_socket(host, port, timeout=3)
                sock.close()
                return True
            except Exception:
                pass
        time.sleep(1)
    return False


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

    except Exception:
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


# =============================================================================
# Docker Cluster Management Primitives
# =============================================================================
# Used by tests that manipulate the Docker cluster directly (partition, heal,
# run Erlang on nodes). These wrap Docker CLI commands for portability.

# Core containers in the Docker cluster (from docker-compose.yml)
CORE_CONTAINERS = [
    "core-east-1", "core-east-2",
    "core-west-1", "core-west-2",
    "core-eu-1", "core-eu-2",
]

# Docker network used for inter-region backbone connectivity
BACKBONE_NETWORK = "global-cluster_iris_backbone"


def get_cluster_nodes() -> List[str]:
    """
    List running core containers in the Docker cluster.

    Returns:
        List of container names that are currently running.
    """
    running = []
    for container in CORE_CONTAINERS:
        try:
            result = subprocess.run(
                ["docker", "inspect", "--format", "{{.State.Running}}", container],
                capture_output=True, text=True, timeout=5
            )
            if "true" in result.stdout.lower():
                running.append(container)
        except Exception:
            pass
    return running


def run_on_node(container: str, erlang_expr: str, timeout: int = 15) -> str:
    """
    Execute an Erlang expression on a Docker container and return the output.

    Spawns a short-lived hidden Erlang node inside the container that evaluates
    the expression via ``io:format`` and exits.

    Args:
        container: Docker container name (e.g. "core-east-1")
        erlang_expr: Erlang expression to evaluate (must NOT end with a period)
        timeout: Command timeout in seconds

    Returns:
        Stdout from the Erlang evaluation (stripped).
    """
    # Derive the Erlang sname from the container name (e.g. core-east-1 -> core_east_1@coreeast1)
    # We run a *new* hidden node and RPC into the target.
    target_sname = _container_to_sname(container)

    eval_code = (
        f"Res = rpc:call('{target_sname}', erlang, apply, "
        f"[fun() -> {erlang_expr} end, []]), "
        f"io:format(\"~p\", [Res]), init:stop()."
    )

    cmd = [
        "docker", "exec", container,
        "erl", "-noshell", "-hidden",
        "-sname", f"tmp_rpc_{int(time.time() * 1000) % 100000}",
        "-setcookie", "iris_secret",
        "-eval", eval_code,
    ]

    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout
        )
        return result.stdout.strip()
    except subprocess.TimeoutExpired:
        return "<timeout>"
    except Exception as e:
        return f"<error: {e}>"


def partition_nodes(node_a: str, node_b: str) -> bool:
    """
    Create a network partition between two Docker containers by disconnecting
    ``node_b`` from the backbone network.

    Args:
        node_a: Container that stays connected (unused, kept for API symmetry)
        node_b: Container to disconnect from the backbone

    Returns:
        True if the disconnect command succeeded.
    """
    try:
        result = subprocess.run(
            ["docker", "network", "disconnect", BACKBONE_NETWORK, node_b],
            capture_output=True, text=True, timeout=10
        )
        return result.returncode == 0
    except Exception:
        return False


def heal_partition(node_a: str, node_b: str) -> bool:
    """
    Heal a network partition by reconnecting ``node_b`` to the backbone.

    Args:
        node_a: Container that stayed connected (unused, kept for API symmetry)
        node_b: Container to reconnect to the backbone

    Returns:
        True if the reconnect command succeeded.
    """
    try:
        result = subprocess.run(
            ["docker", "network", "connect", BACKBONE_NETWORK, node_b],
            capture_output=True, text=True, timeout=10
        )
        return result.returncode == 0
    except Exception:
        return False


def wait_for_condition(check_fn, timeout: float = 30.0, interval: float = 1.0,
                       description: str = "condition") -> bool:
    """
    Poll until check_fn() returns True or timeout.
    
    AUDIT PHASE 4 FIX: Replaces time.sleep(N) in chaos tests with
    deterministic polling per RFC Section 13.2.
    
    Args:
        check_fn: Callable returning True when condition is met
        timeout: Maximum seconds to wait
        interval: Seconds between polls
        description: Human-readable description for debugging
    
    Returns:
        True if condition met, False on timeout
    """
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            if check_fn():
                return True
        except Exception:
            pass
        time.sleep(interval)
    return False


def wait_for_container_running(container: str, timeout: float = 30.0) -> bool:
    """Poll until a Docker container is running."""
    def check():
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Running}}", container],
            capture_output=True, text=True, timeout=5
        )
        return "true" in result.stdout.lower()
    return wait_for_condition(check, timeout=timeout, interval=2.0,
                              description=f"container {container} running")


def wait_for_container_stopped(container: str, timeout: float = 30.0) -> bool:
    """Poll until a Docker container is stopped."""
    def check():
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Running}}", container],
            capture_output=True, text=True, timeout=5
        )
        return "false" in result.stdout.lower()
    return wait_for_condition(check, timeout=timeout, interval=1.0,
                              description=f"container {container} stopped")


def wait_for_cluster_ready(max_wait: int = 60) -> bool:
    """
    Poll until at least 2 core containers are running.

    Args:
        max_wait: Maximum seconds to wait.

    Returns:
        True if the cluster is ready within the timeout.
    """
    deadline = time.time() + max_wait
    while time.time() < deadline:
        nodes = get_cluster_nodes()
        if len(nodes) >= 2:
            return True
        time.sleep(2)
    return False


def reconnect_edge_to_core(edge_container: str = "edge-east-1",
                           core_node: str = "core_east_1@coreeast1",
                           cookie: str = "iris_secret") -> bool:
    """
    Reconnect an edge node to a core node after the core is killed/restarted.

    After a core container is killed and restarted, edge nodes lose their
    Erlang distribution connection. This pings the core from the edge to
    re-establish the connection (same mechanism as init_cluster.sh reconnect_edges).

    Args:
        edge_container: Docker container name of the edge node
        core_node: Erlang sname of the core node (e.g. core_east_1@coreeast1)
        cookie: Erlang distribution cookie

    Returns:
        True if the reconnect command ran (best-effort).
    """
    try:
        random_id = int(time.time() * 1000) % 100000
        subprocess.run(
            ["docker", "exec", edge_container, "sh", "-c",
             f"erl -noshell -sname reconn_{random_id} -setcookie {cookie} "
             f"-eval \"net_adm:ping('{core_node}'), halt(0).\""],
            capture_output=True, timeout=15
        )
        # Give the edge a moment to re-establish routing
        time.sleep(2)
        return True
    except Exception:
        return False


# Mapping of core containers to their primary edge containers and snames
CORE_TO_EDGE = {
    "core-east-1": ("edge-east-1", "core_east_1@coreeast1"),
    "core-east-2": ("edge-east-2", "core_east_2@coreeast2"),
    "core-west-1": ("edge-west-1", "core_west_1@corewest1"),
    "core-west-2": ("edge-west-2", "core_west_2@corewest2"),
    "core-eu-1":   ("edge-eu-1",   "core_eu_1@coreeu1"),
    "core-eu-2":   ("edge-eu-2",   "core_eu_2@coreeu2"),
}


def reconnect_edges_after_core_restart(core_container: str) -> None:
    """
    Reconnect all relevant edge nodes after a core container is restarted.

    Looks up the primary edge for the given core and reconnects it.
    Also reconnects edge-east-1 as a fallback (it's the most commonly
    used edge in tests on port 8085).

    Args:
        core_container: Docker container name of the restarted core
    """
    if core_container in CORE_TO_EDGE:
        edge, core_sname = CORE_TO_EDGE[core_container]
        reconnect_edge_to_core(edge, core_sname)

    # Always reconnect edge-east-1 (port 8085) since most tests target it
    if core_container != "core-east-1":
        reconnect_edge_to_core("edge-east-1", _container_to_sname(core_container))


def _container_to_sname(container: str) -> str:
    """
    Convert a Docker container name to the Erlang -sname used inside it.

    Mapping (from docker-compose.yml):
        core-east-1  -> core_east_1@coreeast1
        core-west-2  -> core_west_2@corewest2
        core-eu-1    -> core_eu_1@coreeu1
    """
    node_part = container.replace("-", "_")             # core_east_1
    host_part = container.replace("-", "")              # coreeast1
    return f"{node_part}@{host_part}"
