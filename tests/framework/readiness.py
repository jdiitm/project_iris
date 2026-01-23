"""
Readiness Polling Utilities

Replaces time.sleep() with explicit readiness checks per TEST_CONTRACT.md.
These utilities poll for conditions rather than waiting blindly.
"""

import socket
import time
import subprocess
from typing import Callable, Optional, Any


class ReadinessTimeout(Exception):
    """Raised when readiness check times out."""
    pass


def wait_for_condition(
    condition: Callable[[], bool],
    timeout: float = 30.0,
    poll_interval: float = 0.5,
    description: str = "condition"
) -> bool:
    """
    Poll for a condition to become true.
    
    Args:
        condition: Callable that returns True when ready
        timeout: Maximum time to wait in seconds
        poll_interval: Time between polls in seconds
        description: Human-readable description for error messages
        
    Returns:
        True if condition met within timeout
        
    Raises:
        ReadinessTimeout: If timeout exceeded
    """
    start = time.time()
    while time.time() - start < timeout:
        try:
            if condition():
                return True
        except Exception:
            pass  # Condition check failed, retry
        time.sleep(poll_interval)
    
    raise ReadinessTimeout(f"Timeout waiting for {description} after {timeout}s")


def wait_for_port(
    host: str,
    port: int,
    timeout: float = 30.0,
    poll_interval: float = 0.5
) -> bool:
    """
    Wait for a TCP port to accept connections.
    
    Args:
        host: Hostname to connect to
        port: Port number
        timeout: Maximum time to wait
        poll_interval: Time between connection attempts
        
    Returns:
        True if port is open
        
    Raises:
        ReadinessTimeout: If timeout exceeded
    """
    def check_port():
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(1.0)
        try:
            result = sock.connect_ex((host, port))
            return result == 0
        finally:
            sock.close()
    
    return wait_for_condition(
        check_port,
        timeout=timeout,
        poll_interval=poll_interval,
        description=f"port {host}:{port}"
    )


def wait_for_http(
    url: str,
    timeout: float = 30.0,
    poll_interval: float = 1.0,
    expected_status: int = 200
) -> bool:
    """
    Wait for an HTTP endpoint to return expected status.
    
    Args:
        url: URL to check
        timeout: Maximum time to wait
        poll_interval: Time between requests
        expected_status: Expected HTTP status code
        
    Returns:
        True if endpoint responds with expected status
    """
    import urllib.request
    import urllib.error
    
    def check_http():
        try:
            req = urllib.request.urlopen(url, timeout=2)
            return req.getcode() == expected_status
        except (urllib.error.URLError, urllib.error.HTTPError):
            return False
    
    return wait_for_condition(
        check_http,
        timeout=timeout,
        poll_interval=poll_interval,
        description=f"HTTP {url}"
    )


def wait_for_erlang_node(
    node_name: str,
    cookie: str = "iris_secret",
    timeout: float = 30.0,
    poll_interval: float = 1.0
) -> bool:
    """
    Wait for an Erlang node to be pingable.
    
    Args:
        node_name: Full Erlang node name (e.g., 'iris_core@localhost')
        cookie: Erlang cookie
        timeout: Maximum time to wait
        poll_interval: Time between pings
        
    Returns:
        True if node responds to ping
    """
    def check_node():
        result = subprocess.run(
            ["erl", "-noshell", "-sname", f"probe_{int(time.time()*1000)}",
             "-setcookie", cookie, "-eval",
             f"case net_adm:ping('{node_name}') of pong -> halt(0); pang -> halt(1) end."],
            capture_output=True,
            timeout=5
        )
        return result.returncode == 0
    
    return wait_for_condition(
        check_node,
        timeout=timeout,
        poll_interval=poll_interval,
        description=f"Erlang node {node_name}"
    )


def wait_for_docker_container(
    container_name: str,
    timeout: float = 60.0,
    poll_interval: float = 2.0,
    require_healthy: bool = True
) -> bool:
    """
    Wait for a Docker container to be running (and optionally healthy).
    
    Args:
        container_name: Name of the container
        timeout: Maximum time to wait
        poll_interval: Time between checks
        require_healthy: If True, wait for 'healthy' health status
        
    Returns:
        True if container is running/healthy
    """
    def check_container():
        if require_healthy:
            result = subprocess.run(
                ["docker", "inspect", "--format", "{{.State.Health.Status}}", container_name],
                capture_output=True,
                text=True
            )
            return result.returncode == 0 and "healthy" in result.stdout
        else:
            result = subprocess.run(
                ["docker", "inspect", "--format", "{{.State.Running}}", container_name],
                capture_output=True,
                text=True
            )
            return result.returncode == 0 and "true" in result.stdout
    
    return wait_for_condition(
        check_container,
        timeout=timeout,
        poll_interval=poll_interval,
        description=f"Docker container {container_name}"
    )


def wait_for_mnesia_ready(
    container: str,
    node_name: str,
    cookie: str = "iris_secret",
    timeout: float = 60.0,
    poll_interval: float = 2.0
) -> bool:
    """
    Wait for Mnesia to be running on a node inside a Docker container.
    
    Args:
        container: Docker container name
        node_name: Erlang node name
        cookie: Erlang cookie
        timeout: Maximum time to wait
        poll_interval: Time between checks
        
    Returns:
        True if Mnesia is running
    """
    def check_mnesia():
        result = subprocess.run(
            ["docker", "exec", container, "erl", "-noshell",
             "-sname", f"probe_{int(time.time()*1000)}", "-setcookie", cookie, "-eval",
             f"case rpc:call('{node_name}', mnesia, system_info, [is_running], 5000) of "
             f"yes -> halt(0); _ -> halt(1) end."],
            capture_output=True,
            timeout=10
        )
        return result.returncode == 0
    
    return wait_for_condition(
        check_mnesia,
        timeout=timeout,
        poll_interval=poll_interval,
        description=f"Mnesia on {node_name}"
    )


def wait_for_cluster_ready(
    containers: list,
    primary_node: str,
    cookie: str = "iris_secret",
    timeout: float = 120.0
) -> bool:
    """
    Wait for a multi-node cluster to be fully formed.
    
    Args:
        containers: List of container names
        primary_node: Primary node name to check cluster state from
        cookie: Erlang cookie
        timeout: Maximum time to wait
        
    Returns:
        True if cluster is formed
    """
    start = time.time()
    
    # First, wait for all containers to have Mnesia running
    for container in containers:
        remaining = timeout - (time.time() - start)
        if remaining <= 0:
            raise ReadinessTimeout(f"Timeout waiting for cluster (stuck on {container})")
        
        try:
            wait_for_docker_container(container, timeout=remaining, require_healthy=True)
        except ReadinessTimeout:
            # Container might not have health check, try basic running check
            wait_for_docker_container(container, timeout=5, require_healthy=False)
    
    return True


# Convenience function for common test patterns
def poll_until(
    check_fn: Callable[[], Any],
    expected: Any = True,
    timeout: float = 30.0,
    poll_interval: float = 0.5,
    description: str = "expected value"
) -> Any:
    """
    Poll until a function returns an expected value.
    
    Args:
        check_fn: Function to call
        expected: Expected return value (or callable to check value)
        timeout: Maximum time to wait
        poll_interval: Time between calls
        description: Description for timeout message
        
    Returns:
        The actual value returned by check_fn
        
    Raises:
        ReadinessTimeout: If timeout exceeded
    """
    start = time.time()
    last_value = None
    
    while time.time() - start < timeout:
        try:
            value = check_fn()
            last_value = value
            
            if callable(expected):
                if expected(value):
                    return value
            elif value == expected:
                return value
        except Exception:
            pass
        
        time.sleep(poll_interval)
    
    raise ReadinessTimeout(
        f"Timeout waiting for {description}: last value was {last_value}, expected {expected}"
    )
