"""
Polling-based Wait Utilities (TEST_DETERMINISM.md compliance)

Replaces time.sleep() with deterministic polling in tests.

Usage:
    from tests.framework.wait import wait_for, wait_for_count, poll_until

    # Wait for a condition
    wait_for(lambda: tracker.received >= 10, timeout=30, description="message delivery")

    # Wait for a count to reach target
    wait_for_count(lambda: len(results), target=100, timeout=60)

    # Poll with custom interval
    poll_until(check_server_ready, interval=0.5, timeout=30)

Benefits:
- Deterministic: Tests complete as fast as possible, not fixed delays
- Debuggable: Clear timeout messages indicate what failed
- Reliable: No flaky failures from arbitrary sleep times
"""

import time
from typing import Callable, Any, Optional, TypeVar, Union

T = TypeVar('T')


class WaitTimeout(Exception):
    """Raised when a wait condition times out."""
    pass


def wait_for(
    condition: Callable[[], bool],
    timeout: float = 30.0,
    interval: float = 0.1,
    description: str = "condition"
) -> bool:
    """
    Poll until condition() returns True or timeout.
    
    Args:
        condition: Callable that returns True when ready
        timeout: Maximum seconds to wait
        interval: Seconds between polls
        description: Description for error messages
        
    Returns:
        True if condition was met
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    deadline = time.monotonic() + timeout
    last_exception = None
    
    while time.monotonic() < deadline:
        try:
            if condition():
                return True
        except Exception as e:
            last_exception = e
        time.sleep(interval)
    
    msg = f"Timeout ({timeout}s) waiting for {description}"
    if last_exception:
        msg += f" (last error: {last_exception})"
    raise WaitTimeout(msg)


def wait_for_count(
    get_count: Callable[[], int],
    target: int,
    timeout: float = 30.0,
    interval: float = 0.1,
    description: str = "count"
) -> int:
    """
    Poll until get_count() >= target.
    
    Args:
        get_count: Callable that returns current count
        target: Target count to reach
        timeout: Maximum seconds to wait
        interval: Seconds between polls
        description: Description for error messages
        
    Returns:
        Final count (>= target)
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    deadline = time.monotonic() + timeout
    count = 0
    
    while time.monotonic() < deadline:
        count = get_count()
        if count >= target:
            return count
        time.sleep(interval)
    
    raise WaitTimeout(
        f"Timeout ({timeout}s) waiting for {description}: got {count}, expected {target}"
    )


def poll_until(
    check: Callable[[], Optional[T]],
    timeout: float = 30.0,
    interval: float = 0.1,
    description: str = "result"
) -> T:
    """
    Poll until check() returns a truthy value.
    
    Args:
        check: Callable that returns None/False until ready, then result
        timeout: Maximum seconds to wait
        interval: Seconds between polls
        description: Description for error messages
        
    Returns:
        The truthy result from check()
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    deadline = time.monotonic() + timeout
    
    while time.monotonic() < deadline:
        result = check()
        if result:
            return result
        time.sleep(interval)
    
    raise WaitTimeout(f"Timeout ({timeout}s) waiting for {description}")


def wait_for_server(
    host: str,
    port: int,
    timeout: float = 30.0,
    interval: float = 0.5
) -> bool:
    """
    Wait for a server to accept connections.
    
    Args:
        host: Server hostname
        port: Server port
        timeout: Maximum seconds to wait
        interval: Seconds between checks
        
    Returns:
        True when server is accepting connections
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    import socket
    
    def check_port():
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(1)
            result = sock.connect_ex((host, port))
            sock.close()
            return result == 0
        except Exception:
            return False
    
    return wait_for(
        check_port,
        timeout=timeout,
        interval=interval,
        description=f"server at {host}:{port}"
    )


def wait_for_file(
    path: str,
    timeout: float = 30.0,
    interval: float = 0.5
) -> bool:
    """
    Wait for a file to exist.
    
    Args:
        path: File path
        timeout: Maximum seconds to wait
        interval: Seconds between checks
        
    Returns:
        True when file exists
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    import os
    
    return wait_for(
        lambda: os.path.exists(path),
        timeout=timeout,
        interval=interval,
        description=f"file {path}"
    )


def wait_for_process(
    pid: int,
    timeout: float = 30.0,
    interval: float = 0.5,
    wait_for_exit: bool = True
) -> bool:
    """
    Wait for a process to start or exit.
    
    Args:
        pid: Process ID
        timeout: Maximum seconds to wait
        interval: Seconds between checks
        wait_for_exit: If True, wait for process to exit; otherwise wait for it to exist
        
    Returns:
        True when condition is met
        
    Raises:
        WaitTimeout: If timeout exceeded
    """
    import os
    
    def process_exists():
        try:
            os.kill(pid, 0)
            return True
        except ProcessLookupError:
            return False
        except PermissionError:
            return True  # Process exists but we can't signal it
    
    if wait_for_exit:
        return wait_for(
            lambda: not process_exists(),
            timeout=timeout,
            interval=interval,
            description=f"process {pid} to exit"
        )
    else:
        return wait_for(
            process_exists,
            timeout=timeout,
            interval=interval,
            description=f"process {pid} to start"
        )


class PollingContext:
    """
    Context manager for polling with automatic cleanup.
    
    Usage:
        with PollingContext(timeout=30) as ctx:
            while ctx.active:
                if check_condition():
                    break
                ctx.poll()
    """
    
    def __init__(self, timeout: float = 30.0, interval: float = 0.1):
        self.timeout = timeout
        self.interval = interval
        self.deadline = None
        self.active = False
    
    def __enter__(self):
        self.deadline = time.monotonic() + self.timeout
        self.active = True
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.active = False
        return False
    
    def poll(self):
        """Sleep for interval and check if still active."""
        if time.monotonic() >= self.deadline:
            self.active = False
            return False
        time.sleep(self.interval)
        return self.active
    
    def remaining(self) -> float:
        """Get remaining time until timeout."""
        return max(0, self.deadline - time.monotonic())


# Convenience aliases
wait = wait_for
poll = poll_until
