"""
Test Helpers - Shared utilities for test isolation and determinism.

This module provides helpers to ensure tests are isolated and deterministic,
preventing flakiness caused by state pollution between test runs.
"""

import time
import uuid


def unique_user(prefix: str) -> str:
    """Generate unique username for test isolation.
    
    Combines millisecond timestamp + random UUID suffix to guarantee uniqueness
    even under rapid test execution. This prevents race conditions where old
    connection terminate() calls delete new connection's ETS entries.
    
    Args:
        prefix: Human-readable prefix for the username (e.g., "alice", "sender")
        
    Returns:
        Unique username like "alice_1706547123456_a1b2c3"
        
    Example:
        >>> sender = unique_user("sender")
        >>> receiver = unique_user("receiver")
        >>> client.login(sender)
    """
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def unique_suffix() -> str:
    """Generate a unique suffix for test isolation.
    
    Useful when you need to append uniqueness to multiple related names.
    
    Returns:
        Unique suffix like "1706547123456_a1b2c3"
    """
    return f"{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"
