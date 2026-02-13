"""
Test Helpers - Shared utilities for test isolation and determinism.

This module provides helpers to ensure tests are isolated and deterministic,
preventing flakiness caused by state pollution between test runs.

Includes:
- Unique username generation for test isolation
- Seeded random number generation for test determinism (RFC §13.2 TST-3)
- Polling helpers to replace time.sleep() (AUDIT P2-3)
"""

import time
import uuid
import random
from typing import Sequence, TypeVar, List

T = TypeVar('T')

# Global seeded RNG for deterministic tests (RFC §13.2 TST-3)
_test_rng = random.Random(42)  # Default seed


def seed_random(seed: int = 42) -> None:
    """Seed the test RNG for determinism.
    
    Call this at the start of tests that use random values to ensure
    reproducible test runs. Per RFC §13.2 TST-3, all tests MUST use
    seeded random for determinism.
    
    Args:
        seed: Integer seed value (default: 42)
        
    Example:
        >>> seed_random(12345)
        >>> value = test_randint(1, 100)  # Always same sequence with same seed
    """
    global _test_rng
    _test_rng = random.Random(seed)


def test_randint(a: int, b: int) -> int:
    """Generate deterministic random integer in [a, b].
    
    Use instead of random.randint() in tests for determinism.
    
    Args:
        a: Lower bound (inclusive)
        b: Upper bound (inclusive)
        
    Returns:
        Random integer between a and b
    """
    return _test_rng.randint(a, b)


def test_random() -> float:
    """Generate deterministic random float in [0.0, 1.0).
    
    Use instead of random.random() in tests for determinism.
    
    Returns:
        Random float between 0.0 and 1.0
    """
    return _test_rng.random()


def test_choice(seq: Sequence[T]) -> T:
    """Choose deterministic random element from sequence.
    
    Use instead of random.choice() in tests for determinism.
    
    Args:
        seq: Non-empty sequence to choose from
        
    Returns:
        Randomly selected element
    """
    return _test_rng.choice(seq)


def test_shuffle(lst: List[T]) -> None:
    """Shuffle list in place deterministically.
    
    Use instead of random.shuffle() in tests for determinism.
    
    Args:
        lst: List to shuffle in place
    """
    _test_rng.shuffle(lst)


def test_sample(population: Sequence[T], k: int) -> List[T]:
    """Return deterministic random sample from population.
    
    Use instead of random.sample() in tests for determinism.
    
    Args:
        population: Sequence to sample from
        k: Number of elements to sample
        
    Returns:
        List of k randomly selected elements
    """
    return _test_rng.sample(population, k)


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


# =============================================================================
# AUDIT P2-3: Polling helpers to replace time.sleep() calls
# =============================================================================
# Use these instead of time.sleep() to make tests deterministic and faster.
# Tests wait only as long as needed, with bounded total wait time.
# =============================================================================

from typing import Callable, Optional, Any


def wait_until(
    predicate: Callable[[], bool],
    timeout: float = 10.0,
    interval: float = 0.1,
    description: str = "condition",
) -> bool:
    """Poll until a predicate returns True or timeout is reached.
    
    Replacement for time.sleep() — waits only as long as needed.
    
    Args:
        predicate: Callable that returns True when condition is met.
        timeout: Maximum seconds to wait (default: 10).
        interval: Seconds between checks (default: 0.1).
        description: Human-readable description for error messages.
        
    Returns:
        True if predicate was satisfied, False if timeout was reached.
        
    Example:
        >>> # Instead of: time.sleep(2)
        >>> wait_until(lambda: client.is_connected(), timeout=5, description="client connect")
    """
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            if predicate():
                return True
        except Exception:
            pass  # Predicate may raise while condition is not yet met
        time.sleep(interval)
    return False


def wait_until_or_fail(
    predicate: Callable[[], bool],
    timeout: float = 10.0,
    interval: float = 0.1,
    description: str = "condition",
) -> None:
    """Poll until predicate returns True, or raise AssertionError.
    
    Like wait_until() but raises on timeout for use in test assertions.
    
    Args:
        predicate: Callable that returns True when condition is met.
        timeout: Maximum seconds to wait (default: 10).
        interval: Seconds between checks (default: 0.1).
        description: Human-readable description for error message.
        
    Raises:
        AssertionError: If timeout is reached without predicate returning True.
        
    Example:
        >>> wait_until_or_fail(
        ...     lambda: len(client.received_messages()) >= 3,
        ...     timeout=5,
        ...     description="receive 3 messages"
        ... )
    """
    if not wait_until(predicate, timeout, interval, description):
        raise AssertionError(
            f"Timed out after {timeout}s waiting for: {description}"
        )


def wait_for_value(
    supplier: Callable[[], Any],
    expected: Any,
    timeout: float = 10.0,
    interval: float = 0.1,
    description: str = "expected value",
) -> bool:
    """Poll until supplier() returns expected value.
    
    Args:
        supplier: Callable that returns a value to check.
        expected: The expected value.
        timeout: Maximum seconds to wait.
        interval: Seconds between checks.
        description: Human-readable description.
        
    Returns:
        True if expected value was returned, False on timeout.
        
    Example:
        >>> # Instead of: time.sleep(1); assert client.state() == "connected"
        >>> wait_for_value(client.state, "connected", timeout=5)
    """
    return wait_until(
        lambda: supplier() == expected,
        timeout=timeout,
        interval=interval,
        description=f"{description} == {expected}",
    )
