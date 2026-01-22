"""
Project Iris - Test Configuration and Determinism Utilities

This module provides seeded randomness and deterministic ID generation
to ensure reproducible test execution across all environments.

Usage:
    from tests.conftest import get_seeded_random, get_test_id, deterministic_uuid

    # Get a seeded random instance
    rng = get_seeded_random()
    username = ''.join(rng.choices(string.ascii_lowercase, k=8))

    # Get a deterministic test ID
    msg_id = get_test_id("my_test")  # Returns "my_test_00001"

    # Get a deterministic UUID
    uid = deterministic_uuid()  # Returns "00000000-0000-4000-8000-000000000001"

Environment Variables:
    TEST_SEED: Master seed for all random operations (default: 42)
    TEST_RUN_ID: Unique run identifier (auto-generated from seed if not set)
"""

import os
import random
import string
import threading
from typing import Optional

# =============================================================================
# Configuration
# =============================================================================

# Master seed - can be overridden via environment variable
DEFAULT_SEED = 42
MASTER_SEED = int(os.environ.get("TEST_SEED", str(DEFAULT_SEED)))

# Run ID for this test execution
TEST_RUN_ID = os.environ.get("TEST_RUN_ID", f"run_{MASTER_SEED}")

# =============================================================================
# Thread-Safe Seeded Random
# =============================================================================

class SeededRandom:
    """
    Thread-safe wrapper around random.Random with a fixed seed.
    
    Each test gets deterministic random values based on the master seed.
    """
    
    def __init__(self, seed: int):
        self._seed = seed
        self._random = random.Random(seed)
        self._lock = threading.Lock()
    
    def __getattr__(self, name):
        """Delegate to the underlying Random instance with thread safety."""
        attr = getattr(self._random, name)
        if callable(attr):
            def thread_safe_call(*args, **kwargs):
                with self._lock:
                    return attr(*args, **kwargs)
            return thread_safe_call
        return attr
    
    def reset(self):
        """Reset the random state to initial seed."""
        with self._lock:
            self._random.seed(self._seed)
    
    @property
    def seed(self) -> int:
        """Return the seed used for this random instance."""
        return self._seed


# Global seeded random instance
_seeded_random: Optional[SeededRandom] = None
_seeded_random_lock = threading.Lock()


def get_seeded_random() -> SeededRandom:
    """
    Get the global seeded random instance.
    
    This returns a thread-safe random instance seeded with TEST_SEED.
    Use this instead of the stdlib random module for deterministic tests.
    
    Returns:
        SeededRandom: Thread-safe random instance
    
    Example:
        rng = get_seeded_random()
        value = rng.randint(1, 100)
        name = ''.join(rng.choices(string.ascii_lowercase, k=8))
    """
    global _seeded_random
    
    with _seeded_random_lock:
        if _seeded_random is None:
            _seeded_random = SeededRandom(MASTER_SEED)
        return _seeded_random


def reset_seeded_random():
    """
    Reset the global seeded random to its initial state.
    
    Call this at the start of each test suite to ensure deterministic ordering.
    """
    rng = get_seeded_random()
    rng.reset()


# =============================================================================
# Deterministic ID Generation
# =============================================================================

class IDGenerator:
    """
    Generates deterministic, sequential test IDs.
    
    IDs are of the form: "{prefix}_{counter:05d}"
    """
    
    def __init__(self):
        self._counters: dict = {}
        self._lock = threading.Lock()
    
    def get_id(self, prefix: str = "test") -> str:
        """
        Get the next ID for a given prefix.
        
        Args:
            prefix: Prefix for the ID (e.g., "user", "msg", "test")
        
        Returns:
            str: Deterministic ID like "user_00001"
        """
        with self._lock:
            if prefix not in self._counters:
                self._counters[prefix] = 0
            self._counters[prefix] += 1
            return f"{prefix}_{self._counters[prefix]:05d}"
    
    def reset(self):
        """Reset all counters to zero."""
        with self._lock:
            self._counters.clear()


# Global ID generator
_id_generator: Optional[IDGenerator] = None
_id_generator_lock = threading.Lock()


def get_test_id(prefix: str = "test") -> str:
    """
    Get a deterministic test ID.
    
    Args:
        prefix: Prefix for the ID (default: "test")
    
    Returns:
        str: Deterministic ID like "test_00001", "test_00002", etc.
    
    Example:
        user_id = get_test_id("user")     # "user_00001"
        msg_id = get_test_id("msg")       # "msg_00001"
        user_id2 = get_test_id("user")    # "user_00002"
    """
    global _id_generator
    
    with _id_generator_lock:
        if _id_generator is None:
            _id_generator = IDGenerator()
        return _id_generator.get_id(prefix)


def reset_test_ids():
    """Reset all ID counters to zero."""
    global _id_generator
    
    with _id_generator_lock:
        if _id_generator is not None:
            _id_generator.reset()


# =============================================================================
# Deterministic UUID Generation
# =============================================================================

class DeterministicUUID:
    """
    Generates deterministic UUIDs for tests that require UUID format.
    
    UUIDs follow the format: 00000000-0000-4000-8000-{counter:012d}
    This is a valid UUID v4 format but with predictable values.
    """
    
    def __init__(self, seed: int = 0):
        self._counter = seed
        self._lock = threading.Lock()
    
    def uuid4(self) -> str:
        """
        Generate a deterministic UUID in v4 format.
        
        Returns:
            str: UUID like "00000000-0000-4000-8000-000000000001"
        """
        with self._lock:
            self._counter += 1
            return f"00000000-0000-4000-8000-{self._counter:012d}"
    
    @property
    def hex(self) -> str:
        """Generate a deterministic UUID and return just the hex portion."""
        return self.uuid4().replace("-", "")
    
    def reset(self, seed: int = 0):
        """Reset counter to specified seed."""
        with self._lock:
            self._counter = seed


# Global UUID generator
_uuid_generator: Optional[DeterministicUUID] = None
_uuid_generator_lock = threading.Lock()


def deterministic_uuid() -> str:
    """
    Generate a deterministic UUID.
    
    Returns:
        str: Deterministic UUID in v4 format
    
    Example:
        uid1 = deterministic_uuid()  # "00000000-0000-4000-8000-000000000001"
        uid2 = deterministic_uuid()  # "00000000-0000-4000-8000-000000000002"
    """
    global _uuid_generator
    
    with _uuid_generator_lock:
        if _uuid_generator is None:
            _uuid_generator = DeterministicUUID(MASTER_SEED)
        return _uuid_generator.uuid4()


def deterministic_uuid_hex() -> str:
    """
    Generate a deterministic UUID hex string (no dashes).
    
    Returns:
        str: 32-character hex string
    """
    return deterministic_uuid().replace("-", "")


def reset_deterministic_uuid():
    """Reset UUID generator to initial state."""
    global _uuid_generator
    
    with _uuid_generator_lock:
        if _uuid_generator is not None:
            _uuid_generator.reset(MASTER_SEED)


# =============================================================================
# Deterministic Username/String Generation
# =============================================================================

def deterministic_username(prefix: str = "user") -> str:
    """
    Generate a deterministic username.
    
    Args:
        prefix: Prefix for the username (default: "user")
    
    Returns:
        str: Username like "user_00001"
    """
    return get_test_id(prefix)


def deterministic_string(length: int = 8, prefix: str = "") -> str:
    """
    Generate a deterministic random-looking string.
    
    Uses seeded random to generate consistent strings across runs.
    
    Args:
        length: Length of random portion
        prefix: Optional prefix
    
    Returns:
        str: Deterministic string
    """
    rng = get_seeded_random()
    chars = ''.join(rng.choices(string.ascii_lowercase, k=length))
    return f"{prefix}{chars}" if prefix else chars


# =============================================================================
# Test Suite Setup/Teardown Helpers
# =============================================================================

def reset_all_determinism():
    """
    Reset all determinism state for a fresh test run.
    
    Call this at the start of a test suite to ensure reproducibility.
    """
    reset_seeded_random()
    reset_test_ids()
    reset_deterministic_uuid()


def get_determinism_info() -> dict:
    """
    Get information about current determinism state.
    
    Useful for logging at the start of test runs.
    
    Returns:
        dict: Determinism configuration
    """
    return {
        "seed": MASTER_SEED,
        "run_id": TEST_RUN_ID,
        "default_seed": DEFAULT_SEED,
    }


# =============================================================================
# Module Initialization
# =============================================================================

# Print seed info when module is loaded (helps with reproduction)
if os.environ.get("DEBUG"):
    print(f"[conftest] Determinism initialized: seed={MASTER_SEED}, run_id={TEST_RUN_ID}")
