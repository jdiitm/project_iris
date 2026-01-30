"""
Test Utilities Package

Shared utilities for Iris tests including:
- Client library (iris_client.py)
- Cluster control scripts
- Test isolation helpers (helpers.py)
"""

from .iris_client import IrisClient
from .helpers import unique_user, unique_suffix

__all__ = ["IrisClient", "unique_user", "unique_suffix"]
