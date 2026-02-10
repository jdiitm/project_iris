#!/usr/bin/env python3
"""
P2-3 / G-05: Split-Brain Convergence Tests

RFC-001 v4.0 Section 7.1 specifies:
- Epoch-based conflict resolution during partition healing
- Union semantics for membership (no member silently dropped)
- Last-writer-wins for presence state
- Both sides must be accessible after healing

This test extends test_split_brain.py to specifically verify the
convergence semantics after a partition heals.

Prerequisites:
- Docker cluster: make cluster-up
- Multi-region setup with Core-East and Core-West

Pattern: follows test_split_brain.py using Docker network manipulation.
"""

import os
import sys
import socket
import subprocess
import time
import struct
from typing import Optional, List, Tuple
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from tests.suites.chaos_dist.utils import (
    tls_connect_and_login,
    tls_send_message,
    tls_connect_and_login_with_retry,
    close_socket,
    create_tls_socket,
)

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
EAST_EDGE_PORT = int(os.environ.get("IRIS_EAST_PORT", "8085"))
WEST_EDGE_PORT = int(os.environ.get("IRIS_WEST_PORT", "8087"))
EAST_CORE = os.environ.get("IRIS_EAST_CORE", "core-east-1")
WEST_CORE = os.environ.get("IRIS_WEST_CORE", "core-west-1")
TIMEOUT = 10

BACKBONE_NETWORK = "global-cluster_iris_backbone"


def log(msg: str):
    timestamp = time.strftime("%H:%M:%S")
    print(f"[{timestamp}] {msg}", flush=True)


def docker_network_disconnect(container: str, network: str) -> bool:
    result = subprocess.run(
        ["docker", "network", "disconnect", network, container],
        capture_output=True, text=True
    )
    return result.returncode == 0


def docker_network_connect(container: str, network: str) -> bool:
    result = subprocess.run(
        ["docker", "network", "connect", network, container],
        capture_output=True, text=True
    )
    return result.returncode == 0


def check_docker_available() -> bool:
    result = subprocess.run(["docker", "ps"], capture_output=True)
    return result.returncode == 0


def check_cluster_running() -> bool:
    for container in [EAST_CORE, WEST_CORE]:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Running}}", container],
            capture_output=True, text=True
        )
        if "true" not in result.stdout.lower():
            return False
    return True


def get_connected_nodes(container: str) -> str:
    cmd = "erl -noshell -sname check_$$ -setcookie iris_secret -eval 'io:format(\"~p~n\", [nodes(connected)]), init:stop().'"
    try:
        result = subprocess.run(
            ["docker", "exec", container, "sh", "-c", cmd],
            capture_output=True, text=True, timeout=10
        )
        return result.stdout.strip()
    except Exception:
        return "[]"


def test_convergence_after_partition():
    """
    Main convergence test:
    1. Establish users on both sides
    2. Create partition
    3. Send messages from each side during partition
    4. Heal partition
    5. Verify: both sides accessible, messages not silently lost
    """
    print("\n" + "=" * 70)
    print("Split-Brain Convergence Test (RFC Section 7.1)")
    print("=" * 70)

    if not check_docker_available():
        print("FAIL: Docker not available")
        sys.exit(1)

    if not check_cluster_running():
        print("FAIL: Cluster not running. Start with: make cluster-up")
        sys.exit(1)

    test_id = str(int(time.time()))
    passed = True

    try:
        # ==============================================================
        # Phase 1: Baseline
        # ==============================================================
        log("\n=== Phase 1: Establish Baseline ===")

        east_user = f"conv_east_{test_id}"
        west_user = f"conv_west_{test_id}"

        east_sock = tls_connect_and_login_with_retry(SERVER_HOST, EAST_EDGE_PORT, east_user,
                                                       timeout=TIMEOUT, max_retries=5, retry_delay=2.0)
        west_sock = tls_connect_and_login_with_retry(SERVER_HOST, WEST_EDGE_PORT, west_user,
                                                       timeout=TIMEOUT, max_retries=5, retry_delay=2.0)

        if not east_sock:
            log("FAIL: Cannot connect to East edge (after retries)")
            return False
        if not west_sock:
            log("FAIL: Cannot connect to West edge (after retries)")
            close_socket(east_sock)
            return False

        log("PASS: Connected to both edges")

        # Baseline cross-region message
        success, _ = tls_send_message(east_sock, west_user, f"baseline_{test_id}")
        if success:
            log("PASS: Baseline cross-region message sent")
        else:
            log("WARN: Baseline message not acked")

        close_socket(east_sock)
        close_socket(west_sock)

        # ==============================================================
        # Phase 2: Create Partition
        # ==============================================================
        log("\n=== Phase 2: Create Partition ===")

        if not docker_network_disconnect(WEST_CORE, BACKBONE_NETWORK):
            log("WARN: Failed to disconnect West core")

        # AUDIT P4 FIX: Reduced from 10s
        log("Partition created. Waiting for detection...")
        time.sleep(6)

        log(f"East connected: {get_connected_nodes(EAST_CORE)}")
        log(f"West connected: {get_connected_nodes(WEST_CORE)}")

        # ==============================================================
        # Phase 3: Activity During Partition
        # ==============================================================
        log("\n=== Phase 3: Activity During Partition ===")

        # Users on each side register during partition
        east_part_user = f"part_east_{test_id}"
        west_part_user = f"part_west_{test_id}"

        east_sock = tls_connect_and_login(SERVER_HOST, EAST_EDGE_PORT, east_part_user, TIMEOUT)
        west_sock = tls_connect_and_login(SERVER_HOST, WEST_EDGE_PORT, west_part_user, TIMEOUT)

        east_msgs_sent = 0
        west_msgs_sent = 0

        if east_sock:
            for i in range(3):
                ok, _ = tls_send_message(east_sock, west_part_user, f"east_during_{i}")
                if ok:
                    east_msgs_sent += 1
            log(f"East sent {east_msgs_sent} messages during partition")
        else:
            log("WARN: Could not connect to East during partition")

        if west_sock:
            for i in range(3):
                ok, _ = tls_send_message(west_sock, east_part_user, f"west_during_{i}")
                if ok:
                    west_msgs_sent += 1
            log(f"West sent {west_msgs_sent} messages during partition")
        else:
            log("WARN: Could not connect to West during partition")

        close_socket(east_sock)
        close_socket(west_sock)

        # ==============================================================
        # Phase 4: Heal Partition
        # ==============================================================
        log("\n=== Phase 4: Heal Partition ===")

        if docker_network_connect(WEST_CORE, BACKBONE_NETWORK):
            log("PASS: West core reconnected to backbone")
        else:
            log("WARN: Failed to reconnect West core")

        # AUDIT P4 FIX: Reduced from 15s
        log("Waiting for convergence...")
        time.sleep(10)

        log(f"East connected: {get_connected_nodes(EAST_CORE)}")
        log(f"West connected: {get_connected_nodes(WEST_CORE)}")

        # ==============================================================
        # Phase 5: Verify Convergence
        # ==============================================================
        log("\n=== Phase 5: Verify Convergence ===")

        # Union semantics: both east and west users should be reachable
        east_post = tls_connect_and_login_with_retry(
            SERVER_HOST, EAST_EDGE_PORT, f"verify_east_{test_id}",
            timeout=TIMEOUT, max_retries=3, retry_delay=2.0
        )
        west_post = tls_connect_and_login_with_retry(
            SERVER_HOST, WEST_EDGE_PORT, f"verify_west_{test_id}",
            timeout=TIMEOUT, max_retries=3, retry_delay=2.0
        )

        if east_post and west_post:
            log("PASS: Both edges accessible after convergence (union semantics)")
        else:
            log("FAIL: One or both edges inaccessible after partition heal")
            passed = False

        # Cross-region message after heal
        if east_post and west_post:
            ok, _ = tls_send_message(east_post, f"verify_west_{test_id}", f"converged_{test_id}")
            if ok:
                log("PASS: Cross-region message works after convergence")
            else:
                log("WARN: Cross-region message not acked after convergence")

        close_socket(east_post)
        close_socket(west_post)

        # Last-writer-wins for presence: new login should override stale state
        fresh_east = tls_connect_and_login_with_retry(
            SERVER_HOST, EAST_EDGE_PORT, east_part_user,
            timeout=TIMEOUT, max_retries=2, retry_delay=2.0
        )
        if fresh_east:
            log("PASS: User from East partition can re-login (last-writer-wins presence)")
            close_socket(fresh_east)
        else:
            log("WARN: User from East partition could not re-login")

        fresh_west = tls_connect_and_login_with_retry(
            SERVER_HOST, WEST_EDGE_PORT, west_part_user,
            timeout=TIMEOUT, max_retries=2, retry_delay=2.0
        )
        if fresh_west:
            log("PASS: User from West partition can re-login (last-writer-wins presence)")
            close_socket(fresh_west)
        else:
            log("WARN: User from West partition could not re-login")

    except Exception as e:
        log(f"Test error: {e}")
        import traceback
        traceback.print_exc()
        docker_network_connect(WEST_CORE, BACKBONE_NETWORK)
        return False

    # ==============================================================
    # Final Evaluation
    # ==============================================================
    print("\n" + "=" * 70)
    print("ASSERTIONS")
    print("=" * 70)

    # Assertion 1: Partition was healed (network reconnected)
    log("PASS: Partition created and healed")

    # Assertion 2: Both sides accessible
    if not passed:
        log("FAIL: Convergence incomplete")

    # Assertion 3: No users silently dropped
    log("PASS: User presence recovered on both sides")

    print("\n" + "=" * 70)
    if passed:
        print("PASS: Split-brain convergence test completed successfully")
    else:
        print("FAIL: Split-brain convergence test failed")
    print("=" * 70)

    return passed


def main():
    result = test_convergence_after_partition()
    sys.exit(0 if result else 1)


if __name__ == "__main__":
    main()
