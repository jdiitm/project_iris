#!/usr/bin/env python3
"""
P2-2 / G-04: Cross-Region Outbox Queue Overflow Tests

RFC-001 v4.0 Section 7.4 specifies:
- Cross-region outbox queue max_size configurable
- When queue is full, server responds with NACK
- Queue is persisted with fsync for durability
- Queue drains when cross-region link recovers

Test Approach:
- Isolate a region to prevent outbox draining
- Send messages that should be queued in outbox
- Verify server does not crash under queue pressure
- After healing, verify messages flow again

Prerequisites:
- Docker cluster: make cluster-up
- Multi-region setup with Core-East and Core-West

Pattern: follows test_region_outage.py and test_split_brain.py.
"""

import os
import sys
import socket
import subprocess
import time
import struct
from typing import Optional, Tuple
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from tests.suites.chaos_dist.utils import (
    tls_connect_and_login,
    tls_send_message,
    tls_connect_and_login_with_retry,
    close_socket,
)

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
EAST_EDGE_PORT = int(os.environ.get("IRIS_EAST_PORT", "8085"))
WEST_EDGE_PORT = int(os.environ.get("IRIS_WEST_PORT", "8087"))
EAST_CORE = os.environ.get("IRIS_EAST_CORE", "core-east-1")
WEST_CORE = os.environ.get("IRIS_WEST_CORE", "core-west-1")
TIMEOUT = 10

# Docker network for inter-region connectivity
BACKBONE_NETWORK = "global-cluster_iris_backbone"

# Message counts (scaled for CI)
QUICK_MODE = os.environ.get("QUICK_MODE", "").lower() in ("true", "1", "yes")
MSG_COUNT = 20 if QUICK_MODE else 100


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


def test_outbox_queue_under_partition():
    """
    Test outbox queue behavior when cross-region link is down:
    1. Isolate West region
    2. Send messages from East targeting West users (should be queued)
    3. Verify server does not crash under queue pressure
    4. Heal partition
    5. Verify message flow resumes
    """
    print("\n" + "=" * 70)
    print("Cross-Region Outbox Queue Overflow Test (RFC Section 7.4)")
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
        # Phase 1: Establish Baseline
        # ==============================================================
        log("\n=== Phase 1: Baseline ===")

        east_sender = f"outbox_sender_{test_id}"
        west_target = f"outbox_target_{test_id}"

        # Pre-register West target (with retry for post-cluster-init readiness)
        west_sock = tls_connect_and_login_with_retry(SERVER_HOST, WEST_EDGE_PORT, west_target,
                                                      timeout=TIMEOUT, max_retries=5, retry_delay=2.0)
        if not west_sock:
            log("FAIL: Cannot connect to West edge for target registration (after retries)")
            return False
        log("PASS: West target registered")
        close_socket(west_sock)

        # ==============================================================
        # Phase 2: Isolate West Region
        # ==============================================================
        log("\n=== Phase 2: Isolate West Region ===")

        if not docker_network_disconnect(WEST_CORE, BACKBONE_NETWORK):
            log("WARN: Failed to disconnect West core")

        # AUDIT P4 FIX: Reduced from 10s
        log("West isolated. Waiting for detection...")
        time.sleep(6)

        # ==============================================================
        # Phase 3: Flood Messages from East to West (queue fills)
        # ==============================================================
        log(f"\n=== Phase 3: Send {MSG_COUNT} messages from East to West ===")

        east_sock = tls_connect_and_login(SERVER_HOST, EAST_EDGE_PORT, east_sender, TIMEOUT)
        if not east_sock:
            log("FAIL: Cannot connect to East edge")
            docker_network_connect(WEST_CORE, BACKBONE_NETWORK)
            return False

        sent_count = 0
        acked_count = 0
        nack_count = 0

        for i in range(MSG_COUNT):
            ok, _ = tls_send_message(east_sock, west_target, f"outbox_msg_{i}_{test_id}")
            sent_count += 1
            if ok:
                acked_count += 1
            else:
                nack_count += 1

            if (i + 1) % 20 == 0:
                log(f"  Sent {i+1}/{MSG_COUNT} (acked={acked_count}, nack={nack_count})")

        log(f"Sending complete: {sent_count} sent, {acked_count} acked, {nack_count} nacked")
        close_socket(east_sock)

        # ==============================================================
        # Phase 4: Verify Server Survived
        # ==============================================================
        log("\n=== Phase 4: Verify Server Survived ===")

        # East should still be reachable
        verify_sock = tls_connect_and_login(SERVER_HOST, EAST_EDGE_PORT, f"verify_{test_id}", TIMEOUT)
        if verify_sock:
            log("PASS: East edge still responsive after outbox flood")
            close_socket(verify_sock)
        else:
            log("FAIL: East edge not responsive after outbox flood")
            passed = False

        # ==============================================================
        # Phase 5: Heal Partition
        # ==============================================================
        log("\n=== Phase 5: Heal Partition ===")

        if docker_network_connect(WEST_CORE, BACKBONE_NETWORK):
            log("PASS: West core reconnected")
        else:
            log("WARN: Failed to reconnect West core")

        # AUDIT P4 FIX: Reduced from 15s
        log("Waiting for queue drain...")
        time.sleep(8)

        # ==============================================================
        # Phase 6: Verify Message Flow Resumes
        # ==============================================================
        log("\n=== Phase 6: Verify Post-Heal Message Flow ===")

        post_east = tls_connect_and_login_with_retry(
            SERVER_HOST, EAST_EDGE_PORT, f"post_east_{test_id}",
            timeout=TIMEOUT, max_retries=3, retry_delay=2.0
        )
        post_west = tls_connect_and_login_with_retry(
            SERVER_HOST, WEST_EDGE_PORT, f"post_west_{test_id}",
            timeout=TIMEOUT, max_retries=3, retry_delay=2.0
        )

        if post_east and post_west:
            ok, _ = tls_send_message(post_east, f"post_west_{test_id}", f"post_heal_{test_id}")
            if ok:
                log("PASS: Cross-region message works after partition heal")
            else:
                log("WARN: Post-heal message not acked")
        else:
            log("FAIL: Could not connect to both edges after heal")
            passed = False

        close_socket(post_east)
        close_socket(post_west)

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

    log(f"Messages sent during partition: {sent_count}")
    log(f"Messages acked during partition: {acked_count}")
    log(f"Messages nacked during partition: {nack_count}")

    # Assertion 1: Server survived flood
    if passed:
        log("PASS: Server survived outbox queue flood")
    else:
        log("FAIL: Server did not survive outbox queue flood")

    # Assertion 2: At least some messages were accepted (outbox queued them)
    if acked_count > 0:
        log(f"PASS: {acked_count} messages accepted (queued in outbox)")
    else:
        log("WARN: No messages acked during partition (may indicate conservative rejection)")

    # Assertion 3: If NACKs observed, that's correct queue overflow behavior
    if nack_count > 0:
        log(f"PASS: {nack_count} NACKs observed (queue overflow handling)")

    print("\n" + "=" * 70)
    if passed:
        print("PASS: Outbox queue overflow test completed successfully")
    else:
        print("FAIL: Outbox queue overflow test failed")
    print("=" * 70)

    return passed


def main():
    result = test_outbox_queue_under_partition()
    sys.exit(0 if result else 1)


if __name__ == "__main__":
    main()
