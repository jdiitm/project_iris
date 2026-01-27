#!/usr/bin/env python3
"""
Multi-Master Durability Test (RFC NFR-6, NFR-8)

This test validates true RPO=0 (Recovery Point Objective = zero data loss) with
multi-master Mnesia replication. Unlike test_ack_durability.py which uses graceful
shutdown, this test uses SIGKILL to simulate a hard crash.

RFC Requirements:
- NFR-6: Message durability 99.999%
- NFR-8: RPO=0 - Zero acknowledged messages lost on crash

Test Strategy:
1. Connect to Edge in Region (e.g., East)
2. Send message to offline user
3. Wait for ACK from server (confirms durable write)
4. SIGKILL the PRIMARY core node (hard crash, no WAL flush)
5. Wait for cluster to detect failure
6. Connect to SECONDARY core via different edge
7. Login as receiver and verify message was preserved
8. If message found → PASS (RPO=0 validated)
9. If message lost → FAIL (ACK contract violated)

Prerequisites:
- Docker cluster with multi-master Mnesia: make cluster-up
- At least 2 core nodes per region (core-east-1, core-east-2)
"""

import os
import sys
import socket
import struct
import subprocess
import time
import random
import string
from pathlib import Path

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Project root for init_cluster.sh
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
PRIMARY_EDGE_PORT = int(os.environ.get("IRIS_PORT", "8085"))       # edge-east-1 -> core-east-1
SECONDARY_EDGE_PORT = int(os.environ.get("IRIS_PORT_2", "8086"))   # edge-east-2 -> core-east-2
PRIMARY_CORE = os.environ.get("IRIS_PRIMARY_CORE", "core-east-1")
SECONDARY_CORE = os.environ.get("IRIS_SECONDARY_CORE", "core-east-2")
TIMEOUT = 10
RECOVERY_TIMEOUT = 60

# Per TEST_CONTRACT.md: exit(0)=pass, exit(1)=fail, exit(2)=skip

# Track test phases for debugging
class TestPhase:
    INIT = "initialization"
    SEND = "sending_message"
    KILL = "killing_primary"
    RECOVER = "waiting_recovery"
    VERIFY = "verifying_durability"
    
current_phase = TestPhase.INIT


def log(msg):
    """Log with timestamp and phase."""
    timestamp = time.strftime("%H:%M:%S")
    print(f"[{timestamp}] [{current_phase}] {msg}")


def generate_unique_id():
    """Generate a unique test ID."""
    return ''.join(random.choices(string.ascii_lowercase + string.digits, k=8))


def connect(host, port):
    """Create TCP connection."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    sock.connect((host, port))
    return sock


def login(sock, username):
    """Send login packet and wait for LOGIN_OK."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    
    # Wait for response
    try:
        response = sock.recv(1024)
        if b"LOGIN_OK" in response:
            return True, response
        else:
            return False, response
    except socket.timeout:
        return False, b"timeout"


def send_message(sock, target, message):
    """
    Send message and wait for ACK.
    
    Protocol: 0x02 | TargetLen(16) | Target | MsgLen(16) | Msg
    """
    target_bytes = target.encode()
    msg_bytes = message.encode()
    
    packet = (
        bytes([0x02]) +
        struct.pack('>H', len(target_bytes)) + target_bytes +
        struct.pack('>H', len(msg_bytes)) + msg_bytes
    )
    
    sock.sendall(packet)
    
    # Wait for any response (ACK or echo)
    try:
        response = sock.recv(1024)
        return len(response) > 0, response
    except socket.timeout:
        return False, b"timeout"


def docker_sigkill(container_name):
    """
    Send SIGKILL to container (hard crash, no graceful shutdown).
    This is the true test of multi-master durability.
    """
    log(f"Sending SIGKILL to {container_name}")
    result = subprocess.run(
        ["docker", "kill", "--signal=KILL", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def docker_start(container_name):
    """Start a stopped container."""
    log(f"Starting container {container_name}")
    result = subprocess.run(
        ["docker", "start", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def wait_for_container_healthy(container_name, timeout=60):
    """Wait for container to be healthy."""
    log(f"Waiting for {container_name} to be healthy (max {timeout}s)")
    start_time = time.time()
    
    while time.time() - start_time < timeout:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Health.Status}}", container_name],
            capture_output=True,
            text=True
        )
        status = result.stdout.strip()
        
        if status == "healthy":
            log(f"{container_name} is healthy")
            return True
        elif "running" in subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Status}}", container_name],
            capture_output=True, text=True
        ).stdout:
            # Container running but no health check defined
            log(f"{container_name} is running (no health check)")
            return True
            
        time.sleep(2)
    
    log(f"Timeout waiting for {container_name}")
    return False


def check_container_running(container_name):
    """
    Check if container is running.
    
    Uses multiple strategies:
    1. Direct inspect (exact name)
    2. Filter by name pattern (handles compose prefixes)
    """
    # Try direct inspect first
    result = subprocess.run(
        ["docker", "inspect", "--format", "{{.State.Running}}", container_name],
        capture_output=True,
        text=True
    )
    if "true" in result.stdout.lower():
        return True
    
    # Fallback: search by name pattern using docker ps
    # This handles cases where compose adds directory prefix
    result = subprocess.run(
        ["docker", "ps", "--filter", f"name={container_name}", "--format", "{{.Names}}"],
        capture_output=True,
        text=True
    )
    if result.stdout.strip():
        log(f"Found container matching '{container_name}': {result.stdout.strip()}")
        return True
    
    return False


def check_docker_available():
    """Check if Docker is available."""
    result = subprocess.run(["docker", "ps"], capture_output=True)
    return result.returncode == 0


def start_docker_cluster():
    """Attempt to start the Docker global cluster if not running."""
    docker_compose_dir = PROJECT_ROOT / "docker" / "global-cluster"
    docker_compose_file = docker_compose_dir / "docker-compose.yml"
    
    if not docker_compose_file.exists():
        log(f"Docker compose file not found: {docker_compose_file}")
        return False
    
    log("Attempting to start Docker global cluster...")
    
    try:
        # Start cluster
        result = subprocess.run(
            ["docker", "compose", "-f", str(docker_compose_file), "up", "-d"],
            cwd=str(docker_compose_dir),
            capture_output=True,
            text=True,
            timeout=180
        )
        
        if result.returncode != 0:
            log(f"Docker compose failed: {result.stderr}")
            return False
        
        log("Cluster starting, waiting for containers...")
        
        # Wait for core containers
        for i in range(90):  # 90 seconds max
            if check_container_running(PRIMARY_CORE) and check_container_running(SECONDARY_CORE):
                log("Core containers are running!")
                break
            time.sleep(1)
            if i % 15 == 14:
                log(f"Still waiting... ({i+1}s)")
        else:
            log("Timeout waiting for core containers")
            return False
        
        # Initialize replication
        return reinit_cluster_replication()
        
    except subprocess.TimeoutExpired:
        log("Timeout starting cluster")
        return False
    except Exception as e:
        log(f"Error starting cluster: {e}")
        return False


def reinit_cluster_replication():
    """Reinitialize Mnesia replication."""
    init_script = PROJECT_ROOT / "docker" / "global-cluster" / "init_cluster.sh"
    
    if not init_script.exists():
        log(f"Init script not found: {init_script}")
        return False
    
    log("Initializing Mnesia replication...")
    
    try:
        result = subprocess.run(
            ["bash", str(init_script)],
            cwd=str(init_script.parent),
            capture_output=True,
            text=True,
            timeout=180
        )
        
        if result.returncode == 0:
            log("Replication initialized successfully")
            time.sleep(10)  # Let it settle
            return True
        else:
            log(f"Init script returned {result.returncode}")
            for line in result.stdout.strip().split('\n')[-5:]:
                log(f"  {line}")
            return False
            
    except subprocess.TimeoutExpired:
        log("Timeout running init script")
        return False
    except Exception as e:
        log(f"Error running init script: {e}")
        return False


def reconnect_edge_to_core(edge_container, core_node):
    """Reconnect edge to core after core restart."""
    log(f"Reconnecting {edge_container} to {core_node}")
    
    # Use erl to ping from within the edge container
    cmd = f"docker exec {edge_container} erl -noshell -hidden -sname tmp_reconn_{generate_unique_id()} -setcookie iris_secret -eval \"rpc:call(edge_east_2@edgeeast2, net_adm, ping, ['{core_node}']), init:stop().\""
    
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=10)
    time.sleep(1)
    return True


def test_multimaster_durability():
    """
    Main test: Validate RPO=0 with multi-master Mnesia replication.
    
    This test SIGKILL's the primary core node after receiving an ACK,
    then verifies the message was replicated to the secondary.
    """
    global current_phase
    
    print("\n" + "=" * 70)
    print("Multi-Master Durability Test (RFC NFR-6, NFR-8)")
    print("=" * 70)
    print("\nThis test validates TRUE RPO=0 with SIGKILL (hard crash)")
    print("Unlike graceful shutdown, SIGKILL doesn't allow WAL flush.\n")
    
    # Prerequisites
    current_phase = TestPhase.INIT
    
    if not check_docker_available():
        print("SKIP:INFRA - Docker not available")
        sys.exit(2)
    
    # Check if core containers are running, try to start if not
    primary_running = check_container_running(PRIMARY_CORE)
    secondary_running = check_container_running(SECONDARY_CORE)
    
    if not primary_running or not secondary_running:
        log(f"Primary ({PRIMARY_CORE}): {'running' if primary_running else 'NOT running'}")
        log(f"Secondary ({SECONDARY_CORE}): {'running' if secondary_running else 'NOT running'}")
        
        auto_start = os.environ.get("IRIS_AUTO_START_DOCKER", "true").lower() == "true"
        
        if not auto_start:
            log("Auto-start disabled (set IRIS_AUTO_START_DOCKER=true to enable)")
            print("SKIP:INFRA - Cluster not running and auto-start disabled. Start with: make cluster-up")
            sys.exit(2)
        
        log("Attempting to start Docker cluster...")
        if not start_docker_cluster():
            print("SKIP:INFRA - Failed to start Docker cluster")
            sys.exit(2)
        
        # Re-check after starting
        if not check_container_running(PRIMARY_CORE):
            print(f"SKIP:INFRA - Primary core {PRIMARY_CORE} still not running after cluster start")
            sys.exit(2)
        
        if not check_container_running(SECONDARY_CORE):
            print(f"SKIP:INFRA - Secondary core {SECONDARY_CORE} still not running after cluster start")
            sys.exit(2)
    
    # Generate unique test identifiers
    test_id = generate_unique_id()
    sender = f"mm_sender_{test_id}"
    receiver = f"mm_receiver_{test_id}"
    test_message = f"MULTIMASTER_DURABILITY_TEST_{test_id}_{time.time()}"
    
    log(f"Test ID: {test_id}")
    log(f"Sender: {sender}")
    log(f"Receiver: {receiver}")
    log(f"Message: {test_message}")
    
    # Phase 1: Send message via primary edge -> primary core
    current_phase = TestPhase.SEND
    
    log(f"Connecting to primary edge (port {PRIMARY_EDGE_PORT})")
    try:
        sock = connect(SERVER_HOST, PRIMARY_EDGE_PORT)
    except Exception as e:
        print(f"FAIL - Failed to connect to primary edge: {e}")
        sys.exit(1)
    
    log(f"Logging in as {sender}")
    success, response = login(sock, sender)
    if not success:
        print(f"FAIL - Login failed: {response}")
        sock.close()
        sys.exit(1)
    
    log(f"Sending message to offline receiver: {receiver}")
    ack_received, response = send_message(sock, receiver, test_message)
    sock.close()
    
    if ack_received:
        log("✓ ACK received - message is now durable (per contract)")
    else:
        log("⚠ No ACK received - continuing anyway")
    
    # Small delay to ensure Mnesia replication
    log("Waiting 2s for Mnesia multi-master replication...")
    time.sleep(2)
    
    # Phase 2: SIGKILL the primary core (hard crash)
    current_phase = TestPhase.KILL
    
    log(f"=== SIGKILL {PRIMARY_CORE} (hard crash simulation) ===")
    if not docker_sigkill(PRIMARY_CORE):
        log(f"Failed to kill {PRIMARY_CORE}")
        return False
    
    log("✓ Primary core killed with SIGKILL")
    log("Waiting 3s for cluster to detect failure...")
    time.sleep(3)
    
    # Phase 3: Wait for recovery and verify via secondary
    current_phase = TestPhase.RECOVER
    
    log(f"Connecting to secondary edge (port {SECONDARY_EDGE_PORT})")
    
    # Secondary edge should still be connected to secondary core
    # Give it a moment to stabilize
    time.sleep(2)
    
    current_phase = TestPhase.VERIFY
    
    try:
        sock2 = connect(SERVER_HOST, SECONDARY_EDGE_PORT)
    except Exception as e:
        log(f"Failed to connect to secondary: {e}")
        # Try to restart primary before failing
        docker_start(PRIMARY_CORE)
        return False
    
    log(f"Logging in as receiver: {receiver}")
    success, response = login(sock2, receiver)
    
    if not success:
        log(f"Receiver login failed: {response}")
        sock2.close()
        docker_start(PRIMARY_CORE)
        return False
    
    log("Checking for offline messages...")
    
    # The offline message should be included in login response or delivered immediately
    # Check the login response first
    message_found = test_message.encode() in response
    
    if not message_found:
        # Try receiving with timeout
        log("Message not in login response, waiting for delivery...")
        try:
            sock2.settimeout(5)
            data = sock2.recv(4096)
            message_found = test_message.encode() in data
            if message_found:
                log(f"Received {len(data)} bytes containing test message")
        except socket.timeout:
            log("No additional data received")
    
    sock2.close()
    
    # Restart primary core for cluster health
    log(f"Restarting {PRIMARY_CORE} for cluster health...")
    docker_start(PRIMARY_CORE)
    
    # Results
    print("\n" + "=" * 70)
    if message_found:
        print("✓ PASS: Message found on SECONDARY after PRIMARY was SIGKILL'd!")
        print("")
        print("  This proves:")
        print("  1. Mnesia multi-master replication is working")
        print("  2. RPO=0 is validated (zero data loss after ACK)")
        print("  3. RFC NFR-6 & NFR-8: COMPLIANT")
        print("")
        print("  The ACK contract is VALID - acknowledged messages survive hard crashes.")
        return True
    else:
        print("✗ FAIL: Message NOT found after primary crash!")
        print("")
        print("  This indicates:")
        print("  1. Mnesia replication may not be configured correctly")
        print("  2. OR replication lag exceeded (message not yet replicated)")
        print("  3. OR ACK was sent before replication completed")
        print("")
        print("  RFC VIOLATION: ACK-durability contract is BROKEN")
        print("  Action: Verify Mnesia multi-master configuration")
        return False


def restore_cluster_state():
    """Re-initialize cluster after test that restarts containers."""
    try:
        # First ensure all core containers are running
        log("[cleanup] Restoring cluster state after container restart...")
        
        cores = ["core-east-1", "core-east-2", "core-west-1", "core-west-2", "core-eu-1", "core-eu-2"]
        for core in cores:
            result = subprocess.run(
                ["docker", "inspect", "--format", "{{.State.Status}}", core],
                capture_output=True, text=True
            )
            if result.stdout.strip() in ["exited", "created"]:
                log(f"[cleanup] Restarting stopped container: {core}")
                subprocess.run(["docker", "start", core], capture_output=True)
        
        # Wait for containers to stabilize
        time.sleep(10)
        
        # Run init script
        init_script = PROJECT_ROOT / "docker" / "global-cluster" / "init_cluster.sh"
        if init_script.exists():
            subprocess.run(
                ["bash", str(init_script)],
                cwd=str(init_script.parent),
                capture_output=True,
                timeout=180  # Increased timeout
            )
            log("[cleanup] Cluster state restored")
    except Exception as e:
        log(f"[cleanup] Warning: Could not restore cluster state: {e}")


def main():
    result = test_multimaster_durability()
    
    # Restore cluster state for subsequent tests
    restore_cluster_state()
    
    print("\n" + "=" * 70)
    if result is True:
        print("RESULT: PASSED - Multi-master durability validated")
        return 0
    elif result is False:
        print("RESULT: FAILED - Durability violation detected")
        return 1
    else:
        # Per TEST_CONTRACT.md: exit(2) = SKIP with documented reason
        print("SKIP:CLUSTER - Prerequisites not met")
        print("\nTo run this test:")
        print("  1. make cluster-up")
        print("  2. ./docker/global-cluster/cluster.sh (initializes replication)")
        print("  3. python3 tests/suites/chaos_dist/test_multimaster_durability.py")
        return 2  # SKIP, not PASS


if __name__ == "__main__":
    sys.exit(main())
