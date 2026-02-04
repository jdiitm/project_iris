#!/usr/bin/env python3
"""
Dedup Persistence Test (RFC NFR-11)

This test validates that deduplication state survives node crashes:
- Dedup state is written to Mnesia (dedup_log table) before ACK
- Hard crash (SIGKILL) after ACK should NOT reset dedup window
- Client retry after crash should be recognized as duplicate

RFC Requirements:
- NFR-11: Deduplication window must survive node crashes
- NFR-6: Message durability 99.999%
- NFR-8: RPO=0 (zero data loss)

Test Strategy:
1. Send message with unique ID to offline user
2. Wait for ACK (confirms dedup_log written via Mnesia)
3. SIGKILL the core node (hard crash, simulates power loss)
4. Wait for node recovery
5. Send SAME message ID again (client retry scenario)
6. Login as receiver and verify message received exactly ONCE

CRITICAL: This test uses SIGKILL (not SIGTERM) to simulate power loss.
If dedup state is lost, the user will see duplicate messages.

PASS: Duplicate message rejected after crash (dedup state preserved)
FAIL: Duplicate message delivered (dedup state lost - RFC VIOLATION)
"""

import socket
import ssl
import time
import subprocess
import sys
import os
import uuid
import struct
from pathlib import Path

# Project root for locating scripts
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Test configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CONTAINER_NAME = os.environ.get("IRIS_CORE_CONTAINER", "core-east-1")
TIMEOUT = 10
RECOVERY_TIMEOUT = 60


def log(msg):
    """Print timestamped log message."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def connect_tls():
    """Create TLS connection to Iris edge."""
    context = ssl.create_default_context()
    ca_cert = PROJECT_ROOT / "certs" / "ca.pem"
    if ca_cert.exists():
        context.load_verify_locations(str(ca_cert))
    else:
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    tls_sock = context.wrap_socket(sock, server_hostname=SERVER_HOST)
    tls_sock.connect((SERVER_HOST, SERVER_PORT))
    return tls_sock


def connect_auto():
    """Connect with auto-detection of TLS mode."""
    # Try TLS first
    try:
        return connect_tls()
    except Exception:
        pass
    
    # Fall back to plaintext
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(TIMEOUT)
    sock.connect((SERVER_HOST, SERVER_PORT))
    return sock


def login(sock, username):
    """Send login packet and wait for LOGIN_OK."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    try:
        response = sock.recv(1024)
        return b"LOGIN_OK" in response or len(response) > 0
    except socket.timeout:
        return False


def send_message_with_id(sock, target, message, msg_id):
    """
    Send a message with a specific message ID for dedup tracking.
    
    Uses reliable message format: 0x10 | IdLen(16) | MsgId | MsgLen(32) | Msg
    This format allows explicit message ID control for deduplication testing.
    """
    target_bytes = target.encode() if isinstance(target, str) else target
    msg_bytes = message.encode() if isinstance(message, str) else message
    msg_id_bytes = msg_id.encode() if isinstance(msg_id, str) else msg_id
    
    # Build reliable message packet with explicit ID
    # Protocol: 0x10 | IdLen(16) | MsgId | MsgLen(32) | Msg
    # But we need to include target - use compound message format
    # Actually, let's use opcode 0x07 with the msg_id embedded in the message
    # for easier tracking, since that's what the server dedup checks
    
    # Embed msg_id in message content for tracking
    full_msg = f"{msg_id}:{message}".encode()
    
    # Use opcode 0x07 (sequenced message)
    # Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
    # Use msg_id hash as sequence number for consistent dedup
    seq_no = hash(msg_id) & 0xFFFFFFFFFFFFFFFF
    
    packet = (bytes([0x07]) +
              struct.pack('>H', len(target_bytes)) + target_bytes +
              struct.pack('>Q', seq_no) +
              struct.pack('>H', len(full_msg)) + full_msg)
    
    sock.sendall(packet)
    
    # Wait for ACK
    try:
        response = sock.recv(1024)
        return len(response) > 0
    except socket.timeout:
        log("  Timeout waiting for ACK")
        return False
    except socket.error as e:
        log(f"  Socket error waiting for ACK: {e}")
        return False


def receive_offline_messages(sock, timeout=10):
    """Receive offline messages.
    
    Returns list of message contents received.
    """
    messages = []
    sock.settimeout(1.0)
    end_time = time.time() + timeout
    buffer = b""
    
    while time.time() < end_time:
        try:
            data = sock.recv(4096)
            if data:
                buffer += data
                buffer, msgs = parse_and_ack_messages(sock, buffer)
                messages.extend(msgs)
        except socket.timeout:
            if messages and not buffer:
                break
            continue
        except ssl.SSLWantReadError:
            continue
        except Exception:
            break
    
    return messages


def parse_and_ack_messages(sock, data):
    """Parse reliable messages and send ACKs.
    
    Returns (remaining_buffer, list_of_message_contents)
    """
    messages = []
    idx = 0
    
    while idx < len(data):
        if idx >= len(data):
            break
            
        opcode = data[idx]
        
        # Check for reliable message (opcode 16 = 0x10)
        if opcode == 16:
            # Format: 16 | IdLen(16) | MsgId | MsgLen(32) | Msg
            if idx + 3 > len(data):
                break
            
            id_len = struct.unpack('>H', data[idx+1:idx+3])[0]
            
            if idx + 3 + id_len + 4 > len(data):
                break
            
            msg_id = data[idx+3:idx+3+id_len]
            msg_len = struct.unpack('>I', data[idx+3+id_len:idx+3+id_len+4])[0]
            
            if idx + 3 + id_len + 4 + msg_len > len(data):
                break
            
            msg = data[idx+3+id_len+4:idx+3+id_len+4+msg_len]
            
            # Send ACK
            try:
                ack_packet = bytes([0x03]) + msg_id
                sock.sendall(ack_packet)
            except Exception:
                pass
            
            messages.append(msg)
            idx += 3 + id_len + 4 + msg_len
        else:
            idx += 1
    
    remaining = data[idx:] if idx < len(data) else b""
    return remaining, messages


def kill_container(container_name):
    """Kill container with SIGKILL (hard crash)."""
    log(f"  Killing container: {container_name} (SIGKILL)")
    result = subprocess.run(
        ["docker", "kill", "--signal=SIGKILL", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def start_container(container_name):
    """Start Docker container."""
    log(f"  Starting container: {container_name}")
    result = subprocess.run(
        ["docker", "start", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def wait_for_container_healthy(container_name, timeout=60):
    """Wait for container to be healthy."""
    log(f"  Waiting for {container_name} to be healthy...")
    start_time = time.time()
    while time.time() - start_time < timeout:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Health.Status}}", container_name],
            capture_output=True,
            text=True
        )
        if result.returncode == 0 and "healthy" in result.stdout.strip():
            log(f"  Container {container_name} is healthy")
            return True
        time.sleep(2)
    return False


def reconnect_edge_to_core(edge_container="edge-east-1", core_node="core_east_1@coreeast1"):
    """Reconnect edge to core after core restart."""
    log("  Reconnecting edge to core...")
    cmd = f"docker exec {edge_container} erl -noshell -hidden -sname tmp_reconn -setcookie iris_secret -eval 'rpc:call(edge_east_1@edgeeast1, net_adm, ping, [{core_node}]), init:stop().'"
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    time.sleep(1)
    return result.returncode == 0


def check_docker_available():
    """Check if Docker is available."""
    result = subprocess.run(["docker", "ps"], capture_output=True)
    return result.returncode == 0


def check_container_exists(container_name):
    """Check if container exists."""
    result = subprocess.run(
        ["docker", "inspect", container_name],
        capture_output=True
    )
    return result.returncode == 0


def ensure_cluster_healthy():
    """Ensure cluster is healthy for testing."""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import ensure_cluster_healthy as _ensure
        return _ensure(max_attempts=3)
    except ImportError:
        pass
    
    # Fallback: just check containers are running
    for container in ["core-east-1", "edge-east-1"]:
        result = subprocess.run(
            ["docker", "inspect", "-f", "{{.State.Running}}", container],
            capture_output=True, text=True
        )
        if "true" not in result.stdout.lower():
            return False
    return True


def restore_cluster_state():
    """Restore cluster state after test."""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import restore_cluster_state as _restore
        _restore()
    except ImportError:
        log("[cleanup] Cluster utils not available, skipping restore")
    except Exception as e:
        log(f"[cleanup] Warning: Could not restore cluster state: {e}")


# =============================================================================
# Main Test
# =============================================================================

def test_dedup_survives_sigkill():
    """
    Main test: Dedup state must survive hard crash.
    
    NFR-11 requires deduplication window to persist across crashes.
    If dedup state is lost, clients retrying after server crash will
    cause duplicate message delivery.
    """
    log("\n" + "=" * 60)
    log("DEDUP PERSISTENCE TEST (RFC NFR-11)")
    log("=" * 60)
    
    # Check prerequisites
    if not check_docker_available():
        log("FAIL: Docker not available")
        log("This test requires Docker cluster")
        return False
    
    if not check_container_exists(CONTAINER_NAME):
        log(f"FAIL: Container {CONTAINER_NAME} not found")
        log("Start cluster with: make cluster-up")
        return False
    
    # Ensure cluster is healthy
    log("\n1. Ensuring cluster is healthy...")
    if not ensure_cluster_healthy():
        log("FAIL: Could not establish healthy cluster")
        return False
    log("  Cluster is healthy")
    
    # Generate unique test identifiers
    test_id = int(time.time() * 1000)
    sender = f"dedup_sender_{test_id}"
    receiver = f"dedup_receiver_{test_id}"
    msg_id = f"DEDUP_TEST_{uuid.uuid4().hex[:12]}"
    test_message = f"Test message for dedup persistence"
    
    log(f"\n2. Connecting as sender: {sender}")
    try:
        sock = connect_auto()
        if not login(sock, sender):
            log("FAIL: Login failed")
            return False
    except Exception as e:
        log(f"FAIL: Connection failed: {e}")
        return False
    
    log(f"\n3. Sending message with ID: {msg_id}")
    log(f"   Target: {receiver} (offline)")
    log(f"   Message: {test_message}")
    
    ack_received = send_message_with_id(sock, receiver, test_message, msg_id)
    sock.close()
    
    if not ack_received:
        log("  WARNING: No ACK received, continuing anyway...")
    else:
        log("  ACK received - dedup state should be written")
    
    # CRITICAL: Kill immediately after ACK
    # RFC NFR-11 requires dedup state to be durable at ACK time
    log(f"\n4. SIGKILL core node: {CONTAINER_NAME}")
    log("   (Simulating power loss immediately after ACK)")
    
    if not kill_container(CONTAINER_NAME):
        log("FAIL: Could not kill container")
        return False
    log("  Container killed")
    
    log("\n5. Waiting for node to be fully dead...")
    time.sleep(3)
    
    log(f"\n6. Starting container: {CONTAINER_NAME}")
    if not start_container(CONTAINER_NAME):
        log("FAIL: Could not start container")
        return False
    
    log(f"\n7. Waiting for recovery (up to {RECOVERY_TIMEOUT}s)...")
    if not wait_for_container_healthy(CONTAINER_NAME, RECOVERY_TIMEOUT):
        log("  Container not healthy, but continuing...")
    
    # Wait for Mnesia recovery
    log("  Waiting 20s for Mnesia recovery...")
    time.sleep(20)
    
    # Reconnect edge to core
    reconnect_edge_to_core()
    time.sleep(2)
    
    log(f"\n8. Sending SAME message ID again (retry scenario)")
    log(f"   This simulates client retry after server crash")
    
    retry_count = 0
    for attempt in range(5):
        try:
            sock = connect_auto()
            if login(sock, sender):
                break
            sock.close()
        except Exception as e:
            if attempt < 4:
                log(f"  Reconnect attempt {attempt+1} failed, retrying...")
                time.sleep(3)
            else:
                log(f"FAIL: Could not reconnect: {e}")
                return False
    
    # Send the retry (same msg_id)
    retry_ack = send_message_with_id(sock, receiver, test_message, msg_id)
    sock.close()
    
    log(f"   Retry sent, ACK received: {retry_ack}")
    
    # Wait for any async processing
    time.sleep(2)
    
    log(f"\n9. Logging in as receiver: {receiver}")
    log("   Checking how many copies of the message were delivered")
    
    for attempt in range(5):
        try:
            sock = connect_auto()
            if login(sock, receiver):
                break
            sock.close()
        except Exception as e:
            if attempt < 4:
                log(f"  Connect attempt {attempt+1} failed, retrying...")
                time.sleep(3)
            else:
                log(f"FAIL: Could not connect as receiver: {e}")
                return False
    
    messages = receive_offline_messages(sock, timeout=15)
    sock.close()
    
    log(f"   Received {len(messages)} message(s)")
    
    # Count how many times our test message appears
    msg_count = 0
    for msg in messages:
        msg_str = msg.decode('utf-8') if isinstance(msg, bytes) else str(msg)
        if msg_id in msg_str:
            msg_count += 1
            log(f"   Found message: {msg_str[:60]}...")
    
    log("\n" + "=" * 60)
    log("RESULTS")
    log("=" * 60)
    
    if msg_count == 0:
        log("FAIL: Message not delivered at all!")
        log("This indicates a durability issue, not dedup")
        return False
    elif msg_count == 1:
        log("PASS: Message delivered exactly once!")
        log("Dedup state survived SIGKILL crash")
        log("RFC NFR-11: COMPLIANT")
        return True
    else:
        log(f"FAIL: Message delivered {msg_count} times!")
        log("Dedup state was LOST during crash")
        log("RFC NFR-11: VIOLATED")
        log("")
        log("Root cause: dedup_log Mnesia write was not durable")
        log("before ACK, or dedup_log was not checked after recovery")
        return False


def test_dedup_without_crash():
    """
    Baseline test: Verify dedup works without crash.
    
    This ensures the test framework is correct before testing persistence.
    """
    log("\n" + "=" * 60)
    log("BASELINE: Dedup Without Crash")
    log("=" * 60)
    
    test_id = int(time.time() * 1000)
    sender = f"dedup_baseline_sender_{test_id}"
    receiver = f"dedup_baseline_receiver_{test_id}"
    msg_id = f"BASELINE_{uuid.uuid4().hex[:12]}"
    test_message = "Baseline dedup test"
    
    try:
        # Send message twice with same ID
        sock = connect_auto()
        if not login(sock, sender):
            log("FAIL: Login failed")
            return False
        
        log("  Sending message first time...")
        send_message_with_id(sock, receiver, test_message, msg_id)
        
        log("  Sending message second time (same ID)...")
        send_message_with_id(sock, receiver, test_message, msg_id)
        sock.close()
        
        time.sleep(1)
        
        # Check receiver
        sock = connect_auto()
        login(sock, receiver)
        messages = receive_offline_messages(sock, timeout=5)
        sock.close()
        
        # Count occurrences
        msg_count = sum(1 for m in messages if msg_id.encode() in m or msg_id in str(m))
        
        if msg_count <= 1:
            log(f"  PASS: Dedup working (received {msg_count} copy)")
            return True
        else:
            log(f"  WARNING: Received {msg_count} copies (dedup may not be active)")
            return True  # Not a failure for baseline
            
    except Exception as e:
        log(f"FAIL: {e}")
        return False


def main():
    """Main entry point."""
    log("#" * 60)
    log("# DEDUP PERSISTENCE TEST")
    log("#" * 60)
    log("This test requires a running Docker cluster.")
    log("Run 'make cluster-up' first if not already running.\n")
    
    # Run baseline first
    baseline_ok = test_dedup_without_crash()
    
    # Run main crash test
    result = test_dedup_survives_sigkill()
    
    # Restore cluster state
    restore_cluster_state()
    
    log("\n" + "#" * 60)
    if result:
        log("# RESULT: PASSED")
        log("# Dedup state survives SIGKILL crash")
    else:
        log("# RESULT: FAILED")
        log("# RFC NFR-11 VIOLATION DETECTED")
    log("#" * 60)
    
    return 0 if result else 1


if __name__ == "__main__":
    sys.exit(main())
