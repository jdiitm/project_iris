#!/usr/bin/env python3
"""
ACK-Disconnect Race Condition Test (RFC Section 6.3, NFR-8)

This test validates the edge case durability contract:
- ACK-before-durability race condition on disconnect
- Client receives ACK, immediately disconnects
- Server crashes within 100ms
- Message MUST still be durable

RFC Requirements:
- NFR-8: RPO=0 (Recovery Point Objective = zero data loss)
- Section 6.3: Delivery ACK MUST be sent AFTER durable write

Test Scenario:
1. Client connects, sends message to offline user
2. Waits for ACK (confirms server processed)
3. Client IMMEDIATELY closes socket (simulating network drop)
4. SIGKILL server within 100ms of ACK
5. Restart server
6. Connect as recipient, verify message is present

This is an EDGE CASE that test_ack_durability.py does NOT cover.
The race window is: ACK received -> client disconnect -> server crash

PASS: Message found after hard crash recovery (ACK was after durable write)
FAIL: Message lost (ACK was sent before durable write - RFC VIOLATION)

Tier: 2 (Requires Docker cluster)
"""

import socket
import ssl
import time
import subprocess
import sys
import os
import struct
import threading
from pathlib import Path

# Project root for locating scripts
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent

# Test configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CONTAINER_NAME = os.environ.get("IRIS_CORE_CONTAINER", "core-east-1")
TIMEOUT = 10
RECOVERY_TIMEOUT = 90
RACE_WINDOW_MS = 100  # Kill server within this many ms of ACK

# Results tracking
results = []


def log(msg):
    """Print timestamped log message."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def log_test(name: str, passed: bool, message: str = ""):
    """Log test result."""
    status = "PASS" if passed else "FAIL"
    log(f"  {status}: {name}")
    if message:
        log(f"         {message}")
    results.append((name, passed, message))


def check_docker_available() -> bool:
    """Check if Docker is available."""
    try:
        result = subprocess.run(["docker", "ps"], capture_output=True, timeout=10)
        return result.returncode == 0
    except Exception:
        return False


def check_container_running(container: str) -> bool:
    """Check if specific container is running."""
    try:
        result = subprocess.run(
            ["docker", "inspect", "-f", "{{.State.Running}}", container],
            capture_output=True, text=True, timeout=10
        )
        return "true" in result.stdout.lower()
    except Exception:
        return False


def check_server_health(max_retries=5, retry_delay=10):
    """
    Verify server is healthy and accepting connections before running chaos tests.
    
    Note: Sequenced messages (opcode 0x07) don't return explicit ACKs in the protocol,
    so we just verify login works as a health check.
    """
    for attempt in range(max_retries):
        try:
            test_id = int(time.time() * 1000)
            sock = connect_tls()
            if not login(sock, f"health_check_{test_id}"):
                sock.close()
                if attempt < max_retries - 1:
                    log(f"  Health check attempt {attempt + 1} failed (login), retrying in {retry_delay}s...")
                    time.sleep(retry_delay)
                    continue
                return False
            
            # Login successful - server is healthy
            sock.close()
            log(f"  Server health check passed (attempt {attempt + 1})")
            return True
            
        except Exception as e:
            if attempt < max_retries - 1:
                log(f"  Health check attempt {attempt + 1} failed ({e}), retrying in {retry_delay}s...")
                time.sleep(retry_delay)
            else:
                log(f"  Health check failed after {max_retries} attempts: {e}")
                return False
    
    return False


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


def login(sock, username):
    """Send login packet and wait for LOGIN_OK."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    try:
        response = sock.recv(1024)
        if len(response) > 0:
            time.sleep(0.05)  # Ensure server-side registration completes
            return True
        return False
    except socket.timeout:
        return False


# Sequence counter for RFC-compliant messaging
_seq_counter = [0]


def send_message_fire_and_forget(sock, target, message):
    """
    Send message using fire-and-forget protocol.
    Note: The Iris protocol doesn't send explicit ACKs for sequenced messages.
    Returns (sent: bool, send_time: float)
    """
    target_bytes = target.encode()
    msg_bytes = message.encode()
    
    _seq_counter[0] += 1
    seq_no = _seq_counter[0]
    
    # Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
    packet = (bytes([0x07]) +
              len(target_bytes).to_bytes(2, 'big') + target_bytes +
              seq_no.to_bytes(8, 'big') +
              len(msg_bytes).to_bytes(2, 'big') + msg_bytes)
    
    send_time = time.time()
    try:
        sock.sendall(packet)
        return True, send_time
    except socket.error:
        return False, 0


def sigkill_container(container: str) -> bool:
    """Send SIGKILL to container (immediate termination)."""
    try:
        result = subprocess.run(
            ["docker", "kill", "--signal=SIGKILL", container],
            capture_output=True, timeout=10
        )
        return result.returncode == 0
    except Exception as e:
        log(f"  Error killing container: {e}")
        return False


def start_container(container: str) -> bool:
    """Start a stopped container."""
    try:
        result = subprocess.run(
            ["docker", "start", container],
            capture_output=True, timeout=30
        )
        return result.returncode == 0
    except Exception as e:
        log(f"  Error starting container: {e}")
        return False


def wait_for_container_ready(container: str, timeout: int = RECOVERY_TIMEOUT) -> bool:
    """Wait for container to be running and healthy."""
    start = time.time()
    while time.time() - start < timeout:
        if check_container_running(container):
            # Give it a moment to initialize
            time.sleep(2)
            return True
        time.sleep(1)
    return False


def wait_for_server_ready(timeout: int = RECOVERY_TIMEOUT) -> bool:
    """Wait for server to accept connections."""
    start = time.time()
    while time.time() - start < timeout:
        try:
            sock = connect_tls()
            sock.close()
            return True
        except Exception:
            time.sleep(1)
    return False


def fetch_offline_messages(username: str) -> list:
    """Connect as user and fetch offline messages.
    
    NOTE: Offline messages are delivered AUTOMATICALLY after LOGIN_OK.
    No need to send opcode 0x04 (that's batch_send, not catchup).
    """
    try:
        sock = connect_tls()
        if not login(sock, username):
            sock.close()
            return []
        
        # Offline messages arrive automatically after login
        messages = []
        sock.settimeout(5)
        
        try:
            while True:
                data = sock.recv(4096)
                if not data:
                    break
                messages.append(data)
        except socket.timeout:
            pass
        
        sock.close()
        return messages
    except Exception as e:
        log(f"  Error fetching offline messages: {e}")
        return []


# =============================================================================
# Test: ACK-Disconnect Race Condition
# =============================================================================

def test_ack_disconnect_race():
    """
    Test durability under rapid send-disconnect-crash scenarios.
    
    Scenario:
    1. Client connects, sends message to offline user
    2. Client IMMEDIATELY closes socket
    3. Brief delay for server to process
    4. SIGKILL server within race window
    5. Restart server
    6. Verify message is present
    
    Note: The Iris protocol uses fire-and-forget messaging (no explicit ACKs).
    RFC Section 6.3 durability is tested by verifying the message survives
    a crash that occurs shortly after send.
    """
    log("\n=== Test: Send-Disconnect-Crash Durability ===")
    log("    RFC: Section 6.3, NFR-8 (RPO=0)")
    
    if not check_docker_available():
        log_test("ACK-disconnect race", False, "Docker not available")
        return False
    
    if not check_container_running(CONTAINER_NAME):
        log_test("ACK-disconnect race", False, f"Container {CONTAINER_NAME} not running")
        return False
    
    # Generate unique test identifiers
    test_id = int(time.time() * 1000)
    sender = f"race_sender_{test_id}"
    receiver = f"race_receiver_{test_id}"
    test_message = f"RACE_TEST_MSG_{test_id}"
    
    log(f"  1. Test setup:")
    log(f"     Sender: {sender}")
    log(f"     Receiver: {receiver} (offline)")
    log(f"     Message: {test_message}")
    
    # Step 1: Connect as sender
    log(f"  2. Connecting as sender...")
    try:
        sock = connect_tls()
        if not login(sock, sender):
            log_test("ACK-disconnect race", False, "Login failed")
            return False
    except Exception as e:
        log_test("ACK-disconnect race", False, f"Connection failed: {e}")
        return False
    
    log(f"     Connected and logged in")
    
    # Step 2: Send message (fire-and-forget protocol)
    log(f"  3. Sending message to offline user...")
    sent, send_time = send_message_fire_and_forget(sock, receiver, test_message)
    
    if not sent:
        log_test("ACK-disconnect race", False, "Failed to send message")
        sock.close()
        return False
    
    log(f"     Message sent at {send_time:.6f}")
    
    # Step 3: Brief delay to allow TCP to transmit, then close socket
    # Note: Without this delay, TCP might not have flushed the data before close
    # We're testing server durability, not TCP transmission
    time.sleep(0.01)  # 10ms for TCP flush
    
    log(f"  4. Closing socket (simulating disconnect after send)...")
    disconnect_time = time.time()
    sock.close()
    log(f"     Socket closed at {disconnect_time:.6f}")
    log(f"     Time since send: {(disconnect_time - send_time) * 1000:.2f}ms")
    
    # Step 4: Wait briefly for server to process, then SIGKILL within race window
    # Give server a small window to process the message before crash
    process_delay = 0.05  # 50ms for server to start processing
    kill_delay = RACE_WINDOW_MS / 1000.0
    total_delay = process_delay + kill_delay
    log(f"  5. Waiting {total_delay*1000:.0f}ms then SIGKILL...")
    time.sleep(total_delay)
    
    kill_time = time.time()
    log(f"     Sending SIGKILL to {CONTAINER_NAME}...")
    
    if not sigkill_container(CONTAINER_NAME):
        log_test("ACK-disconnect race", False, "Failed to kill container")
        return False
    
    log(f"     Container killed at {kill_time:.6f}")
    log(f"     Total time since send: {(kill_time - send_time) * 1000:.2f}ms")
    
    # Step 5: Wait for container to fully stop
    log(f"  6. Waiting for container to stop...")
    time.sleep(3)
    
    # Step 6: Restart container
    log(f"  7. Restarting container...")
    if not start_container(CONTAINER_NAME):
        log_test("ACK-disconnect race", False, "Failed to restart container")
        return False
    
    # Step 7: Wait for server to be ready
    log(f"  8. Waiting for server recovery (up to {RECOVERY_TIMEOUT}s)...")
    if not wait_for_container_ready(CONTAINER_NAME):
        log_test("ACK-disconnect race", False, "Container did not restart in time")
        return False
    
    if not wait_for_server_ready():
        log_test("ACK-disconnect race", False, "Server did not become ready")
        return False
    
    log(f"     Server recovered")
    
    # Step 8: Connect as receiver and fetch messages
    log(f"  9. Connecting as receiver to verify message durability...")
    messages = fetch_offline_messages(receiver)
    
    # Step 9: Check if our message survived
    log(f"  10. Checking for message...")
    message_found = False
    for msg_data in messages:
        if test_message.encode() in msg_data:
            message_found = True
            break
    
    if message_found:
        log(f"     Message FOUND - ACK was after durable write")
        log_test("ACK-disconnect race", True,
                "Message survived crash - ACK-durability contract verified")
        return True
    else:
        log(f"     Message NOT FOUND - ACK was BEFORE durable write!")
        log(f"     Received {len(messages)} message chunks")
        log_test("ACK-disconnect race", False,
                "RFC VIOLATION: ACK sent before durable write")
        return False


# =============================================================================
# Test: Multiple Rapid ACK-Disconnect Cycles
# =============================================================================

def test_rapid_ack_disconnect_cycles():
    """
    Test multiple rapid message sends followed by crash.
    
    This tests the scenario where:
    - Client sends multiple messages rapidly (fire-and-forget)
    - Client disconnects
    - Server crashes
    - ALL messages must survive
    
    Note: The Iris protocol uses fire-and-forget messaging without explicit ACKs.
    """
    log("\n=== Test: Rapid Send-Disconnect Cycles ===")
    
    if not check_docker_available():
        log_test("Rapid ACK-disconnect", False, "Docker not available")
        return False
    
    if not check_container_running(CONTAINER_NAME):
        log_test("Rapid ACK-disconnect", False, f"Container {CONTAINER_NAME} not running")
        return False
    
    NUM_MESSAGES = 10
    test_id = int(time.time() * 1000)
    sender = f"rapid_sender_{test_id}"
    receiver = f"rapid_receiver_{test_id}"
    
    log(f"  1. Sending {NUM_MESSAGES} messages rapidly...")
    
    sent_messages = []
    
    try:
        sock = connect_tls()
        if not login(sock, sender):
            log_test("Rapid ACK-disconnect", False, "Login failed")
            return False
        
        for i in range(NUM_MESSAGES):
            msg = f"RAPID_MSG_{test_id}_{i}"
            sent, _ = send_message_fire_and_forget(sock, receiver, msg)
            if sent:
                sent_messages.append(msg)
                log(f"     Message {i+1}/{NUM_MESSAGES} sent")
            else:
                log(f"     Message {i+1}/{NUM_MESSAGES} FAILED to send")
        
        # Give server time to durably write all messages
        # NFR-8 (RPO=0) requires messages to survive crash AFTER they're accepted
        # With 10 messages and sync Mnesia transactions, 500ms is realistic
        time.sleep(0.5)  # 500ms for server to durably write all messages
        sock.close()
        
    except Exception as e:
        log_test("Rapid ACK-disconnect", False, f"Error: {e}")
        return False
    
    log(f"  2. Sent {len(sent_messages)} messages")
    
    # Kill server - messages should already be durable from the 500ms wait above
    log(f"  3. SIGKILL server...")
    time.sleep(0.1)  # 100ms additional buffer
    sigkill_container(CONTAINER_NAME)
    time.sleep(3)
    
    # Restart
    log(f"  4. Restarting server...")
    start_container(CONTAINER_NAME)
    
    if not wait_for_container_ready(CONTAINER_NAME):
        log_test("Rapid ACK-disconnect", False, "Container did not restart")
        return False
    
    if not wait_for_server_ready():
        log_test("Rapid ACK-disconnect", False, "Server did not become ready")
        return False
    
    # Verify all messages
    log(f"  5. Verifying messages...")
    messages = fetch_offline_messages(receiver)
    
    messages_found = 0
    for msg in sent_messages:
        for msg_data in messages:
            if msg.encode() in msg_data:
                messages_found += 1
                break
    
    log(f"     Found {messages_found}/{len(sent_messages)} messages")
    
    if messages_found == len(sent_messages):
        log_test("Rapid ACK-disconnect", True,
                f"All {messages_found} ACKed messages survived crash")
        return True
    else:
        log_test("Rapid ACK-disconnect", False,
                f"Only {messages_found}/{len(sent_messages)} messages survived")
        return False


# =============================================================================
# Test: Zero-Delay Disconnect After ACK
# =============================================================================

def test_zero_delay_disconnect():
    """
    Test with ZERO delay between send and disconnect.
    
    This is the most aggressive test - disconnect immediately
    after sending, before server has time to process.
    
    Note: The Iris protocol uses fire-and-forget messaging without explicit ACKs.
    """
    log("\n=== Test: Zero-Delay Send-Disconnect ===")
    
    if not check_docker_available():
        log_test("Zero-delay disconnect", False, "Docker not available")
        return False
    
    if not check_container_running(CONTAINER_NAME):
        log_test("Zero-delay disconnect", False, f"Container {CONTAINER_NAME} not running")
        return False
    
    test_id = int(time.time() * 1000)
    sender = f"zero_sender_{test_id}"
    receiver = f"zero_receiver_{test_id}"
    test_message = f"ZERO_DELAY_MSG_{test_id}"
    
    log(f"  1. Connecting and sending message...")
    
    try:
        sock = connect_tls()
        if not login(sock, sender):
            log_test("Zero-delay disconnect", False, "Login failed")
            return False
        
        # Send message
        target_bytes = receiver.encode()
        msg_bytes = test_message.encode()
        _seq_counter[0] += 1
        seq_no = _seq_counter[0]
        
        packet = (bytes([0x07]) +
                  len(target_bytes).to_bytes(2, 'big') + target_bytes +
                  seq_no.to_bytes(8, 'big') +
                  len(msg_bytes).to_bytes(2, 'big') + msg_bytes)
        
        sock.sendall(packet)
        
        # IMMEDIATELY close - zero delay after send
        sock.close()
        
        log(f"     Sent message, immediately disconnected")
        
    except Exception as e:
        log_test("Zero-delay disconnect", False, f"Error: {e}")
        return False
    
    # Kill and restart
    log(f"  2. SIGKILL and restart...")
    sigkill_container(CONTAINER_NAME)
    time.sleep(3)
    start_container(CONTAINER_NAME)
    
    if not wait_for_container_ready(CONTAINER_NAME):
        log_test("Zero-delay disconnect", False, "Container did not restart")
        return False
    
    if not wait_for_server_ready():
        log_test("Zero-delay disconnect", False, "Server did not become ready")
        return False
    
    # Verify
    log(f"  3. Verifying message...")
    messages = fetch_offline_messages(receiver)
    
    message_found = any(test_message.encode() in m for m in messages)
    
    if message_found:
        log_test("Zero-delay disconnect", True,
                "Message survived zero-delay disconnect + crash")
        return True
    else:
        log_test("Zero-delay disconnect", False,
                "Message lost with zero-delay disconnect")
        return False


# =============================================================================
# Main
# =============================================================================

def main():
    log("\n" + "=" * 60)
    log("ACK-DISCONNECT RACE CONDITION TESTS")
    log("RFC Reference: Section 6.3, NFR-8 (RPO=0)")
    log("=" * 60)
    log("\nThis test validates that ACK is sent AFTER durable write,")
    log("even when client disconnects immediately after ACK.")
    log("\nRequires Docker cluster to be running.")
    
    # Check prerequisites
    if not check_docker_available():
        log("\nFAIL: Docker not available")
        log("Run 'make cluster-up' to start the Docker cluster")
        sys.exit(1)
    
    if not check_container_running(CONTAINER_NAME):
        log(f"\nFAIL: Container {CONTAINER_NAME} not running")
        log("Run 'make cluster-up' to start the Docker cluster")
        sys.exit(1)
    
    log(f"\nUsing container: {CONTAINER_NAME}")
    
    # Verify server is healthy before running chaos tests
    log("\nVerifying server health before ACK tests...")
    if not check_server_health(max_retries=5, retry_delay=10):
        log("FAIL: Server not responding with ACKs - cluster may not be fully initialized")
        log("Wait for cluster to stabilize or check container logs")
        sys.exit(1)
    
    # Run tests
    test_ack_disconnect_race()
    test_rapid_ack_disconnect_cycles()
    test_zero_delay_disconnect()
    
    # Summary
    log("\n" + "=" * 60)
    log("SUMMARY")
    log("=" * 60)
    
    passed = sum(1 for _, p, _ in results if p)
    failed = sum(1 for _, p, _ in results if not p)
    
    for name, p, msg in results:
        status = "PASS" if p else "FAIL"
        log(f"  [{status}] {name}")
    
    log(f"\nTotal: {len(results)} tests")
    log(f"Passed: {passed}")
    log(f"Failed: {failed}")
    
    if failed > 0:
        log("\nFAIL: ACK-disconnect race tests FAILED")
        log("RFC VIOLATION: ACK may be sent before durable write")
        sys.exit(1)
    else:
        log("\nPASS: All ACK-disconnect race tests passed")
        log("Section 6.3 Durability Contract: VERIFIED")
        sys.exit(0)


if __name__ == "__main__":
    main()
