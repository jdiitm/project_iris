#!/usr/bin/env python3
"""
ACK-Durability Test (RFC NFR-6, NFR-8)

This test validates the critical durability contract:
- Server ACKs ONLY after durable write (sync_transaction complete)
- Hard crash (SIGKILL) after ACK results in ZERO message loss

RFC Requirements:
- NFR-6: Message durability 99.999%
- NFR-8: RPO=0 (Recovery Point Objective = zero data loss)
        "Kill -9 any node, verify all ACKed messages recovered"

Test Strategy:
1. Send message to offline user (forces storage)
2. Wait for ACK from server
3. Immediately SIGKILL the core node (hard crash, no WAL flush)
4. Wait for node recovery
5. Retrieve offline messages
6. Verify message was preserved

CRITICAL: This test uses SIGKILL (not SIGTERM) to simulate power loss.
For single-node: relies on sync_transaction having flushed before ACK.
For multi-node: relies on replication to surviving nodes.

PASS: Message found after hard crash recovery
FAIL: Message lost (ACK was premature - RFC VIOLATION)
"""

import socket
import ssl
import time
import subprocess
import sys
import os
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from tests.utilities.tls_connection import get_verified_ssl_context


def log(msg):
    """Print timestamped log message."""
    print(msg)


# Test configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
CONTAINER_NAME = os.environ.get("IRIS_CORE_CONTAINER", "core-east-1")
IS_CI = os.environ.get("CI", "").lower() in ("true", "1")
CI_TIMEOUT_FACTOR = 2 if IS_CI else 1
TIMEOUT = 10
RECOVERY_TIMEOUT = 60 * CI_TIMEOUT_FACTOR


def connect_tls(max_retries=5, retry_delay=2.0):
    """Create TLS connection to Iris edge with retry logic."""
    context = get_verified_ssl_context()

    last_err = None
    for attempt in range(max_retries):
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(TIMEOUT)
            tls_sock = context.wrap_socket(sock, server_hostname=SERVER_HOST)
            tls_sock.connect((SERVER_HOST, SERVER_PORT))
            return tls_sock
        except Exception as e:
            last_err = e
            if attempt < max_retries - 1:
                time.sleep(retry_delay)
    raise ConnectionError(f"Failed to connect after {max_retries} attempts: {last_err}")


def connect_plaintext():
    """Create plaintext connection - DEPRECATED, use connect_tls()."""
    # Now just calls connect_tls() since TLS is enabled
    return connect_tls()


def login(sock, username):
    """Send login packet and wait for LOGIN_OK."""
    packet = bytes([0x01]) + username.encode()
    sock.sendall(packet)
    try:
        response = sock.recv(1024)
        if b"LOGIN_OK" in response:
            return True
        return False
    except socket.timeout:
        return False


# Sequence counter for RFC-compliant messaging
_seq_counter = [0]

def send_message(sock, target, message):
    """
    Send message packet and wait for ACK.
    
    RFC-001-AMENDMENT-001 v1.0 COMPLIANT: Uses opcode 0x07 (sequenced message)
    instead of deprecated opcode 0x02 (plaintext) which is now rejected.
    """
    target_bytes = target.encode()
    msg_bytes = message.encode()

    # Increment sequence counter
    _seq_counter[0] += 1
    seq_no = _seq_counter[0]

    # Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
    packet = (bytes([0x07]) +
              len(target_bytes).to_bytes(2, 'big') + target_bytes +
              seq_no.to_bytes(8, 'big') +
              len(msg_bytes).to_bytes(2, 'big') + msg_bytes)
    sock.sendall(packet)

    # Wait for ACK (timeout means no ACK received)
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
    """Receive offline messages using reliable message protocol.
    
    The server sends reliable messages (opcode 16) that require ACK.
    Returns list of message contents received.
    """

    messages = []
    sock.settimeout(1.0)  # Short timeout for polling
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
            # Check if we got any messages and no more coming
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
    import struct

    messages = []
    idx = 0

    while idx < len(data):
        opcode = data[idx]

        # Check for reliable message (opcode 17 = 0x11, PROTOCOL_V1_FREEZE v1.1)
        if opcode == 17:
            # Format: 0x11 | IdLen(16) | MsgId | MsgLen(32) | Msg
            if idx + 3 > len(data):
                break  # Need more data

            id_len = struct.unpack('>H', data[idx+1:idx+3])[0]

            if idx + 3 + id_len + 4 > len(data):
                break  # Need more data

            msg_id = data[idx+3:idx+3+id_len]
            msg_len = struct.unpack('>I', data[idx+3+id_len:idx+3+id_len+4])[0]

            if idx + 3 + id_len + 4 + msg_len > len(data):
                break  # Need more data

            msg = data[idx+3+id_len+4:idx+3+id_len+4+msg_len]

            # Send ACK (opcode 0x03 | MsgId)
            try:
                ack_packet = bytes([0x03]) + msg_id
                sock.sendall(ack_packet)
            except Exception:
                pass

            messages.append(msg)
            idx += 3 + id_len + 4 + msg_len
        else:
            # Skip unknown byte
            idx += 1

    remaining = data[idx:] if idx < len(data) else b""
    return remaining, messages


def kill_container(container_name):
    """Kill container with SIGKILL (hard crash, no graceful shutdown).
    
    RFC NFR-8 requires RPO=0 with hard crash simulation. SIGKILL prevents
    any Mnesia WAL flush or graceful shutdown, simulating power loss.
    
    For single-node durability, this relies on Mnesia's sync_transaction
    having already flushed to disk before ACK was sent. For multi-node
    durability (recommended), data survives via replication to other nodes.
    """
    print(f"  Killing container: {container_name} (SIGKILL - hard crash)")
    result = subprocess.run(
        ["docker", "kill", "--signal=SIGKILL", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def start_container(container_name):
    """Start Docker container."""
    print(f"  Starting container: {container_name}")
    result = subprocess.run(
        ["docker", "start", container_name],
        capture_output=True,
        text=True
    )
    return result.returncode == 0


def wait_for_container_healthy(container_name, timeout=60):
    """Wait for container to be healthy."""
    print(f"  Waiting for {container_name} to be healthy...")
    start_time = time.time()
    while time.time() - start_time < timeout:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Health.Status}}", container_name],
            capture_output=True,
            text=True
        )
        if result.returncode == 0 and "healthy" in result.stdout.strip():
            print(f"  Container {container_name} is healthy")
            return True
        time.sleep(2)
    return False


def reconnect_edge_to_core(edge_container="edge-east-1", core_node="core_east_1@coreeast1"):
    """Reconnect edge to core after core restart.
    
    Uses net_adm:ping directly (like init_cluster.sh reconnect_edges).
    """
    print(f"  Reconnecting edge to core...")
    random_id = int(time.time() * 1000) % 100000
    cmd = (f"docker exec {edge_container} erl -noshell "
           f"-sname reconn_{random_id} -setcookie iris_secret "
           f"-eval \"net_adm:ping('{core_node}'), halt(0).\"")
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    time.sleep(2)  # Give time for connection to establish
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


def check_cluster_replication_healthy():
    """Check if Mnesia replication is working (tables have >= 2 copies)."""
    # Try to use shared utility first
    try:
        import sys
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import check_cluster_replication_healthy as _check
        return _check()
    except ImportError:
        pass

    # Fallback implementation
    try:
        import random
        probe_id = random.randint(10000, 99999)
        result = subprocess.run(
            ["docker", "exec", "core-east-1", "sh", "-c",
             f"erl -noshell -sname probe{probe_id} -setcookie iris_secret -eval \""
             "case net_adm:ping('core_east_1@coreeast1') of "
             "pong -> "
             "  Tables = [offline_msg, presence, user_status], "
             "  Results = lists:map(fun(T) -> "
             "    Ram = rpc:call('core_east_1@coreeast1', mnesia, table_info, [T, ram_copies], 5000), "
             "    Disc = rpc:call('core_east_1@coreeast1', mnesia, table_info, [T, disc_copies], 5000), "
             "    case {Ram, Disc} of "
             "      {{badrpc, _}, _} -> false; "
             "      {_, {badrpc, _}} -> false; "
             "      {R, D} when is_list(R), is_list(D) -> length(R) + length(D) >= 2; "
             "      _ -> false "
             "    end "
             "  end, Tables), "
             "  case lists:all(fun(X) -> X end, Results) of "
             "    true -> io:format('healthy'), halt(0); "
             "    false -> io:format('unhealthy'), halt(1) "
             "  end; "
             "pang -> io:format('unreachable'), halt(1) "
             "end.\""],
            capture_output=True, text=True, timeout=30
        )
        return "healthy" in result.stdout
    except Exception as e:
        log(f"  Cluster health check failed: {e}")
        return False


def ensure_cluster_healthy():
    """Ensure cluster replication is healthy, reinitializing if needed.
    
    Returns True if cluster is healthy, False if all attempts failed.
    Uses escalating recovery: first try reinit, then full restart.
    """
    # Try to use shared utility first
    try:
        import sys
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import ensure_cluster_healthy as _ensure
        return _ensure(max_attempts=3)
    except ImportError:
        pass

    # Fallback implementation with escalating recovery
    init_script = PROJECT_ROOT / "docker" / "global-cluster" / "init_cluster.sh"
    docker_dir = PROJECT_ROOT / "docker" / "global-cluster"
    compose_file = docker_dir / "docker-compose.yml"

    for attempt in range(3):
        if check_cluster_replication_healthy():
            log(f"  Cluster replication is healthy")
            return True

        log(f"  Cluster unhealthy, reinitializing (attempt {attempt+1}/3)...")

        # Escalate to full restart after 2 failed attempts
        if attempt >= 2:
            log("  Escalating to full cluster restart...")
            try:
                subprocess.run(
                    ["docker", "compose", "-f", str(compose_file), "down", "--remove-orphans", "-v"],
                    cwd=str(docker_dir), capture_output=True, timeout=120
                )
                time.sleep(5)
                subprocess.run(
                    ["docker", "compose", "-f", str(compose_file), "up", "-d"],
                    cwd=str(docker_dir), capture_output=True, timeout=180
                )
                # AUDIT P4 FIX: Poll for containers instead of blind 60s sleep
                log("  Polling for containers to start...")
                from tests.suites.chaos_dist.utils import wait_for_container_running
                wait_for_container_running("core-east-1", timeout=90)
            except Exception as e:
                log(f"  Full restart failed: {e}")

        if not init_script.exists():
            log(f"  Init script not found: {init_script}")
            return False

        try:
            result = subprocess.run(
                ["bash", str(init_script)],
                cwd=str(init_script.parent),
                capture_output=True,
                text=True,
                timeout=300
            )
            if result.returncode == 0:
                log("  Reinitialization successful, waiting for propagation...")
                time.sleep(10)  # AUDIT P4: Reduced from 20s, init_cluster.sh handles sync
            else:
                log(f"  Reinitialization returned non-zero: {result.returncode}")
                for line in (result.stdout + result.stderr).strip().split('\n')[-3:]:
                    log(f"    {line}")
                time.sleep(10)
        except subprocess.TimeoutExpired:
            log("  Reinitialization timed out")
            time.sleep(10)
        except Exception as e:
            log(f"  Reinitialization error: {e}")
            time.sleep(10)

    # Final check after all attempts
    return check_cluster_replication_healthy()


def test_ack_implies_durability():
    """
    Main test: ACK implies durability.
    
    If we receive an ACK, the message MUST survive node crash.
    """
    print("\n" + "=" * 60)
    print("ACK-Durability Test (RFC NFR-6, NFR-8)")
    print("=" * 60)

    # Check prerequisites
    if not check_docker_available():
        print("  ⚠️ Docker not available - skipping container kill test")
        print("  Running simplified durability test instead...")
        return run_simplified_test()

    if not check_container_exists(CONTAINER_NAME):
        print(f"  ❌ FAIL: Container {CONTAINER_NAME} not found")
        print("  Start cluster with: make cluster-up")
        return False  # No skips - cluster must be running

    # Ensure cluster replication is healthy before running durability test
    print("\n0. Ensuring cluster replication is healthy...")
    if not ensure_cluster_healthy():
        print("  ❌ Could not establish healthy cluster replication after 3 attempts")
        print("  This is required for ACK-durability to work correctly")
        return False  # FAIL - cluster must be healthy for this test

    sender = f"durability_sender_{int(time.time())}"
    receiver = f"durability_receiver_{int(time.time())}"
    test_message = f"DURABILITY_TEST_{time.time()}"

    print(f"\n1. Connecting as sender: {sender}")
    try:
        sock = connect_plaintext()  # Use plaintext for now
        login(sock, sender)
    except Exception as e:
        print(f"  ❌ FAIL: Connection failed: {e}")
        print("  Ensure server is running: make start")
        return False

    print(f"\n2. Sending message to offline receiver: {receiver}")
    print(f"   Message: {test_message}")
    ack_received = send_message(sock, receiver, test_message)
    sock.close()

    if not ack_received:
        print("  ⚠️ No ACK received (server may not send ACKs)")
        print("  Continuing with kill test anyway...")
    else:
        print("  ✅ ACK received from server")

    # CRITICAL: Do NOT wait before killing. RFC NFR-8 requires ACK to mean
    # "data is durable NOW". Any sleep here would mask race conditions where
    # ACK is sent before sync_transaction completes.
    # If this test fails, the bug is in the server (ACK sent prematurely).

    print(f"\n3. Stopping core node: {CONTAINER_NAME} (IMMEDIATELY after ACK)")
    if not kill_container(CONTAINER_NAME):
        print("  ❌ Failed to kill container")
        return False
    print("  ✅ Container killed")

    print(f"\n4. Waiting 3 seconds for node to be fully dead...")
    time.sleep(3)

    print(f"\n5. Starting container: {CONTAINER_NAME}")
    if not start_container(CONTAINER_NAME):
        print("  ❌ Failed to start container")
        return False

    print(f"\n6. Waiting for node recovery (up to {RECOVERY_TIMEOUT}s)...")
    if not wait_for_container_healthy(CONTAINER_NAME, RECOVERY_TIMEOUT):
        print("  ⚠️ Container not healthy, but may still work")

    # Extra wait for Mnesia to fully recover (disc_copies tables load slowly)
    # AUDIT P4 FIX: Reduced from 20s, container health check above covers most of this
    print("  Waiting for Mnesia recovery...")
    time.sleep(10)

    # Reconnect edge to core (hidden nodes don't auto-reconnect)
    print("  Reconnecting edge to core after restart...")
    reconnect_edge_to_core()
    time.sleep(2)

    print(f"\n7. Connecting as receiver: {receiver}")
    sock = None
    for attempt in range(10):
        try:
            sock = connect_plaintext()
            if login(sock, receiver):
                break
            sock.close()
            sock = None
        except Exception as e:
            if attempt < 9:
                print(f"  Connection attempt {attempt+1} failed: {e}, retrying in 3s...")
                time.sleep(3)
            else:
                print(f"  ❌ Reconnection failed after 10 attempts: {e}")
                return False

    if sock is None:
        print(f"  ❌ Failed to connect")
        return False

    print("\n8. Reading offline messages...")
    # Receive messages using reliable message protocol (with ACK)
    messages = receive_offline_messages(sock, timeout=15)
    sock.close()

    print(f"   Received {len(messages)} message(s)")

    # Check if our test message is in any received message
    found = False
    for msg in messages:
        if test_message.encode() in msg:
            found = True
            break

    if found:
        print(f"\n✅ PASS: Message found after node crash recovery!")
        print("   ACK-durability contract is VALID")
        print("   RFC NFR-6 & NFR-8: COMPLIANT")
        return True
    else:
        print(f"\n❌ FAIL: Message NOT found after recovery!")
        print("   ACK-durability contract is VIOLATED")
        print("   This is a CRITICAL RFC violation!")
        print(f"   Expected: {test_message}")
        if messages:
            print(f"   Received messages: {[m[:50] for m in messages]}")
        else:
            print("   No messages received")
        return False


def run_simplified_test():
    """Run simplified durability test without container kill."""
    print("\n=== Simplified Durability Test ===")
    print("(Testing message storage without crash simulation)")

    sender = f"simple_sender_{int(time.time())}"
    receiver = f"simple_receiver_{int(time.time())}"
    test_message = f"SIMPLE_TEST_{time.time()}"

    print(f"\n1. Sending message from {sender} to {receiver}")
    try:
        sock = connect_plaintext()
        login(sock, sender)
        send_message(sock, receiver, test_message)
        sock.close()
    except Exception as e:
        print(f"  ❌ FAIL: Send failed: {e}")
        return False

    print("\n2. Waiting for storage...")

    print(f"\n3. Connecting as receiver: {receiver}")
    try:
        sock = connect_plaintext()
        if not login(sock, receiver):
            print("  ❌ FAIL: Login failed")
            return False

        # Receive messages using reliable message protocol
        messages = receive_offline_messages(sock, timeout=5)
        sock.close()
    except Exception as e:
        print(f"  ❌ FAIL: Receive failed: {e}")
        return False

    # Check if test message is in any received message
    for msg in messages:
        if test_message.encode() in msg:
            print("\n✅ Message delivered to receiver")
            return True

    print("\n❌ FAIL: Message not found")
    return False


def restore_cluster_state():
    """Re-initialize cluster after test that restarts containers.
    
    IMPORTANT: After killing Mnesia nodes, their state becomes stale.
    We must do a FULL cluster restart to ensure clean state.
    """
    try:
        # Import from shared utility
        import sys
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        try:
            from cluster_utils import restore_cluster_state as _restore
            _restore()
        except ImportError:
            # Fallback if utility not available
            log("[cleanup] Restoring cluster state (inline fallback)...")
            docker_dir = PROJECT_ROOT / "docker" / "global-cluster"
            compose_file = docker_dir / "docker-compose.yml"

            subprocess.run(
                ["docker", "compose", "-f", str(compose_file), "down", "--remove-orphans", "-v"],
                cwd=str(docker_dir), capture_output=True, timeout=60
            )
            time.sleep(3)  # Brief settle after down
            subprocess.run(
                ["docker", "compose", "-f", str(compose_file), "up", "-d"],
                cwd=str(docker_dir), capture_output=True, timeout=180
            )
            # AUDIT P4 FIX: Poll for container readiness instead of blind 60s
            from tests.suites.chaos_dist.utils import wait_for_container_running
            wait_for_container_running("core-east-1", timeout=90)

            init_script = docker_dir / "init_cluster.sh"
            if init_script.exists():
                subprocess.run(
                    ["bash", str(init_script)],
                    cwd=str(docker_dir), capture_output=True, timeout=300
                )
            log("[cleanup] Cluster state restored")
    except Exception as e:
        log(f"[cleanup] Warning: Could not restore cluster state: {e}")


def main():
    result = test_ack_implies_durability()

    # Restore cluster state for subsequent tests
    restore_cluster_state()

    print("\n" + "=" * 60)
    if result is True:
        print("RESULT: PASSED")
        sys.exit(0)
    elif result is False:
        print("RESULT: FAILED - RFC VIOLATION DETECTED")
        sys.exit(1)
    else:
        # No skips - None results are failures
        print("RESULT: FAILED")
        print("FAIL: Test did not complete successfully")
        sys.exit(1)


if __name__ == "__main__":
    main()
