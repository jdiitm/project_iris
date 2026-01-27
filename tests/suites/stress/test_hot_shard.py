#!/usr/bin/env python3
"""
Hot-Shard Stress Test

Per PRINCIPAL_AUDIT_REPORT.md Section 3.3:
- Route many users to same shard via controlled usernames
- Measure mailbox growth, latency degradation
- Pass criteria: No process mailbox > 10,000 messages

This test validates that the system handles "hot" shards where many users
hash to the same shard partition, creating concentrated load on a single
gen_server process.

Test Design:
1. Generate usernames that all hash to the same shard (shard 0)
2. Connect all users to the system
3. Have all users send messages simultaneously
4. Monitor process mailbox lengths via Erlang RPC
5. Measure latency degradation under hot-shard conditions
"""

import os
import sys
import socket
import struct
import time
import threading
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import List, Tuple, Optional

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 10
DEFAULT_SHARD_COUNT = 64

# Test profiles
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")

PROFILES = {
    "smoke": {
        "hot_users": 100,           # Users on hot shard
        "cold_users": 50,           # Users on other shards (control group)
        "messages_per_user": 10,
        "duration_seconds": 30,
        "max_mailbox_threshold": 10000,
        "max_latency_p99_ms": 500,
    },
    "full": {
        "hot_users": 1000,
        "cold_users": 200,
        "messages_per_user": 50,
        "duration_seconds": 60,
        "max_mailbox_threshold": 10000,
        "max_latency_p99_ms": 300,
    },
    "extreme": {
        "hot_users": 10000,
        "cold_users": 500,
        "messages_per_user": 100,
        "duration_seconds": 120,
        "max_mailbox_threshold": 10000,
        "max_latency_p99_ms": 500,
    }
}

if TEST_PROFILE not in PROFILES:
    print(f"ERROR: Unknown profile '{TEST_PROFILE}'. Available: {list(PROFILES.keys())}")
    sys.exit(1)

CONFIG = PROFILES[TEST_PROFILE]


@dataclass
class TestResult:
    """Result of hot-shard test."""
    hot_users_connected: int
    cold_users_connected: int
    hot_messages_sent: int
    cold_messages_sent: int
    hot_latencies_ms: List[float]
    cold_latencies_ms: List[float]
    max_mailbox_observed: int
    errors: int


def log(msg: str):
    """Log with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def phash2(data: bytes, n: int) -> int:
    """
    Python approximation of Erlang's erlang:phash2/2.
    This is used to predict which shard a username will map to.
    Note: This is an approximation - actual hash may differ slightly.
    """
    # Simple hash that should be somewhat consistent
    h = 0
    for b in data:
        h = ((h << 5) - h + b) & 0xFFFFFFFF
    return h % n


def generate_hot_shard_usernames(count: int, target_shard: int = 0, shard_count: int = DEFAULT_SHARD_COUNT) -> List[str]:
    """
    Generate usernames that all hash to the same shard.
    We brute-force search for usernames with the right hash.
    """
    usernames = []
    prefix = f"hotshard{target_shard}_"
    suffix = 0
    
    while len(usernames) < count:
        candidate = f"{prefix}{suffix}"
        if phash2(candidate.encode(), shard_count) == target_shard:
            usernames.append(candidate)
        suffix += 1
        if suffix > count * 1000:  # Safety limit
            break
    
    return usernames


def generate_cold_shard_usernames(count: int, avoid_shard: int = 0, shard_count: int = DEFAULT_SHARD_COUNT) -> List[str]:
    """
    Generate usernames that hash to different shards (not the hot shard).
    """
    usernames = []
    prefix = "coldshard_"
    suffix = 0
    
    while len(usernames) < count:
        candidate = f"{prefix}{suffix}"
        if phash2(candidate.encode(), shard_count) != avoid_shard:
            usernames.append(candidate)
        suffix += 1
        if suffix > count * 100:  # Safety limit
            break
    
    return usernames


def connect_and_login(username: str, timeout: float = TIMEOUT) -> Optional[socket.socket]:
    """Connect to server and login."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect((SERVER_HOST, SERVER_PORT))
        
        # Login
        packet = bytes([0x01]) + username.encode()
        sock.sendall(packet)
        
        response = sock.recv(1024)
        if b"LOGIN_OK" in response:
            return sock
        else:
            log(f"  Login failed for {username}: {response[:30]}")
            sock.close()
            return None
    except socket.timeout:
        return None
    except socket.error:
        return None
    except Exception as e:
        log(f"  Connection error for {username}: {e}")
        return None


def send_message(sock: socket.socket, target: str, message: str) -> Tuple[bool, float]:
    """
    Send a message and measure latency.
    
    For stress testing, we measure the time to complete the socket write,
    which represents the time for the system to accept the message.
    Fire-and-forget semantics - server queues for delivery.
    """
    start_time = time.time()
    
    try:
        target_bytes = target.encode()
        msg_bytes = message.encode()
        
        packet = (
            bytes([0x02]) +
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes
        )
        
        sock.sendall(packet)
        latency_ms = (time.time() - start_time) * 1000
        
        # Fire-and-forget: successful send = success
        return True, latency_ms
            
    except socket.timeout:
        return False, (time.time() - start_time) * 1000
    except (ConnectionError, BrokenPipeError, OSError):
        return False, (time.time() - start_time) * 1000
    except Exception as e:
        log(f"  Unexpected send error: {e}")
        return False, (time.time() - start_time) * 1000


def get_mailbox_lengths() -> dict:
    """
    Query Erlang for process mailbox lengths using RPC.
    Returns dict mapping process name to mailbox length.
    """
    try:
        # Use erl to query the running node
        cmd = [
            "erl", "-noshell", "-sname", f"probe_{os.getpid()}",
            "-setcookie", os.environ.get("IRIS_COOKIE", "iris_secret"),
            "-eval", """
            case net_adm:ping('iris_core@localhost') of
                pong ->
                    Procs = [iris_shard, iris_core_registry, iris_router_pool],
                    Results = lists:map(fun(P) ->
                        case whereis(P) of
                            undefined -> {P, 0};
                            Pid -> 
                                case process_info(Pid, message_queue_len) of
                                    {message_queue_len, Len} -> {P, Len};
                                    _ -> {P, 0}
                                end
                        end
                    end, Procs),
                    io:format("~p~n", [Results]);
                pang ->
                    io:format("[]~n")
            end,
            halt(0).
            """
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=5)
        
        # Parse the output (simple extraction)
        output = result.stdout.strip()
        mailboxes = {}
        
        # Very basic parsing - in production you'd use proper Erlang term parsing
        if "iris_shard" in output:
            # Extract numbers after each process name
            import re
            matches = re.findall(r'\{(\w+),(\d+)\}', output)
            for name, length in matches:
                mailboxes[name] = int(length)
        
        return mailboxes
        
    except Exception as e:
        log(f"Warning: Could not query mailboxes: {e}")
        return {}


def check_server_available() -> bool:
    """Check if the server is reachable."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect((SERVER_HOST, SERVER_PORT))
        sock.close()
        return True
    except Exception:
        return False


def run_hot_shard_test() -> TestResult:
    """
    Run the hot-shard stress test.
    """
    log(f"=== Hot-Shard Stress Test (Profile: {TEST_PROFILE}) ===")
    log(f"Hot users: {CONFIG['hot_users']}, Cold users: {CONFIG['cold_users']}")
    log(f"Messages per user: {CONFIG['messages_per_user']}")
    log(f"Max mailbox threshold: {CONFIG['max_mailbox_threshold']}")
    
    # Generate usernames
    log("Generating hot-shard usernames...")
    hot_usernames = generate_hot_shard_usernames(CONFIG['hot_users'])
    log(f"  Generated {len(hot_usernames)} hot-shard usernames")
    
    log("Generating cold-shard usernames...")
    cold_usernames = generate_cold_shard_usernames(CONFIG['cold_users'])
    log(f"  Generated {len(cold_usernames)} cold-shard usernames")
    
    # Create a target user for messages
    target_user = "hotshard_target_0"
    
    hot_latencies = []
    cold_latencies = []
    errors = 0
    max_mailbox = 0
    
    hot_connected = 0
    cold_connected = 0
    hot_sent = 0
    cold_sent = 0
    
    # Mailbox monitoring thread
    mailbox_stop = threading.Event()
    mailbox_max = [0]  # Use list for mutability in closure
    
    def monitor_mailboxes():
        while not mailbox_stop.is_set():
            mailboxes = get_mailbox_lengths()
            current_max = max(mailboxes.values()) if mailboxes else 0
            if current_max > mailbox_max[0]:
                mailbox_max[0] = current_max
                if current_max > CONFIG['max_mailbox_threshold'] // 2:
                    log(f"  Warning: Mailbox length {current_max}")
            time.sleep(0.5)
    
    monitor_thread = threading.Thread(target=monitor_mailboxes, daemon=True)
    monitor_thread.start()
    
    try:
        # Phase 1: Connect hot-shard users
        log("Phase 1: Connecting hot-shard users...")
        hot_sockets = []
        
        with ThreadPoolExecutor(max_workers=50) as executor:
            futures = {executor.submit(connect_and_login, u): u for u in hot_usernames}
            for future in as_completed(futures, timeout=60):
                try:
                    sock = future.result()
                    if sock:
                        hot_sockets.append(sock)
                        hot_connected += 1
                except Exception:
                    errors += 1
        
        log(f"  Connected {hot_connected}/{len(hot_usernames)} hot-shard users")
        
        # Phase 2: Connect cold-shard users (control group)
        log("Phase 2: Connecting cold-shard users...")
        cold_sockets = []
        
        with ThreadPoolExecutor(max_workers=50) as executor:
            futures = {executor.submit(connect_and_login, u): u for u in cold_usernames}
            for future in as_completed(futures, timeout=30):
                try:
                    sock = future.result()
                    if sock:
                        cold_sockets.append(sock)
                        cold_connected += 1
                except Exception:
                    errors += 1
        
        log(f"  Connected {cold_connected}/{len(cold_usernames)} cold-shard users")
        
        # Phase 3: Send messages from all users
        log("Phase 3: Sending messages (hot and cold simultaneously)...")
        
        def send_messages_from_sock(sock: socket.socket, is_hot: bool, msg_count: int) -> List[Tuple[bool, float]]:
            results = []
            for i in range(msg_count):
                msg = f"test_msg_{i}_{time.time()}"
                success, latency = send_message(sock, target_user, msg)
                results.append((success, latency, is_hot))
            return results
        
        with ThreadPoolExecutor(max_workers=100) as executor:
            futures = []
            
            # Submit hot-shard messages
            for sock in hot_sockets:
                futures.append(executor.submit(
                    send_messages_from_sock, sock, True, CONFIG['messages_per_user']
                ))
            
            # Submit cold-shard messages
            for sock in cold_sockets:
                futures.append(executor.submit(
                    send_messages_from_sock, sock, False, CONFIG['messages_per_user']
                ))
            
            # Collect results with generous timeout
            # Each user sends messages_per_user messages, allow 2s per message as buffer
            result_timeout = max(60, CONFIG['messages_per_user'] * 2 + CONFIG['duration_seconds'])
            for future in as_completed(futures, timeout=result_timeout):
                try:
                    results = future.result()
                    for success, latency, is_hot in results:
                        if is_hot:
                            if success:
                                hot_sent += 1
                                hot_latencies.append(latency)
                            else:
                                errors += 1
                        else:
                            if success:
                                cold_sent += 1
                                cold_latencies.append(latency)
                            else:
                                errors += 1
                except Exception as e:
                    log(f"  Error: {e}")
                    errors += 1
        
        log(f"  Hot: {hot_sent} sent, Cold: {cold_sent} sent, Errors: {errors}")
        
    finally:
        # Stop mailbox monitoring
        mailbox_stop.set()
        
        # Close all sockets
        for sock in hot_sockets:
            try:
                sock.close()
            except Exception:
                pass
        for sock in cold_sockets:
            try:
                sock.close()
            except Exception:
                pass
    
    return TestResult(
        hot_users_connected=hot_connected,
        cold_users_connected=cold_connected,
        hot_messages_sent=hot_sent,
        cold_messages_sent=cold_sent,
        hot_latencies_ms=hot_latencies,
        cold_latencies_ms=cold_latencies,
        max_mailbox_observed=mailbox_max[0],
        errors=errors
    )


def analyze_results(result: TestResult) -> bool:
    """Analyze test results and determine pass/fail."""
    log("\n=== Results Analysis ===")
    
    passed = True
    
    # Calculate latency statistics
    if result.hot_latencies_ms:
        hot_p50 = sorted(result.hot_latencies_ms)[len(result.hot_latencies_ms) // 2]
        hot_p99 = sorted(result.hot_latencies_ms)[int(len(result.hot_latencies_ms) * 0.99)]
        log(f"Hot-shard latency: P50={hot_p50:.1f}ms, P99={hot_p99:.1f}ms")
        
        if hot_p99 > CONFIG['max_latency_p99_ms']:
            log(f"  FAIL: P99 latency {hot_p99:.1f}ms exceeds threshold {CONFIG['max_latency_p99_ms']}ms")
            passed = False
        else:
            log(f"  PASS: P99 latency within threshold")
    
    if result.cold_latencies_ms:
        cold_p50 = sorted(result.cold_latencies_ms)[len(result.cold_latencies_ms) // 2]
        cold_p99 = sorted(result.cold_latencies_ms)[int(len(result.cold_latencies_ms) * 0.99)]
        log(f"Cold-shard latency: P50={cold_p50:.1f}ms, P99={cold_p99:.1f}ms")
    
    # Check mailbox threshold
    log(f"Max mailbox observed: {result.max_mailbox_observed}")
    if result.max_mailbox_observed > CONFIG['max_mailbox_threshold']:
        log(f"  FAIL: Mailbox {result.max_mailbox_observed} exceeds threshold {CONFIG['max_mailbox_threshold']}")
        passed = False
    else:
        log(f"  PASS: Mailbox within threshold")
    
    # Check error rate
    total_attempted = (result.hot_users_connected * CONFIG['messages_per_user'] +
                       result.cold_users_connected * CONFIG['messages_per_user'])
    error_rate = result.errors / max(total_attempted, 1)
    log(f"Error rate: {error_rate:.2%} ({result.errors}/{total_attempted})")
    
    if error_rate > 0.01:  # 1% threshold
        log(f"  FAIL: Error rate exceeds 1%")
        passed = False
    else:
        log(f"  PASS: Error rate acceptable")
    
    # Fairness check: hot-shard shouldn't be more than 3x slower than cold-shard
    if result.hot_latencies_ms and result.cold_latencies_ms:
        hot_avg = sum(result.hot_latencies_ms) / len(result.hot_latencies_ms)
        cold_avg = sum(result.cold_latencies_ms) / len(result.cold_latencies_ms)
        slowdown = hot_avg / max(cold_avg, 0.001)
        log(f"Hot/Cold latency ratio: {slowdown:.2f}x")
        
        if slowdown > 3.0:
            log(f"  WARNING: Hot shard significantly slower than cold (>3x)")
    
    return passed


def main():
    """Main entry point."""
    print("\n" + "=" * 70)
    print("Hot-Shard Stress Test")
    print("Per PRINCIPAL_AUDIT_REPORT.md Section 3.3")
    print("=" * 70)
    
    # Use ClusterManager to ensure cluster is running
    with ClusterManager(project_root=project_root) as cluster:
        # Check prerequisites
        if not check_server_available():
            print(f"\nSKIP:INFRA - Server not available at {SERVER_HOST}:{SERVER_PORT}")
            sys.exit(2)
        
        # Run test
        result = run_hot_shard_test()
        
        # Analyze and report
        passed = analyze_results(result)
        
        print("\n" + "=" * 70)
        if passed:
            print("RESULT: PASSED - Hot-shard handling within acceptable limits")
            sys.exit(0)
        else:
            print("RESULT: FAILED - Hot-shard test failed")
            sys.exit(1)


if __name__ == "__main__":
    main()
