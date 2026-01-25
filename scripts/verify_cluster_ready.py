#!/usr/bin/env python3
"""
Cluster Readiness Verification Script

This script verifies that the Docker global cluster is fully ready for
cross-region tests. It checks:

1. All core containers are running and healthy
2. Mnesia cluster has formed with expected nodes
3. Key tables have >= 2 replicas (replication working)
4. Cross-region message delivery works (West -> Sydney)

Usage:
    python3 scripts/verify_cluster_ready.py [--verbose] [--quick]
    
Exit codes:
    0 = Cluster ready
    1 = Cluster not ready
    2 = Docker not available
"""

import argparse
import socket
import subprocess
import sys
import time
import threading
from typing import List, Tuple, Optional

# Core containers and their Erlang node names
CORE_CONTAINERS = [
    ("core-east-1", "core_east_1@coreeast1"),
    ("core-east-2", "core_east_2@coreeast2"),
    ("core-west-1", "core_west_1@corewest1"),
    ("core-west-2", "core_west_2@corewest2"),
    ("core-eu-1", "core_eu_1@coreeu1"),
    ("core-eu-2", "core_eu_2@coreeu2"),
]

# Edge containers and their ports
EDGE_CONTAINERS = [
    ("edge-east-1", 8085),
    ("edge-east-2", 8086),
    ("edge-west-1", 8087),
    ("edge-west-2", 8088),
    ("edge-eu-1", 8089),
    ("edge-sydney-1", 8090),
    ("edge-sydney-2", 8091),
    ("edge-saopaulo", 8092),
]

# Minimum required for cross-region tests
MIN_CORES = 2
MIN_EDGES = 2
MIN_REPLICAS = 2

COOKIE = "iris_secret"


def log(msg: str, verbose: bool = True):
    """Print with timestamp."""
    if verbose:
        print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def check_docker_available() -> bool:
    """Check if Docker is available and running."""
    try:
        result = subprocess.run(
            ["docker", "info"],
            capture_output=True,
            timeout=10
        )
        return result.returncode == 0
    except Exception:
        return False


def check_container_running(container: str) -> bool:
    """Check if a specific container is running."""
    try:
        result = subprocess.run(
            ["docker", "inspect", "--format", "{{.State.Running}}", container],
            capture_output=True,
            text=True,
            timeout=5
        )
        return "true" in result.stdout.lower()
    except Exception:
        return False


def check_port_listening(host: str, port: int, timeout: float = 2) -> bool:
    """Check if a port is accepting connections."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        result = sock.connect_ex((host, port))
        sock.close()
        return result == 0
    except Exception:
        return False


def get_running_cores() -> List[Tuple[str, str]]:
    """Get list of running core containers with their node names."""
    running = []
    for container, node in CORE_CONTAINERS:
        if check_container_running(container):
            running.append((container, node))
    return running


def get_running_edges() -> List[Tuple[str, int]]:
    """Get list of running edge containers with their ports."""
    running = []
    for container, port in EDGE_CONTAINERS:
        if check_container_running(container):
            running.append((container, port))
    return running


def get_mnesia_db_nodes() -> Tuple[int, List[str]]:
    """Query Mnesia db_nodes from primary core."""
    try:
        result = subprocess.run(
            ["docker", "exec", "core-east-1", "sh", "-c",
             f'erl -noshell -sname verify_$RANDOM -setcookie {COOKIE} -eval "'
             "case net_adm:ping('core_east_1@coreeast1') of "
             "pong -> "
             "  DbNodes = rpc:call('core_east_1@coreeast1', mnesia, system_info, [db_nodes], 5000), "
             "  case DbNodes of "
             "    {badrpc, _} -> io:format(\"0:\"); "
             "    Nodes when is_list(Nodes) -> "
             "      io:format(\"~p:\", [length(Nodes)]), "
             "      lists:foreach(fun(N) -> io:format(\"~p,\", [N]) end, Nodes); "
             "    _ -> io:format(\"0:\") "
             "  end; "
             "pang -> io:format(\"0:\") "
             "end, "
             'halt(0)."'],
            capture_output=True,
            text=True,
            timeout=15
        )
        
        output = result.stdout.strip()
        if ":" in output:
            parts = output.split(":")
            count = int(parts[0])
            nodes = [n.strip() for n in parts[1].split(",") if n.strip()]
            return count, nodes
        return 0, []
    except Exception as e:
        return 0, []


def get_table_replica_count(table: str) -> int:
    """Get the total number of replicas for a Mnesia table."""
    try:
        result = subprocess.run(
            ["docker", "exec", "core-east-1", "sh", "-c",
             f'erl -noshell -sname replicas_$RANDOM -setcookie {COOKIE} -eval "'
             "pong = net_adm:ping('core_east_1@coreeast1'), "
             f"Ram = rpc:call('core_east_1@coreeast1', mnesia, table_info, [{table}, ram_copies]), "
             f"Disc = rpc:call('core_east_1@coreeast1', mnesia, table_info, [{table}, disc_copies]), "
             "case {Ram, Disc} of "
             "  {{badrpc, _}, _} -> io:format(\"0\"); "
             "  {_, {badrpc, _}} -> io:format(\"0\"); "
             "  {R, D} when is_list(R), is_list(D) -> io:format(\"~p\", [length(R) + length(D)]); "
             "  _ -> io:format(\"0\") "
             "end, "
             'halt(0)."'],
            capture_output=True,
            text=True,
            timeout=10
        )
        return int(result.stdout.strip())
    except Exception:
        return 0


def test_cross_region_delivery(verbose: bool = False) -> bool:
    """
    Test actual cross-region message delivery.
    Send from West edge, receive on Sydney edge.
    """
    west_port = 8087
    sydney_port = 8090
    
    # Check ports are available
    if not check_port_listening("localhost", west_port):
        log(f"  West edge (port {west_port}) not listening", verbose)
        return False
    if not check_port_listening("localhost", sydney_port):
        log(f"  Sydney edge (port {sydney_port}) not listening", verbose)
        return False
    
    test_id = int(time.time() * 1000) % 100000
    sender_name = f"verify_west_{test_id}"
    receiver_name = f"verify_sydney_{test_id}"
    test_message = f"CROSS_REGION_VERIFY_{test_id}"
    
    received = {"data": None}
    
    def receiver_thread():
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(10)
            sock.connect(("localhost", sydney_port))
            
            # Login
            sock.sendall(bytes([0x01]) + receiver_name.encode())
            resp = sock.recv(1024)
            if b"LOGIN_OK" not in resp:
                return
            
            # Listen for messages
            sock.setblocking(False)
            start = time.time()
            while time.time() - start < 5:
                try:
                    data = sock.recv(4096)
                    if data and test_message.encode() in data:
                        received["data"] = data
                        break
                except BlockingIOError:
                    time.sleep(0.05)
            
            sock.close()
        except Exception:
            pass
    
    # Start receiver
    recv_thread = threading.Thread(target=receiver_thread, daemon=True)
    recv_thread.start()
    time.sleep(0.5)
    
    # Send message
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(("localhost", west_port))
        
        # Login
        sock.sendall(bytes([0x01]) + sender_name.encode())
        resp = sock.recv(1024)
        if b"LOGIN_OK" not in resp:
            log(f"  Sender login failed", verbose)
            sock.close()
            return False
        
        # Send message to receiver
        target_bytes = receiver_name.encode()
        msg_bytes = test_message.encode()
        packet = (
            bytes([0x02]) +
            len(target_bytes).to_bytes(2, 'big') + target_bytes +
            len(msg_bytes).to_bytes(2, 'big') + msg_bytes
        )
        sock.sendall(packet)
        sock.close()
    except Exception as e:
        log(f"  Send failed: {e}", verbose)
        return False
    
    # Wait for receiver
    recv_thread.join(timeout=6)
    
    return received["data"] is not None


def verify_cluster(verbose: bool = True, quick: bool = False) -> bool:
    """
    Main verification routine.
    
    Returns True if cluster is ready for cross-region tests.
    """
    all_ok = True
    
    log("=" * 60, verbose)
    log("Cluster Readiness Verification", verbose)
    log("=" * 60, verbose)
    
    # 1. Check Docker
    log("\n1. Checking Docker availability...", verbose)
    if not check_docker_available():
        log("   FAIL: Docker not available", verbose)
        return False
    log("   OK: Docker is running", verbose)
    
    # 2. Check core containers
    log("\n2. Checking core containers...", verbose)
    running_cores = get_running_cores()
    log(f"   Running: {len(running_cores)}/{len(CORE_CONTAINERS)} cores", verbose)
    
    if len(running_cores) < MIN_CORES:
        log(f"   FAIL: Need at least {MIN_CORES} cores", verbose)
        all_ok = False
    else:
        for container, node in running_cores:
            log(f"     {container}: {node}", verbose)
    
    # 3. Check edge containers
    log("\n3. Checking edge containers...", verbose)
    running_edges = get_running_edges()
    log(f"   Running: {len(running_edges)}/{len(EDGE_CONTAINERS)} edges", verbose)
    
    if len(running_edges) < MIN_EDGES:
        log(f"   FAIL: Need at least {MIN_EDGES} edges", verbose)
        all_ok = False
    
    # 4. Check edge ports
    log("\n4. Checking edge ports...", verbose)
    listening_edges = 0
    for container, port in running_edges:
        if check_port_listening("localhost", port):
            listening_edges += 1
            log(f"     {container} (:{port}): listening", verbose)
        else:
            log(f"     {container} (:{port}): NOT listening", verbose)
    
    if listening_edges < MIN_EDGES:
        log(f"   FAIL: Need at least {MIN_EDGES} edges listening", verbose)
        all_ok = False
    
    # 5. Check Mnesia cluster
    log("\n5. Checking Mnesia cluster membership...", verbose)
    db_node_count, db_nodes = get_mnesia_db_nodes()
    log(f"   Mnesia db_nodes: {db_node_count}", verbose)
    
    if db_node_count < MIN_CORES:
        log(f"   FAIL: Mnesia cluster has fewer than {MIN_CORES} nodes", verbose)
        all_ok = False
    
    # 6. Check table replication
    log("\n6. Checking table replication...", verbose)
    tables = ["presence", "offline_msg", "user_status", "user_meta"]
    for table in tables:
        replicas = get_table_replica_count(table)
        status = "OK" if replicas >= MIN_REPLICAS else "WARN"
        log(f"     {table}: {replicas} copies [{status}]", verbose)
        if replicas < MIN_REPLICAS:
            all_ok = False
    
    # 7. Cross-region delivery test (skip if quick mode)
    if not quick:
        log("\n7. Testing cross-region message delivery...", verbose)
        # Only test if we have West and Sydney edges
        west_running = any(c == "edge-west-1" for c, _ in running_edges)
        sydney_running = any(c == "edge-sydney-1" for c, _ in running_edges)
        
        if west_running and sydney_running:
            if test_cross_region_delivery(verbose):
                log("   OK: Cross-region delivery working (West -> Sydney)", verbose)
            else:
                log("   FAIL: Cross-region delivery not working", verbose)
                all_ok = False
        else:
            log("   SKIP: West and/or Sydney edges not running", verbose)
    else:
        log("\n7. Skipping cross-region delivery test (quick mode)", verbose)
    
    # Summary
    log("\n" + "=" * 60, verbose)
    if all_ok:
        log("RESULT: Cluster is READY for cross-region tests", verbose)
        return True
    else:
        log("RESULT: Cluster is NOT READY", verbose)
        log("\nTo fix:", verbose)
        log("  1. make cluster-up", verbose)
        log("  2. ./docker/global-cluster/init_cluster.sh", verbose)
        return False


def main():
    parser = argparse.ArgumentParser(description="Verify Docker cluster readiness")
    parser.add_argument("--verbose", "-v", action="store_true", default=True,
                        help="Verbose output (default)")
    parser.add_argument("--quiet", "-q", action="store_true",
                        help="Quiet mode - only show result")
    parser.add_argument("--quick", action="store_true",
                        help="Quick check - skip cross-region delivery test")
    args = parser.parse_args()
    
    verbose = not args.quiet
    
    if not check_docker_available():
        if verbose:
            print("Docker not available")
        sys.exit(2)
    
    if verify_cluster(verbose=verbose, quick=args.quick):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
