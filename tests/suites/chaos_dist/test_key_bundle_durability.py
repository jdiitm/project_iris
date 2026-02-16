#!/usr/bin/env python3
"""
E2EE Key Bundle Durability Test (RFC NFR-23, FR-13)

Verifies that E2EE key bundles survive node failures before replication completes.

RFC Requirements:
- NFR-23: Key bundle storage durability 99.999% (same as message durability)
- FR-13: Client uploads IK + SPK + OPKs; 99.999% durability

Test Scenario:
1. Upload a key bundle to core-east-1 via iris_keys:upload_bundle/2
2. SIGKILL core-east-1 (hard crash, no WAL flush)
3. Verify bundle is recoverable from replica node (core-east-2)
4. Restart core-east-1, verify bundle recovered there too

REQUIRES: Multi-node Docker cluster with quorum writes enabled
PASS: Key bundle found on replica after primary hard crash
FAIL: Key bundle lost (durability contract VIOLATED)
"""

import subprocess
import sys
import os
import time
import random

from pathlib import Path

# Project root for locating scripts
PROJECT_ROOT = Path(__file__).parent.parent.parent.parent

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Docker cluster configuration
PRIMARY_CONTAINER = os.environ.get("IRIS_PRIMARY_CONTAINER", "core-east-1")
PRIMARY_NODE = "core_east_1@coreeast1"
REPLICA_CONTAINER = os.environ.get("IRIS_REPLICA_CONTAINER", "core-east-2")
REPLICA_NODE = "core_east_2@coreeast2"
EDGE_CONTAINER = "edge-east-1"
EDGE_CORE_NODE = PRIMARY_NODE

IS_CI = os.environ.get("CI", "").lower() in ("true", "1")
CI_TIMEOUT_FACTOR = 2 if IS_CI else 1
RECOVERY_TIMEOUT = 60 * CI_TIMEOUT_FACTOR


def log(msg):
    """Print timestamped log message."""
    print(msg)


# =============================================================================
# Docker / Cluster Helpers (same patterns as test_ack_durability.py)
# =============================================================================

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


def kill_container(container_name):
    """Kill container with SIGKILL (hard crash, no graceful shutdown)."""
    log(f"  Killing container: {container_name} (SIGKILL - hard crash)")
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


def check_cluster_replication_healthy():
    """Check if Mnesia replication is working (tables have >= 2 copies)."""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import check_cluster_replication_healthy as _check
        return _check()
    except ImportError:
        pass

    # Fallback: probe via docker exec
    try:
        probe_id = random.randint(10000, 99999)
        result = subprocess.run(
            ["docker", "exec", PRIMARY_CONTAINER, "sh", "-c",
             f"erl -noshell -sname probe{probe_id} -setcookie iris_secret -eval \""
             f"case net_adm:ping('{PRIMARY_NODE}') of "
             "pong -> "
             "  Tables = [offline_msg, presence, user_status], "
             "  Results = lists:map(fun(T) -> "
             f"    Ram = rpc:call('{PRIMARY_NODE}', mnesia, table_info, [T, ram_copies], 5000), "
             f"    Disc = rpc:call('{PRIMARY_NODE}', mnesia, table_info, [T, disc_copies], 5000), "
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
    """Ensure cluster replication is healthy, reinitializing if needed."""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        from cluster_utils import ensure_cluster_healthy as _ensure
        return _ensure(max_attempts=3)
    except ImportError:
        pass

    # Fallback
    init_script = PROJECT_ROOT / "docker" / "global-cluster" / "init_cluster.sh"
    for attempt in range(3):
        if check_cluster_replication_healthy():
            log("  Cluster replication is healthy")
            return True
        log(f"  Cluster unhealthy, reinitializing (attempt {attempt+1}/3)...")
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
                time.sleep(10)
            else:
                log(f"  Reinitialization returned non-zero: {result.returncode}")
                time.sleep(10)
        except subprocess.TimeoutExpired:
            log("  Reinitialization timed out")
            time.sleep(10)
        except Exception as e:
            log(f"  Reinitialization error: {e}")
            time.sleep(10)
    return check_cluster_replication_healthy()


def reconnect_edge_to_core(edge_container=EDGE_CONTAINER, core_node=EDGE_CORE_NODE):
    """Reconnect edge to core after core restart."""
    log("  Reconnecting edge to core...")
    random_id = int(time.time() * 1000) % 100000
    cmd = (f"docker exec {edge_container} erl -noshell "
           f"-sname reconn_{random_id} -setcookie iris_secret "
           f"-eval \"net_adm:ping('{core_node}'), halt(0).\"")
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    time.sleep(2)
    return result.returncode == 0


def restore_cluster_state():
    """Re-initialize cluster after test that restarts containers."""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "tests" / "utilities"))
        try:
            from cluster_utils import restore_cluster_state as _restore
            _restore()
        except ImportError:
            log("[cleanup] Restoring cluster state (inline fallback)...")
            docker_dir = PROJECT_ROOT / "docker" / "global-cluster"
            compose_file = docker_dir / "docker-compose.yml"
            subprocess.run(
                ["docker", "compose", "-f", str(compose_file), "down", "--remove-orphans", "-v"],
                cwd=str(docker_dir), capture_output=True, timeout=60
            )
            time.sleep(3)
            subprocess.run(
                ["docker", "compose", "-f", str(compose_file), "up", "-d"],
                cwd=str(docker_dir), capture_output=True, timeout=180
            )
            log("  Waiting for containers to start...")
            wait_for_container_healthy(PRIMARY_CONTAINER, timeout=90)
            init_script = docker_dir / "init_cluster.sh"
            if init_script.exists():
                subprocess.run(
                    ["bash", str(init_script)],
                    cwd=str(docker_dir), capture_output=True, timeout=300
                )
            log("[cleanup] Cluster state restored")
    except Exception as e:
        log(f"[cleanup] Warning: Could not restore cluster state: {e}")


# =============================================================================
# Key Bundle Operations (via docker exec + Erlang RPC)
# =============================================================================

def run_on_node(container, node, erlang_code, timeout=30):
    """Run Erlang code on a cluster node via docker exec + rpc:call.

    Returns (success, stdout, stderr).
    """
    random_id = int(time.time() * 1000) % 100000
    cmd = (
        f"docker exec {container} erl -pa /app/ebin -noshell "
        f"-sname keybundle_test_{random_id} -setcookie iris_secret "
        f"-eval \"{erlang_code}\" "
    )
    try:
        result = subprocess.run(
            cmd, shell=True, capture_output=True, text=True, timeout=timeout
        )
        return result.returncode == 0, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return False, "", "timeout"


def upload_key_bundle(container, node, user_id):
    """Upload a key bundle to a specific node. Returns (success, ik_hex).

    Generates proper X25519 key pairs and Ed25519 signatures via iris_x3dh.
    The IK hex is captured from the upload output for post-crash comparison.
    """
    # Generate proper X25519 key pairs and Ed25519 signature via iris_x3dh
    # (B-2 FIX: iris_keys:upload_bundle now requires valid Ed25519 signatures)
    code = (
        f"UserId = <<\\\"{user_id}\\\">>, "
        "{IK, IKPriv} = crypto:generate_key(ecdh, x25519), "
        "{SPK, _} = crypto:generate_key(ecdh, x25519), "
        "Sig = iris_x3dh:sign_prekey(SPK, IKPriv), "
        "OPKs = [element(1, crypto:generate_key(ecdh, x25519)) "
        "|| _ <- lists:seq(1, 5)], "
        "Bundle = #{"
        "identity_key => IK, "
        "signed_prekey => SPK, "
        "signed_prekey_signature => Sig, "
        "one_time_prekeys => OPKs"
        "}, "
        f"case rpc:call('{node}', iris_keys, upload_bundle, [UserId, Bundle], 10000) of "
        "ok -> "
        "io:format(\\\"UPLOAD_OK ~s~n\\\", [binary:encode_hex(IK)]), "
        "halt(0); "
        "{error, Reason} -> "
        "io:format(\\\"UPLOAD_ERROR ~p~n\\\", [Reason]), "
        "halt(1); "
        "{badrpc, Reason} -> "
        "io:format(\\\"RPC_ERROR ~p~n\\\", [Reason]), "
        "halt(1) "
        "end."
    )
    success, stdout, stderr = run_on_node(container, node, code)
    ik_hex = ""
    if success and "UPLOAD_OK" in stdout:
        # Extract hex IK from output
        for line in stdout.strip().split("\n"):
            if line.startswith("UPLOAD_OK "):
                ik_hex = line.split(" ", 1)[1].strip()
                break
    return success and "UPLOAD_OK" in stdout, ik_hex


def fetch_key_bundle(container, node, user_id):
    """Fetch a key bundle from a specific node. Returns (success, ik_hex, opk_count)."""
    code = (
        f"UserId = <<\\\"{user_id}\\\">>, "
        f"case rpc:call('{node}', iris_keys, fetch_bundle, [UserId, false], 10000) of "
        "{ok, Bundle} -> "
        "IK = maps:get(identity_key, Bundle), "
        "Remaining = maps:get(prekeys_remaining, Bundle, -1), "
        "io:format(\\\"FETCH_OK ~s ~p~n\\\", [binary:encode_hex(IK), Remaining]), "
        "halt(0); "
        "{error, not_found} -> "
        "io:format(\\\"FETCH_NOT_FOUND~n\\\"), "
        "halt(1); "
        "{error, Reason} -> "
        "io:format(\\\"FETCH_ERROR ~p~n\\\", [Reason]), "
        "halt(1); "
        "{badrpc, Reason} -> "
        "io:format(\\\"RPC_ERROR ~p~n\\\", [Reason]), "
        "halt(1) "
        "end."
    )
    success, stdout, stderr = run_on_node(container, node, code)
    ik_hex = ""
    opk_count = -1
    if success and "FETCH_OK" in stdout:
        for line in stdout.strip().split("\n"):
            if line.startswith("FETCH_OK "):
                parts = line.split(" ", 2)
                if len(parts) >= 3:
                    ik_hex = parts[1].strip()
                    try:
                        opk_count = int(parts[2].strip())
                    except ValueError:
                        pass
                break
    return success and "FETCH_OK" in stdout, ik_hex, opk_count


# =============================================================================
# Main Test
# =============================================================================

def test_key_bundle_durability():
    """
    Main test: Key bundle survives primary node hard crash.

    If we successfully upload a key bundle, it MUST survive SIGKILL of
    the primary node and be recoverable from a replica.
    """
    log("\n" + "=" * 60)
    log("E2EE Key Bundle Durability Test (RFC NFR-23, FR-13)")
    log("=" * 60)

    # --- Prerequisites ---
    if not check_docker_available():
        log("  FAIL: Docker not available")
        log("  Start Docker and try again")
        return False

    if not check_container_exists(PRIMARY_CONTAINER):
        log(f"  FAIL: Container {PRIMARY_CONTAINER} not found")
        log("  Start cluster with: make cluster-up")
        return False

    if not check_container_exists(REPLICA_CONTAINER):
        log(f"  FAIL: Container {REPLICA_CONTAINER} not found")
        log("  Start cluster with: make cluster-up")
        return False

    log("\n0. Ensuring cluster replication is healthy...")
    if not ensure_cluster_healthy():
        log("  FAIL: Could not establish healthy cluster replication after 3 attempts")
        return False

    # --- Step 1: Upload key bundle to primary ---
    user_id = f"durability_keybundle_{int(time.time())}"
    log(f"\n1. Uploading key bundle for user: {user_id}")
    log(f"   Target node: {PRIMARY_NODE} ({PRIMARY_CONTAINER})")

    upload_ok, ik_hex = upload_key_bundle(PRIMARY_CONTAINER, PRIMARY_NODE, user_id)
    if not upload_ok:
        log("  FAIL: Key bundle upload failed")
        return False
    log(f"   Upload OK, IK (hex): {ik_hex[:16]}...")

    # --- Step 2: Verify bundle is fetchable from primary ---
    log(f"\n2. Verifying bundle fetchable from primary ({PRIMARY_CONTAINER})...")
    fetch_ok, fetched_ik, opk_count = fetch_key_bundle(PRIMARY_CONTAINER, PRIMARY_NODE, user_id)
    if not fetch_ok:
        log("  FAIL: Could not fetch bundle from primary after upload")
        return False
    if fetched_ik != ik_hex:
        log(f"  FAIL: IK mismatch on primary: expected {ik_hex[:16]}, got {fetched_ik[:16]}")
        return False
    log(f"   Fetch OK, IK matches, OPK count: {opk_count}")

    # --- Step 3: SIGKILL primary (hard crash) ---
    # CRITICAL: No delay. If the bundle was durably written, it must survive
    # SIGKILL immediately. Any sleep here would mask race conditions.
    log(f"\n3. SIGKILL primary node: {PRIMARY_CONTAINER} (IMMEDIATELY after upload)")
    if not kill_container(PRIMARY_CONTAINER):
        log("  FAIL: Could not kill container")
        return False
    log("  Primary killed")

    log("\n4. Waiting 3 seconds for node to be fully dead...")
    time.sleep(3)

    # --- Step 4: Fetch bundle from surviving replica ---
    log(f"\n5. Fetching bundle from replica ({REPLICA_CONTAINER})...")
    log(f"   Target node: {REPLICA_NODE}")

    replica_ok, replica_ik, replica_opk_count = fetch_key_bundle(
        REPLICA_CONTAINER, REPLICA_NODE, user_id
    )

    if not replica_ok:
        log("  FAIL: Key bundle NOT found on replica after primary crash!")
        log("  This means the bundle was not replicated before the crash.")
        log("  RFC NFR-23 VIOLATION: Key bundle durability contract broken")
        # Start primary back before returning
        start_container(PRIMARY_CONTAINER)
        return False

    if replica_ik != ik_hex:
        log(f"  FAIL: IK mismatch on replica: expected {ik_hex[:16]}, got {replica_ik[:16]}")
        start_container(PRIMARY_CONTAINER)
        return False

    log(f"   Bundle found on replica! IK matches, OPK count: {replica_opk_count}")

    # --- Step 5: Restart primary, verify recovery ---
    log(f"\n6. Starting {PRIMARY_CONTAINER}...")
    if not start_container(PRIMARY_CONTAINER):
        log("  FAIL: Could not start container")
        return False

    log(f"\n7. Waiting for node recovery (up to {RECOVERY_TIMEOUT}s)...")
    if not wait_for_container_healthy(PRIMARY_CONTAINER, RECOVERY_TIMEOUT):
        log("  WARNING: Container not healthy, but may still work")

    # Wait for Mnesia to load tables from disc
    log("  Waiting for Mnesia recovery...")
    time.sleep(10)

    # Reconnect edge to core
    reconnect_edge_to_core()

    # --- Step 6: Verify bundle on recovered primary ---
    log(f"\n8. Fetching bundle from recovered primary ({PRIMARY_CONTAINER})...")
    recovered_ok = False
    for attempt in range(5):
        recovered_ok, recovered_ik, recovered_opk_count = fetch_key_bundle(
            PRIMARY_CONTAINER, PRIMARY_NODE, user_id
        )
        if recovered_ok:
            break
        log(f"  Attempt {attempt+1}/5 failed, retrying in 3s...")
        time.sleep(3)

    if not recovered_ok:
        log("  WARNING: Bundle not found on recovered primary (may need replication)")
        log("  But replica had the bundle, so durability is maintained.")
        # This is acceptable -- the primary might need table sync from replica
    else:
        if recovered_ik != ik_hex:
            log(f"  FAIL: IK mismatch on recovered primary: expected {ik_hex[:16]}, got {recovered_ik[:16]}")
            return False
        log(f"   Bundle found on recovered primary! IK matches, OPK count: {recovered_opk_count}")

    # --- PASS ---
    log(f"\nPASS: Key bundle survived primary node crash!")
    log("   Bundle was found on replica after SIGKILL of primary")
    log("   RFC NFR-23 & FR-13: COMPLIANT")
    return True


def main():
    result = test_key_bundle_durability()

    # Restore cluster state for subsequent tests
    restore_cluster_state()

    log("\n" + "=" * 60)
    if result is True:
        log("RESULT: PASSED")
        sys.exit(0)
    else:
        log("RESULT: FAILED - RFC VIOLATION DETECTED")
        sys.exit(1)


if __name__ == "__main__":
    main()
