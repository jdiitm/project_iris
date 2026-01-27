#!/usr/bin/env python3
"""
Shared cluster management utilities for chaos/distributed tests.

This module provides robust functions for:
- Checking cluster health
- Restoring cluster state after disruptive tests
- Ensuring cluster is ready before tests run

IMPORTANT: After tests that kill Mnesia nodes, their state becomes stale.
A full cluster restart (docker compose down -v && up) is required to ensure
clean state for subsequent tests.
"""

import subprocess
import time
from pathlib import Path


# Project root for locating scripts
PROJECT_ROOT = Path(__file__).parent.parent.parent


def log(msg: str):
    """Print log message."""
    print(msg, flush=True)


def get_docker_dir() -> Path:
    """Get Docker cluster directory."""
    return PROJECT_ROOT / "docker" / "global-cluster"


def get_compose_file() -> Path:
    """Get Docker compose file path."""
    return get_docker_dir() / "docker-compose.yml"


def get_init_script() -> Path:
    """Get cluster init script path."""
    return get_docker_dir() / "init_cluster.sh"


def check_cluster_replication_healthy() -> bool:
    """Check if Mnesia replication is working (tables have >= 2 copies).
    
    Returns True if cluster replication appears healthy, False otherwise.
    """
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
        return False


def ensure_cluster_healthy(max_attempts: int = 3) -> bool:
    """Ensure cluster replication is healthy, reinitializing if needed.
    
    Args:
        max_attempts: Maximum number of reinitialization attempts
        
    Returns:
        True if cluster is healthy, False if all attempts failed.
    """
    init_script = get_init_script()
    
    for attempt in range(max_attempts):
        if check_cluster_replication_healthy():
            log(f"  Cluster replication is healthy")
            return True
        
        log(f"  Cluster unhealthy, reinitializing (attempt {attempt+1}/{max_attempts})...")
        
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
                time.sleep(20)
            else:
                log(f"  Reinitialization returned non-zero: {result.returncode}")
                # Print last few lines for debugging
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


def full_cluster_restart(wait_time: int = 60) -> bool:
    """Perform a FULL cluster restart to clear all Mnesia state.
    
    This is the nuclear option - stops all containers, removes volumes,
    and starts fresh. Use after tests that corrupt cluster state.
    
    Args:
        wait_time: Seconds to wait for containers to be healthy
        
    Returns:
        True if cluster started and initialized successfully, False otherwise.
    """
    try:
        docker_dir = get_docker_dir()
        compose_file = get_compose_file()
        init_script = get_init_script()
        
        log("[cluster] Performing full cluster restart (clearing Mnesia state)...")
        
        # Stop and remove all containers AND volumes
        log("[cluster] Stopping and removing containers...")
        subprocess.run(
            ["docker", "compose", "-f", str(compose_file), "down", "--remove-orphans", "-v"],
            cwd=str(docker_dir),
            capture_output=True,
            timeout=120
        )
        time.sleep(5)
        
        # Start fresh
        log("[cluster] Starting fresh cluster...")
        result = subprocess.run(
            ["docker", "compose", "-f", str(compose_file), "up", "-d"],
            cwd=str(docker_dir),
            capture_output=True,
            text=True,
            timeout=180
        )
        
        if result.returncode != 0:
            log(f"[cluster] Docker compose up failed: {result.returncode}")
            return False
        
        # Wait for containers to be healthy
        log(f"[cluster] Waiting for containers to be healthy ({wait_time}s)...")
        time.sleep(wait_time)
        
        # Run init script to set up replication
        if init_script.exists():
            log("[cluster] Running init_cluster.sh...")
            result = subprocess.run(
                ["bash", str(init_script)],
                cwd=str(docker_dir),
                capture_output=True,
                text=True,
                timeout=300  # Allow 5 minutes for full init
            )
            if result.returncode == 0:
                log("[cluster] Full cluster restart complete - cluster is ready")
                return True
            else:
                log(f"[cluster] init_cluster.sh returned {result.returncode}")
                # Log last few lines
                for line in result.stdout.strip().split('\n')[-5:]:
                    log(f"[cluster]   {line}")
                return False
        else:
            log("[cluster] Init script not found - cluster may not be fully configured")
            return False
            
    except subprocess.TimeoutExpired:
        log("[cluster] Full restart timed out")
        return False
    except Exception as e:
        log(f"[cluster] Full restart failed: {e}")
        return False


def restore_cluster_state():
    """Re-initialize cluster after test that restarts/kills containers.
    
    IMPORTANT: After killing Mnesia nodes, their state becomes stale.
    We must do a FULL cluster restart to ensure clean state.
    """
    try:
        log("[cleanup] Restoring cluster state after disruptive test...")
        
        # Always do a full restart after tests that kill containers
        # This is slower but guarantees clean Mnesia state
        if full_cluster_restart(wait_time=60):
            log("[cleanup] Cluster state fully restored")
        else:
            log("[cleanup] Warning: Full cluster restart had issues")
            
    except Exception as e:
        log(f"[cleanup] Warning: Could not restore cluster state: {e}")


def quick_cluster_check() -> bool:
    """Quick check if cluster is accessible (doesn't verify replication).
    
    Returns True if at least one edge is responding, False otherwise.
    """
    import socket
    
    edges = [
        ("localhost", 8085),  # edge-east-1
        ("localhost", 8086),  # edge-east-2
        ("localhost", 8087),  # edge-west-1
    ]
    
    for host, port in edges:
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(2)
            result = sock.connect_ex((host, port))
            sock.close()
            if result == 0:
                return True
        except Exception:
            pass
    
    return False
