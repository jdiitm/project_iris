"""
Cluster Management for Tests

Handles Iris cluster lifecycle: start, stop, health checks.
Designed for laptop/constrained environments.
"""

import os
import subprocess
import time
from pathlib import Path
from typing import Optional, List, Dict
import socket


class ClusterManager:
    """
    Manages Iris cluster lifecycle for test execution.
    
    Supports:
    - Starting core + edge nodes
    - Graceful shutdown
    - Health checks
    - Process cleanup
    """
    
    def __init__(
        self,
        project_root: Optional[Path] = None,
        core_port: int = 4369,  # Erlang EPMD
        edge_port: int = 8085,
        default_edge_count: int = 1
    ):
        if project_root:
            self.project_root = Path(project_root)
        else:
            self.project_root = Path(os.environ.get(
                "IRIS_PROJECT_ROOT",
                Path(__file__).parent.parent.parent
            ))
        
        self.core_port = core_port
        self.edge_port = edge_port
        self.default_edge_count = default_edge_count
        self._core_proc: Optional[subprocess.Popen] = None
        self._edge_procs: List[subprocess.Popen] = []
        self._hostname = self._get_hostname()
    
    def _get_hostname(self) -> str:
        """Get short hostname for Erlang node names."""
        try:
            result = subprocess.run(
                ["hostname", "-s"],
                capture_output=True,
                text=True,
                timeout=5
            )
            return result.stdout.strip()
        except Exception:
            return "localhost"
    
    def _run_make(self, target: str, timeout: int = 60) -> bool:
        """Run a make target."""
        try:
            result = subprocess.run(
                ["make", target],
                cwd=str(self.project_root),
                capture_output=True,
                text=True,
                timeout=timeout
            )
            return result.returncode == 0
        except Exception as e:
            print(f"Make {target} failed: {e}")
            return False
    
    def is_port_open(self, port: int, timeout: float = 1.0) -> bool:
        """Check if a port is accepting connections."""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(timeout)
            result = sock.connect_ex(('localhost', port))
            sock.close()
            return result == 0
        except Exception:
            return False
    
    def wait_for_port(self, port: int, timeout: int = 30) -> bool:
        """Wait for a port to become available."""
        start = time.time()
        while time.time() - start < timeout:
            if self.is_port_open(port):
                return True
            time.sleep(0.5)
        return False
    
    def build(self) -> bool:
        """Build the project."""
        print("[Cluster] Building project...")
        return self._run_make("all")
    
    def start_core(self, wait: bool = True) -> bool:
        """Start the core node."""
        print("[Cluster] Starting core node...")
        success = self._run_make("start_core", timeout=30)
        
        if wait and success:
            time.sleep(2)  # Give Mnesia time to initialize
        
        return success
    
    def start_edge(self, port: int = 8085, edge_id: int = 1, wait: bool = True) -> bool:
        """Start an edge node."""
        print(f"[Cluster] Starting edge node {edge_id} on port {port}...")
        
        make_target = f"start_edge{edge_id}"
        success = self._run_make(make_target, timeout=30)
        
        if wait and success:
            if self.wait_for_port(port, timeout=15):
                print(f"[Cluster] Edge {edge_id} ready on port {port}")
                return True
            else:
                print(f"[Cluster] Edge {edge_id} failed to start on port {port}")
                return False
        
        return success
    
    def start(self, edge_count: Optional[int] = None) -> bool:
        """Start full cluster (core + edges)."""
        count = edge_count if edge_count is not None else self.default_edge_count
        print(f"[Cluster] Starting cluster with {count} edges...")
        
        # Clean any existing processes
        self.force_stop()
        
        # Build
        if not self.build():
            print("[Cluster] Build failed")
            return False
        
        # Start core
        if not self.start_core():
            print("[Cluster] Core node failed to start")
            return False
        
        # Start edges
        for i in range(1, count + 1):
            port = 8085 + i - 1  # 8085, 8086, etc.
            if not self.start_edge(port=port, edge_id=i):
                print(f"[Cluster] Edge {i} failed to start")
                return False
            time.sleep(1)
        
        if not self._mesh_nodes(count):
            print("[Cluster] Warning: Mesh might be incomplete")

        print("[Cluster] Cluster started successfully")
        return True

    def _mesh_nodes(self, edge_count: int) -> bool:
        """Force mesh the cluster nodes via RPC ping."""
        print("[Cluster] Meshing nodes...")
        try:
            suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
            hostname = self._hostname
            core = f"iris_core{suffix}@{hostname}"
            
            for i in range(1, edge_count + 1):
                edge = f"iris_edge{i}{suffix}@{hostname}"
                cmd = [
                    "erl", "-noshell", "-sname", f"mesher_{i}",
                    "-setcookie", "iris_secret",
                    "-eval", f"io:format('~p', [rpc:call('{edge}', net_adm, ping, ['{core}'])]), init:stop()."
                ]
                # Try pinging
                res = subprocess.run(cmd, capture_output=True, text=True, timeout=5)
                if "pong" not in res.stdout:
                    print(f"[Cluster] Failed to mesh {edge} -> {core}: {res.stdout}")
            return True
        except Exception as e:
            print(f"[Cluster] Meshing failed: {e}")
            return False
    
    def stop(self) -> bool:
        """Gracefully stop the cluster."""
        print("[Cluster] Stopping cluster...")
        return self._run_make("stop", timeout=30)
    
    def force_stop(self):
        """Force stop all Erlang processes."""
        print("[Cluster] Force stopping all Erlang processes...")
        
        # Kill make stop first
        self._run_make("stop", timeout=10)
        
        # Kill any remaining beam.smp processes
        try:
            subprocess.run(
                ["killall", "beam.smp"],
                capture_output=True,
                timeout=10
            )
        except Exception:
            pass
        
        # Kill epmd
        try:
            subprocess.run(
                ["killall", "epmd"],
                capture_output=True,
                timeout=5
            )
        except Exception:
            pass
        
        # Clean up Mnesia directory
        try:
            import shutil
            for mnesia_dir in self.project_root.glob("Mnesia.*"):
                shutil.rmtree(mnesia_dir, ignore_errors=True)
            for log_file in self.project_root.glob("*.log"):
                try:
                    os.remove(log_file)
                except:
                    pass
        except Exception:
            pass
        
        time.sleep(1)
    
    def health_check(self) -> Dict[str, bool]:
        """Check health of cluster components."""
        return {
            "edge_8085": self.is_port_open(8085),
            "edge_8086": self.is_port_open(8086),
            "epmd": self.is_port_open(4369)
        }
    
    def is_healthy(self) -> bool:
        """Check if minimum cluster is healthy (core + 1 edge)."""
        health = self.health_check()
        return health.get("edge_8085", False)

    def wait_until(self, condition_func, timeout=30, description="condition"):
        """Wait until condition_func returns True."""
        start = time.time()
        while time.time() - start < timeout:
            if condition_func():
                return True
            time.sleep(0.5)
        print(f"[Cluster] Timeout waiting for {description}")
        return False

    def wait_for_log(self, filename: str, pattern: str, timeout=30) -> bool:
        """Wait for a pattern to appear in a log file."""
        log_path = self.project_root / filename
        start = time.time()
        
        while time.time() - start < timeout:
            if log_path.exists():
                try:
                    with open(log_path, 'r', errors='ignore') as f:
                        if pattern in f.read():
                            return True
                except Exception:
                    pass
            time.sleep(0.5)
        
        print(f"[Cluster] Timeout waiting for '{pattern}' in {filename}")
        return False

    def __enter__(self):
        """Context manager entry - start cluster."""
        if not self.start():
            raise RuntimeError("Cluster failed to start")
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - stop cluster."""
        self.stop()
        return False


# Convenience functions
_cluster: Optional[ClusterManager] = None


def get_cluster() -> ClusterManager:
    """Get the global cluster manager instance."""
    global _cluster
    if _cluster is None:
        _cluster = ClusterManager()
    return _cluster


def ensure_cluster() -> bool:
    """Ensure cluster is running, starting if needed."""
    cluster = get_cluster()
    if not cluster.is_healthy():
        return cluster.start()
    return True


def stop_cluster():
    """Stop the global cluster."""
    global _cluster
    if _cluster:
        _cluster.stop()

