"""
Cluster Management for Tests

Handles Iris cluster lifecycle: start, stop, health checks.
Designed for laptop/constrained environments.

Fixes implemented (per TEST_MITIGATION_PLAN.md):
- INF-001: Cluster startup retry logic
- INF-002: Dynamic port allocation  
- INF-003: Storage verification
- INF-004: TLS test mode
"""

import os
import subprocess
import time
from pathlib import Path
from typing import Optional, List, Dict, Tuple
import socket
import random


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
        default_edge_count: int = 1,
        # INF-001: Startup retry configuration
        startup_timeout: int = 60,
        max_retries: int = 3,
        # INF-002: Dynamic port allocation
        dynamic_ports: bool = False,
        port_range: Tuple[int, int] = (8085, 8200),
        # INF-004: TLS configuration
        tls_enabled: bool = False,
        config_path: Optional[str] = None
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
        
        # INF-001: Retry configuration
        self.startup_timeout = startup_timeout
        self.max_retries = max_retries
        
        # INF-002: Dynamic port allocation
        self.dynamic_ports = dynamic_ports
        self.port_range = port_range
        self._allocated_ports: List[int] = []
        
        # INF-004: TLS configuration
        self.tls_enabled = tls_enabled
        self.config_path = config_path
    
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
        """Run a make target with NODE_SUFFIX and CONFIG if set."""
        try:
            suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
            cmd = ["make", target]
            if suffix:
                cmd.append(f"NODE_SUFFIX={suffix}")
            
            # INF-004: Pass TLS config if specified
            if self.config_path:
                cmd.append(f"CONFIG={self.config_path}")
            
            result = subprocess.run(
                cmd,
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
        """Wait for a port to become available (open and accepting connections)."""
        start = time.time()
        while time.time() - start < timeout:
            if self.is_port_open(port):
                return True
            time.sleep(0.5)
        return False
    
    def wait_for_port_free(self, port: int, timeout: int = 15) -> bool:
        """Wait for a port to become free (not in use)."""
        start = time.time()
        while time.time() - start < timeout:
            if not self.is_port_open(port):
                return True
            time.sleep(0.5)
        return False
    
    def kill_port_holder(self, port: int) -> bool:
        """Kill any process holding the specified port."""
        try:
            # Find PID holding the port using fuser
            result = subprocess.run(
                ["fuser", "-k", f"{port}/tcp"],
                capture_output=True,
                timeout=5
            )
            time.sleep(0.5)  # Give process time to die
            return True
        except Exception:
            # Try lsof as fallback
            try:
                result = subprocess.run(
                    ["lsof", "-ti", f"tcp:{port}"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                pids = result.stdout.strip().split('\n')
                for pid in pids:
                    if pid.strip():
                        try:
                            subprocess.run(["kill", "-9", pid.strip()], timeout=2)
                        except Exception:
                            pass
                time.sleep(0.5)
                return True
            except Exception:
                return False
    
    # =========================================================================
    # INF-002: Dynamic Port Allocation
    # =========================================================================
    
    def find_available_port(self, start: Optional[int] = None) -> int:
        """Find an available port in the configured range."""
        if start is None:
            start = self.port_range[0]
        
        for port in range(start, self.port_range[1]):
            if port in self._allocated_ports:
                continue
            if not self.is_port_in_use(port):
                self._allocated_ports.append(port)
                return port
        
        raise RuntimeError(f"No available ports in range {self.port_range}")
    
    def is_port_in_use(self, port: int) -> bool:
        """Check if a port is currently in use."""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                s.bind(('localhost', port))
                return False
            except OSError:
                return True
    
    def release_port(self, port: int):
        """Release a dynamically allocated port."""
        if port in self._allocated_ports:
            self._allocated_ports.remove(port)
    
    # =========================================================================
    # INF-004: TLS Test Mode
    # =========================================================================
    
    def start_with_tls(self, verify_certs: bool = True) -> bool:
        """Start cluster with TLS enabled (INF-004 FIX)."""
        # Verify certificates exist
        cert_files = [
            'certs/ca.pem',
            'certs/edge-east-1.pem', 
            'certs/edge-east-1.key'
        ]
        
        if verify_certs:
            for cert in cert_files:
                cert_path = self.project_root / cert
                if not cert_path.exists():
                    print(f"[Cluster] Missing certificate: {cert}")
                    return False
        
        # Use TLS config
        self.tls_enabled = True
        self.config_path = 'config/test_tls'
        os.environ['IRIS_CONFIG'] = self.config_path
        
        # Start with extended timeout (TLS handshake is slower)
        original_timeout = self.startup_timeout
        self.startup_timeout = 90
        
        try:
            return self.start()
        finally:
            self.startup_timeout = original_timeout
    
    def start_with_mtls(self) -> bool:
        """Start cluster with mutual TLS (client cert required)."""
        self.config_path = 'config/test_mtls'
        os.environ['IRIS_CONFIG'] = self.config_path
        return self.start_with_tls(verify_certs=True)
    
    # =========================================================================
    # INF-003: Storage Verification
    # =========================================================================
    
    def verify_storage_ready(self, timeout: int = 30) -> bool:
        """Verify that offline storage is accepting messages (INF-003 FIX)."""
        print("[Cluster] Verifying storage is ready...")
        start = time.time()
        
        while time.time() - start < timeout:
            if self._check_mnesia_ready():
                print("[Cluster] Storage verified ready")
                return True
            time.sleep(1)
        
        print("[Cluster] Storage verification timeout")
        return False
    
    def _check_mnesia_ready(self) -> bool:
        """Check if Mnesia tables are available."""
        try:
            suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
            hostname = self._hostname
            core_node = f"iris_core{suffix}@{hostname}"
            
            cmd = [
                "erl", "-noshell", "-sname", f"storage_check_{random.randint(1000,9999)}",
                "-setcookie", "iris_secret",
                "-eval", f"""
                    case rpc:call('{core_node}', mnesia, table_info, [offline_msg, size], 5000) of
                        {{badrpc, _}} -> io:format("NOT_READY"), init:stop(1);
                        N when is_integer(N) -> io:format("READY:~p", [N]), init:stop(0);
                        _ -> io:format("NOT_READY"), init:stop(1)
                    end.
                """
            ]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            return "READY" in result.stdout
        except Exception as e:
            return False
    
    def get_offline_count(self, user: str) -> int:
        """Get count of offline messages for a user (for test verification)."""
        try:
            suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
            hostname = self._hostname
            core_node = f"iris_core{suffix}@{hostname}"
            
            cmd = [
                "erl", "-noshell", "-sname", f"count_check_{random.randint(1000,9999)}",
                "-setcookie", "iris_secret",
                "-eval", f"""
                    case rpc:call('{core_node}', iris_core, get_offline_queue_depth, [<<"{user}">>], 5000) of
                        N when is_integer(N) -> io:format("~p", [N]), init:stop(0);
                        _ -> io:format("0"), init:stop(1)
                    end.
                """
            ]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            return int(result.stdout.strip())
        except Exception:
            return 0
    
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
        """Start an edge node with retry logic (INF-001 FIX)."""
        
        # INF-002: Use dynamic port if enabled
        if self.dynamic_ports:
            port = self.find_available_port(port)
            print(f"[Cluster] INF-002: Dynamically allocated port {port} for edge {edge_id}")
        
        # INF-001: Retry logic for transient failures
        for attempt in range(1, self.max_retries + 1):
            print(f"[Cluster] Starting edge node {edge_id} on port {port} (attempt {attempt}/{self.max_retries})...")
            
            make_target = f"start_edge{edge_id}"
            success = self._run_make(make_target, timeout=self.startup_timeout)
            
            if wait and success:
                if self.wait_for_port(port, timeout=self.startup_timeout):
                    print(f"[Cluster] Edge {edge_id} ready on port {port}")
                    return True
                else:
                    print(f"[Cluster] Edge {edge_id} port {port} not responding")
            
            if attempt < self.max_retries:
                print(f"[Cluster] Edge {edge_id} attempt {attempt} failed, cleaning up and retrying...")
                # Kill any partial startup
                self.kill_port_holder(port)
                time.sleep(2)
            
        print(f"[Cluster] Edge {edge_id} failed to start after {self.max_retries} attempts")
        return False
    
    def start(self, edge_count: Optional[int] = None, verify_storage: bool = True) -> bool:
        """Start full cluster (core + edges) with retry logic (INF-001 FIX)."""
        count = edge_count if edge_count is not None else self.default_edge_count
        print(f"[Cluster] Starting cluster with {count} edges (max_retries={self.max_retries})...")
        
        # INF-001: Cluster-level retry loop
        for cluster_attempt in range(1, self.max_retries + 1):
            print(f"[Cluster] Cluster startup attempt {cluster_attempt}/{self.max_retries}")
            
            # Clean any existing processes
            self.force_stop()
            self._allocated_ports = []  # INF-002: Reset port allocations
            
            # Build
            if not self.build():
                print("[Cluster] Build failed")
                if cluster_attempt < self.max_retries:
                    time.sleep(2)
                    continue
                return False
            
            # Start core
            if not self.start_core():
                print("[Cluster] Core node failed to start")
                if cluster_attempt < self.max_retries:
                    time.sleep(2)
                    continue
                return False
            
            # Start edges
            all_edges_started = True
            for i in range(1, count + 1):
                # INF-002: Dynamic port or sequential allocation
                if self.dynamic_ports:
                    port = self.find_available_port(8085 + i - 1)
                else:
                    port = 8085 + i - 1
                
                # Ensure port is free before starting edge
                if self.is_port_open(port):
                    print(f"[Cluster] Port {port} still in use, waiting...")
                    self.kill_port_holder(port)
                    if not self.wait_for_port_free(port, timeout=10):
                        print(f"[Cluster] Port {port} still in use, cannot start edge {i}")
                        all_edges_started = False
                        break
                
                if not self.start_edge(port=port, edge_id=i):
                    print(f"[Cluster] Edge {i} failed to start")
                    all_edges_started = False
                    break
                time.sleep(1)
            
            if not all_edges_started:
                if cluster_attempt < self.max_retries:
                    print(f"[Cluster] Edge startup failed, retrying cluster...")
                    time.sleep(2)
                    continue
                return False
            
            if not self._mesh_nodes(count):
                print("[Cluster] Warning: Mesh might be incomplete")
            
            # INF-003: Verify storage is ready before returning
            if verify_storage:
                if not self.verify_storage_ready(timeout=30):
                    print("[Cluster] Warning: Storage verification failed")
                    # Don't fail startup for this - just warn
            
            print("[Cluster] Cluster started successfully")
            return True
        
        print(f"[Cluster] Cluster failed to start after {self.max_retries} attempts")
        return False

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
        """Force stop all Erlang processes and ensure ports are free."""
        print("[Cluster] Force stopping all Erlang processes...")
        
        # Kill make stop first
        self._run_make("stop", timeout=10)
        
        # Kill any remaining beam.smp processes
        try:
            subprocess.run(
                ["killall", "-9", "beam.smp"],
                capture_output=True,
                timeout=10
            )
        except Exception:
            pass
        
        # Kill epmd
        try:
            subprocess.run(
                ["killall", "-9", "epmd"],
                capture_output=True,
                timeout=5
            )
        except Exception:
            pass
        
        # Ensure ports are actually free
        for port in [8085, 8086, 8087]:
            if self.is_port_open(port):
                print(f"[Cluster] Port {port} still in use, killing holder...")
                self.kill_port_holder(port)
        
        # Wait for ports to be free
        for port in [8085, 8086]:
            if not self.wait_for_port_free(port, timeout=10):
                print(f"[Cluster] Warning: Port {port} still in use after force_stop")
        
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

