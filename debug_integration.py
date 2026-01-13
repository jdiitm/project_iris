
import sys
import os
import time
import subprocess

# Ensure we can import tests
sys.path.insert(0, os.getcwd())

from tests.framework.cluster import ClusterManager

def run_erl_eval(node_name, expr):
    """Run an Erlang expression on a node."""
    cmd = [
        "erl", "-noshell", 
        "-name", node_name, 
        "-setcookie", "iris_secret", 
        "-eval", f"io:format(\"~p\", [{expr}]), init:stop()."
    ]
    # In test env, nodes are sname?
    # ClusterManager uses sname.
    # So we should use sname.
    # But wait, we need to know the hidden node name to connect?
    # No, we can just use `erl -sname hidden ... -rpc ...`?
    # Or just `erl_call`? 
    # Let's use `erl -sname` and `rpc:call`.
    
    # We need a unique name for this debugger
    debug_name = f"debugger_{int(time.time())}"
    
    # Construct RPC command
    # RPC to node_name
    rpc_cmd = f"rpc:call('{node_name}', {expr.split(':')[0]}, {expr.split(':')[1].split('(')[0]}, {expr.split('(')[1].rstrip(')')})"
    # Actually, simpler:
    # rpc:call(Node, Module, Function, Args)
    
    # But parsing expr is hard.
    # Let's just assume expr is "mod:func(args)" and we run it via -eval on a connected node?
    # No, easier to just use `erl_call` if available?
    # Or `escript`?
    
    # Let's try raw rpc via net_adm:ping
    pass

def check_cluster_state():
    print("--- Starting Debug Cluster ---")
    cm = ClusterManager()
    
    try:
        # Start Cluster
        if not cm.start(edge_count=1):
            print("Failed to start cluster")
            return

        # Identify Nodes
        # ClusterManager uses specific naming convention
        suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
        if not suffix:
            # ClusterManager sets it? No, it reads env?
            # It uses make.
            # We need to grep the logs or ps to find the node names?
            # Or just set the suffix ourselves.
            suffix = f"_{int(time.time())}"
            os.environ["IRIS_NODE_SUFFIX"] = suffix
            
        hostname = subprocess.check_output(["hostname", "-s"], text=True).strip()
        core_node = f"iris_core{suffix}@{hostname}"
        edge_node = f"iris_edge1{suffix}@{hostname}"
        
        print(f"Target Nodes: {core_node}, {edge_node}")
        
        # Wait for nodes to be up (ClusterManager.start waits for ports)
        # But we need to wait for them to be pingable
        
        # Start a hidden node to interact
        my_name = f"debug{suffix}"
        
        def rpc(target, mod, func, args_str="[]"):
            cmd = [
                "erl", "-noshell", "-sname", my_name, 
                "-setcookie", "iris_secret",
                "-eval", f"net_adm:ping('{target}'), io:format('RESULT: ~p~n', [rpc:call('{target}', {mod}, {func}, {args_str})]), init:stop()."
            ]
            res = subprocess.run(cmd, capture_output=True, text=True)
            return res.stdout
            
        print("\n--- Checking Mesh ---")
        print(f"Core Nodes view: {rpc(core_node, 'erlang', 'nodes')}")
        print(f"Edge Nodes view: {rpc(edge_node, 'erlang', 'nodes')}")
        
        print("\n--- Checking PG ---")
        # Check Core Join
        print(f"Core PG Members (Local): {rpc(core_node, 'pg', 'get_members', '[iris_shards]')}")
        
        # Check Edge View of Core
        print(f"Edge PG Members (Remote): {rpc(edge_node, 'pg', 'get_members', '[iris_shards]')}")
        
        # Check Router Workers using PG
        # Can't easily inspect internal state of router, but PG is the key.
        
    finally:
        print("\n--- Shutting Down ---")
        cm.stop()

if __name__ == "__main__":
    check_cluster_state()
