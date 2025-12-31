#!/usr/bin/env python3
import subprocess
import time
import os
import sys
import threading
import signal

# --- Configuration ---
CORE_NODE = "iris_core"
EDGE_NODE = "iris_edge1"
USER_COUNT = 200000  # 200k Users
OFFLINE_WORKERS = 500  # Workers flooding offline msgs
DURATION = 180       # 3 Minutes Total

def get_node_name(short_name):
    hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
    return f"{short_name}@{hostname}"

EDGE_FULL = get_node_name(EDGE_NODE)

def run_cmd(c, async_run=False, ignore_fail=False):
    if async_run:
        return subprocess.Popen(c, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        try:
            return subprocess.check_output(c, shell=True).decode()
        except subprocess.CalledProcessError as e:
            if not ignore_fail:
                # print(f"DTOOL ERROR: {c} failed with {e}") # Reduce noise
                pass
            return ""

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}")

def monitor_system(duration, tag):
    start = time.time()
    while time.time() - start < duration:
        time.sleep(5)
        try:
            # Check Process Count on Edge Node
            cmd = f"erl -sname probe_{int(time.time())} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, system_info, [process_count])]), init:stop().\""
            count = run_cmd(cmd, ignore_fail=True)
            if count.strip():
                print(f"[{tag}] Process Count: {count.strip()}")
        except: pass

def get_interface():
    return "lo"

def apply_network_chaos(enable=True):
    iface = get_interface()
    if enable:
        print(f"[NET] Applying TC Netem Rules to {iface}...")
        # Use sudo -n for non-interactive
        cmd = f"sudo -n tc qdisc add dev {iface} root netem delay 100ms 50ms distribution normal loss 1% duplicate 1%"
        run_cmd(cmd, ignore_fail=True)
    else:
        print(f"[NET] Removing TC Netem Rules from {iface}...")
        run_cmd(f"sudo -n tc qdisc del dev {iface} root", ignore_fail=True)

def main():
    print_section("PROJECT IRIS: KITCHEN SINK CHAOS TEST")
    print(f"Target: {USER_COUNT} Users | Network Chaos | Offline Flooding")
    
    # Clean Start
    print("[INIT] Cleaning environment...")
    os.system("make stop >/dev/null 2>&1")
    os.system("killall beam.smp >/dev/null 2>&1")
    apply_network_chaos(False) # Ensure clean network
    
    run_cmd("make clean && make all")
    # Compile chaos tools
    os.system("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl src/iris_extreme_gen.erl")
    
    print("[INIT] Starting Nodes...")
    run_cmd("make start_core") 
    time.sleep(2)
    run_cmd("make start_edge1")
    time.sleep(2)
    
    # 1. Ramp Up
    print_section("PHASE 1: THE GATHERING (Ramp Up)")
    # Using iris_extreme_gen for efficient connection holding
    # We spawn a loader process that spawns the workers
    load_cmd = f"/usr/bin/erl +P 2000000 -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, {DURATION}, normal), timer:sleep(infinity).\""
    p_load = run_cmd(load_cmd, async_run=True)
    
    print("Waiting 30s for connections to establish...")
    monitor_system(30, "RAMP-UP")
    
    # 2. Offline Flood
    print_section("PHASE 2: THE STORM (Offline Flood)")
    # Start separate flooders targeting offline users
    flood_cmd = f"/usr/bin/erl +P 2000000 -sname flooder -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OFFLINE_WORKERS}, {DURATION}, offline_flood), timer:sleep(infinity).\""
    p_flood = run_cmd(flood_cmd, async_run=True)
    
    monitor_system(30, "FLOODING")
    
    # 3. Network & Process Chaos
    print_section("PHASE 3: THE WILD (Chaos Unleashed)")
    
    # A. Network Chaos (Latency/Loss)
    apply_network_chaos(True)
    
    # B. Process Chaos (Chaos Monkey)
    # Target: iris_router_worker (Core Routing Logic) and iris_edge_conn (Users)
    print("[CHAOS] Releasing Chaos Monkey...")
    # 1. Kill Random Connections (10 per second)
    run_cmd(f"erl -sname monkey_conn -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [100, 1]), init:stop().\"")
    # 2. Kill System Processes (Router Workers) every 2s
    run_cmd(f"erl -sname monkey_Sys -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, kill_system, [2000, [iris_router, iris_router_worker]]), init:stop().\"")

    print(f"Running Chaos for {DURATION-60} seconds...")
    monitor_system(DURATION - 60, "CHAOS")
    
    # 4. Recovery
    print_section("PHASE 4: THE AFTERMATH (Recovery)")
    apply_network_chaos(False)
    run_cmd(f"erl -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, stop, []), init:stop().\"")
    
    print("Chaos Stopped. Monitoring Recovery (30s)...")
    monitor_system(30, "RECOVERY")
    
    print_section("TEST COMPLETE")
    p_load.kill()
    p_flood.kill()
    os.system("make stop")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nAborted by user.")
        apply_network_chaos(False)
        os.system("make stop")
        os.system("killall beam.smp")
