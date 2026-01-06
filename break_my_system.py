#!/usr/bin/env python3
import subprocess
import time
import os
import sys

def run_cmd(c, bg=False):
    if bg:
        return subprocess.Popen(c, shell=True)
    return os.system(c)

def setup():
    print("[*] Setting up nodes...")
    run_cmd("make stop >/dev/null 2>&1")
    run_cmd("killall beam.smp >/dev/null 2>&1")
    run_cmd("make clean >/dev/null; make all >/dev/null")
    run_cmd("make start_core >/dev/null; sleep 3")
    run_cmd("make start_edge1 >/dev/null; sleep 3")

def run_split_brain():
    print("\n--- SCENARIO: The Split Brain (Network Partitions) ---")
    setup()
    
    hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
    node = f"iris_edge1@{hostname}"
    
    # 1. Start Load (Normal Mode) - 50k connections
    print("[*] Starting Load (50k connections)...")
    run_cmd(f"erl -sname gen_load -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(50000, 300, normal), timer:sleep(infinity).\"", bg=True)
    
    # 2. Start Chaos Dist
    print("[*] Starting Distribution Chaos (Disconnect random node every 5s)...")
    run_cmd(f"erl -sname chaos_dist -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_dist, start, [5000]), init:stop().\"")
    
    # 3. Monitor
    for i in range(20):
        time.sleep(5)
        # Check if nodes are connected
        out = subprocess.check_output(f"erl -sname check_{i} -hidden -noshell -pa ebin -eval \"N = rpc:call('{node}', erlang, nodes, []), io:format('~p', [N]), init:stop().\"", shell=True).decode()
        print(f"[{i*5}s] Connected Nodes: {out.strip()}")
        
    run_cmd("make stop >/dev/null")

def run_oom():
    print("\n--- SCENARIO: The Slow Consumer (Memory Exhaustion) ---")
    setup()
    
    # 1. Start Slow Consumer Load - 100k connections flooding each other
    # But NOT reading.
    print("[*] Starting Slow Consumers (100k connections)...")
    p = run_cmd(f"erl -sname gen_oom -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(100000, 300, slow_consumer), timer:sleep(infinity).\"", bg=True)
    
    # 2. Monitor RAM
    try:
        for i in range(120):
            time.sleep(2)
            # Grep Memory of edge node
            mem = subprocess.getoutput("ps aux | grep iris_edge1 | grep -v grep | awk '{print $6}'")
            if not mem:
                print("!!! CRASH DETECTED !!! Node died.")
                break
            # Handle multiple PIDs (e.g., epmd, beam.smp, erl)
            # Take the maximum memory usage found
            kb = max([int(x) for x in mem.split() if x.strip().isdigit()])
            print(f"[{i*2}s] Edge Node RAM: {kb/1024:.2f} MB")
            if kb > 20*1024*1024: # 20GB
                print("!!! DANGER !!! RAM Exceeded 20GB")
    except KeyboardInterrupt:
        pass
        
    p.kill()
    run_cmd("make stop >/dev/null")

def run_disk():
    print("\n--- SCENARIO: The Disk Crusher (Offline Message Flood) ---")
    setup()
    
    # 1. Start Offline Flood - 100k connections sending to offline users
    print("[*] Starting Offline Flood (100k connections)...")
    p = run_cmd(f"erl -sname gen_disk -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(100000, 300, offline_flood), timer:sleep(infinity).\"", bg=True)
    
    # 2. Monitor Disk I/O (via Mnesia Dir size)
    try:
        for i in range(120):
            time.sleep(2)
            # Check size of Mnesia dir
            hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
            size_out = subprocess.getoutput(f"du -sh Mnesia.iris_core@{hostname} | awk '{{print $1}}'")
            print(f"[{i*2}s] Mnesia DB Size: {size_out}")
    except KeyboardInterrupt:
        pass
        
    p.kill()
    run_cmd("make stop >/dev/null")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: ./break_my_system.py [oom|split]")
        sys.exit(1)
        
    if sys.argv[1] == "oom":
        run_oom()
    elif sys.argv[1] == "split":
        run_split_brain()
    elif sys.argv[1] == "disk":
        run_disk()
