#!/usr/bin/env python3
import subprocess
import time
import os
import sys

# Baseline Load
USER_COUNT = 50000

def run_cmd(c, async_run=False):
    if async_run:
        return subprocess.Popen(c, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        try:
            return subprocess.check_output(c, shell=True).decode()
        except: return ""

def print_section(title):
    print(f"\n{'='*50}\n {title}\n{'='*50}")

def monitor_system(duration, tag):
    start = time.time()
    while time.time() - start < duration:
        time.sleep(5)
        # Check active connections
        try:
            count = run_cmd("erl -sname probe -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('iris_edge1@$(hostname -s)', erlang, system_info, [process_count])]), init:stop().\"")
            print(f"[{tag}] Process Count: {count.strip()}")
        except: pass

def main():
    print_section("TOTAL CHAOS: The Kitchen Sink Test")
    
    # 1. Start Fresh
    print("[1] Restarting Environment...")
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    # Compile chaos tools
    os.system("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl")
    
    run_cmd("make start_core")
    time.sleep(2)
    run_cmd("make start_edge1")
    time.sleep(2)
    
    hostname = run_cmd("hostname -s").strip()
    edge = f"iris_edge1@{hostname}"
    
    # 2. Establish Baseline Load (50k Users)
    print_section("Phase 1: Baseline Load (50k Users)")
    load_cmd = f"/usr/bin/erl +P 2000000 -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, 1000, idle), timer:sleep(infinity).\""
    p_load = run_cmd(load_cmd, async_run=True)
    time.sleep(30) # Let them connect
    
    # 3. Scenario A: The CPU Anvil
    print_section("Phase 2: The CPU Anvil (100% Load)")
    # Burn 24 cores
    os.system(f"/usr/bin/erl -sname anvil -hidden -noshell -pa ebin -eval \"rpc:call('{edge}', chaos_resources, burn_cpu, [24]), init:stop().\"")
    monitor_system(30, "CPU-STRESS")
    
    # 4. Scenario B: The Sniper (Kill Router Workers)
    print_section("Phase 3: The Sniper (Killing Router Workers)")
    # Kill 'iris_router' process every 2s
    # Note: iris_router is a named process.
    os.system(f"/usr/bin/erl -sname sniper -hidden -noshell -pa ebin -eval \"rpc:call('{edge}', chaos_monkey, kill_system, [2000, [iris_router]]), init:stop().\"")
    monitor_system(30, "SNIPER")
    
    # 5. Scenario C: The Memory Leak
    print_section("Phase 4: The Memory Leak (Eat 5GB)")
    # Eat 5000 MB
    os.system(f"/usr/bin/erl -sname leak -hidden -noshell -pa ebin -eval \"rpc:call('{edge}', chaos_resources, eat_memory, [5000]), init:stop().\"")
    monitor_system(30, "MEM-LEAK")
    
    print_section("Test Complete")
    p_load.kill()
    os.system("make stop")

if __name__ == "__main__":
    main()
