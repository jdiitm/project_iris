#!/usr/bin/env python3
"""
Combined Resilience Test Suite

Tests system behavior under various failure conditions:
- Split brain / network partition
- Memory exhaustion (slow consumer / OOM)
- Disk pressure (Mnesia stress)
- Backpressure detection
- Offline message verification under chaos

Modes:
  --mode split      : Network partition / split brain test
  --mode oom        : Slow consumer / memory exhaustion test
  --mode disk       : Disk pressure test (Mnesia stress)
  --mode backpressure : Router queue backpressure detection
  --mode offline    : Offline verification under chaos

Tier: 1 (Resilience testing)
"""

import subprocess
import time
import os
import sys
import re
import argparse

# ============================================================================
# Utilities
# ============================================================================

def get_hostname():
    return subprocess.check_output("hostname -s", shell=True).decode().strip()

def get_node(name):
    return f"{name}@{get_hostname()}"

def run_cmd(cmd, bg=False, ignore_fail=False):
    if bg:
        return subprocess.Popen(cmd, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        return subprocess.check_output(cmd, shell=True).decode()
    except subprocess.CalledProcessError:
        if not ignore_fail:
            pass
        return ""

def setup_cluster():
    """Start fresh cluster."""
    print("[SETUP] Restarting cluster...")
    os.system("make stop >/dev/null 2>&1; killall beam.smp >/dev/null 2>&1")
    os.system("make clean >/dev/null && make all >/dev/null")
    os.system("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl src/chaos_dist.erl src/iris_extreme_gen.erl src/iris_verification_gen.erl 2>/dev/null")
    # Redirect output to prevent check_output from hanging on open pipes
    os.system("make start_core >/dev/null 2>&1")
    time.sleep(3)
    os.system("make start_edge1 >/dev/null 2>&1")
    time.sleep(3)

def cleanup():
    os.system("make stop >/dev/null 2>&1")

# ============================================================================
# Test: Split Brain
# ============================================================================

def run_split_brain(args):
    """Test system behavior during network partitions."""
    print("\n--- RESILIENCE TEST: Split Brain (Network Partitions) ---")
    setup_cluster()
    
    node = get_node("iris_edge1")
    
    # Start load
    print("[*] Starting load (50k connections)...")
    p_load = run_cmd(f"erl -sname gen_load -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(50000, 300, normal), timer:sleep(infinity).\"", bg=True)
    
    # Start distribution chaos
    print("[*] Starting distribution chaos (disconnect random node every 5s)...")
    run_cmd(f"erl -sname chaos_dist -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_dist, start, [5000]), init:stop().\"")
    
    # Monitor
    for i in range(args.duration // 5):
        time.sleep(5)
        out = run_cmd(f"erl -sname check_{i} -hidden -noshell -pa ebin -eval \"N = rpc:call('{node}', erlang, nodes, []), io:format('~p', [N]), init:stop().\"", ignore_fail=True)
        print(f"[{i*5}s] Connected nodes: {out.strip()}")
    
    if p_load:
        p_load.kill()
    cleanup()
    print("[RESULT] Split brain test completed - check for crash logs")

# ============================================================================
# Test: OOM (Slow Consumer)
# ============================================================================

def run_oom(args):
    """Test memory exhaustion with slow consumers."""
    print("\n--- RESILIENCE TEST: Slow Consumer (Memory Exhaustion) ---")
    setup_cluster()
    
    print(f"[*] Starting slow consumers ({args.users} connections)...")
    p_load = run_cmd(f"erl -sname gen_oom -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 300, slow_consumer), timer:sleep(infinity).\"", bg=True)
    
    baseline_kb = 0
    max_kb = 0
    
    try:
        for i in range(args.duration // 2):
            time.sleep(2)
            mem = subprocess.getoutput("ps aux | grep iris_edge1 | grep -v grep | awk '{print $6}'")
            
            if not mem:
                print("!!! CRASH DETECTED !!! Node died.")
                break
            
            try:
                kb = max([int(x) for x in mem.split() if x.strip().isdigit()])
                if i == 0:
                    baseline_kb = kb
                max_kb = max(max_kb, kb)
                growth_pct = (kb - baseline_kb) / baseline_kb * 100 if baseline_kb > 0 else 0
                print(f"[{i*2}s] RAM: {kb/1024:.0f}MB (baseline: {baseline_kb/1024:.0f}MB, growth: {growth_pct:.0f}%)")
                
                if kb > 10 * 1024 * 1024:  # 10GB
                    print("!!! DANGER !!! RAM exceeded 10GB")
            except (ValueError, ZeroDivisionError):
                pass
    except KeyboardInterrupt:
        pass
    
    if p_load:
        p_load.kill()
    cleanup()
    
    growth_factor = max_kb / baseline_kb if baseline_kb > 0 else 0
    if growth_factor < 5:
        print(f"[RESULT] PASS - Memory growth factor: {growth_factor:.1f}x")
    else:
        print(f"[RESULT] WARNING - High memory growth: {growth_factor:.1f}x")

# ============================================================================
# Test: Disk Pressure
# ============================================================================

def run_disk(args):
    """Test disk pressure with offline message flood."""
    print("\n--- RESILIENCE TEST: Disk Crusher (Mnesia Stress) ---")
    setup_cluster()
    
    print(f"[*] Starting offline flood ({args.users} connections)...")
    p_load = run_cmd(f"erl -sname gen_disk -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 300, offline_flood), timer:sleep(infinity).\"", bg=True)
    
    try:
        for i in range(args.duration // 2):
            time.sleep(2)
            hostname = get_hostname()
            size_out = subprocess.getoutput(f"du -sh Mnesia.iris_core@{hostname} 2>/dev/null | awk '{{print $1}}'")
            print(f"[{i*2}s] Mnesia DB Size: {size_out or 'N/A'}")
    except KeyboardInterrupt:
        pass
    
    if p_load:
        p_load.kill()
    cleanup()
    print("[RESULT] Disk pressure test completed")

# ============================================================================
# Test: Backpressure Detection
# ============================================================================

def run_backpressure(args):
    """Test router queue backpressure under extreme load."""
    print("\n--- RESILIENCE TEST: Backpressure Detection (800k connections) ---")
    setup_cluster()
    
    node = get_node("iris_edge1")
    
    # Start chaos monkey (extreme mode)
    print("[*] Releasing extreme chaos monkey...")
    run_cmd(f"erl -sname chaos_starter -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [50, 10]), init:stop().\"")
    
    # Start load generator
    print(f"[*] Starting extreme load ({args.users} connections)...")
    p_load = run_cmd(f"erl -sname gen_node -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 600), timer:sleep(infinity).\"", bg=True)
    
    print("[*] Monitoring router queue...")
    max_queue = 0
    backpressure_detected = False
    
    try:
        for i in range(args.duration // 2):
            time.sleep(2)
            mon_name = f"mon_{int(time.time() * 1000)}"
            q_cmd = f"erl -sname {mon_name} -hidden -noshell -pa ebin -eval \"Q = rpc:call('{node}', erlang, process_info, [whereis(iris_router_1), message_queue_len]), io:format('~p', [Q]), init:stop().\""
            
            try:
                out = run_cmd(q_cmd, ignore_fail=True)
                m = re.search(r"(\d+)", out)
                if m:
                    q_len = int(m.group(1))
                    max_queue = max(max_queue, q_len)
                    status = "!!BACKPRESSURE!!" if q_len > 10000 else ""
                    print(f"[{i*2}s] Router queue: {q_len} | Max: {max_queue} {status}")
                    
                    if q_len > 10000:
                        backpressure_detected = True
            except:
                print(f"[{i*2}s] Node unresponsive?")
    except KeyboardInterrupt:
        pass
    
    if p_load:
        p_load.kill()
    cleanup()
    
    if backpressure_detected:
        print(f"[RESULT] BACKPRESSURE DETECTED - Max queue: {max_queue}")
    else:
        print(f"[RESULT] PASS - Max queue: {max_queue}")

# ============================================================================
# Test: Offline Verification Under Chaos
# ============================================================================

def run_offline_verify(args):
    """Test offline message integrity under chaos conditions."""
    print("\n--- RESILIENCE TEST: Offline Verification Under Chaos ---")
    setup_cluster()
    
    node = get_node("iris_edge1")
    
    # Fill storage
    print(f"[*] Filling storage for {args.users} users...")
    p_fill = run_cmd(f"erl +P 2000000 -sname filler -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({args.users}, 60, offline_flood), timer:sleep(infinity).\"", bg=True)
    
    time.sleep(70)
    if p_fill:
        p_fill.kill()
    os.system("pkill -f filler 2>/dev/null")
    
    # Start chaos
    print("[*] Unleashing chaos monkey...")
    run_cmd(f"erl -sname chaos -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [1000, 5]), init:stop().\"")
    
    # Verify
    print(f"[*] Starting verification ({args.users} users)...")
    p_verif = run_cmd(f"erl +P 2000000 -sname verifier -hidden -noshell -pa ebin -eval \"iris_verification_gen:start({args.users}, 600, verify), timer:sleep(300000).\" > verif.log 2>&1", bg=True)
    
    start_v = time.time()
    last_pos = 0
    
    while time.time() - start_v < min(args.duration, 300):
        time.sleep(5)
        try:
            with open("verif.log", "r") as f:
                f.seek(last_pos)
                new_data = f.read()
                if new_data:
                    print(new_data, end="")
                    last_pos = f.tell()
        except:
            pass
    
    if p_verif:
        p_verif.kill()
    cleanup()
    os.remove("verif.log") if os.path.exists("verif.log") else None
    print("[RESULT] Offline verification completed - check logs for errors")

# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Combined Resilience Test Suite')
    parser.add_argument('--mode', choices=['split', 'oom', 'disk', 'backpressure', 'offline'],
                        default='oom', help='Test mode')
    parser.add_argument('--users', type=int, default=50000, help='User/connection count')
    parser.add_argument('--duration', type=int, default=120, help='Test duration (seconds)')
    args = parser.parse_args()
    
    handlers = {
        'split': run_split_brain,
        'oom': run_oom,
        'disk': run_disk,
        'backpressure': run_backpressure,
        'offline': run_offline_verify
    }
    
    handlers[args.mode](args)

if __name__ == "__main__":
    main()
