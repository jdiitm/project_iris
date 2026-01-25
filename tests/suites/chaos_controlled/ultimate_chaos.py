#!/usr/bin/env python3
import subprocess
import time
import os
import sys

# --- Configuration ---
CORE_NODE = "iris_core"
EDGE_NODE = "iris_edge1"

# Profile-based configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
PROFILES = {
    "smoke": {"user_count": 100, "offline_workers": 10, "duration": 15},
    "full": {"user_count": 1000000, "offline_workers": 1000, "duration": 300},
}
_config = PROFILES.get(TEST_PROFILE, PROFILES["smoke"])
USER_COUNT = _config["user_count"]
OFFLINE_WORKERS = _config["offline_workers"]
DURATION = _config["duration"]

def get_node_name(short_name):
    hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
    suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
    return f"{short_name}{suffix}@{hostname}"

EDGE_FULL = get_node_name(EDGE_NODE)

def run_cmd(c, async_run=False, ignore_fail=False):
    if async_run:
        return subprocess.Popen(c, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        try:
            return subprocess.check_output(c, shell=True).decode()
        except subprocess.CalledProcessError as e:
            if not ignore_fail:
                # print(f"DTOOL ERROR: {c} failed with {e}")
                pass
            return ""

def setup_ip_aliases():
    print("[INIT] Setting up IP Aliases (127.0.0.1 - 127.0.0.20)...")
    # Need sudo
    for i in range(1, 21):
        run_cmd(f"sudo -n ifconfig lo:{i} 127.0.0.{i} up", ignore_fail=True)

def cleanup_ip_aliases():
    print("[CLEAN] Cleaning IP Aliases...")
    for i in range(1, 21):
        run_cmd(f"sudo -n ifconfig lo:{i} down", ignore_fail=True)

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}")

def monitor_system(duration, tag):
    start = time.time()
    while time.time() - start < duration:
        time.sleep(5)
        try:
            cmd = f"erl -sname probe_{int(time.time())} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, system_info, [process_count])]), init:stop().\""
            count = run_cmd(cmd, ignore_fail=True)
            if count.strip():
                print(f"[{tag}] Process Count: {count.strip()}")
                
            # Additional Obs Logic: Memory
            mem_cmd = f"erl -sname probe_m_{int(time.time())} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, memory, [total])]), init:stop().\""
            mem = run_cmd(mem_cmd, ignore_fail=True)
            if mem.strip() and mem.strip().isdigit():
                mem_mb = int(mem.strip()) / 1024 / 1024
                print(f"[{tag}] Total Memory: {mem_mb:.2f} MB")
                
        except: pass

def main():
    # Profile-based timing
    ramp_time = 5 if TEST_PROFILE == "smoke" else 60
    flood_time = 5 if TEST_PROFILE == "smoke" else 30
    chaos_time = 5 if TEST_PROFILE == "smoke" else 120
    recovery_time = 5 if TEST_PROFILE == "smoke" else 30
    
    print_section(f"PROJECT IRIS: ULTIMATE CHAOS ({USER_COUNT} USERS)")
    print(f"Target: {USER_COUNT} Users | Profile: {TEST_PROFILE}")
    
    # 0. Prep
    setup_ip_aliases()
    os.system("make stop >/dev/null 2>&1; killall beam.smp >/dev/null 2>&1")
    
    # Ensure we use the correct Erlang
    erl_path = "/usr/bin/erl" if os.path.exists("/usr/bin/erl") else "erl"
    suffix = os.environ.get("IRIS_NODE_SUFFIX", "")
    make_cmd = f"PATH=/usr/bin:$PATH NODE_SUFFIX={suffix} make ERL={erl_path}"
    
    run_cmd(f"{make_cmd} clean && {make_cmd} all")
    os.system("erlc -o ebin test_utils/chaos_resources.erl test_utils/chaos_monkey.erl test_utils/iris_extreme_gen.erl")
    
    run_cmd(f"{make_cmd} start_core")
    time.sleep(2)
    run_cmd(f"{make_cmd} start_edge1")
    time.sleep(3 if TEST_PROFILE == "smoke" else 5)
    
    # 1. Ramp Up (The Million March)
    print_section("PHASE 1: RAMP UP")
    load_cmd = f"/usr/bin/erl +P 2000000 -sname loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, {DURATION+60}, normal), timer:sleep(infinity).\""
    p_load = run_cmd(load_cmd, async_run=True)
    
    print(f"Allowing {ramp_time}s for ramp up...")
    monitor_system(ramp_time, "RAMP-UP")
    
    # 2. Disk Crusher (Offline Flood)
    print_section("PHASE 2: DISK CRUSHER (Mnesia Stress)")
    flood_cmd = f"/usr/bin/erl +P 2000000 -sname flooder -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OFFLINE_WORKERS}, {DURATION}, offline_flood), timer:sleep(infinity).\""
    p_flood = run_cmd(flood_cmd, async_run=True)
    
    monitor_system(flood_time, "FLOODING")
    
    # 3. Protocol & PID Corruption
    print_section("PHASE 3: PROTOCOL CORRUPTION")
    
    # A. Corrupt PIDs (Garbage to processes)
    print("[CHAOS] Starting PID Corruption (Random Garbage Messages)...")
    run_cmd(f"erl -sname monkey_corrupt -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [100, corrupt_pids]), init:stop().\"")
    
    # B. The Sniper (Kill Router)
    print("[CHAOS] Starting Sniper (Kill Router)...")
    run_cmd(f"erl -sname monkey_sniper -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, kill_system, [5000, [iris_router, iris_router_worker]]), init:stop().\"")

    print(f"Holding Load for {chaos_time}s...")
    monitor_system(chaos_time, "CHAOS-HOLD")
    
    # recovery
    print_section("PHASE 4: RECOVERY")
    run_cmd(f"erl -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, stop, []), init:stop().\"")
    monitor_system(recovery_time, "RECOVERY")
    
    print_section("TEST COMPLETE")
    p_load.kill()
    p_flood.kill()
    os.system("make stop")
    cleanup_ip_aliases()

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        cleanup_ip_aliases()
        os.system("make stop")
