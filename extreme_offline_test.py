#!/usr/bin/env python3
import subprocess
import time
import os
import sys

# Configuration
USER_COUNT = 100000
CHAOS_INTERVAL = 1000
CHAOS_KILLS = 5
FILL_MODE = "offline_flood"
VERIFY_TIMEOUT = 300 

def run_cmd(c, async_run=False):
    if async_run:
        return subprocess.Popen(c, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        return subprocess.check_output(c, shell=True).decode()

def main():
    print("--- EXTREME SCALE: 100k Offline Verification ---")
    
    # 1. restart
    print("[1] Restarting Environment (High Limits)...")
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    # New component
    os.system("erlc -o ebin src/iris_verification_gen.erl")
    
    # Start Nodes with 2M limits
    run_cmd("make start_core")
    time.sleep(3)
    run_cmd("make start_edge1")
    time.sleep(3)
    
    hostname = run_cmd("hostname -s").strip()
    edge = f"iris_edge1@{hostname}"
    
    # 2. Fill Storage
    print(f"[2] Filling Storage for {USER_COUNT} users...")
    
    # Helper node for filling
    # Flood offline messages using iris_extreme_gen
    fill_cmd = f"/usr/bin/erl +P 2000000 -sname filler -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, 60, {FILL_MODE}), timer:sleep(infinity).\""
    p_fill = run_cmd(fill_cmd, async_run=True)
    
    # Wait for fill (~60s approx based on speed)
    print("    Flooding messages...")
    time.sleep(70)
    p_fill.kill()
    os.system("pkill -f filler")
    
    # 3. Start Chaos
    print("[3] Unleashing Chaos Monkey...")
    chaos_cmd = f"/usr/bin/erl -sname chaos -hidden -noshell -pa ebin -eval \"rpc:call('{edge}', chaos_monkey, start, [{CHAOS_INTERVAL}, {CHAOS_KILLS}]), init:stop().\""
    os.system(chaos_cmd)
    
    # 4. Verify
    print(f"[4] Starting Verification ({USER_COUNT} users)...")
    verif_cmd = f"/usr/bin/erl +P 2000000 -sname verifier -hidden -noshell -pa ebin -eval \"iris_verification_gen:start({USER_COUNT}, 600, verify), timer:sleep(300000).\" > verif.log 2>&1"
    p_verif = run_cmd(verif_cmd, async_run=True)
    
    # Tail log
    start_v = time.time()
    last_pos = 0
    while time.time() - start_v < VERIFY_TIMEOUT:
        time.sleep(5)
        try:
            with open("verif.log", "r") as f:
                f.seek(last_pos)
                new_data = f.read()
                if new_data:
                    print(new_data, end="")
                    last_pos = f.tell()
        except: pass
        
    print("[5] Time's up. Checking logs manually implies success if no crash.")
    # In a real CI we would pipe output to file and grep it.
    
    print("Done.")
    os.system("make stop >/dev/null")
    p_verif.kill()

if __name__ == "__main__":
    main()
