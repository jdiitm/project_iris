#!/usr/bin/env python3
import subprocess
import time
import os
import re

def run_cmd(c): os.system(c)

def main():
    print("--- EXTREME SCALE TEST: 800,000 Real Connections ---")
    
    # 1. Cleanup & Start
    run_cmd("make stop >/dev/null 2>&1")
    run_cmd("killall beam.smp >/dev/null 2>&1")
    run_cmd("make clean >/dev/null; make all >/dev/null")
    run_cmd("make start_core >/dev/null; sleep 3")
    run_cmd("make start_edge1 >/dev/null; sleep 3")
    
    hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
    node = f"iris_edge1@{hostname}"
    
    print(f"[1] Target: {node}")
    
    # 2. Start Extreme Gen
    # We run it on the *Same Node* for simplicity (load_gen code loaded in edge1)? 
    # No, load gen creates client sockets. If run inside edge1, it competes for schedulers.
    # Better to run separate node.
    # But `iris_extreme_gen` needs to be compiled.
    
    print("[2] Compiling & Starting Generator...")
    run_cmd("erlc -o ebin src/iris_extreme_gen.erl")
    
    # Start Chaos Monkey on Target
    print("    [!] Releasing Chaos Monkey (Interval: 100ms)...")
    chaos_cmd = f"/usr/bin/erl -sname chaos_starter -hidden -noshell -pa ebin -eval \"rpc:call('{node}', chaos_monkey, start, [100, 5]), init:stop().\""
    run_cmd(chaos_cmd)
    
    # Start separate erlang node for generation
    # It will spawn 800k procs. Needs high ulimit.
    gen_cmd = f"/usr/bin/erl -sname gen_node -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start(800000, 60), timer:sleep(infinity).\""
    p_gen = subprocess.Popen(gen_cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    # 3. Monitor Loop
    print("[3] Ramping up (this takes ~80s)... Monitoring System...")
    
    try:
        max_q = 0
        for i in range(100):
            time.sleep(2)
            # Check Router Queue
            # rpc call to check queue len
            q_cmd = f"/usr/bin/erl -sname mon -hidden -noshell -pa ebin -eval \"Q = rpc:call('{node}', erlang, process_info, [whereis(iris_router), message_queue_len]), io:format('~p', [Q]), init:stop().\""
            try:
                out = subprocess.check_output(q_cmd, shell=True).decode()
                # Output format: {message_queue_len, 50}
                m = re.search(r"(\d+)", out)
                if m:
                    q_len = int(m.group(1))
                    max_q = max(max_q, q_len)
                    print(f"    T+{i*2}s | Router Queue: {q_len} | Max: {max_q}")
                    
                    if q_len > 10000:
                        print("    [FLAW FOUND] BACKPRESSURE DETECTED! Router queue > 10k")
            except:
                print("    Node unresponsive?")
                
    except KeyboardInterrupt:
        pass
        
    print("\n[4] Done. Checking Logs...")
    # Clean
    run_cmd("make stop >/dev/null")
    p_gen.kill()

if __name__ == "__main__":
    main()
