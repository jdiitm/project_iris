#!/usr/bin/env python3
import subprocess
import time
import os
import sys
import threading
import signal
import socket
import struct
import statistics

# --- Configuration ---
# 200k Real Users (Normal Mode: Login + Send/Recv)
USER_COUNT = 200000 
# OOM Attempt: 10k Slow Consumers (Login + Flood + No Read)
OOM_WORKERS = 10000
# DB Saturation: 1000 Workers targeting Offline users (Disk Writes)
OFFLINE_WORKERS = 1000

DURATION = 900 # 15 Minutes
CORE_NODE = "iris_core"
EDGE_NODE = "iris_edge1"

# Metrics Globals
LATENCIES = []
THROUGHPUT_SAMPLES = []
ACTIVE_USERS_MAX = 0
LOCK = threading.Lock()

def get_node_name(short_name):
    try:
        hostname = subprocess.check_output("hostname -s", shell=True).decode().strip()
        return f"{short_name}@{hostname}"
    except:
        return f"{short_name}@localhost"

EDGE_FULL = get_node_name(EDGE_NODE)

def run_cmd(c, async_run=False, ignore_fail=False):
    if async_run:
        return subprocess.Popen(c, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        try:
            return subprocess.check_output(c, shell=True).decode()
        except subprocess.CalledProcessError as e:
            if not ignore_fail:
                pass
            return ""

def print_section(title):
    print(f"\n{'='*60}\n {title}\n{'='*60}")

class MetricsProbe(threading.Thread):
    def __init__(self, stop_event):
        super().__init__()
        self.stop_event = stop_event
        self.daemon = True
        self.sock = None

    def connect(self):
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.settimeout(2.0)
            self.sock.connect(('localhost', 8085))
            # Login
            user = b"probe_metrics"
            self.sock.sendall(b'\x01' + user)
            ack = self.sock.recv(1024)
            if b"LOGIN_OK" in ack:
                return True
        except:
            if self.sock: self.sock.close()
            self.sock = None
        return False

    def run(self):
        print("[PROBE] Metrics Probe Started...")
        while not self.stop_event.is_set():
            if not self.sock:
                if not self.connect():
                    time.sleep(1)
                    continue
            
            try:
                # Send Ping (Self Message)
                # Protocol: 0x02 | TLen(16) | Target | MLen(16) | Msg
                target = b"probe_metrics"
                ts_ns = time.time_ns()
                msg = struct.pack('>Q', ts_ns) # 8 bytes timestamp
                
                pkt = b'\x02' + struct.pack('>H', len(target)) + target + struct.pack('>H', len(msg)) + msg
                
                send_t = time.time()
                self.sock.sendall(pkt)
                
                # Receive
                # We might receive other stuff (if ID reused), but loop until we get ours or timeout
                while True:
                    d = self.sock.recv(1024)
                    if not d: 
                        self.sock.close(); self.sock = None; break
                    
                    if len(d) >= 8:
                        # Extract TS
                        rtt_us = (time.time() - send_t) * 1000000
                        with LOCK:
                            LATENCIES.append(rtt_us)
                        break
                        
                time.sleep(0.5) # 2 Pings/sec
                
            except Exception as e:
                # print(f"[PROBE] Error: {e}")
                if self.sock: self.sock.close()
                self.sock = None
                time.sleep(1)

def monitor_system(duration, tag):
    global ACTIVE_USERS_MAX
    start = time.time()
    while time.time() - start < duration:
        time.sleep(5)
        try:
            # Process Count
            cmd_proc = f"erl -sname probe_p_{int(time.time())} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, system_info, [process_count])]), init:stop().\""
            count_str = run_cmd(cmd_proc, ignore_fail=True).strip()
            
            # Memory
            cmd_mem = f"erl -sname probe_m_{int(time.time())} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, memory, [total])]), init:stop().\""
            mem_raw = run_cmd(cmd_mem, ignore_fail=True).strip()
            
            mem_str = ""
            if mem_raw and mem_raw.isdigit():
                mem_str = f"| RAM: {int(mem_raw)/1024/1024:.2f} MB"
            
            lat_str = ""
            with LOCK:
                if LATENCIES:
                    avg_lat = statistics.mean(LATENCIES[-10:]) if len(LATENCIES) > 10 else 0
                    lat_str = f"| Lat: {avg_lat:.0f} us"
            
            if count_str and count_str.isdigit():
                cnt = int(count_str)
                with LOCK:
                    ACTIVE_USERS_MAX = max(ACTIVE_USERS_MAX, cnt)
                print(f"[{tag}] Procs: {cnt} {mem_str} {lat_str}")
                
        except: pass

def main():
    print_section("PROJECT IRIS: GOD MODE CHAOS (15 MIN EXTREME)")
    print(f"Normal Users: {USER_COUNT} (Real Traffic)")
    print(f"OOM Consumers: {OOM_WORKERS}")
    print(f"DB Flooders: {OFFLINE_WORKERS}")
    print("Chaos: PID Kills, Router Kills, Protocol Corruption, SPLIT BRAIN")
    
    # Clean Start
    print("[INIT] Cleaning environment...")
    os.system("make stop >/dev/null 2>&1")
    os.system("killall beam.smp >/dev/null 2>&1")
    run_cmd("make clean && make all")
    # Compile chaos resources including chaos_dist
    os.system("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl src/chaos_dist.erl src/iris_extreme_gen.erl")
    
    print("[INIT] Starting Nodes...")
    os.system("make start_core >/dev/null") 
    time.sleep(2)
    os.system("make start_edge1 >/dev/null")
    time.sleep(2)
    
    # Start Metrics Probe
    stop_probe = threading.Event()
    probe = MetricsProbe(stop_probe)
    probe.start()
    
    # 1. Ramp Up Normal Users
    print_section("PHASE 1: THE LEGION (200k Real Traffic)")
    # Using 'normal' mode for bi-directional traffic
    load_cmd = f"erl +P 2000000 -sname legion_loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, {DURATION+60}, normal), timer:sleep(infinity).\""
    p_legion = run_cmd(load_cmd, async_run=True)
    
    print("Ramping up (60s)...")
    monitor_system(60, "RAMP-LEGION")
    
    # 2. Start Stressors (OOM + DB)
    print_section("PHASE 2: THE PRESSURE (OOM + DB Saturation)")
    
    # OOM Attempt (Slow Consumer)
    oom_cmd = f"erl +P 2000000 -sname oom_loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OOM_WORKERS}, {DURATION}, slow_consumer), timer:sleep(infinity).\""
    p_oom = run_cmd(oom_cmd, async_run=True)
    
    # DB Flood (Disk Crusher)
    db_cmd = f"erl +P 2000000 -sname db_loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OFFLINE_WORKERS}, {DURATION}, offline_flood), timer:sleep(infinity).\""
    p_db = run_cmd(db_cmd, async_run=True)
    
    monitor_system(30, "PRESSURE-BUILD")
    
    # 3. Chaos
    print_section("PHASE 3: GOD MODE CHAOS")
    
    # A. Chaos Monkey (User Disconnects)
    print("[CHAOS] Starting Chaos Monkey (Random User Kills)...")
    run_cmd(f"erl -sname monkey_conn -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [50, 1]), init:stop().\"") # 50/sec
    
    # B. The Sniper (System Process Kills)
    print("[CHAOS] Starting Sniper (Kill Router)...")
    run_cmd(f"erl -sname monkey_sys -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, kill_system, [5000, [iris_router, iris_router_worker]]), init:stop().\"")
    
    # C. Corruption
    print("[CHAOS] Starting Protocol Corruption...")
    run_cmd(f"erl -sname monkey_corr -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [50, corrupt_pids]), init:stop().\"")

    # D. Split Brain
    print("[CHAOS] Starting Split Brain (Network Partitions)...")
    run_cmd(f"erl -sname monkey_dist -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_dist, start, [5000]), init:stop().\"")

    print(f"Sustaining Chaos for {DURATION-90} seconds...")
    monitor_system(DURATION - 90, "GOD-MODE")
    
    # 4. Recovery
    print_section("PHASE 4: CEASE FIRE")
    run_cmd(f"erl -sname monkey_stop -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, stop, []), init:stop().\"")
    
    print("Monitoring Recovery (30s)...")
    monitor_system(30, "RECOVERY")
    
    # Report
    print_section("CHAOS METRICS REPORT")
    with LOCK:
        if LATENCIES:
            print(f"Latency P99:   {statistics.quantiles(LATENCIES, n=100)[98]:.2f} us")
            print(f"Latency Avg:   {statistics.mean(LATENCIES):.2f} us")
            print(f"Latency Max:   {max(LATENCIES):.2f} us")
        print(f"Peak Procs:    {ACTIVE_USERS_MAX}")
        
    print_section("TEST COMPLETE")
    stop_probe.set()
    p_legion.kill()
    p_oom.kill()
    p_db.kill()
    os.system("make stop")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nAborted.")
        os.system("make stop")
        os.system("killall beam.smp")
