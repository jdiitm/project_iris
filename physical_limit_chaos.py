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
import csv
from datetime import datetime

# --- Configuration ---
# PHYSICAL LIMIT ATTEMPT: 500k Real Users
# Based on 200k = ~3GB RAM. 500k target = ~7.5GB RAM.
# Machine has ~10GB available. This leaves ~2.5GB headroom.
USER_COUNT = 500000 
OOM_WORKERS = 25000   # Increased from 10k
OFFLINE_WORKERS = 2000 # Increased from 1k

DURATION = 900 # 15 Minutes
CORE_NODE = "iris_core"
EDGE_NODE = "iris_edge1"
LOG_FILE = "physical_limit_metrics.csv"

# Metrics Globals
LATENCIES = []
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
            user = b"probe_limit"
            self.sock.sendall(b'\x01' + user)
            ack = self.sock.recv(1024)
            if b"LOGIN_OK" in ack:
                return True
        except:
            if self.sock: self.sock.close()
            self.sock = None
        return False

    def run(self):
        while not self.stop_event.is_set():
            if not self.sock:
                if not self.connect():
                    time.sleep(1)
                    continue
            try:
                target = b"probe_limit"
                ts_ns = time.time_ns()
                msg = struct.pack('>Q', ts_ns)
                pkt = b'\x02' + struct.pack('>H', len(target)) + target + struct.pack('>H', len(msg)) + msg
                send_t = time.time()
                self.sock.sendall(pkt)
                while True:
                    d = self.sock.recv(1024)
                    if not d: 
                        self.sock.close(); self.sock = None; break
                    if len(d) >= 8:
                        rtt_us = (time.time() - send_t) * 1000000
                        with LOCK:
                            LATENCIES.append(rtt_us)
                        break
                time.sleep(0.5)
            except:
                if self.sock: self.sock.close()
                self.sock = None
                time.sleep(1)

def init_csv():
    with open(LOG_FILE, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Timestamp', 'Seconds_Elapsed', 'Process_Count', 'RAM_MB', 'Latency_Avg_Us', 'Latency_P99_Us'])

def log_csv(elapsed, procs, ram, lat_avg, lat_p99):
    with open(LOG_FILE, 'a', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow([datetime.now().isoformat(), int(elapsed), procs, ram, f"{lat_avg:.2f}", f"{lat_p99:.2f}"])

def monitor_system(duration, tag):
    global ACTIVE_USERS_MAX
    start = time.time()
    next_log = start
    
    while time.time() - start < duration:
        time.sleep(5)
        now = time.time()
        
        try:
            # Stats
            cmd_proc = f"erl -sname probe_p_{int(now)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, system_info, [process_count])]), init:stop().\""
            count_str = run_cmd(cmd_proc, ignore_fail=True).strip()
            
            cmd_mem = f"erl -sname probe_m_{int(now)} -hidden -noshell -pa ebin -eval \"io:format('~p', [rpc:call('{EDGE_FULL}', erlang, memory, [total])]), init:stop().\""
            mem_raw = run_cmd(cmd_mem, ignore_fail=True).strip()
            
            mem_val = 0.0
            if mem_raw and mem_raw.isdigit():
                mem_val = int(mem_raw)/1024/1024
            
            lat_avg = 0.0
            lat_p99 = 0.0
            with LOCK:
                if LATENCIES:
                    recent = LATENCIES[-20:] # Last 10 secs roughly
                    if recent:
                        lat_avg = statistics.mean(recent)
                        lat_p99 = statistics.quantiles(recent, n=100)[98] if len(recent) >= 2 else lat_avg
            
            cnt = 0
            if count_str and count_str.isdigit():
                cnt = int(count_str)
                with LOCK:
                    ACTIVE_USERS_MAX = max(ACTIVE_USERS_MAX, cnt)
            
            print(f"[{tag}] Procs: {cnt} | RAM: {mem_val:.2f} MB | Lat: {lat_avg:.0f} us")
            
            elapsed = now - start
            log_csv(elapsed, cnt, mem_val, lat_avg, lat_p99)
            
        except Exception as e:
            pass

def main():
    print_section("PROJECT IRIS: PHYSICAL LIMIT TEST (500k TARGET)")
    print(f"Target Users: {USER_COUNT}")
    print(f"Telemetry: {LOG_FILE}")
    
    init_csv()
    
    print("[INIT] Cleaning environment...")
    os.system("make stop >/dev/null 2>&1")
    os.system("killall beam.smp >/dev/null 2>&1")
    run_cmd("make clean && make all")
    os.system("erlc -o ebin src/chaos_resources.erl src/chaos_monkey.erl src/chaos_dist.erl src/iris_extreme_gen.erl")
    
    print("[INIT] Starting Nodes...")
    os.system("make start_core >/dev/null") 
    time.sleep(2)
    os.system("make start_edge1 >/dev/null")
    time.sleep(2)
    
    stop_probe = threading.Event()
    probe = MetricsProbe(stop_probe)
    probe.start()
    
    # 1. Ramp Up 
    print_section("PHASE 1: THE HORDES (500k Ramp Up)")
    # Split into 2 loaders to avoid single-beam bottleneck on client side (optional but safer)
    # Actually beam handles it, but let's stick to one huge loader for simplicity of kill
    load_cmd = f"erl +P 2000000 -sname limit_loader -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({USER_COUNT}, {DURATION+120}, normal), timer:sleep(infinity).\""
    p_load = run_cmd(load_cmd, async_run=True)
    
    print("Ramping up (120s)...")
    monitor_system(120, "RAMP")
    
    # 2. Stress
    print_section("PHASE 2: THE CRUSH (OOM + DB)")
    oom_cmd = f"erl +P 2000000 -sname oom_ldr -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OOM_WORKERS}, {DURATION}, slow_consumer), timer:sleep(infinity).\""
    p_oom = run_cmd(oom_cmd, async_run=True)
    
    db_cmd = f"erl +P 2000000 -sname db_ldr -hidden -noshell -pa ebin -eval \"iris_extreme_gen:start({OFFLINE_WORKERS}, {DURATION}, offline_flood), timer:sleep(infinity).\""
    p_db = run_cmd(db_cmd, async_run=True)
    
    monitor_system(30, "STRESS")
    
    # 3. Chaos
    print_section("PHASE 3: TOTAL CHAOS")
    # All vectors at once
    run_cmd(f"erl -sname monkey_all -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, start, [50, 1]), rpc:call('{EDGE_FULL}', chaos_monkey, start, [50, corrupt_pids]), init:stop().\"")
    # Sniper
    run_cmd(f"erl -sname monkey_sys -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_monkey, kill_system, [5000, [iris_router, iris_router_worker]]), init:stop().\"")
    # Split Brain
    run_cmd(f"erl -sname monkey_dist -hidden -noshell -pa ebin -eval \"rpc:call('{EDGE_FULL}', chaos_dist, start, [5000]), init:stop().\"")

    print(f"Running Main Duration ({DURATION-150}s)...")
    monitor_system(DURATION - 150, "LIMIT")
    
    print_section("DONE")
    stop_probe.set()
    p_load.kill()
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
