import socket
import os
import struct
import time
import threading
import sys
import statistics
import random

# Config
# 1 Billion User Simulation (Scaled Down)
# Traffic Mix:
# - 10% Traffic -> Hotspot (Messi) -> Should be Cached (Cheap)
# - 90% Traffic -> Random Friends -> Should be RPC (Cost)
# Goal: Verify Cache Effectiveness and Non-Hotspot Latency

TARGET_HOTSPOT = "messi_the_goat"
NUM_WORKERS = 50 
DURATION = 15

# Stats
stats_lock = threading.Lock()
stats = {
    "hotspot_reqs": 0, "hotspot_lats": [],
    "random_reqs": 0, "random_lats": [],
    "errors": 0
}

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_get_status(target):
    t_bytes = target.encode('utf-8')
    return b'\x05' + struct.pack('>H', len(t_bytes)) + t_bytes

def worker(idx):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect(('localhost', 8085))
        s.sendall(packet_login(f"user_{idx}"))
        s.recv(1024) # Ack
        
        end_time = time.time() + DURATION
        while time.time() < end_time:
            # 10% Chance Hotspot
            is_hotspot = random.random() < 0.10
            target = TARGET_HOTSPOT if is_hotspot else f"friend_{random.randint(0, 1000000)}"
            
            start = time.time()
            s.sendall(packet_get_status(target))
            resp = s.recv(1024)
            lat = (time.time() - start) * 1000
            
            if len(resp) > 0:
                with stats_lock:
                    if is_hotspot:
                        stats["hotspot_reqs"] += 1
                        stats["hotspot_lats"].append(lat)
                    else:
                        stats["random_reqs"] += 1
                        stats["random_lats"].append(lat)
            else:
                 with stats_lock: stats["errors"] += 1
                 break
            
            # Sleep slightly to prevent total client CPU saturation
            # time.sleep(0.001) 
        s.close()
    except Exception:
        with stats_lock: stats["errors"] += 1

def run_test():
    print(f"--- GLOBAL PRESENCE SIMULATION (Mix: 10% Messi, 90% Random) ---")

    # Restart Server
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    os.system("make start_core >/dev/null; sleep 2")
    os.system("make start_edge1 >/dev/null; sleep 2")
    
    
    # login Messi
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 8085))
    s.sendall(packet_login(TARGET_HOTSPOT))
    s.recv(1024)
    print("Messi is Online.")
    
    threads = []
    start_global = time.time()
    
    for i in range(NUM_WORKERS):
        t = threading.Thread(target=worker, args=(i, ))
        t.start()
        threads.append(t)
        
    for t in threads:
        t.join()
        
    total_time = time.time() - start_global
    
    h_reqs = stats["hotspot_reqs"]
    r_reqs = stats["random_reqs"]
    
    h_avg = statistics.mean(stats["hotspot_lats"]) if stats["hotspot_lats"] else 0
    r_avg = statistics.mean(stats["random_lats"]) if stats["random_lats"] else 0
    
    print("\n--- RESULTS ---")
    print(f"Total Requests: {h_reqs + r_reqs}")
    print(f"Hotspot Requests (Messi): {h_reqs} (Avg Latency: {h_avg:.3f} ms)")
    print(f"Random Requests (Global): {r_reqs} (Avg Latency: {r_avg:.3f} ms)")
    
    print(f"\nThroughput: {(h_reqs + r_reqs)/total_time:.2f} Ops/Sec")
    
    if h_avg > 1.0:
       print("FAIL: Hotspot Latency too high! Cache not working?")
    if r_avg < h_avg:
       print("WARNING: Random faster than Hotspot? Impossible.")

if __name__ == "__main__":
    run_test()
