import sys
import time
import threading
import random
import os
import subprocess
import socket
import struct

# Configuration
# Two Regions: Region 1 (Producers), Region 2 (Consumers for Inter-Region)
# For Intra-Region: Region 1 -> Region 1
NUM_REGIONS = 2
USERS_PER_REGION = 2000 # Total 4000 users
DURATION = 20
BATCH_SIZE = 100

# Stats
stats_lock = threading.Lock()
stats = {
    "intra_sent": 0,
    "intra_received": 0,
    "inter_sent": 0,
    "inter_received": 0,
    "errors": 0
}

def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    p_bytes = payload.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(p_bytes)) + p_bytes

def setup_cluster():
    log("--- Setting up Geo Cluster ---")
    os.system("pkill -9 beam; killall -9 beam.smp; rm -rf Mnesia.*; rm *.log")
    
    # Start Core
    core_log = open("iris_core.log", "w")
    cmd_core = ["/usr/bin/erl", "-setcookie", "iris_secret", "-pa", "ebin", 
                "-sname", "iris_core", "-eval", "application:ensure_all_started(iris_core)"]
    subprocess.Popen(cmd_core, stdout=core_log, stderr=core_log)
    time.sleep(2)
    
    # Start Regions
    for i in range(1, NUM_REGIONS + 1):
        port = 8090 + i
        region_log = open(f"iris_region_{i}.log", "w")
        cmd_region = ["/usr/bin/erl", "-setcookie", "iris_secret", "-pa", "ebin", 
                      "-sname", f"iris_region_{i}", "-iris_edge", "port", str(port), 
                      "-eval", "application:ensure_all_started(iris_edge)"]
        subprocess.Popen(cmd_region, stdout=region_log, stderr=region_log)
        log(f"Started Region {i} on port {port}")
    
    time.sleep(5)
    log("Cluster Ready.")

def client_worker(region_id, user_id, mode):
    # Mode: 'intra' (Send to self-region), 'inter' (Send to other region)
    port = 8090 + region_id
    host = 'localhost'
    
    my_user = f"user_r{region_id}_{user_id}"
    
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))
        s.sendall(packet_login(my_user))
        s.recv(1024) # Ack
        
        # Receiver Loop (Background)
        s.settimeout(0.01) 
        
        end_time = time.time() + DURATION
        while time.time() < end_time:
            # 1. Send
            if mode == 'intra':
                # Send to random user in SAME region
                target = f"user_r{region_id}_{random.randint(0, USERS_PER_REGION-1)}"
                payload = "INTRA_MSG"
                s.sendall(packet_msg(target, payload))
                with stats_lock: stats["intra_sent"] += 1
            else:
                # Send to random user in OTHER region
                other_region = 2 if region_id == 1 else 1
                target = f"user_r{other_region}_{random.randint(0, USERS_PER_REGION-1)}"
                payload = "INTER_MSG"
                s.sendall(packet_msg(target, payload))
                with stats_lock: stats["inter_sent"] += 1
            
            # 2. Receive
            try:
                data = s.recv(4096)
                if data:
                    decoded = data.decode('utf-8', errors='ignore')
                    intra = decoded.count("INTRA_MSG")
                    inter = decoded.count("INTER_MSG")
                    with stats_lock:
                        stats["intra_received"] += intra
                        stats["inter_received"] += inter
            except socket.timeout:
                pass
            except Exception:
                break
                
            # Rate limit
            time.sleep(0.01)
            
        s.close()
    except Exception as e:
        log(f"Client Error: {e}")
        with stats_lock: stats["errors"] += 1

def main():
    setup_cluster()
    
    # Phase 1: Local Switching Test (Intra-Region)
    # Both Regions running, but users only talk to neighbors.
    log("--- PHASE 1: INTRA-REGION FLOOD (Local Switching) ---")
    threads = []
    
    # Launch R1 Users (Talk to R1)
    for i in range(50): # 50 Active Workers per region to generate load
        t = threading.Thread(target=client_worker, args=(1, i, 'intra'))
        t.start()
        threads.append(t)
        
    # Launch R2 Users (Talk to R2)
    for i in range(50):
        t = threading.Thread(target=client_worker, args=(2, i, 'intra'))
        t.start()
        threads.append(t)
        
    for t in threads: t.join()
    
    log(f"Phase 1 Stats: {stats}")
    
    # Reset Stats
    stats["intra_sent"] = 0
    stats["intra_received"] = 0
    stats["inter_sent"] = 0
    stats["inter_received"] = 0
    
    # Phase 2: Global Routing Test (Inter-Region)
    log("--- PHASE 2: INTER-REGION FLOOD (Core RPC) ---")
    threads = []
    
    # Launch R1 Users (Talk to R2)
    for i in range(50):
        t = threading.Thread(target=client_worker, args=(1, i, 'inter'))
        t.start()
        threads.append(t)

    # Launch R2 Users (Talk to R1) -- Need to be online to receive
    # But for simplicity, we just have R2 users *also* sending Inter or just listening.
    # Let's have them send Inter too.
    for i in range(50):
        t = threading.Thread(target=client_worker, args=(2, i, 'inter'))
        t.start()
        threads.append(t)
        
    for t in threads: t.join()
    
    log(f"Phase 2 Stats: {stats}")
    
    log("Test Complete.")

if __name__ == "__main__":
    main()
