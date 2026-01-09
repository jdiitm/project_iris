import sys
import time
import threading
import random
import os
import subprocess
import socket
import struct

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster

# Configuration
# Two Regions: Region 1 (Producers), Region 2 (Consumers for Inter-Region)
# For Intra-Region: Region 1 -> Region 1
NUM_REGIONS = 2
USERS_PER_REGION = 100000 # Increased to 100,000 (Total 200,000)
DURATION = 60 # Increased duration
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

# CSV Logging
CSV_FILE = os.environ.get("IRIS_LATENCY_CSV", "latency_metrics.csv")
csv_lock = threading.Lock()
start_time = time.time()

def init_csv():
    # Append header if file doesn't exist, but careful about race if multiple tests write to same file
    # We'll just write if not exists.
    if not os.path.exists(CSV_FILE):
        with open(CSV_FILE, "w") as f:
            f.write("timestamp,elapsed_sec,latency_ms,type\n")

def log_latency(duration_sec, msg_type):
    if duration_sec is None: return
    # Log 10% of samples (high throughput)
    if random.random() < 0.1:
        with csv_lock:
            with open(CSV_FILE, "a") as f:
                now = time.time()
                elapsed = now - start_time
                f.write(f"{now},{elapsed:.2f},{duration_sec*1000:.2f},{msg_type}\n")

def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    p_bytes = payload.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(p_bytes)) + p_bytes

def client_worker(region_id, user_id, mode):
    # Mode: 'intra' (Send to self-region), 'inter' (Send to other region)
    port = 8090 + region_id # Correct mapping for stress_geo_scale?
    # Original used 8090 + i where i starts at 1 -> 8091, 8092
    # ClusterManager starts edges at 8085 + i - 1 -> 8085, 8086
    # Let's adjust to ClusterManager ports: 8085 + region_id - 1
    # Assuming region_id is 1-based (1, 2)
    port = 8085 + (region_id - 1)
    
    host = 'localhost'
    
    my_user = f"user_r{region_id}_{user_id}"
    
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(10.0) # Set timeout for connect and login
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
                payload = f"INTRA_MSG:{time.time()}"
                s.sendall(packet_msg(target, payload))
                with stats_lock: stats["intra_sent"] += 1
            else:
                # Send to random user in OTHER region
                other_region = 2 if region_id == 1 else 1
                target = f"user_r{other_region}_{random.randint(0, USERS_PER_REGION-1)}"
                payload = f"INTER_MSG:{time.time()}"
                s.sendall(packet_msg(target, payload))
                with stats_lock: stats["inter_sent"] += 1
            
            # 2. Receive
            try:
                data = s.recv(4096)
                if data:
                    # Dirty parse - we might get partial chunks or merged packets.
                    # Ideally we parse frames. But 'packet_msg' format is:
                    # Op(1) | TLen(2) | Target | PLen(2) | Payload
                    # This simple client doesn't properly buffer/frame.
                    # It just decodes whatever it gets.
                    # For metrics, let's just regex for the pattern we sent.
                    # payload = f"INTRA_MSG:{time.time()}"
                    decoded = data.decode('utf-8', errors='ignore')
                    
                    # Find potential timestamps
                    # INTRA_MSG:1234567890.123
                    
                    import re
                    # Find all matches
                    matches = re.findall(r"(INTRA|INTER)_MSG:(\d+\.\d+)", decoded)
                    
                    curr_time = time.time()
                    intra_count = 0
                    inter_count = 0
                    
                    for m in matches:
                        m_type, ts_str = m
                        try:
                            ts = float(ts_str)
                            latency = curr_time - ts
                            if m_type == "INTRA":
                                intra_count += 1
                                log_latency(latency, "intra_region")
                            else:
                                inter_count += 1
                                log_latency(latency, "inter_region")
                        except:
                            pass
                            
                    with stats_lock:
                        stats["intra_received"] += intra_count
                        stats["inter_received"] += inter_count
            except socket.timeout:
                pass
            except Exception:
                break
                
            # Rate limit
            time.sleep(0.01)
            
        s.close()
    except Exception as e:
        # log(f"Client Error: {e}")
        with stats_lock: stats["errors"] += 1

def main():
    # Ensure CWD
    os.chdir(project_root)
    
    with ClusterManager(project_root=project_root, default_edge_count=NUM_REGIONS) as cluster:
        # Phase 1: Local Switching Test (Intra-Region)
        # Both Regions running, but users only talk to neighbors.
        log("--- PHASE 1: INTRA-REGION FLOOD (Local Switching) ---")
        init_csv() # Init CSV
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
        stats["errors"] = 0
        
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
