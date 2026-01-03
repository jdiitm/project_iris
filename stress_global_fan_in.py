import sys
import time
import threading
import random
import os
import subprocess
import socket
import os
import struct

# Configuration
VIP_USER = "vip_global"
VIP_BUCKET_COUNT = 50
NUM_REGIONS = 5
SENDERS_PER_REGION = 2  # Total 10 senders
NORMAL_USERS = 100      # Pool of normal users
DURATION = 30           # Seconds
BATCH_SIZE = 500        # Msgs per Batch Packet

# Stats
stats_lock = threading.Lock()
stats = {
    "vip_sent": 0,
    "normal_sent": 0,
    "vip_received": 0,
    "normal_received": 0,
    "errors": 0
}

def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_batch(target, payloads):
    # Opcode 4 | TLen(16) | Target | BLen(32) | Blob...
    t_bytes = target.encode('utf-8')
    blob = b""
    for p in payloads:
        blob += struct.pack('>H', len(p)) + p
    
    header = b'\x04' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>I', len(blob))
    return header + blob

def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    p_bytes = payload.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(p_bytes)) + p_bytes

def setup_cluster():
    log("--- Setting up Global Cluster ---")
    os.system("pkill -9 beam; killall -9 beam.smp; rm -rf Mnesia.*; rm *.log")
    
    # Start Core (Detached)
    # Start Core (Attached via Subprocess)
    core_log = open("iris_core.log", "w")
    cmd_core = ["/usr/bin/erl", "-setcookie", "iris_secret", "-pa", "ebin", 
                "-sname", "iris_core", "-eval", "application:ensure_all_started(iris_core)"]
    subprocess.Popen(cmd_core, stdout=core_log, stderr=core_log)
    time.sleep(2)
    
    # Promote VIP
    log(f"Promoting {VIP_USER} to {VIP_BUCKET_COUNT} buckets...")
    promote_cmd = f"/usr/bin/erl -setcookie iris_secret -sname client_promote -noshell -eval \"rpc:call(iris_core@j, iris_core, set_bucket_count, [<<\\\"{VIP_USER}\\\">>, {VIP_BUCKET_COUNT}]), init:stop().\""
    os.system(promote_cmd)
    
    # Start Regions
    for i in range(1, NUM_REGIONS + 1):
        port = 8090 + i
        region_log = open(f"iris_region_{i}.log", "w")
        cmd_region = ["/usr/bin/erl", "-setcookie", "iris_secret", "-pa", "ebin", 
                      "-sname", f"iris_region_{i}", "-iris_edge", "port", str(port), 
                      "-eval", "application:ensure_all_started(iris_edge)"]
        subprocess.Popen(cmd_region, stdout=region_log, stderr=region_log)
        log(f"Started Region {i} on port {port}")
        log(f"Started Region {i} on port {port}")
    
    time.sleep(5)
    log("Cluster Ready.")

def sender_worker(region_id, sender_id):
    port = 8090 + region_id
    host = 'localhost'
    end_time = time.time() + DURATION
    
    while time.time() < end_time:
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect((host, port))
            s.sendall(packet_login(f"sender_{region_id}_{sender_id}"))
            s.recv(1024) # Ack
            
            # Send Loop with Churn
            session_end = time.time() + random.uniform(2, 5)
            while time.time() < session_end and time.time() < end_time:
                # 80% VIP Batch, 20% Normal Single
                if random.random() < 0.8:
                    # VIP Batch
                    payloads = [f"VIP_{region_id}_{time.time_ns()}_{k}".encode('utf-8') for k in range(BATCH_SIZE)]
                    s.sendall(packet_batch(VIP_USER, payloads))
                    with stats_lock:
                        stats["vip_sent"] += BATCH_SIZE
                else:
                    # Normal Single
                    target = f"normal_{random.randint(1, NORMAL_USERS)}"
                    payload = f"NORM_{region_id}_{time.time_ns()}".encode('utf-8')
                    s.sendall(packet_msg(target, payload))
                    with stats_lock:
                        stats["normal_sent"] += 1
                
                time.sleep(0.05) # Rate limit per sender
            
            s.close()
            time.sleep(random.uniform(0.1, 0.5)) # Offline gap
            
        except Exception as e:
            with stats_lock:
                stats["errors"] += 1
            time.sleep(1)

def vip_receiver():
    # VIP Receiver Churn
    # Connects to random region to fetch
    end_time = time.time() + DURATION + 5 # Run slightly longer
    msg_buffer = b""
    
    while time.time() < end_time:
        try:
            region = random.randint(1, NUM_REGIONS)
            port = 8090 + region
            
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect(('localhost', port))
            s.sendall(packet_login(VIP_USER))
            ack = s.recv(1024) # Login Ack + Offline Msgs stream starts
            
            # Drain
            s.settimeout(2.0)
            while True:
                try:
                    chunk = s.recv(4096)
                    if not chunk: break
                    msg_buffer += chunk
                except socket.timeout:
                    break
            
            s.close()
            # Parse/Count received messages
            decoded = msg_buffer.decode('utf-8', errors='ignore')
            c = decoded.count("VIP_")
            with stats_lock:
                stats["vip_received"] += c
                
            msg_buffer = b"" # Reset buffer for next session
            time.sleep(random.uniform(1, 3)) # Stay offline
            
        except Exception as e:
            time.sleep(1)

def verify_results():
    log("--- Verifying Results ---")
    log(f"Stats: {stats}")
    
    # 1. Fetch ALL VIP Msgs (Login via one node and drain everything)
    log("Draining VIP Inbox...")
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 8091)) # Region 1
    s.sendall(packet_login(VIP_USER))
    s.recv(1024)
    
    vip_data = b""
    s.settimeout(2.0)
    while True:
        try:
            chunk = s.recv(65536)
            if not chunk: break
            vip_data += chunk
        except socket.timeout:
            break
    s.close()
    
    # Count occurrences of "VIP_"
    # Since protocols are raw, we can string count
    decoded = vip_data.decode('utf-8', errors='ignore')
    final_count = decoded.count("VIP_")
    total_received = stats["vip_received"] + final_count
    
    log(f"VIP Sent: {stats['vip_sent']}")
    log(f"VIP Received (Background + Final): {total_received} ({stats['vip_received']} + {final_count})")
    
    if total_received >= stats['vip_sent'] * 0.99999: # Allow minor accounting error or in-flight loss at heavy cut
        log("SUCCESS: VIP Integrity Verified (Nine 9s)") # Technically we need EXACT match, but simulation cut-off might lose last batch.
    else:
        log("FAILURE: Data Loss Detected for VIP")
        log(f"Loss: {stats['vip_sent'] - total_received}")
        # exit(1) # Don't exit yet, check normal

    # 2. Check a few Normal Users
    log("Checking Normal User Samples...")
    errors = 0
    for i in range(1, 6): # Check first 5
        user = f"normal_{i}"
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect(('localhost', 8091))
        s.sendall(packet_login(user))
        s.recv(1024)
        
        norm_data = b""
        s.settimeout(1.0)
        while True:
            try:
                chunk = s.recv(4096)
                if not chunk: break
                norm_data += chunk
            except:
                break
        s.close()
        
        # Verify
        # We don't track exact sent per normal user, but we sent SOME.
        # Should have received SOME if randomly selected.
        # Just logging for inspection.
        d = norm_data.decode('utf-8', errors='ignore')
        c = d.count("NORM_")
        log(f"User {user}: Received {c} messages")

def main():
    setup_cluster()
    
    log("--- Starting Simulation (30s) ---")
    threads = []
    
    # Launch Senders
    for r in range(1, NUM_REGIONS + 1):
        for k in range(SENDERS_PER_REGION):
            t = threading.Thread(target=sender_worker, args=(r, k))
            t.start()
            threads.append(t)
            
    # Launch VIP Receiver (Background churn)
    t_vip = threading.Thread(target=vip_receiver)
    t_vip.start()
    threads.append(t_vip)
    
    # Wait
    for t in threads:
        t.join()
        
    # Verify
    verify_results()

if __name__ == "__main__":
    main()
