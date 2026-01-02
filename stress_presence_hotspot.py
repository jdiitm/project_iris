import socket
import struct
import time
import threading
import sys
import statistics

# Config
TARGET_USER = "messi_the_goat"
NUM_FANS = 1 # Start small, we observe QPS per fan
DURATION = 10 

# Stats
stats_lock = threading.Lock()
stats = {"requests": 0, "errors": 0, "latencies": []}

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_get_status(target):
    t_bytes = target.encode('utf-8')
    return b'\x05' + struct.pack('>H', len(t_bytes)) + t_bytes

def fan_worker(idx):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect(('localhost', 8085))
        s.sendall(packet_login(f"fan_{idx}"))
        s.recv(1024) # Ack

        end_time = time.time() + DURATION
        while time.time() < end_time:
            start = time.time()
            s.sendall(packet_get_status(TARGET_USER))
            resp = s.recv(1024)
            lat = (time.time() - start) * 1000 # ms
            
            if len(resp) > 0 and resp[0] == 6:
                with stats_lock:
                    stats["requests"] += 1
                    stats["latencies"].append(lat)
            else:
                 with stats_lock: stats["errors"] += 1
                 # Reconnect on error
                 break
        s.close()
    except Exception as e:
        with stats_lock: stats["errors"] += 1

def run_test():
    print(f"--- PRESENCE HOTSPOT TEST ({NUM_FANS} Fans polling '{TARGET_USER}') ---")
    
    # login Messi
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 8085))
    s.sendall(packet_login(TARGET_USER))
    s.recv(1024)
    print("Messi is Online.")
    
    threads = []
    start_global = time.time()
    
    for i in range(NUM_FANS):
        t = threading.Thread(target=fan_worker, args=(i, ))
        t.daemon = True
        t.start()
        threads.append(t)
        
    for t in threads:
        t.join()
        
    total_time = time.time() - start_global
    reqs = stats["requests"]
    qps = reqs / total_time
    errs = stats["errors"]
    avg_lat = statistics.mean(stats["latencies"]) if stats["latencies"] else 0
    p99_lat = sorted(stats["latencies"])[int(0.99 * len(stats["latencies"]))] if stats["latencies"] else 0
    
    print("\n--- RESULTS ---")
    print(f"Total Requests: {reqs}")
    print(f"Duration:       {total_time:.2f}s")
    print(f"Throughput:     {qps:.2f} Ops/Sec")
    print(f"Errors:         {errs}")
    print(f"Avg Latency:    {avg_lat:.2f} ms")
    print(f"P99 Latency:    {p99_lat:.2f} ms")
    
    s.close()

if __name__ == "__main__":
    run_test()
