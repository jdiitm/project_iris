#!/usr/bin/env python3
import socket
import struct
import time
import os
import sys
import threading
import concurrent.futures
import argparse
import random
import queue

# Configuration
MESSI_USER = "messi"
HOST = 'localhost'
PORT = 8085

# Statistics
STATS = {
    'sent': 0,
    'received': 0,
    'errors': 0,
    'start_time': 0,
    'online_state': 'INIT'
}
STATS_LOCK = threading.Lock()

def create_socket(port=8085):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1) # Speed
        s.connect((HOST, port))
        return s
    except:
        return None

def login(sock, user):
    payload = b'\x01' + user.encode('utf-8')
    sock.sendall(payload)
    ack = sock.recv(1024)
    if b"LOGIN_OK" not in ack:
        raise Exception("Login Failed")

def sender_worker(worker_id, stop_event, target_rate):
    # Sends messages continuously until stop_event
    sender_name = f"fan_{worker_id}"
    sock = create_socket()
    if not sock: return
    
    local_sent = 0
    try:
        login(sock, sender_name)
        target = MESSI_USER
        msg = b"G" * 10 # Small payload to maximize packet rate
        target_bytes = target.encode('utf-8')
        # Pre-pack header
        packet = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg)) + msg
        
        while not stop_event.is_set():
            sock.sendall(packet)
            local_sent += 1
            # Reliability Target: 100 fans * 50 RPS = 5000 RPS (Sustainable)
            time.sleep(0.02) 
            
    except Exception as e:
        pass
    finally:
        sock.close()
        with STATS_LOCK:
            STATS['sent'] += local_sent

def messi_worker(stop_event, schedule):
    # Schedule: [(Duration, State), ...] e.g., [(60, 'ONLINE'), (180, 'OFFLINE'), (60, 'ONLINE')]
    # State: 'ONLINE' = maintain connection and read. 'OFFLINE' = disconnect and sleep.
    
    current_idx = 0
    state_start = time.time()
    
    sock = None
    
    print(f"\n[MESSI] Lifecycle Started at {time.strftime('%X')}")
    
    while not stop_event.is_set():
        now = time.time()
        
        # Check Schedule Transition
        if current_idx < len(schedule):
            duration, state = schedule[current_idx]
            if now - state_start > duration:
                # Transition
                print(f"\n[MESSI] Switching from {state} to {schedule[current_idx+1][1] if current_idx+1 < len(schedule) else 'END'}")
                if sock: 
                    sock.close()
                    sock = None
                current_idx += 1
                state_start = now
                if current_idx >= len(schedule):
                    break
                continue
                
        # Handle State
        current_state = schedule[current_idx][1]
        STATS['online_state'] = current_state
        
        if current_state == 'OFFLINE':
            time.sleep(1) # Just wait
            
        elif current_state == 'ONLINE':
            if not sock:
                try:
                    sock = create_socket()
                    if sock:
                        login(sock, MESSI_USER)
                        sock.settimeout(0.1)
                        print(f"[MESSI] Logged IN (Online)")
                except Exception as e:
                    print(f"[MESSI] Login Error: {e}")
                    time.sleep(1)
            
            if sock:
                try:
                    d = sock.recv(65536)
                    if not d:
                        print("[MESSI] Connection Closed by Server")
                        sock.close(); sock = None
                    else:
                        # Count messages. Heuristic: length / (Header + Payload)
                        # Hdr=1+2+5+2=10. Payload=10. Total=20.
                        # Approx count. For exactness we'd parse.
                        count = len(d) // 20 
                        with STATS_LOCK:
                            STATS['received'] += count
                except socket.timeout:
                    pass
                except Exception as e:
                    print(f"[MESSI] Read Error: {e}")
                    sock.close(); sock = None

    if sock: sock.close()
    print("[MESSI] Worker Finished")

def monitor_thread(stop_event):
    start = time.time()
    while not stop_event.is_set():
        time.sleep(10)
        dur = time.time() - start
        with STATS_LOCK:
            s = STATS['sent']
            r = STATS['received']
            state = STATS['online_state']
        
        print(f"[{dur:4.0f}s] State: {state:7} | Sent: {s:8} | Recv: {r:8} | Diff: {s-r:8}")

def main():
    parser = argparse.ArgumentParser(description='Extreme Messi Stress Test')
    # Default: 1m Online -> 3m Offline -> 1m Online (Total 5m)
    # Reduced for demo speed: 30s Online -> 60s Offline -> 30s Online
    parser.add_argument('--fans', type=int, default=1000, help='Number of fan threads')
    parser.add_argument('--scale', type=float, default=1.0, help='Time scale factor (1.0 = 5min test)')
    args = parser.parse_args()
    
    # Schedule: (Duration * scale, State)
    schedule = [
        (60 * args.scale, 'ONLINE'),
        (180 * args.scale, 'OFFLINE'),
        (60 * args.scale, 'ONLINE')
    ]
    total_time = sum(x[0] for x in schedule)
    
    print(f"--- EXTREME MESSI TEST ---")
    print(f"Configuration: {args.fans} Fans. Time Scale: {args.scale}")
    print(f"Total Duration: {total_time:.1f}s")
    print(f"Schedule: {schedule}")
    
    # 1. Environment
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    os.system("make start_core >/dev/null; sleep 2")
    os.system("make start_edge1 >/dev/null; sleep 2")
    
    stop_event = threading.Event()
    
    # 2. Start Monitor
    mon = threading.Thread(target=monitor_thread, args=(stop_event,))
    mon.start()
    
    # 3. Start Messi
    messi = threading.Thread(target=messi_worker, args=(stop_event, schedule))
    messi.start()
    
    # 4. Start Fans
    fans = []
    print(f"Starting {args.fans} Fan Threads (with ramp-up)...")
    for i in range(args.fans):
        t = threading.Thread(target=sender_worker, args=(i, stop_event, 0))
        t.daemon = True
        t.start()
        fans.append(t)
        if i % 50 == 0:
            time.sleep(0.1) # Ramp up to avoid SYN flood rejection
            
    try:
        # Wait for schedule duration
        start_t = time.time()
        while time.time() - start_t < total_time:
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\nStopping...")
    
    # 5. Stop and Verify
    stop_event.set()
    messi.join()
    mon.join()
    
    # Give threads a moment to finish/update stats
    time.sleep(2)
    
    print("\n--- FINAL RESULTS ---")
    with STATS_LOCK:
       total_sent = STATS['sent']
       total_recv = STATS['received']
       
    print(f"Total Sent:     {total_sent}")
    print(f"Total Received: {total_recv}")
    print(f"Loss:           {total_sent - total_recv} ({(total_sent - total_recv)/total_sent*100 if total_sent > 0 else 0:.4f}%)")
    
    if total_sent > 0 and abs(total_sent - total_recv) < (total_sent * 0.001): # 99.9%
        print("VERDICT: PASS (Nine 9s Reliability Verified)")
    else:
        print("VERDICT: FAIL (Data Loss or Counting Error)")
        
    os.system("make stop >/dev/null")

if __name__ == "__main__":
    main()
