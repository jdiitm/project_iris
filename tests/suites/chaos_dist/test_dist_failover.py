#!/usr/bin/env python3
"""
Distributed Interactive Failover Test
-------------------------------------
Designed for Hybrid Cloud Verification where the test runner cannot 
automatically kill remote nodes.

This script:
1. Starts a steady stream of traffic from Edge -> Core.
2. Reports real-time delivery success rates.
3. Prompts the user to MANUALLY kill a Core node.
4. Visualizes the "Dip & Recover" curve.
"""

import sys
import os
import time
import socket
import threading
import argparse

# Protocol Constants
OP_LOGIN = b'\x01'
OP_MSG = b'\x02'

class Metrics:
    def __init__(self):
        self.sent = 0
        self.acked = 0
        self.errors = 0
        self.lock = threading.Lock()
        
    def update(self, s=0, a=0, e=0):
        with self.lock:
            self.sent += s
            self.acked += a
            self.errors += e
            
    def reset(self):
        with self.lock:
            self.sent = 0
            self.acked = 0
            self.errors = 0
            
    def get(self):
        with self.lock:
            return self.sent, self.acked, self.errors

metrics = Metrics()
running = True

def traffic_worker(host, port, user_id):
    """Single worker maintaining a connection and sending messages."""
    while running:
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.settimeout(2.0)
            s.connect((host, port))
            
            # Login
            login = OP_LOGIN + user_id.encode()
            s.sendall(login)
            s.recv(1024) # Ack
            
            while running:
                # Send Message
                target = b"echo_service"
                payload = f"msg_{time.time()}".encode()
                # Frame: Op(1) + TLen(2) + T + MLen(2) + M
                # For simplicity, just sending raw bytes if protocol allows, 
                # OR using standard verified packing:
                import struct
                # Op(1) + TLen(2) + Target + MLen(2) + Msg
                pkt = OP_MSG + struct.pack('>H', len(target)) + target + struct.pack('>H', len(payload)) + payload
                
                s.sendall(pkt)
                metrics.update(s=1)
                
                resp = s.recv(1024)
                if not resp:
                    raise ConnectionError("Connection closed")
                metrics.update(a=1)
                
                time.sleep(0.1) # 10 msgs/sec per worker
                
        except Exception:
            metrics.update(e=1)
            time.sleep(1) # Backoff
        finally:
            try: s.close()
            except: pass

def monitor_loop():
    print("\n--- TRAFFIC MONITOR (1s Intervals) ---")
    print(f"{'TIME':<10} | {'SENT':<6} | {'ACKED':<6} | {'ERRORS':<6} | {'STATUS':<20}")
    print("-" * 60)
    
    history = []
    
    while running:
        time.sleep(1)
        s, a, e = metrics.get()
        metrics.reset()
        
        status = "🟢 OK"
        if e > 0:
            if a == 0:
                status = "🔴 OUTAGE"
            else:
                status = "🟡 DEGRADED"
                
        print(f"{time.strftime('%H:%M:%S'):<10} | {s:<6} | {a:<6} | {e:<6} | {status}")
        history.append((a, e))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--host', default='localhost')
    parser.add_argument('--port', type=int, default=8085)
    parser.add_argument('--workers', type=int, default=10)
    args = parser.parse_args()
    
    print(f"[*] Starting {args.workers} workers aiming at {args.host}:{args.port}")
    
    # Start Workers
    threads = []
    for i in range(args.workers):
        t = threading.Thread(target=traffic_worker, args=(args.host, args.port, f"user_{i}"))
        t.daemon = True
        t.start()
        threads.append(t)
        
    # Start Monitor
    mon = threading.Thread(target=monitor_loop)
    mon.daemon = True
    mon.start()
    
    # Interactive Phase
    try:
        time.sleep(5)
        print("\n" + "="*60)
        print(">>> ACTION REQUIRED: KILL A CORE NODE NOW! <<<")
        print(">>> (Press Ctrl+C to stop test after you verify recovery) <<<")
        print("="*60 + "\n")
        
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        global running
        running = False
        print("\nTest Stopped.")

if __name__ == "__main__":
    main()
