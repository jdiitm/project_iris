#!/usr/bin/env python3
import socket
import struct
import time
import os
import sys
import threading
import random

# Configuration
USER_COUNT = 100
MSGS_PER_USER = 10
CHAOS = True

HOST = 'localhost'
PORT = 8085

def create_socket(port=8085):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
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

def send_offline_batch(tid, start_id, end_id, target_user):
    # Sender login
    sender_name = f"sender_{tid}"
    sock = create_socket()
    if not sock: return
    try:
        login(sock, sender_name)
        
        target = f"user_{target_user}"
        msg = b"X" * 10 # Small payload
        
        # Protocol: 0x02 | TLen(16) | Target | MLen(16) | Msg
        target_bytes = target.encode('utf-8')
        hdr = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg))
        packet = hdr + msg
        
        # Send 10 messages
        for _ in range(MSGS_PER_USER):
            sock.sendall(packet)
            # No ack wait for speed in this phase, rely on TCP
            time.sleep(0.001) 
            
    except Exception as e:
        pass
    finally:
        sock.close()

def consumer_worker(tid, start_id, end_id):
    success_consumes = 0
    clean_checks = 0
    
    for i in range(start_id, end_id):
        user = f"user_{i}"
        
        # 1. Login to retrieve (and delete)
        s1 = create_socket()
        if not s1: continue
        try:
            login(s1, user)
            
            # Read all
            s1.settimeout(2.0)
            received = 0
            while True:
                try:
                    d = s1.recv(4096)
                    if not d: break
                    received += len(d)
                except socket.timeout:
                    break
            
            if received > 0:
                success_consumes += 1
            
            s1.close()
            
            # 2. Re-login to verify empty
            s2 = create_socket()
            if not s2: continue
            login(s2, user)
            s2.settimeout(1.0)
            try:
                d = s2.recv(1024)
                if d:
                    print(f"[FAIL] {user} received data after cleanup!")
                else:
                    clean_checks += 1
            except socket.timeout:
                clean_checks += 1
            s2.close()
            
        except Exception as e:
            # chaos might kill connections
            pass
            
    print(f"Worker {tid}: Consumed {success_consumes}, Verifed Clean {clean_checks}")

def main():
    print("--- STRESS TEST: Offline Delete Cycle ---")
    
    # 1. Setup Environment
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    # Use high limits
    os.system("make start_core >/dev/null; sleep 2")
    os.system("make start_edge1 >/dev/null; sleep 2")
    
    print("[1] Filling Offline Storage...")
    # We need to fill storage BEFORE users log in.
    # We can use a script or just do it here. 
    # Use threads to fill fast.
    
    fillers = []
    chunk = USER_COUNT // 10
    for i in range(10):
        t = threading.Thread(target=lambda: [send_offline_batch(i, 0, 0, u) for u in range(i*chunk, (i+1)*chunk)])
        fillers.append(t)
        t.start()
        
    for t in fillers: t.join()
    
    print("[2] Storage Filled. Starting Consumers...")
    
    # Start consumers
    consumers = []
    start_t = time.time()
    for i in range(10):
         t = threading.Thread(target=consumer_worker, args=(i, i*chunk, (i+1)*chunk))
         consumers.append(t)
         t.start()
         
    for t in consumers: t.join()
    end_t = time.time()
    
    print(f"\n[3] Done in {end_t - start_t:.2f}s")
    print("Check verify counts above. If high success, logic holds under load.")
    
    os.system("make stop >/dev/null")

if __name__ == "__main__":
    main()
