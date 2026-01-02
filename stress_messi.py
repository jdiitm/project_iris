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

# Configuration
MESSI_USER = "messi"
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

def send_burst(sender_id, count):
    # Sender logs in and spams messages to "messi"
    sender_name = f"fan_{sender_id}"
    sock = create_socket()
    if not sock: return 0
    
    sent = 0
    try:
        login(sock, sender_name)
        
        target = MESSI_USER
        msg = b"GOAL! " * 2 # Payload
        
        target_bytes = target.encode('utf-8')
        hdr = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg))
        packet = hdr + msg
        
        for _ in range(count):
            sock.sendall(packet)
            sent += 1
            
    except Exception as e:
        # print(f"Sender {sender_id} error: {e}")
        pass
    finally:
        sock.close()
    return sent

def messi_consumer(expected_count):
    sock = create_socket()
    if not sock:
        print("[FAIL] Messi cannot connect!")
        return 0
        
    print(f"\n[MESSI] Coming Online... Expecting ~{expected_count} messages.")
    start_t = time.time()
    received_msgs = 0
    total_bytes = 0
    
    try:
        login(sock, MESSI_USER) # This triggers immediate flood
        
        sock.settimeout(5.0) # Give it some time to start, then read fast
        
        while True:
            try:
                # Read large chunks
                d = sock.recv(65536)
                if not d: break
                total_bytes += len(d)
                # Crude message counting (not exact if fragmented, but robust enough for volume check)
                # Actually, counting exact messages requires parsing.
                # Let's just track volume and time for now.
                # Or count "GOAL!" occurrences?
                received_msgs += d.count(b"GOAL! ") / 2 
            except socket.timeout:
                break
                
    except Exception as e:
        print(f"[MESSI] Error: {e}")
    finally:
        sock.close()
        
    duration = time.time() - start_t
    print(f"[MESSI] Received {int(received_msgs)} messages ({total_bytes} bytes) in {duration:.4f}s")
    print(f"[MESSI] Speed: {received_msgs / duration:.2f} msgs/sec")
    
    return received_msgs

def main():
    parser = argparse.ArgumentParser(description='Messi Hotspot Stress Test')
    parser.add_argument('--fans', type=int, default=10000, help='Number of fans sending messages')
    parser.add_argument('--msgs', type=int, default=5, help='Messages per fan')
    parser.add_argument('--threads', type=int, default=50, help='Sender threads')
    args = parser.parse_args()
    
    total_expected = args.fans * args.msgs
    print(f"--- 'MESSI' HOTSPOT TEST ---")
    print(f"Scenario: {args.fans} fans sending {args.msgs} msgs each to '{MESSI_USER}'. Total: {total_expected}")
    
    # 1. Restart Server for clean slate
    os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
    os.system("make clean >/dev/null; make all >/dev/null")
    os.system("make start_core >/dev/null; sleep 2")
    os.system("make start_edge1 >/dev/null; sleep 2")
    
    # 2. Flood Phase (Messi is Offline)
    print("\n[1] FLOOD PHASE (Messi Offline)")
    start_flood = time.time()
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.threads) as executor:
        futures = []
        chunk = max(1, args.fans // args.threads)
        
        # Ranges for threads
        def fan_task(start, end):
            total = 0
            for i in range(start, end):
                total += send_burst(i, args.msgs)
            return total
            
        for i in range(args.threads):
            s = i * chunk
            e = (i + 1) * chunk if i < args.threads - 1 else args.fans
            futures.append(executor.submit(fan_task, s, e))
            
        concurrent.futures.wait(futures)
        
    flood_dur = time.time() - start_flood
    print(f"[1] Flood Complete in {flood_dur:.2f}s")
    print(f"[1] Ingestion Rate: {total_expected / flood_dur:.2f} msgs/sec")
    
    print("\n[1.5] Waiting 30s (Walking onto the pitch / Queue Drain)...")
    # Waiting matches user scenario (30-45s offline) AND allows queue drain
    time.sleep(30)
    
    # 3. Burst Receive Phase (Messi comes Online)
    print("\n[2] ONLINE BURST PHASE")
    messi_consumer(total_expected)
    
    os.system("make stop >/dev/null")

if __name__ == "__main__":
    main()
