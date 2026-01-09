#!/usr/bin/env python3
import socket
import struct
import time
import os
import sys
import threading
import random
import argparse
import concurrent.futures

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

def send_batch_range(tid, start_id, end_id, avg_msgs_per_user):
    # Sender login (one per thread)
    sender_name = f"sender_{tid}"
    sock = create_socket()
    if not sock: return
    try:
        login(sock, sender_name)
        
        for target_user in range(start_id, end_id):
            target = f"user_{target_user}"
            msg = b"X" * 10 # Small payload
            
            # Protocol: 0x02 | TLen(16) | Target | MLen(16) | Msg
            target_bytes = target.encode('utf-8')
            hdr = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg))
            packet = hdr + msg
            
            # Send messages with randomness (+/- 50% spread)
            if avg_msgs_per_user > 0:
                count = random.randint(int(avg_msgs_per_user * 0.5), int(avg_msgs_per_user * 1.5))
                count = max(1, count)
            else:
                count = 0
    
            for _ in range(count):
                sock.sendall(packet)
                
    except Exception as e:
        print(f"Worker {tid} Error: {e}")
    finally:
        sock.close()

def consumer_worker(worker_id, users):
    success_consumes = 0
    clean_checks = 0
    
    # Keep two reusable sockets: one for reading, one for verification (to simulate separate connections/state if needed, or just speed)
    s1 = create_socket()
    s2 = create_socket()
    
    for user_id in users:
        user = f"user_{user_id}"
        
        # 1. Login to retrieve (and delete)
        if not s1: s1 = create_socket()
        try:
            login(s1, user)
            
            # Read all - aggressive timeout
            s1.settimeout(0.1)
            received = 0
            while True:
                try:
                    d = s1.recv(4096)
                    if not d: 
                        s1.close(); s1 = None; break # Socket closed by server?
                    received += len(d)
                except socket.timeout:
                    break
            
            if received > 0:
                success_consumes += 1
            
        except Exception as e:
            if s1: s1.close(); s1 = None
        
        # 2. Re-login to verify empty
        if not s2: s2 = create_socket()
        try:
            login(s2, user)
            s2.settimeout(0.1)
            try:
                d = s2.recv(1024)
                if d and len(d) > 0:
                    print(f"[FAIL] {user} CLEANUP FAILED. Received: {d} (Len: {len(d)})")
                else:
                    clean_checks += 1
            except socket.timeout:
                clean_checks += 1
            except Exception as e:
                 clean_checks += 1
        except Exception as e:
            if s2: s2.close(); s2 = None

    if s1: s1.close()
    if s2: s2.close()
            
    return success_consumes, clean_checks

def main():
    parser = argparse.ArgumentParser(description='Stress Test Offline Messages')
    parser.add_argument('--users', type=int, default=100, help='Number of users')
    parser.add_argument('--threads', type=int, default=10, help='Number of threads')
    parser.add_argument('--msgs', type=int, default=10, help='Average messages per user')
    args = parser.parse_args()

    print(f"--- STRESS TEST: Offline Delete Cycle ({args.users} users, avg {args.msgs} msgs) ---")
    
    # 1. Setup Environment
    # Only restart if users > 2000 (fresh start for heavy load), otherwise reuse for speed dev
    if args.users > 2000:
        os.system("make stop >/dev/null 2>&1; killall beam.smp 2>/dev/null")
        os.system("make clean >/dev/null; make all >/dev/null")
        # Use high limits
        os.system("make start_core >/dev/null; sleep 2")
        os.system("make start_edge1 >/dev/null; sleep 2")
    
    print("[1] Filling Offline Storage...")
    
    start_fill = time.time()
    # Batch filling
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.threads) as executor:
        futures = []
        
        # Workers pick ranges of users to target
        def fill_task(t_id, start_range, end_range):
             send_batch_range(t_id, start_range, end_range, args.msgs)
        
        chunk = max(1, args.users // args.threads)
        for i in range(args.threads):
            s = i * chunk
            e = (i + 1) * chunk if i < args.threads - 1 else args.users
            futures.append(executor.submit(fill_task, i, s, e))
            
        concurrent.futures.wait(futures)

    print(f"[1] Storage Filled in {time.time() - start_fill:.2f}s.")
    print("[1.5] Waiting 10s for Erlang ingestion queues to drain...")
    time.sleep(10)
    print("[2] Starting Consumers...")
    
    # Start consumers
    start_t = time.time()
    total_consumed = 0
    total_clean = 0
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.threads) as executor:
        futures = []
        chunk = max(1, args.users // args.threads)
        for i in range(args.threads):
            s = i * chunk
            e = (i + 1) * chunk if i < args.threads - 1 else args.users
            user_list = range(s, e)
            futures.append(executor.submit(consumer_worker, i, user_list))
            
        for f in concurrent.futures.as_completed(futures):
            c, cl = f.result()
            total_consumed += c
            total_clean += cl

    end_t = time.time()
    duration = end_t - start_t
    
    print(f"\n[3] Done in {duration:.2f}s")
    print(f"Total Users: {args.users}")
    print(f"Successful Consumes: {total_consumed}")
    print(f"Verified Clean: {total_clean}")
    print(f"Rate: {args.users / duration:.2f} users/sec")
    
    if total_consumed < args.users * 0.9:
        print("WARNING: Low consumption rate.")
    
    # os.system("make stop >/dev/null")

if __name__ == "__main__":
    main()
