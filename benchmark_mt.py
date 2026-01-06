#!/usr/bin/env python3
import socket
import struct
import time
import sys
import threading
import statistics
import argparse

# Configuration
HOST = 'localhost'
PORT = 8085
PAYLOAD_SIZE = 64  # bytes

def create_socket(port=8085):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    s.connect((HOST, port))
    return s

def login(sock, user):
    payload = b'\x01' + user.encode('utf-8')
    sock.sendall(payload)
    ack = sock.recv(1024)
    if b"LOGIN_OK" not in ack:
        raise Exception(f"Login failed for {user}")

def send_msg(sock, target, msg_bytes):
    target_bytes = target.encode('utf-8')
    hdr = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg_bytes))
    sock.sendall(hdr + msg_bytes)

def receive_msg(sock):
    return sock.recv(4096)

def worker(thread_id, msg_count, results, barrier):
    try:
        sender_name = f"mt_sender_{thread_id}"
        receiver_name = f"mt_receiver_{thread_id}"
        
        alice = create_socket()
        login(alice, sender_name)
        
        bob = create_socket()
        login(bob, receiver_name)
        
        # Wait for all threads to be ready
        barrier.wait()
        
        start_t = time.time()
        
        latencies = []
        
        for i in range(msg_count):
            ts_ns = time.time_ns()
            ts_bytes = struct.pack('>Q', ts_ns)
            padding = b'X' * (PAYLOAD_SIZE - 8)
            payload = ts_bytes + padding
            
            send_msg(alice, receiver_name, payload)
            data = receive_msg(bob)
            
            recv_ts_ns = time.time_ns()
            
            if len(data) >= 8:
                try:
                    sent_ts = struct.unpack('>Q', data[:8])[0]
                    lat_us = (recv_ts_ns - sent_ts) / 1000.0
                    latencies.append(lat_us)
                except:
                    pass
                    
        end_t = time.time()
        results.append({
            'thread_id': thread_id,
            'duration': end_t - start_t,
            'msgs': msg_count,
            'latencies': latencies
        })
        
        alice.close()
        bob.close()
        
    except Exception as e:
        print(f"Worker {thread_id} failed: {e}")

def main():
    parser = argparse.ArgumentParser(description='Multithreaded Iris Benchmark')
    parser.add_argument('--threads', type=int, default=10)
    parser.add_argument('--msgs', type=int, default=1000)
    args = parser.parse_args()
    
    print(f"--- MT Benchmark ({args.threads} threads, {args.msgs} msgs/thread) ---")
    
    results = []
    barrier = threading.Barrier(args.threads)
    threads = []
    
    start_global = time.time()
    
    for i in range(args.threads):
        t = threading.Thread(target=worker, args=(i, args.msgs, results, barrier))
        t.start()
        threads.append(t)
        
    for t in threads:
        t.join()
        
    end_global = time.time()
    total_duration = end_global - start_global
    
    total_msgs = sum(r['msgs'] for r in results)
    throughput = total_msgs / total_duration
    
    all_latencies = []
    for r in results:
        all_latencies.extend(r['latencies'])
        
    print(f"\n--- Results ---")
    print(f"Total Sent: {total_msgs}")
    print(f"Total Time: {total_duration:.3f}s")
    print(f"Throughput: {throughput:.2f} msgs/sec")
    
    if all_latencies:
        print(f"Latency (us):")
        print(f"  Min: {min(all_latencies):.2f}")
        print(f"  Max: {max(all_latencies):.2f}")
        print(f"  Avg: {statistics.mean(all_latencies):.2f}")
        try:
            print(f"  P99: {statistics.quantiles(all_latencies, n=100)[98]:.2f}")
        except:
            pass

if __name__ == "__main__":
    main()
