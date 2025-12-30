#!/usr/bin/env python3
import socket
import struct
import time
import sys
import statistics

# Configuration
HOST = 'localhost'
PORT = 8085
MSG_COUNT = 1000
PAYLOAD_SIZE = 64  # bytes

def create_socket(port=8085):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
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
    # Protocol: 0x02 | TLen(16) | Target | MLen(16) | Msg
    hdr = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg_bytes))
    sock.sendall(hdr + msg_bytes)

def receive_msg(sock):
    # Depending on how the server sends it back (raw msg or framed?)
    # iris_edge_conn sends raw Msg content.
    return sock.recv(4096)

def main():
    print(f"--- Iris Benchmark Tool ---")
    print(f"Messages: {MSG_COUNT}")
    print(f"Payload: {PAYLOAD_SIZE} bytes")
    
    try:
        # Setup Sender (Alice)
        alice = create_socket()
        login(alice, "bench_sender")
        
        # Setup Receiver (Bob)
        bob = create_socket()
        login(bob, "bench_receiver")
        
        latencies = []
        start_global = time.time()
        
        print("Starting benchmark...")
        
        for i in range(MSG_COUNT):
            # Create payload with timestamp
            ts_ns = time.time_ns()
            ts_bytes = struct.pack('>Q', ts_ns) # 8 bytes unsigned long long (Big Endian)
            padding = b'X' * (PAYLOAD_SIZE - 8)
            payload = ts_bytes + padding
            
            # Send
            send_msg(alice, "bench_receiver", payload)
            
            # Recv (Blocking wait)
            # Note: For strict E2E measurement, we wait for arrival.
            # This measures RTT roughly if we consider the lock-step, 
            # OR we can treat it as One-Way Flow if we trust clocks (localhost is trusted).
            data = receive_msg(bob)
            
            recv_ts_ns = time.time_ns()
            
            # Extract Send TS
            # Data might be combined if streaming fast, but here we do lock-step 
            # send-wait-recv to measure individual latency accurately without queueing noise first.
            if len(data) >= 8:
                 # In case of coalescing, we might get multiple messages. 
                 # For this simple bench, we assume lock-step prevents coalescing mostly.
                 # If data > payload, we might process multiple? 
                 # Let's just take the first 8 bytes implies the first message latency.
                 try:
                     sent_ts = struct.unpack('>Q', data[:8])[0]
                     lat_us = (recv_ts_ns - sent_ts) / 1000.0
                     latencies.append(lat_us)
                 except:
                     pass
        
        end_global = time.time()
        duration = end_global - start_global
        throughput = MSG_COUNT / duration
        
        print("\n--- Results ---")
        print(f"Total Time: {duration:.3f}s")
        print(f"Throughput: {throughput:.2f} msgs/sec")
        
        if latencies:
            print(f"Latency (us):")
            print(f"  Min: {min(latencies):.2f}")
            print(f"  Max: {max(latencies):.2f}")
            print(f"  Avg: {statistics.mean(latencies):.2f}")
            print(f"  P50: {statistics.median(latencies):.2f}")
            print(f"  P99: {statistics.quantiles(latencies, n=100)[98]:.2f}") # Approx P99
        
        alice.close()
        bob.close()
        
    except Exception as e:
        print(f"Benchmark failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
