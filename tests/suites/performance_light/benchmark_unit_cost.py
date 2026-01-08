#!/usr/bin/env python3
import socket
import struct
import time
import os
import threading
import psutil
import argparse

HOST = 'localhost'
PORT = 8085
MSG_COUNT = 50000

def create_socket():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        s.connect((HOST, PORT))
        return s
    except:
        return None

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(payload)) + payload

def benchmark_worker(results, pid):
    sock = create_socket()
    if not sock: return
    
    # Login
    sock.sendall(packet_login(f"user_{pid}"))
    sock.recv(1024)
    
    # Spam
    target = "recipient_0"
    payload = b"X" * 50
    pkt = packet_msg(target, payload)
    
    start = time.time()
    for _ in range(MSG_COUNT):
        sock.sendall(pkt)
    dur = time.time() - start
    
    sock.close()
    results.append(dur)

def measure_system_resources(pid, duration, container):
    # Monitor Erlang process CPU/RAM
    p = psutil.Process(pid)
    cpu_start = p.cpu_times()
    
    time.sleep(duration)
    
    cpu_end = p.cpu_times()
    container['cpu_user'] = cpu_end.user - cpu_start.user
    container['cpu_sys'] = cpu_end.system - cpu_start.system
    container['mem_rss'] = p.memory_info().rss

def main():
    print("--- UNIT COST BENCHMARK ---")
    
    # 1. restart
    os.system("make stop >/dev/null 2>&1")
    os.system("make start_core >/dev/null; sleep 1")
    os.system("make start_edge1 >/dev/null; sleep 1")
    
    # Find beam pid
    erl_pid = None
    for proc in psutil.process_iter(['pid', 'name']):
        if 'beam' in proc.info['name']:
            erl_pid = proc.info['pid']
            break
            
    if not erl_pid:
        print("Erlang node not found.")
        return

    print(f"Monitoring Erlang PID: {erl_pid}")
    
    # 2. Run Load (Single Thread to minimize overhead noise, just raw msg cost)
    # Actually, we need saturation to see "cost per msg" fully pipeline
    # Let's use 10 threads doing 50k msgs each = 500k msgs total.
    
    THREADS = 10
    TOTAL_MSGS = THREADS * MSG_COUNT
    
    res_container = {}
    monitor = threading.Thread(target=measure_system_resources, args=(erl_pid, 5, res_container))
    monitor.start()
    
    threads = []
    durations = []
    
    start_time = time.time()
    for i in range(THREADS):
        t = threading.Thread(target=benchmark_worker, args=(durations, i))
        t.start()
        threads.append(t)
        
    for t in threads: t.join()
    monitor.join()
    
    total_time = time.time() - start_time
    
    # Calcs
    cpu_total_seconds = res_container.get('cpu_user', 0) + res_container.get('cpu_sys', 0)
    msgs_per_sec = TOTAL_MSGS / total_time
    cpu_per_msg = cpu_total_seconds / TOTAL_MSGS
    
    print("\n--- RESULTS ---")
    print(f"Total Messages: {TOTAL_MSGS}")
    print(f"Total Time:     {total_time:.4f}s")
    print(f"Throughput:     {msgs_per_sec:.2f} msgs/sec")
    print(f"Total CPU Time: {cpu_total_seconds:.4f}s")
    print(f"CPU Cost/Msg:   {cpu_per_msg*1_000_000:.2f} microseconds")
    if cpu_per_msg > 0:
        print(f"Est. Max RPS (1 Core): {1.0/cpu_per_msg:.2f}")
    else:
        print("Est. Max RPS (1 Core): Infinite (CPU < Measurement Threshold)")
    
    os.system("make stop >/dev/null")

if __name__ == "__main__":
    main()
