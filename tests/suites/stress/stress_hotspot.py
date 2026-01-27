#!/usr/bin/env python3
"""
Consolidated Stress Test: Messi Hotspot

Tests celebrity account / hotspot behavior with configurable modes:
- Basic: Quick flood test (offline → online burst)
- Lifecycle: Extended online/offline cycling with reliability tracking

Modes:
  --mode basic      : Quick 30s test (default)
  --mode lifecycle  : Extended 5-min lifecycle test with stats

Tier: 1 (Stress testing)
"""

# ... (imports)
import socket
import struct
import time
import os
import sys
import threading
import concurrent.futures
import argparse
import random

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from tests.framework.cluster import ClusterManager, get_cluster

# Configuration
MESSI_USER = "messi"
HOST = 'localhost'
PORT = 8085

# Statistics (for lifecycle mode)
STATS = {'sent': 0, 'received': 0}
STATS_LOCK = threading.Lock()

# CSV Logging
CSV_FILE = os.environ.get("IRIS_LATENCY_CSV", "latency_metrics.csv")
csv_lock = threading.Lock()
start_time = time.time()

def init_csv():
    if not os.path.exists(CSV_FILE):
        with open(CSV_FILE, "w") as f:
            f.write("timestamp,elapsed_sec,latency_ms,type\n")

def log_latency(duration_sec, msg_type="send"):
    if duration_sec is None: return
    # Downsample high-frequency logs to avoid 50k IOPS on CSV
    # Log 1% of requests or use a buffer
    # Ideally we'd aggregate, but "Irrefutable" might want raw samples for P99 calc.
    # Let's simple random sample 5% for now to balance IO vs data.
    if random.random() < 0.05:
        with csv_lock:
            with open(CSV_FILE, "a") as f:
                now = time.time()
                elapsed = now - start_time
                f.write(f"{now},{elapsed:.2f},{duration_sec*1000:.2f},{msg_type}\n")

# ============================================================================
# Socket Utilities
# ============================================================================

def create_socket(port=8085):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
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

def make_packet(target, msg):
    target_bytes = target.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg)) + msg

# ============================================================================
# Basic Mode: Flood Test
# ============================================================================

def send_burst(sender_id, count):
    """Sender logs in and sends messages to messi."""
    sock = create_socket()
    if not sock:
        return 0
    
    sent = 0
    try:
        login(sock, f"fan_{sender_id}")
        packet = make_packet(MESSI_USER, b"GOAL! " * 2)
        
        for _ in range(count):
            t0 = time.perf_counter()
            sock.sendall(packet)
            # We don't wait for ACK per message in burst mode usually?
            # Basic mode: just floods.
            # But to measure LATENCY we need ACK or some feedback.
            # Protocol: 1 login -> ... 
            # If we don't read ACK, we measure TCP send time which is fast.
            # Real latency is RTT. Let's assume we read 1 byte ack?
            # The server doesn't ACK messages by default in this protocol unless configured?
            # Packet Op 2 (Msg) -> Server relays. No ACK to sender.
            # So send latency is just network buffer time.
            # Better to measure "Login Latency" as proxy for system health?
            # OR modify protocol to support ping/pong?
            # Let's log 'send_time' (TCP buffer push).
            sent += 1
            t1 = time.perf_counter()
            log_latency(t1-t0, "send_ack_simulated") # Technically just send time
    except:
        pass
    finally:
        sock.close()
    return sent

def messi_consume(expected_count, timeout=10):
    """Messi comes online and receives flood."""
    sock = create_socket()
    if not sock:
        print("[FAIL] Messi cannot connect!")
        return 0
    
    print(f"\n[MESSI] Coming online... Expecting ~{expected_count} messages.")
    start_t = time.time()
    received = 0
    total_bytes = 0
    
    try:
        login(sock, MESSI_USER)
        sock.settimeout(5.0)
        
        while time.time() - start_t < timeout:
            try:
                data = sock.recv(65536)
                if not data:
                    break
                total_bytes += len(data)
                received += data.count(b"GOAL! ") // 2
            except socket.timeout:
                break
    except Exception as e:
        print(f"[MESSI] Error: {e}")
    finally:
        sock.close()
    
    duration = time.time() - start_t
    print(f"[MESSI] Received {received} messages ({total_bytes} bytes) in {duration:.2f}s")
    print(f"[MESSI] Speed: {received / max(duration, 0.001):.0f} msgs/sec")
    return received

def run_basic_mode(args):
    """Quick flood test."""
    total_expected = args.fans * args.msgs
    print(f"Scenario: {args.fans} fans × {args.msgs} msgs = {total_expected} total")
    
    # Flood phase (messi offline)
    print("\n[1] FLOOD PHASE (Messi Offline)")
    start_flood = time.time()
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.threads) as executor:
        chunk = max(1, args.fans // args.threads)
        futures = []
        
        for i in range(args.threads):
            start_id = i * chunk
            end_id = (i + 1) * chunk if i < args.threads - 1 else args.fans
            
            def task(s, e):
                total = 0
                for j in range(s, e):
                    total += send_burst(j, args.msgs)
                return total
            
            futures.append(executor.submit(task, start_id, end_id))
        
        concurrent.futures.wait(futures)
    
    flood_dur = time.time() - start_flood
    print(f"[1] Flood complete in {flood_dur:.2f}s ({total_expected / flood_dur:.0f} msgs/sec)")
    
    # Wait for queue drain
    print("\n[1.5] Waiting 10s for queue drain...")
    time.sleep(10)
    
    # Receive phase
    print("\n[2] ONLINE BURST PHASE")
    messi_consume(total_expected)

# ============================================================================
# Lifecycle Mode: Extended Test
# ============================================================================

def sender_worker(worker_id, stop_event):
    """Continuous sender for lifecycle mode."""
    sock = create_socket()
    if not sock:
        return
    
    local_sent = 0
    try:
        login(sock, f"fan_{worker_id}")
        packet = make_packet(MESSI_USER, b"G" * 10)
        
        while not stop_event.is_set():
            sock.sendall(packet)
            local_sent += 1
            time.sleep(0.02)  # ~50 RPS per sender
    except:
        pass
    finally:
        sock.close()
        with STATS_LOCK:
            STATS['sent'] += local_sent

def messi_lifecycle_worker(stop_event, schedule):
    """Messi worker following online/offline schedule."""
    current_idx = 0
    state_start = time.time()
    sock = None
    
    print(f"\n[MESSI] Lifecycle started")
    
    while not stop_event.is_set() and current_idx < len(schedule):
        now = time.time()
        duration, state = schedule[current_idx]
        
        if now - state_start > duration:
            if sock:
                sock.close()
                sock = None
            current_idx += 1
            state_start = now
            if current_idx < len(schedule):
                print(f"[MESSI] Switching to {schedule[current_idx][1]}")
            continue
        
        if state == 'OFFLINE':
            time.sleep(1)
        elif state == 'ONLINE':
            if not sock:
                try:
                    sock = create_socket()
                    if sock:
                        login(sock, MESSI_USER)
                        sock.settimeout(0.1)
                        print("[MESSI] Logged in (Online)")
                except:
                    time.sleep(1)
            
            if sock:
                try:
                    data = sock.recv(65536)
                    if data:
                        count = len(data) // 20
                        with STATS_LOCK:
                            STATS['received'] += count
                except socket.timeout:
                    pass
                except:
                    sock.close()
                    sock = None
    
    if sock:
        sock.close()
    print("[MESSI] Lifecycle finished")

def monitor_worker(stop_event, interval=10):
    """Print stats periodically."""
    start = time.time()
    while not stop_event.is_set():
        time.sleep(interval)
        dur = time.time() - start
        with STATS_LOCK:
            s, r = STATS['sent'], STATS['received']
        print(f"[{dur:4.0f}s] Sent: {s:8} | Recv: {r:8} | Diff: {s-r:8}")

def run_lifecycle_mode(args):
    """Extended lifecycle test."""
    # Schedule: (duration_seconds, state)
    scale = args.scale
    schedule = [
        (30 * scale, 'ONLINE'),
        (60 * scale, 'OFFLINE'),
        (30 * scale, 'ONLINE')
    ]
    total_time = sum(x[0] for x in schedule)
    
    print(f"Schedule: {schedule}")
    print(f"Total duration: {total_time:.0f}s")
    
    stop_event = threading.Event()
    
    # Start monitor
    monitor = threading.Thread(target=monitor_worker, args=(stop_event,))
    monitor.start()
    
    # Start Messi
    messi = threading.Thread(target=messi_lifecycle_worker, args=(stop_event, schedule))
    messi.start()
    
    # Start fans
    print(f"Starting {args.fans} fan threads...")
    fans = []
    for i in range(args.fans):
        t = threading.Thread(target=sender_worker, args=(i, stop_event), daemon=True)
        t.start()
        fans.append(t)
        if i % 50 == 0:
            time.sleep(0.05)
    
    # Wait for test duration
    try:
        time.sleep(total_time)
    except KeyboardInterrupt:
        print("\nStopping...")
    
    stop_event.set()
    messi.join()
    monitor.join()
    
    # Results
    print("\n--- FINAL RESULTS ---")
    with STATS_LOCK:
        sent, recv = STATS['sent'], STATS['received']
    
    print(f"Total Sent:     {sent}")
    print(f"Total Received: {recv}")
    loss_pct = (sent - recv) / sent * 100 if sent > 0 else 0
    print(f"Loss:           {sent - recv} ({loss_pct:.4f}%)")
    
    if sent > 0 and loss_pct < 0.1:
        print("VERDICT: PASS (>99.9% reliability)")
    else:
        print("VERDICT: FAIL (Data loss detected)")
        return 1
    return 0

# ============================================================================
# Main
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description='Messi Hotspot Stress Test')
    parser.add_argument('--mode', choices=['basic', 'lifecycle'], default='basic',
                        help='Test mode')
    parser.add_argument('--fans', type=int, default=50000, help='Number of fans')
    parser.add_argument('--msgs', type=int, default=20, help='Messages per fan (basic mode)')
    parser.add_argument('--threads', type=int, default=200, help='Sender threads (basic mode)')
    parser.add_argument('--scale', type=float, default=1.0, help='Time scale (lifecycle mode)')
    parser.add_argument('--skip-restart', action='store_true', help='Skip cluster restart')
    args = parser.parse_args()
    
    # Ensure correct CWD
    os.chdir(project_root)
    
    print(f"--- MESSI HOTSPOT TEST ({args.mode.upper()}) ---")
    init_csv()
    
    result = 0
    
    if args.skip_restart:
        # Just run
        if args.mode == 'basic':
            run_basic_mode(args)
        else:
            result = run_lifecycle_mode(args)
    else:
        # Use ClusterManager to restart
        with ClusterManager(project_root=project_root) as cluster:
            if args.mode == 'basic':
                run_basic_mode(args)
            else:
                result = run_lifecycle_mode(args)
    
    return result

if __name__ == "__main__":
    sys.exit(main())
