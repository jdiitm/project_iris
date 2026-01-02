import socket
import struct
import time
import os
import subprocess
import sys

def run_cmd(c):
    os.system(c)

def setup():
    print("[*] Setting up nodes...")
    run_cmd("pkill -9 beam; killall -9 beam.smp; rm -rf Mnesia.*; rm *.log")
    # Start Core
    run_cmd("make start_core > core.log 2>&1")
    # Start Edge
    run_cmd("make start_edge1 > edge1.log 2>&1")
    time.sleep(5)

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_get_status(target):
    t_bytes = target.encode('utf-8')
    return b'\x05' + struct.pack('>H', len(t_bytes)) + t_bytes

def decode_status(data):
    # Opcode 6 | UserLen(16) | User | State(1) | Time(64)
    if not data or data[0] != 6:
        return None
    u_len = struct.unpack('>H', data[1:3])[0]
    user = data[3:3+u_len].decode('utf-8')
    state = data[3+u_len] # 1=Online, 0=Offline
    ts = struct.unpack('>Q', data[3+u_len+1:3+u_len+9])[0]
    return {'user': user, 'state': 'online' if state == 1 else 'offline', 'time': ts}

def test_presence():
    setup()
    
    # 1. User A Logs in (Observer)
    print("\n--- TEST: Real-Time Online Check ---")
    s_a = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s_a.connect(('localhost', 8085))
    s_a.sendall(packet_login("alice"))
    ack = s_a.recv(1024)
    print(f"Alice Logged In: {ack}")

    # 2. User B Logs in (Target)
    s_b = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s_b.connect(('localhost', 8085))
    s_b.sendall(packet_login("bob"))
    ack_b = s_b.recv(1024)
    print(f"Bob Logged In: {ack_b}")
    
    time.sleep(1) # Propagate
    
    # 3. Alice asks for Bob's status
    print("\n[Check 1] Alice checks Bob (Should be Online)")
    s_a.sendall(packet_get_status("bob"))
    resp = s_a.recv(1024)
    print(f"RAW RESP: {resp}")
    status = decode_status(resp)
    print(f"Result: {status}")
    if status['state'] != 'online':
        print("FAIL: Bob should be online")
        sys.exit(1)

    # 4. Bob disconnects (Should set Last Seen)
    print("\n[Check 2] Bob disconnects (Should update Last Seen)")
    s_b.close()
    time.sleep(2) # Allow Batcher to flush (500ms interval)
    
    # 5. Alice checks again
    s_a.sendall(packet_get_status("bob"))
    resp = s_a.recv(1024)
    status = decode_status(resp)
    print(f"Result: {status}")
    
    if status['state'] != 'offline':
        print("FAIL: Bob should be offline")
        sys.exit(1)
    
    if status['time'] == 0:
        print("FAIL: Last Seen timestamp is 0 (Not Persisted?)")
        sys.exit(1)

    print(f"Last Seen Timestamp: {status['time']} (Current: {int(time.time())})")
    
    # 6. Persistence Check
    print("\n--- TEST: Persistence (Restart Core) ---")
    run_cmd("pkill -9 beam; killall -9 beam.smp") # Hard Kill
    time.sleep(2)
    run_cmd("make start_core > core_restart.log 2>&1")
    run_cmd("make start_edge1 > edge1_restart.log 2>&1")
    time.sleep(5)
    
    # Alice reconnects
    s_a = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s_a.connect(('localhost', 8085))
    s_a.sendall(packet_login("alice"))
    s_a.recv(1024)
    
    # Check Bob again
    print("[Check 3] Alice checks Bob after Restart (Should verify persistence)")
    s_a.sendall(packet_get_status("bob"))
    resp = s_a.recv(1024)
    status = decode_status(resp)
    print(f"Result: {status}")
    
    if status['state'] != 'offline' or status['time'] == 0:
        print("FAIL: Persistence failed.")
        sys.exit(1)
        
    print("\n>>> PASS: Presence Feature Verified <<<")

if __name__ == "__main__":
    test_presence()
