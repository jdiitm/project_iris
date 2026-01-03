import socket
import struct
import time
import os

HOST = 'localhost'
PORT = 8085

def create_socket():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    return s

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def packet_msg(target, payload):
    t_bytes = target.encode('utf-8')
    p_bytes = payload.encode('utf-8')
    return b'\x02' + struct.pack('>H', len(t_bytes)) + t_bytes + struct.pack('>H', len(p_bytes)) + p_bytes

def set_bucket_count(user, count):
    # Helper to call RPC on Core via Erlang shell or similar
    # For now, we will hack it using an os.system call to erl RPC
    # We must use the correct cookie if set, or default
    # Assuming 'iris_core@j' and no auth or default cookie for localhost tests
    # We need to make sure the name matches the user's hostname 'j'
    cmd = f"""erl -sname client -noshell -eval "rpc:call(iris_core@j, iris_core, set_bucket_count, [<<\\"{user}\\">>, {count}]), init:stop()." """
    os.system(cmd)

def main():
    print("--- HOT-KEY BUCKETING TEST ---", flush=True)
    
    # 1. Promote "vip_user" to BucketCount = 5
    print("Promoting 'vip_user' to 5 buckets...", flush=True)
    set_bucket_count("vip_user", 5)
    
    # 2. Alice sends 10 messages to "vip_user" (Offline)
    print("Alice sending 10 messages to offline 'vip_user'...", flush=True)
    alice = create_socket()
    alice.sendall(packet_login("alice"))
    ack = alice.recv(1024)
    print(f"Alice Login Ack: {ack}", flush=True)
    
    payloads = [f"MSG_{i}" for i in range(10)]
    for i, p in enumerate(payloads):
        alice.sendall(packet_msg("vip_user", p))
        if i % 2 == 0: print(f"Sent {p}", flush=True)
        time.sleep(0.01) # Small delay to ensure order in network
        
    alice.close()
    
    # 3. Log in VIP user and verify retrieval
    print("Logging in 'vip_user'...", flush=True)
    vip = create_socket()
    vip.sendall(packet_login("vip_user"))
    data = vip.recv(1024) # Ack
    print(f"VIP Login Ack: {data}", flush=True)
    
    print(f"VIP Login Ack: {data}", flush=True)
    
    # Receive loop
    # Capture any data that came with the Ack
    received_buffer = data
    vip.settimeout(2.0)
    try:
        while True:
            chunk = vip.recv(4096)
            if not chunk: break
            received_buffer += chunk
    except socket.timeout:
        pass
        
    decoded = received_buffer.decode('utf-8', errors='ignore')
    print(f"Received Buffer: {decoded}")
    
    # Verification
    missing = []
    for p in payloads:
        if p not in decoded:
            missing.append(p)
            
    if not missing:
        print("SUCCESS: All 10 messages received.")
    else:
        print(f"FAILURE: Missing {missing}")
        exit(1)
        
    # Order Check logic is tricky with raw buffer, but 'MSG_0' should appear before 'MSG_9'
    # Since we implemented timestamp sorting in retrieve(), order SHOULD be preserved globally
    idx = -1
    for p in payloads:
        current_idx = decoded.find(p)
        if current_idx < idx:
            print("FAILURE: Out of order!")
            exit(1)
        idx = current_idx
        
    print("SUCCESS: Global Order Preserved.")

if __name__ == "__main__":
    main()
