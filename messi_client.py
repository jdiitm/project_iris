import socket
import struct
import time
import sys

COOKIE = "iris_secret"
VIP_USER = "vip_messi"
EDGE_HOST = "100.82.212.50"
EDGE_PORT = 8080

def packet_login(user):
    return b'\x01' + user.encode('utf-8')

def main():
    print(f"Connecting as {VIP_USER} to {EDGE_HOST}:{EDGE_PORT}...")
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((EDGE_HOST, EDGE_PORT))
        s.sendall(packet_login(VIP_USER))
        
        # Login Ack
        ack = s.recv(1024)
        print(f"Login Response: {ack}")
        
        print("Monitoring incoming traffic...")
        start_time = time.time()
        msg_count = 0
        last_report = start_time
        
        s.settimeout(1.0)
        
        while True:
            try:
                # Naive recv loop - just count bytes or look for headers
                # We expect reliable format: 0x03 (DELIVER)
                chunk = s.recv(65536)
                if not chunk: break
                
                # Rough count: Every message has a 0x03 header? 
                # Actually iris_session sends: <<3, MsgId...>> for reliable OR just payload?
                # iris_router_worker sends {deliver_msg, Msg}.
                # iris_session sends to socket: <<3, ...>> is ACK?
                # No, iris_session sends: 
                # dispatch(Sock, {deliver_msg, Msg}) -> 
                #   Packet = <<2, ...>> ?? 
                # Wait, we need to know what iris_session sends to client for delivered message.
                # Assuming standard protocol.
                # Let's just count packets.
                # Actually, simply counting chunks is enough to see throughput.
                
                msg_count += 1
                
                now = time.time()
                if now - last_report > 5.0:
                    rate = msg_count / (now - last_report)
                    print(f"[{time.strftime('%H:%M:%S')}] Received {msg_count} chunks/msgs (Rate: {rate:.1f}/sec)")
                    msg_count = 0
                    last_report = now
                    
            except socket.timeout:
                continue
            except KeyboardInterrupt:
                break
                
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()
