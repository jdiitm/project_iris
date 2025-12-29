import socket
import struct
import time
import sys

def create_socket(port=8085):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', port))
    return s

def login(sock, user):
    print(f"Logging in as {user}...")
    payload = b'\x01' + user.encode('utf-8')
    sock.sendall(payload)
    time.sleep(0.5)

def send_msg(sock, target, msg):
    print(f"Sending message to {target}: {msg}")
    target_bytes = target.encode('utf-8')
    msg_bytes = msg.encode('utf-8')
    payload = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + msg_bytes
    sock.sendall(payload)

def receive_msg(sock):
    sock.settimeout(2.0)
    try:
        data = sock.recv(1024)
        print(f"Received: {data}")
        return data
    except socket.timeout:
        print("Timeout waiting for message")
        return None

def main():
    try:
        alice = create_socket()
        login(alice, "alice")

        # Alice sends to Charlie (who is offline)
        send_msg(alice, "charlie", "Offline Msg")
        
        alice.close()

        print("Waiting...")
        time.sleep(1)

        # Charlie logs in
        charlie = create_socket()
        login(charlie, "charlie")
        
        # Charlie should receive offline message
        # Note: In iris_edge_conn.erl, offline messages are just printed?
        # Let's check source code.
        
        data = receive_msg(charlie)
        if data:
             print("SUCCESS: Charlie received offline message.")
        else:
             print("FAILURE: Charlie did not receive message.")
             # The code might just print it to stdout in edge node, not send it to socket?
        
        charlie.close()
    except Exception as e:
        print(f"Test Crashed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
