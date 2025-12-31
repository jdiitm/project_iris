#!/usr/bin/env python3
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
    # Wait for Login ACK
    ack = sock.recv(1024)
    if b"LOGIN_OK" not in ack:
        raise Exception(f"Login failed for {user}: {ack}")
    print(f"Login successful for {user}")
    # Return any extra data that might have been coalesced with LOGIN_OK
    parts = ack.split(b"LOGIN_OK")
    if len(parts) > 1 and len(parts[1]) > 0:
        return parts[1]
    return b""

def send_msg(sock, target, msg):
    print(f"Sending message to {target}: {msg}")
    target_bytes = target.encode('utf-8')
    msg_bytes = msg.encode('utf-8')
    payload = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg_bytes)) + msg_bytes
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
        print("Sending ordered offline messages...")
        send_msg(alice, "charlie", "Msg 1")
        send_msg(alice, "charlie", "Msg 2")
        send_msg(alice, "charlie", "Msg 3")
        
        alice.close()

        print("Logged out Alice. Logging in Charlie...")
        # No artificial wait needed if Mnesia transaction is atomic/fast
        
        # Charlie logs in
        # Charlie logs in
        charlie = create_socket()
        extra_data = login(charlie, "charlie")
        print(f"DEBUG: extra_data from login: {extra_data}")

        
        # Charlie should receive ordered messages
        # Since this is TCP, they might arrive in one packet (e.g., b'Msg 1Msg 2Msg 3')
        
        received_buffer = extra_data
        start_time = time.time()
        while time.time() - start_time < 5:
            try:
                chunk = receive_msg(charlie)
                if chunk:
                    received_buffer += chunk
            except Exception:
                pass
            
            if b"Msg 1" in received_buffer and b"Msg 2" in received_buffer and b"Msg 3" in received_buffer:
                break
            time.sleep(0.1)

        print(f"Total Received: {received_buffer}")
        
        expected_seq = b"Msg 1Msg 2Msg 3"
        if expected_seq in received_buffer:
             print("SUCCESS: Received all messages in correct order.")
        else:
             print(f"FAILURE: Buffer mismatch. Got {received_buffer}")
             sys.exit(1)
                
        print("ALL TESTS PASSED")
             # The code might just print it to stdout in edge node, not send it to socket?
        
        charlie.close()
        
        print("Verifying Deletion (Re-login Charlie)...")
        charlie2 = create_socket()
        extra_data2 = login(charlie2, "charlie")
        
        # Check if any messages arrive
        charlie2.settimeout(2.0)
        try:
            chunk = charlie2.recv(1024)
            if chunk:
                 print(f"FAILURE: Received data after deletion: {chunk}")
                 sys.exit(1)
            else:
                 print("SUCCESS: No messages received (Clean).")
        except socket.timeout:
             print("SUCCESS: No messages received (Timeout as expected).")
             
        charlie2.close()
    except Exception as e:
        print(f"Test Crashed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
