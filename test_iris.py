#!/usr/bin/env python3
import socket
import struct
import time
import sys

def create_socket(port=8085):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', port))
    return s

    msg_bytes = msg.encode('utf-8')
    payload = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + msg_bytes
    sock.sendall(payload)

def login(sock, user):
    # Cmd 1 + User bytes
    print(f"Logging in as {user}...")
    payload = b'\x01' + user.encode('utf-8')
    sock.sendall(payload)
    # Wait for Login ACK
    ack = sock.recv(1024)
    if b"LOGIN_OK" not in ack:
        raise Exception(f"Login failed for {user}: {ack}")
    print(f"Login successful for {user}")

def send_msg(sock, target, msg):
    # Cmd 2 + TargetLen(16) + Target + Msg
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

        bob = create_socket()
        login(bob, "bob")

        # Alice sends to Bob
        send_msg(alice, "bob", "Hello Bob!")
        
        # Bob receives
        data = receive_msg(bob)
        if data == b"Hello Bob!":
            print("SUCCESS: Bob received the message.")
        else:
            print(f"FAILURE: Bob received {data}")
            sys.exit(1)

        alice.close()
        bob.close()
    except Exception as e:
        print(f"Test Crashed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
