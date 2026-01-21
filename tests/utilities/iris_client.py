#!/usr/bin/env python3
"""
Iris Client Library - Handles the reliable messaging protocol.

Protocol:
- Login: 0x01 | User
- Send: 0x02 | TargetLen(16) | Target | MsgLen(16) | Msg
- ACK: 0x03 | MsgId
- Reliable Message: 0x10 | IdLen(16) | MsgId | MsgLen(32) | Msg
"""

import socket
import struct

class IrisClient:
    def __init__(self, host='localhost', port=8085):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((host, port))
        self.sock.settimeout(5.0)
        self.buffer = b''
        self.user = None
    
    def login(self, user):
        """Login to the server."""
        self.user = user
        payload = b'\x01' + user.encode('utf-8')
        self.sock.sendall(payload)
        
        # Wait for LOGIN_OK
        ack = self.sock.recv(1024)
        if b"LOGIN_OK" not in ack:
            raise Exception(f"Login failed: {ack}")
        
        # Store any extra data after LOGIN_OK
        idx = ack.find(b"LOGIN_OK")
        if idx >= 0:
            extra = ack[idx + len(b"LOGIN_OK"):]
            if extra:
                self.buffer = extra
        return True
    
    def send_msg(self, target, msg):
        """Send a message to target user."""
        target_bytes = target.encode('utf-8')
        if isinstance(msg, str):
            msg_bytes = msg.encode('utf-8')
        else:
            msg_bytes = msg
        
        # Protocol: 0x02 | TargetLen(16) | Target | MsgLen(16) | Msg
        payload = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg_bytes)) + msg_bytes
        self.sock.sendall(payload)
    
    def recv_msg(self, timeout=5.0):
        """
        Receive a reliable message and send ACK.
        Returns the message payload.
        
        Reliable Message Format: 0x10 | IdLen(16) | MsgId | MsgLen(32) | Msg
        """
        self.sock.settimeout(timeout)
        
        while True:
            # Fill buffer if needed
            if len(self.buffer) < 3:
                try:
                    data = self.sock.recv(4096)
                    if not data:
                        raise Exception("Connection closed")
                    self.buffer += data
                except socket.timeout:
                    raise Exception("Timeout waiting for message")
            
            # Check opcode
            if len(self.buffer) > 0 and self.buffer[0] != 0x10:
                # Not a reliable message, skip byte
                self.buffer = self.buffer[1:]
                continue
            
            # Parse reliable message header: 0x10 | IdLen(16) | MsgId | MsgLen(32) | Msg
            if len(self.buffer) < 3:
                continue
                
            id_len = struct.unpack('>H', self.buffer[1:3])[0]
            
            # Check if we have the full header
            header_len = 1 + 2 + id_len + 4  # opcode + idlen + id + msglen
            if len(self.buffer) < header_len:
                data = self.sock.recv(4096)
                self.buffer += data
                continue
            
            msg_id = self.buffer[3:3+id_len]
            msg_len = struct.unpack('>I', self.buffer[3+id_len:3+id_len+4])[0]
            
            # Check if we have the full message
            total_len = header_len + msg_len
            if len(self.buffer) < total_len:
                data = self.sock.recv(4096)
                self.buffer += data
                continue
            
            # Extract message
            msg = self.buffer[header_len:total_len]
            self.buffer = self.buffer[total_len:]
            
            # Send ACK: 0x03 | MsgId
            ack = b'\x03' + msg_id
            self.sock.sendall(ack)
            
            return msg
    
    def close(self):
        """Close the connection."""
        self.sock.close()


if __name__ == "__main__":
    # Simple test
    import sys
    
    print("Testing IrisClient...")
    
    alice = IrisClient()
    alice.login("alice")
    print("Alice logged in")
    
    bob = IrisClient()
    bob.login("bob")
    print("Bob logged in")
    
    alice.send_msg("bob", "Hello Bob!")
    print("Alice sent message")
    
    msg = bob.recv_msg()
    print(f"Bob received: {msg}")
    
    if msg == b"Hello Bob!":
        print("SUCCESS!")
        sys.exit(0)
    else:
        print(f"FAILURE: Expected b'Hello Bob!', got {msg}")
        sys.exit(1)
