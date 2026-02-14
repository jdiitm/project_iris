#!/usr/bin/env python3
"""
Iris Client Library - Handles the reliable messaging protocol.

Protocol (RFC-001-AMENDMENT-001 v1.0 compliant):
- Login: 0x01 | User
- Send (sequenced): 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
- ACK: 0x03 | MsgId
- Reliable Message: 0x11 | IdLen(16) | MsgId | MsgLen(32) | Msg

NOTE: Opcode 0x02 (plaintext) is DEPRECATED and REJECTED in v1.0.
      Use send_msg() which now uses opcode 0x07 (sequenced messages).
NOTE: PROTOCOL_V1_FREEZE v1.1 moved RELIABLE_MSG from 0x10 to 0x11
      to resolve collision with CBOR_MSG.
"""

import socket
import ssl
import struct
import os
from pathlib import Path

# Default CA certificate path
PROJECT_ROOT = Path(__file__).parent.parent.parent
DEFAULT_CA_CERT = PROJECT_ROOT / "certs" / "ca.pem"

class IrisClient:
    def __init__(self, host='localhost', port=8085, use_tls=True):
        """
        Create an Iris client connection with mandatory TLS.
        
        TLS is MANDATORY per RFC NFR-14. The use_tls parameter is accepted
        for backward compatibility but ignored — TLS is always used.
        
        Uses wrap-then-connect pattern for atomic TCP+TLS handshake.
        
        Args:
            host: Server hostname
            port: Server port
            use_tls: Ignored (TLS is always used). Kept for API compatibility.
        """
        raw_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        raw_sock.settimeout(10.0)
        
        # TLS is mandatory — create context and wrap before connect
        context = ssl.create_default_context()
        
        # Load CA certificate if available
        ca_cert = os.environ.get('IRIS_CA_CERT', str(DEFAULT_CA_CERT))
        if os.path.exists(ca_cert):
            context.load_verify_locations(ca_cert)
        else:
            # For testing: disable cert verification if no CA available
            context.check_hostname = False
            context.verify_mode = ssl.CERT_NONE
        
        # Wrap BEFORE connect — TCP+TLS handshake happens atomically
        self.sock = context.wrap_socket(raw_sock, server_hostname=host)
        self.sock.connect((host, port))
        
        self.sock.settimeout(5.0)
        self.buffer = b''
        self.user = None
        self._seq_counter = 0  # Auto-incrementing sequence number for send_msg()
    
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
        
        # Small delay to ensure server-side registration completes
        # This prevents race conditions where messages are sent before
        # the recipient is fully registered in the presence table
        import time
        time.sleep(0.05)  # 50ms is enough for ETS propagation
        
        return True
    
    def send_msg(self, target, msg):
        """
        Send a message to target user.
        
        RFC-001-AMENDMENT-001 v1.0 COMPLIANT: Uses opcode 0x07 (sequenced message)
        instead of deprecated opcode 0x02 (plaintext) which is now rejected.
        
        Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
        """
        target_bytes = target.encode('utf-8')
        if isinstance(msg, str):
            msg_bytes = msg.encode('utf-8')
        else:
            msg_bytes = msg
        
        # Auto-increment sequence number for ordering
        self._seq_counter += 1
        seq_no = self._seq_counter
        
        # Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
        payload = (b'\x07' + 
                   struct.pack('>H', len(target_bytes)) + target_bytes +
                   struct.pack('>Q', seq_no) +
                   struct.pack('>H', len(msg_bytes)) + msg_bytes)
        self.sock.sendall(payload)
    
    def send_msg_v2(self, target, msg, idempotency_key=None):
        """
        B-1 AUDIT MITIGATION: Send via 0x0D (SEND_SEQ_V2) with idempotency key.
        
        RFC Section 1.2: Server deduplicates by (user_id, idempotency_key).
        This is the ONLY opcode that guarantees RFC-compliant deduplication.
        
        Protocol: 0x0D | TargetLen(16) | Target | IdempotencyKey(16) | SeqNo(64) | MsgLen(16) | Msg
        """
        import uuid
        target_bytes = target.encode('utf-8') if isinstance(target, str) else target
        msg_bytes = msg.encode('utf-8') if isinstance(msg, str) else msg
        
        if idempotency_key is None:
            idempotency_key = uuid.uuid4().bytes  # 16 bytes
        elif isinstance(idempotency_key, str):
            # Pad/truncate to 16 bytes
            idempotency_key = idempotency_key.encode('utf-8')[:16].ljust(16, b'\x00')
        
        self._seq_counter += 1
        seq_no = self._seq_counter
        
        payload = (b'\x0D' +
                   struct.pack('>H', len(target_bytes)) + target_bytes +
                   idempotency_key +
                   struct.pack('>Q', seq_no) +
                   struct.pack('>H', len(msg_bytes)) + msg_bytes)
        self.sock.sendall(payload)
        return idempotency_key
    
    def send_msg_seq(self, target, msg, seq_no):
        """
        Send a message with client-provided sequence number (RFC FR-5).
        
        The sequence number guarantees FIFO ordering even under parallel processing.
        Use this when message ordering is critical.
        
        Protocol: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
        """
        target_bytes = target.encode('utf-8')
        if isinstance(msg, str):
            msg_bytes = msg.encode('utf-8')
        else:
            msg_bytes = msg
        
        payload = (b'\x07' + 
                   struct.pack('>H', len(target_bytes)) + target_bytes +
                   struct.pack('>Q', seq_no) +
                   struct.pack('>H', len(msg_bytes)) + msg_bytes)
        self.sock.sendall(payload)
    
    def recv_msg(self, timeout=5.0):
        """
        Receive a reliable message and send ACK.
        Returns the message payload.
        
        Reliable Message Format: 0x11 | IdLen(16) | MsgId | MsgLen(32) | Msg
        (PROTOCOL_V1_FREEZE v1.1: moved from 0x10 to 0x11)
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
                    raise
            
            # Check opcode — 0x11 is RELIABLE_MSG per PROTOCOL_V1_FREEZE v1.1
            if len(self.buffer) > 0 and self.buffer[0] != 0x11:
                # Not a reliable message, skip byte
                self.buffer = self.buffer[1:]
                continue
            
            # Parse reliable message header: 0x11 | IdLen(16) | MsgId | MsgLen(32) | Msg
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
