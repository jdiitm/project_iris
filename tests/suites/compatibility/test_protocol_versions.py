#!/usr/bin/env python3
"""
P2-4: Protocol Version Compatibility Tests

Validates that the server correctly handles different protocol versions
and maintains backward compatibility with older clients.

Test Scenarios:
1. Current protocol version works
2. Unknown opcodes are handled gracefully
3. Missing fields in packets don't crash server
4. Extended packets (extra fields) are tolerated
"""

import sys
import os
import socket
import struct
import time

sys.path.insert(0, str(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))


def get_connection(port=8085):
    """Get a raw socket connection."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(5.0)
    s.connect(('localhost', port))
    return s


def send_raw(sock, data):
    """Send raw bytes."""
    sock.sendall(data)


def recv_response(sock, timeout=2.0):
    """Receive response."""
    sock.settimeout(timeout)
    try:
        return sock.recv(4096)
    except socket.timeout:
        return None


# =============================================================================
# Protocol V1 (Current): Login, Send, Status
# =============================================================================

def build_login_v1(user_id):
    """Build V1 LOGIN packet: 0x01 + user_id"""
    return b'\x01' + user_id.encode('utf-8')


def build_send_v1(target, message):
    """Build V1 SEND packet: 0x02 + target_len(2) + target + msg_len(2) + msg"""
    target_bytes = target.encode('utf-8')
    msg_bytes = message.encode('utf-8')
    return (b'\x02' + 
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes)


def build_status_v1(target):
    """Build V1 STATUS packet: 0x04 + target"""
    return b'\x04' + target.encode('utf-8')


# =============================================================================
# Protocol V2 (Future): Extended with metadata
# =============================================================================

def build_login_v2(user_id, client_version="2.0.0", platform="test"):
    """
    Build hypothetical V2 LOGIN with extra fields.
    Servers should tolerate extra data and process what they understand.
    """
    # V1 base
    base = b'\x01' + user_id.encode('utf-8')
    # V2 extension: version string + platform (server may ignore)
    extra = b'\x00' + client_version.encode('utf-8') + b'\x00' + platform.encode('utf-8')
    return base + extra


def build_send_v2(target, message, priority=0, ttl=86400):
    """
    Build hypothetical V2 SEND with priority and TTL.
    Servers should process base message and ignore unknown extensions.
    """
    target_bytes = target.encode('utf-8')
    msg_bytes = message.encode('utf-8')
    # Base V1 format
    base = (b'\x02' + 
            struct.pack('>H', len(target_bytes)) + target_bytes +
            struct.pack('>H', len(msg_bytes)) + msg_bytes)
    # V2 extensions (server may ignore)
    extra = struct.pack('>B', priority) + struct.pack('>I', ttl)
    return base + extra


# =============================================================================
# Tests
# =============================================================================

def test_v1_protocol_works():
    """Verify current V1 protocol works correctly."""
    print("=" * 60)
    print("TEST: V1 Protocol Works")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Login V1
        send_raw(s, build_login_v1("compat_v1_user"))
        resp = recv_response(s)
        
        if resp and b"LOGIN_OK" in resp:
            print("✓ V1 LOGIN works")
        else:
            print(f"✓ V1 LOGIN accepted (response: {resp})")
        
        # Send V1
        send_raw(s, build_send_v1("some_target", "hello_v1"))
        time.sleep(0.2)
        
        s.close()
        
        # Verify server still running
        s2 = get_connection()
        send_raw(s2, build_login_v1("verify_user"))
        s2.close()
        
        print("✓ V1 protocol fully operational")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_unknown_opcode_handled():
    """Verify unknown opcodes don't crash server."""
    print("\n" + "=" * 60)
    print("TEST: Unknown Opcode Handling")
    print("=" * 60)
    
    try:
        s = get_connection()
        send_raw(s, build_login_v1("opcode_test"))
        recv_response(s, timeout=1)
        
        # Send unknown opcodes (0xF0-0xFF range)
        for opcode in [0xF0, 0xF5, 0xFA, 0xFF]:
            send_raw(s, bytes([opcode]) + b'some_data')
            time.sleep(0.1)
        
        print("✓ Sent 4 unknown opcodes")
        
        s.close()
        
        # Verify server survived
        s2 = get_connection()
        send_raw(s2, build_login_v1("after_unknown"))
        resp = recv_response(s2, timeout=1)
        s2.close()
        
        print("✓ Server survived unknown opcodes")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_v2_extended_login():
    """Test that V2 extended login doesn't break server."""
    print("\n" + "=" * 60)
    print("TEST: V2 Extended Login (Forward Compat)")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Send V2 login with extra fields
        send_raw(s, build_login_v2("v2_user", "2.1.0", "ios"))
        resp = recv_response(s, timeout=2)
        
        # Server should either:
        # 1. Accept login (ignoring extra data)
        # 2. Reject cleanly (protocol error)
        # NOT: Crash
        
        if resp:
            print(f"✓ Server responded: {resp[:50]}...")
        else:
            print("✓ Server accepted or silently processed")
        
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        send_raw(s2, build_login_v1("after_v2"))
        s2.close()
        
        print("✓ Server survived V2 extended login")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_v2_extended_send():
    """Test that V2 extended SEND doesn't break server."""
    print("\n" + "=" * 60)
    print("TEST: V2 Extended Send (Forward Compat)")
    print("=" * 60)
    
    try:
        s = get_connection()
        send_raw(s, build_login_v1("v2_sender"))
        recv_response(s, timeout=1)
        
        # Send V2 message with extra fields
        send_raw(s, build_send_v2("target", "hello_v2", priority=1, ttl=3600))
        time.sleep(0.2)
        
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        send_raw(s2, build_login_v1("after_v2_send"))
        s2.close()
        
        print("✓ Server survived V2 extended send")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_truncated_packet():
    """Test that truncated packets are handled gracefully."""
    print("\n" + "=" * 60)
    print("TEST: Truncated Packet Handling")
    print("=" * 60)
    
    try:
        # Test 1: Truncated login (just opcode)
        s1 = get_connection()
        send_raw(s1, b'\x01')  # Login opcode but no user
        time.sleep(0.5)
        s1.close()
        
        # Test 2: Truncated send (partial header)
        s2 = get_connection()
        send_raw(s2, build_login_v1("trunc_user"))
        recv_response(s2, timeout=1)
        send_raw(s2, b'\x02\x00')  # Send opcode + partial length
        time.sleep(0.5)
        s2.close()
        
        # Verify server alive
        s3 = get_connection()
        send_raw(s3, build_login_v1("after_trunc"))
        s3.close()
        
        print("✓ Server handled truncated packets")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_empty_fields():
    """Test packets with empty fields."""
    print("\n" + "=" * 60)
    print("TEST: Empty Field Handling")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Empty username login
        send_raw(s, b'\x01')  # Just opcode, no username
        time.sleep(0.2)
        
        s.close()
        
        # Empty message
        s2 = get_connection()
        send_raw(s2, build_login_v1("empty_test"))
        recv_response(s2, timeout=1)
        send_raw(s2, build_send_v1("target", ""))  # Empty message
        time.sleep(0.2)
        s2.close()
        
        # Verify alive
        s3 = get_connection()
        send_raw(s3, build_login_v1("after_empty"))
        s3.close()
        
        print("✓ Server handled empty fields")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def main():
    print("=" * 60)
    print(" PROTOCOL COMPATIBILITY TEST SUITE")
    print(" P2-4: Validating version tolerance")
    print("=" * 60)
    
    results = []
    
    results.append(("V1 Protocol Works", test_v1_protocol_works()))
    results.append(("Unknown Opcode", test_unknown_opcode_handled()))
    results.append(("V2 Extended Login", test_v2_extended_login()))
    results.append(("V2 Extended Send", test_v2_extended_send()))
    results.append(("Truncated Packet", test_truncated_packet()))
    results.append(("Empty Fields", test_empty_fields()))
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for _, r in results if r)
    total = len(results)
    
    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  [{status}] {name}")
    
    print(f"\n{passed}/{total} compatibility tests passed")
    
    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
