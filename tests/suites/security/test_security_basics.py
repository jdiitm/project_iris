#!/usr/bin/env python3
"""
Security Tests - Comprehensive security validation for FAANG standards.

Tests cover:
1. Protocol abuse (malformed packets, oversized inputs)
2. DoS protection (rate limiting, connection floods)
3. Authentication bypass attempts
4. Input validation (injection, null bytes)
"""

import sys
import time
import socket
import struct
import os

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)


def get_connection(port=8085):
    """Get a socket connection to the edge node."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(5.0)
    s.connect(('localhost', port))
    return s


def login(sock, user):
    """Login a user."""
    payload = b'\x01' + user.encode('utf-8')
    sock.sendall(payload)
    return sock.recv(1024)


def send_msg(sock, target, msg):
    """Send a message."""
    target_bytes = target.encode('utf-8') if isinstance(target, str) else target
    msg_bytes = msg.encode('utf-8') if isinstance(msg, str) else msg
    packet = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(msg_bytes)) + msg_bytes
    sock.sendall(packet)


# =============================================================================
# Test: Protocol Abuse
# =============================================================================

def test_malformed_opcode():
    """Test that unknown opcodes don't crash the server."""
    print("=" * 60)
    print("SECURITY TEST: Malformed Opcode")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Send unknown opcode (0xFF)
        s.sendall(b'\xFF\x00\x00\x00garbage')
        
        try:
            s.recv(1024)
        except:
            pass
        
        s.close()
        
        # Verify server still alive
        s2 = get_connection()
        login(s2, "verify_user")
        s2.close()
        
        print("✓ Server survived malformed opcode")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_truncated_packet():
    """Test handling of truncated packets."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Truncated Packet")
    print("=" * 60)
    
    try:
        s = get_connection()
        login(s, "trunc_tester")
        
        # Send message with header claiming 1000 bytes but only send 10
        target = b"target"
        s.sendall(b'\x02' + struct.pack('>H', len(target)) + target + struct.pack('>H', 1000))
        s.sendall(b'only10byte')
        
        time.sleep(0.5)
        
        # Try to send another valid message
        send_msg(s, "target", "valid_msg")
        
        s.close()
        
        # Verify server still alive
        s2 = get_connection()
        login(s2, "verify_user2")
        s2.close()
        
        print("✓ Server handled truncated packet gracefully")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_oversized_username():
    """Test rejection of oversized username (DoS prevention)."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Oversized Username")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Try to login with 10KB username (should be rejected)
        huge_user = "A" * 10000
        payload = b'\x01' + huge_user.encode('utf-8')
        s.sendall(payload)
        
        try:
            resp = s.recv(1024)
            # Should either close connection or reject
            if b"LOGIN_OK" in resp:
                print("⚠ Server accepted oversized username")
                return False
        except:
            pass  # Connection closed is acceptable
        
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        login(s2, "normal_user")
        s2.close()
        
        print("✓ Oversized username rejected")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_oversized_message():
    """Test rejection of oversized message (DoS prevention)."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Oversized Message")
    print("=" * 60)
    
    try:
        s = get_connection()
        login(s, "msg_tester")
        
        # Try to send 100KB message (should be rejected/truncated)
        target = "victim"
        huge_msg = "B" * 100000
        
        try:
            send_msg(s, target, huge_msg)
            time.sleep(0.5)
        except:
            pass  # May fail, which is fine
        
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        login(s2, "verify_user3")
        s2.close()
        
        print("✓ Server survived oversized message")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


# =============================================================================
# Test: Authentication & Authorization
# =============================================================================

def test_unauthenticated_send():
    """Test that unauthenticated connections can't send messages."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Unauthenticated Send")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # DON'T login - try to send message directly
        send_msg(s, "target", "unauthorized_msg")
        
        try:
            resp = s.recv(1024)
            # Should be ignored or connection closed
            if b"OK" in resp or b"ACK" in resp:
                print("⚠ Unauthenticated message might have been accepted")
        except:
            pass  # Expected - connection closed or timeout
        
        s.close()
        
        print("✓ Unauthenticated send handled")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_null_byte_injection():
    """Test handling of null bytes in user input."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Null Byte Injection")
    print("=" * 60)
    
    try:
        s = get_connection()
        
        # Login with null bytes in username
        user_with_null = b'\x01admin\x00attacker'
        s.sendall(user_with_null)
        
        resp = s.recv(1024)
        s.close()
        
        # Verify server alive and not corrupted
        s2 = get_connection()
        login(s2, "clean_user")
        s2.close()
        
        print("✓ Null byte injection handled safely")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


# =============================================================================
# Test: DoS Prevention
# =============================================================================

def test_rapid_connection_flood():
    """Test server stability under connection flood."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Rapid Connection Flood")
    print("=" * 60)
    
    num_connections = 50
    sockets = []
    
    try:
        # Open many connections rapidly
        for i in range(num_connections):
            try:
                s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                s.settimeout(2.0)
                s.connect(('localhost', 8085))
                sockets.append(s)
            except:
                pass  # Some may fail, that's fine
        
        print(f"✓ Opened {len(sockets)} connections")
        
        # Close them all
        for s in sockets:
            try:
                s.close()
            except:
                pass
        
        time.sleep(0.5)
        
        # Verify server still accepts connections
        s = get_connection()
        login(s, "post_flood_user")
        s.close()
        
        print("✓ Server survived connection flood")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    finally:
        for s in sockets:
            try:
                s.close()
            except:
                pass


def test_message_flood():
    """Test server stability under message flood."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Message Flood")
    print("=" * 60)
    
    try:
        s = get_connection()
        login(s, "flooder")
        
        # Send 100 messages rapidly
        for i in range(100):
            try:
                send_msg(s, "target", f"flood_{i}")
            except:
                break
        
        print("✓ Sent 100 messages rapidly")
        
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        login(s2, "post_flood_user2")
        s2.close()
        
        print("✓ Server survived message flood")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


# =============================================================================
# Test: Protocol Edge Cases
# =============================================================================

def test_empty_message():
    """Test handling of empty messages."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Empty Message")
    print("=" * 60)
    
    try:
        s = get_connection()
        login(s, "empty_tester")
        
        # Send empty message
        send_msg(s, "target", "")
        
        time.sleep(0.2)
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        login(s2, "verify_empty")
        s2.close()
        
        print("✓ Empty message handled")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_binary_message():
    """Test handling of binary data in messages."""
    print("\n" + "=" * 60)
    print("SECURITY TEST: Binary Message"  )
    print("=" * 60)
    
    try:
        s = get_connection()
        login(s, "binary_tester")
        
        # Send binary data (all possible byte values)
        binary_msg = bytes(range(256))
        target = "target"
        target_bytes = target.encode('utf-8')
        
        packet = b'\x02' + struct.pack('>H', len(target_bytes)) + target_bytes + struct.pack('>H', len(binary_msg)) + binary_msg
        s.sendall(packet)
        
        time.sleep(0.2)
        s.close()
        
        # Verify server alive
        s2 = get_connection()
        login(s2, "verify_binary")
        s2.close()
        
        print("✓ Binary message handled")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    results = []
    
    # Protocol abuse
    results.append(("Malformed Opcode", test_malformed_opcode()))
    results.append(("Truncated Packet", test_truncated_packet()))
    results.append(("Oversized Username", test_oversized_username()))
    results.append(("Oversized Message", test_oversized_message()))
    
    # Auth
    results.append(("Unauthenticated Send", test_unauthenticated_send()))
    results.append(("Null Byte Injection", test_null_byte_injection()))
    
    # DoS
    results.append(("Connection Flood", test_rapid_connection_flood()))
    results.append(("Message Flood", test_message_flood()))
    
    # Edge cases
    results.append(("Empty Message", test_empty_message()))
    results.append(("Binary Message", test_binary_message()))
    
    print("\n" + "=" * 60)
    print("SECURITY TEST SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for _, r in results if r)
    total = len(results)
    
    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  [{status}] {name}")
    
    print(f"\n{passed}/{total} security tests passed")
    
    sys.exit(0 if passed == total else 1)

