#!/usr/bin/env python3
"""
AUDIT5 P1: Protocol Fuzzing Test

Per Audit5 Finding M4:
"A single fuzzed packet crashes the BEAM"

This test sends malformed/garbage packets to verify:
1. Server doesn't crash
2. Server handles invalid input gracefully
3. No buffer overflows or hangs
"""

import sys
import os
import socket
import struct
import time
import random

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities import IrisClient


def get_raw_socket(port=8085, timeout=5):
    """Get a raw TCP socket."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(timeout)
    s.connect(('localhost', port))
    return s


def server_alive(port=8085):
    """Check if server is still accepting connections."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect(('localhost', port))
        s.close()
        return True
    except:
        return False


def test_garbage_bytes():
    """Send random garbage bytes."""
    print("\n=== Test: Random Garbage Bytes ===")
    
    crashed = False
    
    for i in range(10):
        try:
            s = get_raw_socket()
            garbage = bytes([random.randint(0, 255) for _ in range(random.randint(1, 1024))])
            s.sendall(garbage)
            time.sleep(0.1)
            s.close()
        except:
            pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived random garbage")
        return True
    else:
        print("✗ FAIL: Server crashed from garbage bytes")
        return False


def test_oversized_packets():
    """Send oversized packets."""
    print("\n=== Test: Oversized Packets ===")
    
    try:
        s = get_raw_socket()
        
        # Claim a huge packet size
        s.sendall(b'\x02' + struct.pack('>H', 65535) + b'x' * 100)
        time.sleep(0.2)
        s.close()
        
        # Send actual huge packet
        s = get_raw_socket()
        s.sendall(b'\x01' + b'A' * 10000)  # 10KB username
        time.sleep(0.2)
        s.close()
        
    except:
        pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived oversized packets")
        return True
    else:
        print("✗ FAIL: Server crashed from oversized packets")
        return False


def test_null_bytes():
    """Send packets with null bytes."""
    print("\n=== Test: Null Byte Injection ===")
    
    try:
        s = get_raw_socket()
        s.sendall(b'\x01user\x00name')  # Null in username
        time.sleep(0.1)
        s.close()
        
        s = get_raw_socket()
        s.sendall(b'\x01' + b'\x00' * 100)  # All nulls
        time.sleep(0.1)
        s.close()
        
    except:
        pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived null byte injection")
        return True
    else:
        print("✗ FAIL: Server crashed from null bytes")
        return False


def test_rapid_disconnect():
    """Connect and disconnect rapidly."""
    print("\n=== Test: Rapid Connect/Disconnect ===")
    
    for i in range(50):
        try:
            s = get_raw_socket(timeout=1)
            s.sendall(b'\x01test')
            s.close()
        except:
            pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived rapid reconnects")
        return True
    else:
        print("✗ FAIL: Server crashed from rapid reconnects")
        return False


def test_partial_packets():
    """Send incomplete packets."""
    print("\n=== Test: Incomplete Packets ===")
    
    try:
        # Send opcode only
        s = get_raw_socket()
        s.sendall(b'\x02')
        time.sleep(0.2)
        s.close()
        
        # Send partial length
        s = get_raw_socket()
        s.sendall(b'\x02\x00')
        time.sleep(0.2)
        s.close()
        
        # Send truncated message
        s = get_raw_socket()
        s.sendall(b'\x02\x00\x10short')
        time.sleep(0.2)
        s.close()
        
    except:
        pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived partial packets")
        return True
    else:
        print("✗ FAIL: Server crashed from partial packets")
        return False


def test_binary_protocol_abuse():
    """Test known protocol abuse patterns."""
    print("\n=== Test: Binary Protocol Abuse ===")
    
    evil_packets = [
        b'\xff' * 100,           # Invalid opcode spam
        b'\x01' + b'\xff' * 50,  # Login with binary garbage
        b'\x02\xff\xff' + b'x' * 1000,  # SEND with max length claim
        b'\x00' * 100,           # All zeros
        b'\x01\x01\x01\x01',     # Nested opcodes
        struct.pack('>I', 0xDEADBEEF) * 10,  # Magic numbers
    ]
    
    for packet in evil_packets:
        try:
            s = get_raw_socket(timeout=1)
            s.sendall(packet)
            time.sleep(0.05)
            s.close()
        except:
            pass
    
    time.sleep(0.5)
    
    if server_alive():
        print("✓ Server survived protocol abuse")
        return True
    else:
        print("✗ FAIL: Server crashed from protocol abuse")
        return False


def main():
    print("=" * 60)
    print(" PROTOCOL FUZZING TEST SUITE (AUDIT5 P1)")
    print(" Testing server resilience to malformed input")
    print("=" * 60)
    
    # Pre-check
    print("\n[Pre-check] Server status...")
    if not server_alive():
        print("  ✗ Server not running")
        return 1
    print("  ✓ Server is accepting connections")
    
    tests = [
        ("Garbage Bytes", test_garbage_bytes),
        ("Oversized Packets", test_oversized_packets),
        ("Null Byte Injection", test_null_bytes),
        ("Rapid Disconnect", test_rapid_disconnect),
        ("Partial Packets", test_partial_packets),
        ("Protocol Abuse", test_binary_protocol_abuse),
    ]
    
    results = []
    for name, test_fn in tests:
        try:
            results.append((name, test_fn()))
        except Exception as e:
            print(f"  Exception in {name}: {e}")
            results.append((name, False))
    
    # Final check
    print("\n[Post-check] Server status...")
    final_alive = server_alive()
    
    if final_alive:
        print("  ✓ Server still running after all tests")
    else:
        print("  ✗ CRITICAL: Server crashed during fuzzing")
    
    # Summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for _, r in results if r)
    total = len(results)
    
    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  [{status}] {name}")
    
    print(f"\n{passed}/{total} fuzz tests passed")
    print(f"Server alive: {'YES' if final_alive else 'NO'}")
    
    # Strict: all tests must pass AND server must survive
    if passed == total and final_alive:
        print("\n✓ PROTOCOL FUZZING: PASSED")
        return 0
    else:
        print("\n✗ PROTOCOL FUZZING: FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
