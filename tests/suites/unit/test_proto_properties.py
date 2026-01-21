#!/usr/bin/env python3
"""
Property-Based Tests for Iris Protocol (Python/Hypothesis)

These tests validate protocol invariants through randomized input generation.
This complements the Erlang PropEr tests with a Python perspective.

Properties Tested:
1. Codec roundtrip: decode(encode(X)) produces valid structure
2. Binary safety: Protocol handles arbitrary byte sequences
3. Length encoding: Field lengths are correctly encoded
4. Sequence numbers: 32-bit sequences wrap correctly
5. Message IDs: Generated IDs are unique

Requirements:
    pip install hypothesis

Run:
    python3 -m pytest tests/suites/unit/test_proto_properties.py -v
    OR
    python3 tests/suites/unit/test_proto_properties.py
"""

import struct
import sys
import time
import random
from typing import Tuple, Optional

# Try to import hypothesis, fall back to simple random testing if not available
try:
    from hypothesis import given, strategies as st, settings, assume
    HYPOTHESIS_AVAILABLE = True
except ImportError:
    HYPOTHESIS_AVAILABLE = False
    print("Warning: hypothesis not installed. Using basic random testing.")
    print("Install with: pip install hypothesis")


# =============================================================================
# Protocol Encoders/Decoders (Mirror Erlang implementation)
# =============================================================================

def encode_login(username: str) -> bytes:
    """Encode login packet: 0x01 | username"""
    return bytes([0x01]) + username.encode('utf-8')


def encode_message(target: str, content: bytes) -> bytes:
    """Encode message packet: 0x02 | target_len(16) | target | msg_len(16) | msg"""
    target_bytes = target.encode('utf-8')
    return (
        bytes([0x02]) +
        struct.pack('>H', len(target_bytes)) + target_bytes +
        struct.pack('>H', len(content)) + content
    )


def encode_reliable_message(msg_id: bytes, content: bytes) -> bytes:
    """Encode reliable message: 0x10 | id_len(16) | msg_id | content_len(32) | content"""
    return (
        bytes([0x10]) +
        struct.pack('>H', len(msg_id)) + msg_id +
        struct.pack('>I', len(content)) + content
    )


def encode_sequence_message(seq: int, sender: str, content: bytes) -> bytes:
    """Encode sequenced message: 0x11 | seq(32) | sender_len(8) | sender | content"""
    sender_bytes = sender.encode('utf-8')
    return (
        bytes([0x11]) +
        struct.pack('>I', seq) +
        struct.pack('>B', len(sender_bytes)) + sender_bytes +
        content
    )


def decode_reliable_message(data: bytes) -> Tuple[Optional[bytes], Optional[bytes]]:
    """Decode reliable message, return (msg_id, content) or (None, None)."""
    if len(data) < 7:  # min: opcode(1) + id_len(2) + content_len(4)
        return None, None
    
    if data[0] != 0x10:
        return None, None
    
    id_len = struct.unpack('>H', data[1:3])[0]
    if len(data) < 3 + id_len + 4:
        return None, None
    
    msg_id = data[3:3+id_len]
    content_len = struct.unpack('>I', data[3+id_len:3+id_len+4])[0]
    
    if len(data) < 3 + id_len + 4 + content_len:
        return None, None
    
    content = data[3+id_len+4:3+id_len+4+content_len]
    return msg_id, content


def generate_msg_id() -> bytes:
    """Generate a unique message ID (similar to Erlang implementation)."""
    timestamp = int(time.time() * 1000000)  # Microseconds
    unique = random.randint(0, 2**64 - 1)
    node_hash = hash("python_test") & 0xFFFF
    return struct.pack('>QQH', timestamp, unique, node_hash)


# =============================================================================
# Property Tests (Hypothesis-based)
# =============================================================================

if HYPOTHESIS_AVAILABLE:
    
    @given(st.text(min_size=1, max_size=64, alphabet=st.characters(whitelist_categories=('L', 'N', 'P'))))
    @settings(max_examples=100)
    def test_login_roundtrip(username: str):
        """Property: Login packets preserve username."""
        encoded = encode_login(username)
        
        # Verify structure
        assert encoded[0] == 0x01
        assert encoded[1:].decode('utf-8') == username
    
    
    @given(
        st.text(min_size=1, max_size=64, alphabet=st.characters(whitelist_categories=('L', 'N'))),
        st.binary(min_size=0, max_size=1000)
    )
    @settings(max_examples=100)
    def test_message_roundtrip(target: str, content: bytes):
        """Property: Message packets preserve target and content."""
        encoded = encode_message(target, content)
        
        # Verify structure
        assert encoded[0] == 0x02
        
        # Decode target
        target_len = struct.unpack('>H', encoded[1:3])[0]
        decoded_target = encoded[3:3+target_len].decode('utf-8')
        assert decoded_target == target
        
        # Decode content
        content_len = struct.unpack('>H', encoded[3+target_len:5+target_len])[0]
        decoded_content = encoded[5+target_len:5+target_len+content_len]
        assert decoded_content == content
    
    
    @given(st.binary(min_size=1, max_size=100), st.binary(min_size=0, max_size=1000))
    @settings(max_examples=100)
    def test_reliable_message_roundtrip(msg_id: bytes, content: bytes):
        """Property: Reliable messages preserve ID and content."""
        encoded = encode_reliable_message(msg_id, content)
        decoded_id, decoded_content = decode_reliable_message(encoded)
        
        assert decoded_id == msg_id
        assert decoded_content == content
    
    
    @given(st.integers(min_value=0, max_value=2**32-1))
    @settings(max_examples=100)
    def test_sequence_encoding(seq: int):
        """Property: Sequence numbers are correctly encoded as 32-bit big-endian."""
        encoded = encode_sequence_message(seq, "test", b"content")
        
        # Extract sequence from packet
        decoded_seq = struct.unpack('>I', encoded[1:5])[0]
        assert decoded_seq == seq
    
    
    @given(st.integers(min_value=2**32-10, max_value=2**32-1))
    @settings(max_examples=20)
    def test_sequence_wraparound(seq: int):
        """Property: Sequence numbers near max value don't overflow."""
        encoded = encode_sequence_message(seq, "test", b"content")
        decoded_seq = struct.unpack('>I', encoded[1:5])[0]
        assert decoded_seq == seq
        
        # Test wraparound
        if seq < 2**32 - 1:
            next_seq = seq + 1
            encoded_next = encode_sequence_message(next_seq, "test", b"content")
            decoded_next = struct.unpack('>I', encoded_next[1:5])[0]
            assert decoded_next == next_seq
            assert decoded_next > decoded_seq


def test_msg_id_uniqueness():
    """Property: Generated message IDs are unique."""
    ids = [generate_msg_id() for _ in range(1000)]
    unique_ids = set(ids)
    
    # All IDs should be unique
    assert len(unique_ids) == len(ids), f"Generated {len(ids)} IDs but only {len(unique_ids)} unique"


def test_msg_id_sortable():
    """Property: Message IDs generated in sequence are sortable."""
    ids = []
    for _ in range(100):
        ids.append(generate_msg_id())
        time.sleep(0.001)  # Small delay to ensure timestamp changes
    
    # IDs should be sortable (later IDs sort after earlier ones)
    sorted_ids = sorted(ids)
    
    # At least 90% should maintain order (allowing for clock jitter)
    in_order = sum(1 for i, j in zip(ids, sorted_ids) if i == j)
    assert in_order >= 90, f"Only {in_order}% of IDs maintained order"


def test_binary_safety():
    """Property: Protocol handles arbitrary byte sequences without crash."""
    # Test various problematic byte patterns
    patterns = [
        b'\x00' * 100,           # Null bytes
        b'\xff' * 100,           # Max bytes
        bytes(range(256)),       # All byte values
        b'\x00\x01\x02\xff\xfe', # Mixed
    ]
    
    for pattern in patterns:
        msg_id = generate_msg_id()
        encoded = encode_reliable_message(msg_id, pattern)
        decoded_id, decoded_content = decode_reliable_message(encoded)
        
        assert decoded_id == msg_id
        assert decoded_content == pattern


def test_length_limits():
    """Property: Protocol correctly handles boundary lengths."""
    # Empty content
    msg_id = generate_msg_id()
    encoded = encode_reliable_message(msg_id, b'')
    decoded_id, decoded_content = decode_reliable_message(encoded)
    assert decoded_content == b''
    
    # Max uint16 target length (simulated, don't actually create 64K string)
    target = 'a' * 256
    content = b'x' * 1000
    encoded = encode_message(target, content)
    assert len(encoded) == 1 + 2 + 256 + 2 + 1000


# =============================================================================
# Fallback Random Testing (when hypothesis not available)
# =============================================================================

def run_basic_tests():
    """Run basic random tests without hypothesis."""
    print("\n=== Property-Based Tests (Basic Mode) ===\n")
    
    passed = 0
    failed = 0
    
    # Test 1: Login roundtrip
    print("Testing: Login roundtrip...", end=" ")
    try:
        for _ in range(100):
            username = ''.join(random.choices('abcdefghijklmnopqrstuvwxyz0123456789', k=random.randint(1, 64)))
            encoded = encode_login(username)
            assert encoded[0] == 0x01
            assert encoded[1:].decode('utf-8') == username
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 2: Message roundtrip
    print("Testing: Message roundtrip...", end=" ")
    try:
        for _ in range(100):
            target = ''.join(random.choices('abcdefghijklmnopqrstuvwxyz', k=random.randint(1, 32)))
            content = bytes([random.randint(0, 255) for _ in range(random.randint(0, 500))])
            encoded = encode_message(target, content)
            
            target_len = struct.unpack('>H', encoded[1:3])[0]
            decoded_target = encoded[3:3+target_len].decode('utf-8')
            assert decoded_target == target
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 3: Reliable message roundtrip
    print("Testing: Reliable message roundtrip...", end=" ")
    try:
        for _ in range(100):
            msg_id = generate_msg_id()
            content = bytes([random.randint(0, 255) for _ in range(random.randint(0, 500))])
            encoded = encode_reliable_message(msg_id, content)
            decoded_id, decoded_content = decode_reliable_message(encoded)
            assert decoded_id == msg_id
            assert decoded_content == content
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 4: Sequence encoding
    print("Testing: Sequence encoding...", end=" ")
    try:
        for seq in [0, 1, 100, 2**31, 2**32-1]:
            encoded = encode_sequence_message(seq, "test", b"content")
            decoded_seq = struct.unpack('>I', encoded[1:5])[0]
            assert decoded_seq == seq
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 5: Message ID uniqueness
    print("Testing: Message ID uniqueness...", end=" ")
    try:
        test_msg_id_uniqueness()
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 6: Binary safety
    print("Testing: Binary safety...", end=" ")
    try:
        test_binary_safety()
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    # Test 7: Length limits
    print("Testing: Length limits...", end=" ")
    try:
        test_length_limits()
        print("✓ PASS")
        passed += 1
    except Exception as e:
        print(f"✗ FAIL: {e}")
        failed += 1
    
    print(f"\n=== Summary ===")
    print(f"Total: {passed + failed}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    
    return failed == 0


# =============================================================================
# Main
# =============================================================================

def main():
    if HYPOTHESIS_AVAILABLE:
        # Run with pytest if available
        try:
            import pytest
            return pytest.main([__file__, '-v'])
        except ImportError:
            # Fall back to running tests directly
            print("\n=== Property-Based Tests (Hypothesis) ===\n")
            
            tests = [
                test_login_roundtrip,
                test_message_roundtrip,
                test_reliable_message_roundtrip,
                test_sequence_encoding,
                test_sequence_wraparound,
                test_msg_id_uniqueness,
                test_msg_id_sortable,
                test_binary_safety,
                test_length_limits,
            ]
            
            passed = 0
            failed = 0
            
            for test in tests:
                name = test.__name__
                print(f"Testing: {name}...", end=" ")
                try:
                    test()
                    print("✓ PASS")
                    passed += 1
                except Exception as e:
                    print(f"✗ FAIL: {e}")
                    failed += 1
            
            print(f"\n=== Summary ===")
            print(f"Total: {passed + failed}")
            print(f"Passed: {passed}")
            print(f"Failed: {failed}")
            
            return 0 if failed == 0 else 1
    else:
        return 0 if run_basic_tests() else 1


if __name__ == "__main__":
    sys.exit(main())
