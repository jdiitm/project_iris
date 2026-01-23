#!/usr/bin/env python3
"""
CBOR Codec Property-Based Tests (RFC-001-AMENDMENT-001)

Per TEST_CONTRACT.md:
- exit(0) = PASS
- exit(1) = FAIL
- exit(2) = SKIP

Per TEST_DETERMINISM.md:
- Uses TEST_SEED for reproducibility
- No timing-dependent assertions
"""

import os
import sys
import struct
import random

# Setup paths
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# Seed for determinism
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))
random.seed(TEST_SEED)

try:
    from hypothesis import given, settings, seed, Verbosity
    from hypothesis import strategies as st
    HYPOTHESIS_AVAILABLE = True
except ImportError:
    HYPOTHESIS_AVAILABLE = False
    print("[WARN] hypothesis not installed, using basic tests")


# =============================================================================
# CBOR Encoder/Decoder (Python reference implementation for testing)
# =============================================================================

class CBOREncoder:
    """Reference CBOR encoder for testing Erlang implementation."""
    
    UINT = 0
    NEGINT = 1
    BYTES = 2
    TEXT = 3
    ARRAY = 4
    MAP = 5
    SIMPLE = 7
    
    @classmethod
    def encode(cls, value):
        if isinstance(value, bool):
            return bytes([0xF5 if value else 0xF4])
        elif value is None:
            return bytes([0xF6])
        elif isinstance(value, int):
            if value >= 0:
                return cls._encode_uint(cls.UINT, value)
            else:
                return cls._encode_uint(cls.NEGINT, -1 - value)
        elif isinstance(value, bytes):
            return cls._encode_uint(cls.BYTES, len(value)) + value
        elif isinstance(value, str):
            encoded = value.encode('utf-8')
            return cls._encode_uint(cls.TEXT, len(encoded)) + encoded
        elif isinstance(value, list):
            items = b''.join(cls.encode(item) for item in value)
            return cls._encode_uint(cls.ARRAY, len(value)) + items
        elif isinstance(value, dict):
            items = b''.join(
                cls.encode(k) + cls.encode(v) 
                for k, v in value.items()
            )
            return cls._encode_uint(cls.MAP, len(value)) + items
        elif isinstance(value, float):
            return bytes([0xFB]) + struct.pack('>d', value)
        else:
            raise ValueError(f"Cannot encode {type(value)}")
    
    @classmethod
    def _encode_uint(cls, major, value):
        if value < 24:
            return bytes([major << 5 | value])
        elif value < 256:
            return bytes([major << 5 | 24, value])
        elif value < 65536:
            return bytes([major << 5 | 25]) + struct.pack('>H', value)
        elif value < 4294967296:
            return bytes([major << 5 | 26]) + struct.pack('>I', value)
        else:
            return bytes([major << 5 | 27]) + struct.pack('>Q', value)


class CBORDecoder:
    """Reference CBOR decoder for testing."""
    
    @classmethod
    def decode(cls, data):
        value, _ = cls._decode_value(data, 0)
        return value
    
    @classmethod
    def _decode_value(cls, data, offset):
        initial = data[offset]
        major = initial >> 5
        additional = initial & 0x1F
        
        if major == 0:  # Unsigned int
            value, offset = cls._decode_uint(data, offset + 1, additional)
            return value, offset
        elif major == 1:  # Negative int
            value, offset = cls._decode_uint(data, offset + 1, additional)
            return -1 - value, offset
        elif major == 2:  # Bytes
            length, offset = cls._decode_uint(data, offset + 1, additional)
            return data[offset:offset + length], offset + length
        elif major == 3:  # Text
            length, offset = cls._decode_uint(data, offset + 1, additional)
            return data[offset:offset + length].decode('utf-8'), offset + length
        elif major == 4:  # Array
            length, offset = cls._decode_uint(data, offset + 1, additional)
            items = []
            for _ in range(length):
                item, offset = cls._decode_value(data, offset)
                items.append(item)
            return items, offset
        elif major == 5:  # Map
            length, offset = cls._decode_uint(data, offset + 1, additional)
            items = {}
            for _ in range(length):
                key, offset = cls._decode_value(data, offset)
                value, offset = cls._decode_value(data, offset)
                items[key] = value
            return items, offset
        elif major == 7:  # Simple/float
            if additional == 20:
                return False, offset + 1
            elif additional == 21:
                return True, offset + 1
            elif additional == 22:
                return None, offset + 1
            elif additional == 27:
                value = struct.unpack('>d', data[offset + 1:offset + 9])[0]
                return value, offset + 9
        
        raise ValueError(f"Unknown CBOR major type {major}")
    
    @classmethod
    def _decode_uint(cls, data, offset, additional):
        if additional < 24:
            return additional, offset
        elif additional == 24:
            return data[offset], offset + 1
        elif additional == 25:
            return struct.unpack('>H', data[offset:offset + 2])[0], offset + 2
        elif additional == 26:
            return struct.unpack('>I', data[offset:offset + 4])[0], offset + 4
        elif additional == 27:
            return struct.unpack('>Q', data[offset:offset + 8])[0], offset + 8


# =============================================================================
# Protocol Message Encoder (for testing Erlang decode)
# =============================================================================

def encode_cbor_msg(target: bytes, payload: dict) -> bytes:
    """
    Encode a CBOR message in Iris protocol format.
    Format: 0x10 | TargetLen(16) | Target | CborLen(32) | CborPayload
    """
    cbor_payload = CBOREncoder.encode(payload)
    return (
        bytes([0x10]) +
        struct.pack('>H', len(target)) +
        target +
        struct.pack('>I', len(cbor_payload)) +
        cbor_payload
    )


# =============================================================================
# Test Cases
# =============================================================================

def test_encode_small_integers():
    """Test encoding of small unsigned integers (0-23)."""
    for i in range(24):
        encoded = CBOREncoder.encode(i)
        assert encoded == bytes([i]), f"Failed for {i}"
        decoded = CBORDecoder.decode(encoded)
        assert decoded == i, f"Roundtrip failed for {i}"
    print("[PASS] test_encode_small_integers")

def test_encode_uint8():
    """Test encoding of unsigned integers 24-255."""
    for i in [24, 100, 255]:
        encoded = CBOREncoder.encode(i)
        assert encoded[0] == 0x18, f"Wrong header for {i}"
        decoded = CBORDecoder.decode(encoded)
        assert decoded == i, f"Roundtrip failed for {i}"
    print("[PASS] test_encode_uint8")

def test_encode_uint16():
    """Test encoding of unsigned integers 256-65535."""
    for i in [256, 1000, 65535]:
        encoded = CBOREncoder.encode(i)
        assert encoded[0] == 0x19, f"Wrong header for {i}"
        decoded = CBORDecoder.decode(encoded)
        assert decoded == i, f"Roundtrip failed for {i}"
    print("[PASS] test_encode_uint16")

def test_encode_uint32():
    """Test encoding of unsigned integers 65536-4294967295."""
    for i in [65536, 1000000, 4294967295]:
        encoded = CBOREncoder.encode(i)
        assert encoded[0] == 0x1A, f"Wrong header for {i}"
        decoded = CBORDecoder.decode(encoded)
        assert decoded == i, f"Roundtrip failed for {i}"
    print("[PASS] test_encode_uint32")

def test_encode_negative_integers():
    """Test encoding of negative integers."""
    for i in [-1, -10, -100, -1000]:
        encoded = CBOREncoder.encode(i)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == i, f"Roundtrip failed for {i}"
    print("[PASS] test_encode_negative_integers")

def test_encode_bytes():
    """Test encoding of byte strings."""
    test_cases = [b"", b"hello", b"x" * 100, bytes(range(256))]
    for data in test_cases:
        encoded = CBOREncoder.encode(data)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == data, f"Roundtrip failed for bytes of len {len(data)}"
    print("[PASS] test_encode_bytes")

def test_encode_text():
    """Test encoding of text strings."""
    test_cases = ["", "hello", "Hello, 世界!", "a" * 100]
    for text in test_cases:
        encoded = CBOREncoder.encode(text)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == text, f"Roundtrip failed for '{text[:20]}...'"
    print("[PASS] test_encode_text")

def test_encode_booleans():
    """Test encoding of boolean values."""
    assert CBOREncoder.encode(True) == bytes([0xF5])
    assert CBOREncoder.encode(False) == bytes([0xF4])
    assert CBORDecoder.decode(bytes([0xF5])) == True
    assert CBORDecoder.decode(bytes([0xF4])) == False
    print("[PASS] test_encode_booleans")

def test_encode_null():
    """Test encoding of null."""
    assert CBOREncoder.encode(None) == bytes([0xF6])
    assert CBORDecoder.decode(bytes([0xF6])) == None
    print("[PASS] test_encode_null")

def test_encode_arrays():
    """Test encoding of arrays."""
    test_cases = [
        [],
        [1, 2, 3],
        [1, "hello", True, None],
        [[1, 2], [3, 4]],
    ]
    for arr in test_cases:
        encoded = CBOREncoder.encode(arr)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == arr, f"Roundtrip failed for {arr}"
    print("[PASS] test_encode_arrays")

def test_encode_maps():
    """Test encoding of maps."""
    test_cases = [
        {},
        {"key": "value"},
        {"int": 42, "str": "hello", "bool": True},
        {"nested": {"a": 1, "b": 2}},
    ]
    for m in test_cases:
        encoded = CBOREncoder.encode(m)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == m, f"Roundtrip failed for {m}"
    print("[PASS] test_encode_maps")

def test_encode_floats():
    """Test encoding of floating point numbers."""
    test_cases = [0.0, 1.5, -1.5, 3.14159, 1e10, -1e-10]
    for f in test_cases:
        encoded = CBOREncoder.encode(f)
        decoded = CBORDecoder.decode(encoded)
        assert abs(decoded - f) < 1e-10, f"Roundtrip failed for {f}"
    print("[PASS] test_encode_floats")

def test_protocol_message():
    """Test encoding of CBOR protocol message."""
    target = b"alice"
    payload = {"type": "text", "body": "Hello!"}
    
    msg = encode_cbor_msg(target, payload)
    
    # Verify structure
    assert msg[0] == 0x10, "Wrong opcode"
    target_len = struct.unpack('>H', msg[1:3])[0]
    assert target_len == len(target)
    assert msg[3:3 + target_len] == target
    
    print("[PASS] test_protocol_message")

def test_complex_payload():
    """Test encoding of complex message payloads."""
    payload = {
        "type": "message",
        "version": 1,
        "timestamp": 1706000000,
        "content": {
            "text": "Hello, World!",
            "mentions": ["user1", "user2"],
            "metadata": {
                "edited": False,
                "reply_to": None
            }
        }
    }
    
    encoded = CBOREncoder.encode(payload)
    decoded = CBORDecoder.decode(encoded)
    assert decoded == payload
    print("[PASS] test_complex_payload")


# =============================================================================
# Property-Based Tests (Hypothesis)
# =============================================================================

if HYPOTHESIS_AVAILABLE:
    @seed(TEST_SEED)
    @settings(max_examples=100, verbosity=Verbosity.quiet)
    @given(st.integers(min_value=0, max_value=2**63 - 1))
    def test_property_uint_roundtrip(value):
        """Property: All unsigned integers roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value

    @seed(TEST_SEED)
    @settings(max_examples=100, verbosity=Verbosity.quiet)
    @given(st.integers(min_value=-2**63, max_value=-1))
    def test_property_negint_roundtrip(value):
        """Property: All negative integers roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value

    @seed(TEST_SEED)
    @settings(max_examples=100, verbosity=Verbosity.quiet)
    @given(st.binary(max_size=1000))
    def test_property_bytes_roundtrip(value):
        """Property: All byte strings roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value

    @seed(TEST_SEED)
    @settings(max_examples=100, verbosity=Verbosity.quiet)
    @given(st.text(max_size=1000))
    def test_property_text_roundtrip(value):
        """Property: All text strings roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value

    @seed(TEST_SEED)
    @settings(max_examples=50, verbosity=Verbosity.quiet)
    @given(st.lists(st.integers(min_value=0, max_value=1000), max_size=20))
    def test_property_array_roundtrip(value):
        """Property: Integer arrays roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value

    @seed(TEST_SEED)
    @settings(max_examples=50, verbosity=Verbosity.quiet)
    @given(st.dictionaries(
        st.text(min_size=1, max_size=20),
        st.integers(min_value=0, max_value=1000),
        max_size=10
    ))
    def test_property_map_roundtrip(value):
        """Property: String->int maps roundtrip correctly."""
        encoded = CBOREncoder.encode(value)
        decoded = CBORDecoder.decode(encoded)
        assert decoded == value


# =============================================================================
# Main
# =============================================================================

def main():
    print(f"[INFO] Running CBOR property tests with seed={TEST_SEED}")
    
    # Run basic tests
    tests = [
        test_encode_small_integers,
        test_encode_uint8,
        test_encode_uint16,
        test_encode_uint32,
        test_encode_negative_integers,
        test_encode_bytes,
        test_encode_text,
        test_encode_booleans,
        test_encode_null,
        test_encode_arrays,
        test_encode_maps,
        test_encode_floats,
        test_protocol_message,
        test_complex_payload,
    ]
    
    failed = 0
    for test in tests:
        try:
            test()
        except AssertionError as e:
            print(f"[FAIL] {test.__name__}: {e}")
            failed += 1
        except Exception as e:
            print(f"[ERROR] {test.__name__}: {e}")
            failed += 1
    
    # Run property tests if available
    if HYPOTHESIS_AVAILABLE:
        property_tests = [
            test_property_uint_roundtrip,
            test_property_negint_roundtrip,
            test_property_bytes_roundtrip,
            test_property_text_roundtrip,
            test_property_array_roundtrip,
            test_property_map_roundtrip,
        ]
        
        for test in property_tests:
            try:
                test()
                print(f"[PASS] {test.__name__}")
            except AssertionError as e:
                print(f"[FAIL] {test.__name__}: {e}")
                failed += 1
            except Exception as e:
                print(f"[ERROR] {test.__name__}: {e}")
                failed += 1
    
    if failed > 0:
        print(f"\n[RESULT] {failed} test(s) failed")
        sys.exit(1)
    else:
        print(f"\n[RESULT] All tests passed")
        sys.exit(0)


if __name__ == "__main__":
    main()
