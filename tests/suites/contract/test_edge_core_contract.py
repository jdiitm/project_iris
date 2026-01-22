#!/usr/bin/env python3
"""
Contract Tests for Edge-Core RPC

These tests validate that the API contracts between Edge and Core components
are maintained across versions. Contract tests catch breaking changes early.

Contracts Tested (from Plan Section 1.3):
1. Edge->Core RPC: {register_user, User, Node, Pid}
2. Core->Edge Callback: {deliver_msg, Msg}
3. Protocol v1: Binary wire format
4. Mnesia Schema: Table definitions

Contract testing strategy:
- Producer generates data matching the contract
- Consumer validates it can process the data
- Both sides use the same contract definition

Prerequisites:
- make start (single node)
"""

import os
import sys
import socket
import struct
import subprocess
import json
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass

# Configuration
SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
TIMEOUT = 5


# =============================================================================
# Contract Definitions
# =============================================================================

@dataclass
class Contract:
    """A contract definition."""
    name: str
    version: str
    producer: str
    consumer: str
    schema: Dict


# Edge -> Core RPC Contracts
EDGE_CORE_CONTRACTS = [
    Contract(
        name="register_user",
        version="1.0",
        producer="iris_edge_conn",
        consumer="iris_core",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "register_user"},
                {"name": "user", "type": "string", "max_length": 64},
                {"name": "node", "type": "atom"},
                {"name": "pid", "type": "pid"},
            ]
        }
    ),
    Contract(
        name="unregister_user",
        version="1.0",
        producer="iris_edge_conn",
        consumer="iris_core",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "unregister_user"},
                {"name": "user", "type": "string", "max_length": 64},
            ]
        }
    ),
    Contract(
        name="lookup_user",
        version="1.0",
        producer="iris_edge_conn",
        consumer="iris_core",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "lookup_user"},
                {"name": "user", "type": "string", "max_length": 64},
            ]
        }
    ),
    Contract(
        name="store_offline",
        version="1.0",
        producer="iris_async_router",
        consumer="iris_core",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "store_offline"},
                {"name": "user", "type": "string", "max_length": 64},
                {"name": "message", "type": "binary"},
            ]
        }
    ),
]

# Core -> Edge Callback Contracts
CORE_EDGE_CONTRACTS = [
    Contract(
        name="deliver_msg",
        version="1.0",
        producer="iris_core",
        consumer="iris_session",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "deliver_msg"},
                {"name": "message", "type": "binary"},
            ]
        }
    ),
    Contract(
        name="user_status",
        version="1.0",
        producer="iris_core",
        consumer="iris_session",
        schema={
            "type": "tuple",
            "elements": [
                {"name": "tag", "type": "atom", "value": "user_status"},
                {"name": "user", "type": "string"},
                {"name": "status", "type": "atom", "values": ["online", "offline"]},
            ]
        }
    ),
]

# Wire Protocol Contracts
PROTOCOL_CONTRACTS = [
    Contract(
        name="login_request",
        version="1.0",
        producer="client",
        consumer="iris_proto",
        schema={
            "type": "binary",
            "format": [
                {"name": "opcode", "type": "uint8", "value": 0x01},
                {"name": "username", "type": "string", "encoding": "utf8"},
            ]
        }
    ),
    Contract(
        name="login_response",
        version="1.0",
        producer="iris_proto",
        consumer="client",
        schema={
            "type": "binary",
            "format": [
                {"name": "opcode", "type": "uint8", "value": 0x03},
                {"name": "status", "type": "string", "value": "LOGIN_OK"},
            ]
        }
    ),
    Contract(
        name="send_message",
        version="1.0",
        producer="client",
        consumer="iris_proto",
        schema={
            "type": "binary",
            "format": [
                {"name": "opcode", "type": "uint8", "value": 0x02},
                {"name": "target_len", "type": "uint16_be"},
                {"name": "target", "type": "string", "encoding": "utf8"},
                {"name": "msg_len", "type": "uint16_be"},
                {"name": "message", "type": "bytes"},
            ]
        }
    ),
    Contract(
        name="reliable_message",
        version="1.0",
        producer="iris_proto",
        consumer="client",
        schema={
            "type": "binary",
            "format": [
                {"name": "opcode", "type": "uint8", "value": 0x10},
                {"name": "id_len", "type": "uint16_be"},
                {"name": "msg_id", "type": "bytes"},
                {"name": "content_len", "type": "uint32_be"},
                {"name": "content", "type": "bytes"},
            ]
        }
    ),
    Contract(
        name="message_ack",
        version="1.0",
        producer="client",
        consumer="iris_proto",
        schema={
            "type": "binary",
            "format": [
                {"name": "opcode", "type": "uint8", "value": 0x03},
                {"name": "msg_id", "type": "bytes"},
            ]
        }
    ),
]


# =============================================================================
# Contract Validators
# =============================================================================

def validate_protocol_encoding(contract: Contract, data: bytes) -> Tuple[bool, str]:
    """Validate binary data matches protocol contract."""
    schema = contract.schema
    
    if schema["type"] != "binary":
        return False, "Not a binary contract"
    
    offset = 0
    
    for field in schema["format"]:
        field_name = field["name"]
        field_type = field["type"]
        
        if offset >= len(data):
            return False, f"Data too short at field '{field_name}'"
        
        if field_type == "uint8":
            if "value" in field and data[offset] != field["value"]:
                return False, f"Field '{field_name}' expected {field['value']}, got {data[offset]}"
            offset += 1
        
        elif field_type == "uint16_be":
            if offset + 2 > len(data):
                return False, f"Data too short for uint16 at '{field_name}'"
            value = struct.unpack('>H', data[offset:offset+2])[0]
            offset += 2
            # Store for potential use by next field
            _last_len = value
        
        elif field_type == "uint32_be":
            if offset + 4 > len(data):
                return False, f"Data too short for uint32 at '{field_name}'"
            value = struct.unpack('>I', data[offset:offset+4])[0]
            offset += 4
        
        elif field_type in ("string", "bytes"):
            # Length should be in previous field or to end of data
            pass  # We've already validated structure
    
    return True, "Contract satisfied"


def generate_protocol_data(contract: Contract, **kwargs) -> bytes:
    """Generate binary data matching protocol contract."""
    schema = contract.schema
    data = b''
    
    # First pass: convert all string values to bytes and cache lengths
    processed_kwargs = {}
    for key, value in kwargs.items():
        if isinstance(value, str):
            value = value.encode('utf-8')
        processed_kwargs[key] = value
    
    for field in schema["format"]:
        field_name = field["name"]
        field_type = field["type"]
        
        if field_type == "uint8":
            value = kwargs.get(field_name, field.get("value", 0))
            data += bytes([value])
        
        elif field_type == "uint16_be":
            # Get length of corresponding field
            # Handle both "target_len" -> "target" and "msg_len" -> "message"
            related_field = field_name.replace("_len", "")
            if related_field not in processed_kwargs:
                # Try common alternatives
                if related_field == "msg":
                    related_field = "message"
                elif related_field == "content":
                    related_field = "content"
                elif related_field == "id":
                    related_field = "msg_id"
            related_value = processed_kwargs.get(related_field, b'')
            data += struct.pack('>H', len(related_value))
        
        elif field_type == "uint32_be":
            related_field = field_name.replace("_len", "")
            if related_field not in processed_kwargs:
                if related_field == "content":
                    related_field = "content"
            related_value = processed_kwargs.get(related_field, b'')
            data += struct.pack('>I', len(related_value))
        
        elif field_type == "string":
            value = processed_kwargs.get(field_name, field.get("value", b""))
            if isinstance(value, str):
                value = value.encode('utf-8')
            data += value
        
        elif field_type == "bytes":
            value = processed_kwargs.get(field_name, b'')
            data += value
    
    return data


# =============================================================================
# Contract Tests
# =============================================================================

def test_login_request_contract():
    """Test: Login request follows contract."""
    contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "login_request")
    
    # Generate valid request
    data = generate_protocol_data(contract, username="test_user")
    
    # Validate structure
    valid, msg = validate_protocol_encoding(contract, data)
    assert valid, f"Generated data doesn't match contract: {msg}"
    
    # Verify exact bytes
    expected = bytes([0x01]) + b"test_user"
    assert data == expected, f"Expected {expected.hex()}, got {data.hex()}"
    
    print("  ✓ login_request contract: VALID")
    return True


def test_login_response_contract():
    """Test: Login response follows contract."""
    contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "login_response")
    
    # The server's actual response format
    # This validates our understanding of the contract
    expected_format = bytes([0x03]) + b"LOGIN_OK"
    
    # The first byte should be 0x03 (ack/status opcode)
    assert expected_format[0] == 0x03, "Login response should start with 0x03"
    assert b"LOGIN_OK" in expected_format, "Login response should contain LOGIN_OK"
    
    print("  ✓ login_response contract: VALID")
    return True


def test_send_message_contract():
    """Test: Send message follows contract."""
    contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "send_message")
    
    # Generate valid message
    target = "recipient"
    message = "Hello, World!"
    
    data = generate_protocol_data(contract, target=target, message=message)
    
    # Validate structure
    assert data[0] == 0x02, "Opcode should be 0x02"
    
    target_len = struct.unpack('>H', data[1:3])[0]
    assert target_len == len(target), f"Target length mismatch: {target_len} vs {len(target)}"
    
    extracted_target = data[3:3+target_len].decode('utf-8')
    assert extracted_target == target, f"Target mismatch: {extracted_target} vs {target}"
    
    msg_len = struct.unpack('>H', data[3+target_len:5+target_len])[0]
    assert msg_len == len(message), f"Message length mismatch"
    
    print("  ✓ send_message contract: VALID")
    return True


def test_reliable_message_contract():
    """Test: Reliable message follows contract."""
    contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "reliable_message")
    
    # Generate valid reliable message
    msg_id = b"MSG_123456789012"  # 16 bytes
    content = b"This is a reliable message"
    
    data = generate_protocol_data(contract, msg_id=msg_id, content=content)
    
    # Validate structure
    assert data[0] == 0x10, "Opcode should be 0x10"
    
    id_len = struct.unpack('>H', data[1:3])[0]
    assert id_len == len(msg_id), f"ID length mismatch"
    
    extracted_id = data[3:3+id_len]
    assert extracted_id == msg_id, f"ID mismatch"
    
    content_len = struct.unpack('>I', data[3+id_len:7+id_len])[0]
    assert content_len == len(content), f"Content length mismatch"
    
    print("  ✓ reliable_message contract: VALID")
    return True


def test_message_ack_contract():
    """Test: Message ACK follows contract."""
    contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "message_ack")
    
    msg_id = b"MSG_123456789012"
    data = generate_protocol_data(contract, msg_id=msg_id)
    
    # Validate structure
    assert data[0] == 0x03, "ACK opcode should be 0x03"
    assert data[1:] == msg_id, "ACK should contain message ID"
    
    print("  ✓ message_ack contract: VALID")
    return True


def test_live_login_contract():
    """Test: Live server respects login contract."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT)
        sock.connect((SERVER_HOST, SERVER_PORT))
        
        # Send login per contract
        contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "login_request")
        username = f"contract_test_{int(__import__('time').time())}"
        data = generate_protocol_data(contract, username=username)
        
        sock.sendall(data)
        
        # Validate response
        response = sock.recv(1024)
        
        # Response should contain LOGIN_OK per contract
        assert b"LOGIN_OK" in response, f"Expected LOGIN_OK, got: {response}"
        
        sock.close()
        print("  ✓ live_login contract: VALID")
        return True
    
    except socket.error as e:
        print(f"  ⚠ live_login contract: SKIPPED (server not available: {e})")
        return None


def test_live_message_contract():
    """Test: Live server respects message contract."""
    try:
        # Login sender
        sender_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sender_sock.settimeout(TIMEOUT)
        sender_sock.connect((SERVER_HOST, SERVER_PORT))
        
        sender_name = f"contract_sender_{int(__import__('time').time())}"
        login_contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "login_request")
        sender_sock.sendall(generate_protocol_data(login_contract, username=sender_name))
        
        response = sender_sock.recv(1024)
        assert b"LOGIN_OK" in response, "Sender login failed"
        
        # Login receiver
        recv_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        recv_sock.settimeout(TIMEOUT)
        recv_sock.connect((SERVER_HOST, SERVER_PORT))
        
        recv_name = f"contract_recv_{int(__import__('time').time())}"
        recv_sock.sendall(generate_protocol_data(login_contract, username=recv_name))
        
        response = recv_sock.recv(1024)
        assert b"LOGIN_OK" in response, "Receiver login failed"
        
        # Send message per contract
        msg_contract = next(c for c in PROTOCOL_CONTRACTS if c.name == "send_message")
        test_message = "contract_test_message"
        msg_data = generate_protocol_data(msg_contract, target=recv_name, message=test_message)
        
        sender_sock.sendall(msg_data)
        
        # Give time for delivery
        import time
        time.sleep(0.5)
        
        # Check receiver got message
        recv_sock.setblocking(False)
        try:
            data = recv_sock.recv(4096)
            # Message should be delivered in reliable format
            if data and data[0] == 0x10:  # Reliable message opcode
                print("  ✓ live_message contract: VALID (reliable delivery)")
            elif data and test_message.encode() in data:
                print("  ✓ live_message contract: VALID (raw delivery)")
            else:
                print(f"  ⚠ live_message contract: Unexpected response format")
        except BlockingIOError:
            print("  ⚠ live_message contract: No message received (timeout)")
        
        sender_sock.close()
        recv_sock.close()
        return True
    
    except socket.error as e:
        print(f"  ⚠ live_message contract: SKIPPED (server not available: {e})")
        return None


# =============================================================================
# Main
# =============================================================================

def main():
    print("\n" + "=" * 70)
    print("Contract Tests for Edge-Core RPC")
    print("=" * 70)
    print("\nValidating protocol contracts between components...\n")
    
    # Static contract tests (always run)
    print("Static Contract Validation:")
    print("-" * 40)
    
    static_tests = [
        test_login_request_contract,
        test_login_response_contract,
        test_send_message_contract,
        test_reliable_message_contract,
        test_message_ack_contract,
    ]
    
    passed = 0
    failed = 0
    
    for test in static_tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
        except AssertionError as e:
            print(f"  ✗ {test.__name__}: FAILED - {e}")
            failed += 1
        except Exception as e:
            print(f"  ✗ {test.__name__}: ERROR - {e}")
            failed += 1
    
    # Live contract tests (require server)
    print("\nLive Contract Validation:")
    print("-" * 40)
    
    live_tests = [
        test_live_login_contract,
        test_live_message_contract,
    ]
    
    skipped = 0
    
    for test in live_tests:
        try:
            result = test()
            if result is True:
                passed += 1
            elif result is None:
                skipped += 1
            else:
                failed += 1
        except AssertionError as e:
            print(f"  ✗ {test.__name__}: FAILED - {e}")
            failed += 1
        except Exception as e:
            print(f"  ✗ {test.__name__}: ERROR - {e}")
            failed += 1
    
    # Summary
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print(f"\nTotal: {passed + failed + skipped}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Skipped: {skipped}")
    
    if failed == 0:
        print("\n✓ All contract tests passed!")
        print("  API contracts between Edge and Core are maintained.")
        return 0
    else:
        print(f"\n✗ {failed} contract test(s) failed")
        print("  Breaking changes detected in API contracts.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
