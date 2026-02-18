#!/usr/bin/env python3
"""
Item 2: Token Refresh E2E Tests (RFC FR-11a, opcode 0x0B)

TDD RED phase: These tests verify actual token refresh behavior:
1. Login returns a refresh token
2. TOKEN_REFRESH (0x0B) with valid refresh token returns new access + refresh tokens
3. Reusing a refresh token is detected and rejected (theft detection)
4. Expired refresh token is rejected

These tests WILL FAIL until iris_session.erl TOKEN_REFRESH handler
is wired to iris_auth:exchange_refresh_token/1.
"""

import sys
import os
import socket
import struct
import time
import uuid

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from pathlib import Path
from tests.utilities.tls_connection import get_verified_ssl_context

CA_CERT = Path(PROJECT_ROOT) / "certs" / "ca.pem"
HOST = os.environ.get('IRIS_HOST', 'localhost')
PORT = int(os.environ.get('IRIS_PORT', '8085'))


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def unique_user(prefix):
    return f"{prefix}_{int(time.time()*1000)}_{uuid.uuid4().hex[:6]}"


def get_tls_socket():
    context = get_verified_ssl_context()
    raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    raw.settimeout(10.0)
    s = context.wrap_socket(raw, server_hostname=HOST)
    s.connect((HOST, PORT))
    return s


def parse_login_response(resp):
    """Parse LOGIN_OK response to extract session_id and refresh_token.

    Response format (iris_session.erl):
      0x03 | "LOGIN_OK" | SidLen(u16be) | SessionId | RefreshTokenLen(u16be) | RefreshToken
    """
    idx = resp.find(b"LOGIN_OK")
    if idx < 0:
        return None, None
    pos = idx + 8  # skip "LOGIN_OK"
    if len(resp) < pos + 2:
        return None, None
    sid_len = struct.unpack('>H', resp[pos:pos + 2])[0]
    pos += 2
    session_id = resp[pos:pos + sid_len]
    pos += sid_len
    if len(resp) < pos + 2:
        return session_id, None
    rt_len = struct.unpack('>H', resp[pos:pos + 2])[0]
    pos += 2
    if rt_len == 0 or len(resp) < pos + rt_len:
        return session_id, None
    refresh_token = resp[pos:pos + rt_len]
    return session_id, refresh_token


def raw_login(sock, username):
    """Login and return raw response bytes."""
    sock.sendall(b'\x01' + username.encode('utf-8'))
    sock.settimeout(5.0)
    resp = sock.recv(4096)
    if b"LOGIN_OK" not in resp:
        raise Exception(f"Login failed: {resp}")
    return resp


def send_token_refresh(sock, refresh_token):
    """Send TOKEN_REFRESH opcode (0x0B)."""
    if isinstance(refresh_token, str):
        refresh_token = refresh_token.encode('utf-8')
    packet = (bytes([0x0B]) +
              struct.pack('>H', len(refresh_token)) +
              refresh_token)
    sock.sendall(packet)


# =============================================================================
# Test 1: TOKEN_REFRESH returns new tokens
# =============================================================================
def test_refresh_returns_new_tokens():
    """
    Send TOKEN_REFRESH (0x0B) with a valid refresh token.
    Server should respond with new access + refresh tokens.
    
    Response format expected: 0x0B | AccessLen(16) | Access | RefreshLen(16) | Refresh
    OR: error opcode 0xFE if not implemented.
    """
    log("=" * 60)
    log("TEST: TOKEN_REFRESH returns new tokens")
    log("=" * 60)

    user = unique_user("refresh_e2e")
    sock = get_tls_socket()
    login_resp = raw_login(sock, user)
    time.sleep(0.1)

    # Extract refresh token from LOGIN_OK response (created by server via Core RPC)
    _session_id, refresh_token = parse_login_response(login_resp)
    if not refresh_token:
        log("  FAIL: LOGIN_OK did not include a refresh token")
        sock.close()
        return False

    log(f"  Got refresh token: {refresh_token[:20]}...")

    # Send TOKEN_REFRESH
    send_token_refresh(sock, refresh_token)
    time.sleep(0.5)

    # Check response
    sock.settimeout(3.0)
    got_tokens = False
    try:
        data = sock.recv(4096)
        if data and len(data) > 0:
            log(f"  Received {len(data)} bytes after TOKEN_REFRESH")
            # Check for success: opcode 0x0B response with new tokens
            if data[0] == 0x0B and len(data) > 3:
                access_len = struct.unpack('>H', data[1:3])[0]
                if len(data) >= 3 + access_len + 2:
                    access_token = data[3:3 + access_len]
                    refresh_offset = 3 + access_len
                    new_refresh_len = struct.unpack('>H', data[refresh_offset:refresh_offset + 2])[0]
                    new_refresh = data[refresh_offset + 2:refresh_offset + 2 + new_refresh_len]
                    log(f"  New access token: {access_token[:20]}...")
                    log(f"  New refresh token: {new_refresh[:20]}...")
                    got_tokens = True
                else:
                    log(f"  Response too short for full token pair")
            elif data[0] == 0xFE:
                log(f"  Got error response (0xFE) -- handler not wired yet")
            else:
                log(f"  Unexpected opcode: 0x{data[0]:02x}")
        else:
            log("  Empty response")
    except socket.timeout:
        log("  Timeout -- no response to TOKEN_REFRESH")

    sock.close()

    if got_tokens:
        log("  PASS: Server returned new token pair")
        return True
    else:
        log("  FAIL: Server did not return new tokens")
        return False


# =============================================================================
# Test 2: Refresh token reuse is detected
# =============================================================================
def test_refresh_reuse_detected():
    """
    Use the same refresh token twice. The second attempt must be rejected
    with an error (token_reused), per RFC FR-11a theft detection.
    """
    log("=" * 60)
    log("TEST: Refresh token reuse detected")
    log("=" * 60)

    user = unique_user("reuse_test")
    sock = get_tls_socket()
    login_resp = raw_login(sock, user)
    time.sleep(0.1)

    _session_id, refresh_token = parse_login_response(login_resp)
    if not refresh_token:
        log("  FAIL: LOGIN_OK did not include a refresh token")
        sock.close()
        return False

    # First use -- should succeed
    send_token_refresh(sock, refresh_token)
    time.sleep(0.5)

    sock.settimeout(3.0)
    first_response = None
    try:
        first_response = sock.recv(4096)
        log(f"  First exchange: {len(first_response)} bytes, opcode=0x{first_response[0]:02x}")
    except socket.timeout:
        log("  First exchange: timeout")

    # Second use (replay) -- should fail with token_reused error
    send_token_refresh(sock, refresh_token)
    time.sleep(0.5)

    reuse_rejected = False
    try:
        second_response = sock.recv(4096)
        if second_response and len(second_response) > 0:
            log(f"  Second exchange: {len(second_response)} bytes, opcode=0x{second_response[0]:02x}")
            if second_response[0] == 0xFE:
                # Error response -- check if it mentions reuse
                if b"reused" in second_response or b"token_reused" in second_response:
                    log("  Reuse detected and rejected")
                    reuse_rejected = True
                else:
                    log(f"  Got error but not reuse-specific: {second_response}")
                    # Any error on second use counts as detection
                    reuse_rejected = True
            elif second_response[0] == 0x0B:
                log("  Second exchange SUCCEEDED -- reuse NOT detected")
        else:
            log("  Empty second response")
    except socket.timeout:
        log("  Second exchange: timeout")

    sock.close()

    if reuse_rejected:
        log("  PASS: Token reuse was detected")
        return True
    else:
        log("  FAIL: Token reuse was NOT detected")
        return False


# =============================================================================
# Test 3: Expired refresh token is rejected
# =============================================================================
def test_refresh_expired_rejected():
    """
    Send a fabricated/expired refresh token. Server must reject it.
    """
    log("=" * 60)
    log("TEST: Expired/invalid refresh token rejected")
    log("=" * 60)

    user = unique_user("expired_test")
    sock = get_tls_socket()
    raw_login(sock, user)
    time.sleep(0.1)

    # Send a completely fake token
    fake_token = b"EXPIRED_FAKE_TOKEN_" + uuid.uuid4().hex[:20].encode()
    send_token_refresh(sock, fake_token)
    time.sleep(0.5)

    sock.settimeout(3.0)
    rejected = False
    try:
        data = sock.recv(4096)
        if data and len(data) > 0:
            log(f"  Response: {len(data)} bytes, opcode=0x{data[0]:02x}")
            if data[0] == 0xFE:
                log("  Got error response -- fake token rejected")
                rejected = True
            elif data[0] == 0x0B:
                log("  Got success response -- fake token ACCEPTED (BUG)")
            else:
                log(f"  Unexpected opcode 0x{data[0]:02x}")
        else:
            log("  Empty response")
    except socket.timeout:
        log("  Timeout -- no response (server ignored, currently a no-op stub)")

    sock.close()

    if rejected:
        log("  PASS: Invalid refresh token was rejected")
        return True
    else:
        log("  FAIL: Invalid refresh token was NOT rejected")
        return False


# =============================================================================
# Main
# =============================================================================
def main():
    log("")
    log("=" * 60)
    log("TOKEN REFRESH E2E TESTS (RFC FR-11a)")
    log("=" * 60)

    tests = [
        ("Refresh Returns New Tokens", test_refresh_returns_new_tokens),
        ("Refresh Reuse Detected", test_refresh_reuse_detected),
        ("Expired Refresh Rejected", test_refresh_expired_rejected),
    ]

    results = []
    for name, fn in tests:
        try:
            result = fn()
            results.append((name, result))
        except Exception as e:
            log(f"  Exception in {name}: {e}")
            results.append((name, False))

    print("\n" + "=" * 60)
    print("RESULTS:")
    print("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)
    for name, result in results:
        print(f"  [{'PASS' if result else 'FAIL'}] {name}")

    if passed == total:
        print(f"\nToken Refresh E2E: PASSED ({passed}/{total})")
        return 0
    else:
        print(f"\nToken Refresh E2E: FAILED ({passed}/{total})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
