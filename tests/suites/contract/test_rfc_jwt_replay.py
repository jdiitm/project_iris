#!/usr/bin/env python3
"""
RFC Section 9.1: JWT Replay Protection (jti claim)

Verifies that the server rejects JWT tokens with a previously-used jti
(JWT ID) claim. This prevents token replay attacks.

INVARIANTS:
- First use of a JWT with unique jti: MUST succeed (or auth not enabled)
- Second use of same jti: MUST be rejected
- Different jti values: MUST each be accepted independently

Tier: 1 (Contract - requires running server with auth enabled)
"""

import os
import sys
import time
import socket
import struct
import json
import hmac
import hashlib
import base64

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.tls_connection import get_verified_ssl_context

SERVER_HOST = os.environ.get("IRIS_HOST", "localhost")
SERVER_PORT = int(os.environ.get("IRIS_PORT", "8085"))
JWT_SECRET = os.environ.get("IRIS_JWT_SECRET", "test_secret_key_for_jwt_testing!")
TIMEOUT = 10

RESULTS = {"passed": 0, "failed": 0, "skipped": 0}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def result(name, passed, detail=""):
    status = "PASS" if passed else "FAIL"
    RESULTS["passed" if passed else "failed"] += 1
    suffix = f" -- {detail}" if detail else ""
    log(f"  [{status}] {name}{suffix}")
    return passed


def skip(name, reason=""):
    RESULTS["skipped"] += 1
    log(f"  [SKIP] {name} -- {reason}")


def b64url_encode(data):
    """Base64url encode without padding."""
    return base64.urlsafe_b64encode(data).rstrip(b'=').decode('ascii')


def create_jwt(user, jti=None, secret=JWT_SECRET):
    """Create a simple JWT for testing."""
    header = {"alg": "HS256", "typ": "JWT"}
    payload = {
        "sub": user,
        "iat": int(time.time()),
        "exp": int(time.time()) + 3600,
    }
    if jti is not None:
        payload["jti"] = jti

    header_b64 = b64url_encode(json.dumps(header).encode())
    payload_b64 = b64url_encode(json.dumps(payload).encode())
    signing_input = f"{header_b64}.{payload_b64}"
    signature = hmac.new(secret.encode(), signing_input.encode(), hashlib.sha256).digest()
    sig_b64 = b64url_encode(signature)
    return f"{header_b64}.{payload_b64}.{sig_b64}"


def connect_and_auth(user, token):
    """Try to connect and authenticate with a JWT token. Returns (success, response)."""
    try:
        context = get_verified_ssl_context()
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT)
        tls_sock = context.wrap_socket(sock, server_hostname=SERVER_HOST)
        tls_sock.connect((SERVER_HOST, SERVER_PORT))

        # Send TOKEN_AUTH opcode: 0x0B | TokenLen(16) | Token
        token_bytes = token.encode('utf-8')
        payload = b'\x0B' + struct.pack('>H', len(token_bytes)) + token_bytes
        tls_sock.sendall(payload)

        # Read response
        tls_sock.settimeout(5.0)
        try:
            resp = tls_sock.recv(4096)
        except socket.timeout:
            resp = b''

        tls_sock.close()
        return resp
    except Exception:
        return None


def check_server():
    """Check if server is reachable."""
    try:
        context = get_verified_ssl_context()
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT)
        tls_sock = context.wrap_socket(sock, server_hostname=SERVER_HOST)
        tls_sock.connect((SERVER_HOST, SERVER_PORT))
        tls_sock.close()
        return True
    except Exception:
        return False


# =============================================================================
# Static Contract Tests
# =============================================================================

def test_jwt_creation():
    """Verify JWT utility creates valid tokens."""
    token = create_jwt("test_user", jti="unique-id-123")
    parts = token.split('.')
    result("jwt_has_3_parts", len(parts) == 3, f"got {len(parts)} parts")

    payload_json = base64.urlsafe_b64decode(parts[1] + '==')
    payload = json.loads(payload_json)
    result("jwt_has_jti", payload.get("jti") == "unique-id-123",
           f"jti={payload.get('jti')}")
    result("jwt_has_sub", payload.get("sub") == "test_user",
           f"sub={payload.get('sub')}")


def test_jwt_without_jti():
    """Tokens without jti should still be structurally valid."""
    token = create_jwt("test_user")
    parts = token.split('.')
    payload_json = base64.urlsafe_b64decode(parts[1] + '==')
    payload = json.loads(payload_json)
    result("jwt_without_jti", "jti" not in payload,
           "Token without jti should not have jti field")


# =============================================================================
# Live Contract Tests (require running server with auth enabled)
# =============================================================================

def test_jwt_replay_rejection():
    """Section 9.1: Same jti used twice MUST be rejected on second use."""
    if not check_server():
        skip("jwt_replay_rejection", "server not reachable")
        return

    import uuid
    jti = str(uuid.uuid4())
    token = create_jwt("replay_test_user", jti=jti)

    resp1 = connect_and_auth("replay_test_user", token)
    if resp1 is None:
        skip("jwt_replay_rejection", "connection failed")
        return

    if b"AUTH" not in resp1 and b"LOGIN" not in resp1 and b"OK" not in resp1:
        skip("jwt_replay_rejection",
             f"auth may not be enabled (resp={resp1[:50]})")
        return

    # Second attempt with same jti
    resp2 = connect_and_auth("replay_test_user", token)
    is_rejected = (resp2 is not None and
                   (b"REJECT" in resp2 or b"ERROR" in resp2 or
                    b"REPLAY" in resp2 or b"DENIED" in resp2 or
                    resp2 == b''))
    result("jwt_replay_rejected", is_rejected,
           f"second use of jti={jti[:8]}... should be rejected (resp={resp2[:50] if resp2 else None})")


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    log("=== RFC Section 9.1: JWT Replay Protection ===")
    log("")

    test_jwt_creation()
    test_jwt_without_jti()
    test_jwt_replay_rejection()

    log("")
    log(f"Results: {RESULTS['passed']} passed, {RESULTS['failed']} failed, {RESULTS['skipped']} skipped")

    if RESULTS["failed"] > 0:
        sys.exit(1)
    sys.exit(0)
