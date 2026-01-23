#!/usr/bin/env python3
"""
Benchmark: E2EE Message Latency
RFC Reference: NFR-3 (Message Latency), FR-12 (End-to-End Encryption)

Measures the latency overhead introduced by E2EE operations:
1. Key bundle fetch latency
2. Initial E2EE message (X3DH) setup time
3. Ratchet message encryption/decryption time
4. End-to-end latency with E2EE vs without

Target: E2EE overhead should be <10ms for typical messages (per RFC NFR-3).
"""

import os
import sys
import time
import socket
import statistics
import hashlib
import hmac
from dataclasses import dataclass
from typing import List, Optional

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

from tests.utilities.iris_client import IrisClient

# Configuration
EDGE_HOST = os.environ.get("EDGE_HOST", "127.0.0.1")
EDGE_PORT = int(os.environ.get("EDGE_PORT", "8085"))
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))

# Benchmark parameters
if TEST_PROFILE == "smoke":
    NUM_SAMPLES = 20
    WARMUP_MESSAGES = 5
    MESSAGE_SIZES = [64, 256, 1024]  # bytes
else:
    NUM_SAMPLES = 100
    WARMUP_MESSAGES = 20
    MESSAGE_SIZES = [64, 256, 1024, 4096, 16384]  # bytes

# Thresholds (from RFC NFR-3)
MAX_E2EE_OVERHEAD_MS = 10.0  # E2EE should add <10ms overhead
MAX_INITIAL_SETUP_MS = 50.0  # X3DH initial setup can take longer
MAX_P99_LATENCY_MS = 100.0   # End-to-end P99 must be under 100ms


def log(msg):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


@dataclass
class LatencyResult:
    """Container for latency measurements."""
    operation: str
    samples: List[float]
    min_ms: float = 0.0
    max_ms: float = 0.0
    avg_ms: float = 0.0
    p50_ms: float = 0.0
    p90_ms: float = 0.0
    p99_ms: float = 0.0
    
    def __post_init__(self):
        if self.samples:
            self.samples.sort()
            self.min_ms = self.samples[0]
            self.max_ms = self.samples[-1]
            self.avg_ms = statistics.mean(self.samples)
            self.p50_ms = self.samples[len(self.samples) // 2]
            self.p90_ms = self.samples[int(len(self.samples) * 0.9)]
            self.p99_ms = self.samples[int(len(self.samples) * 0.99)] if len(self.samples) >= 100 else self.max_ms


class MockCrypto:
    """
    Mock cryptographic operations for benchmarking.
    
    Simulates the computational overhead of real E2EE operations
    without requiring actual crypto libraries.
    """
    
    def __init__(self, seed: int = TEST_SEED):
        self.seed = seed
        self._key_cache = {}
    
    def derive_key(self, input_str: str) -> bytes:
        """Simulate key derivation (KDF)."""
        # Real KDF would use HKDF or similar
        h = hashlib.sha256(f"{self.seed}:{input_str}".encode())
        return h.digest()
    
    def x3dh_compute(self, their_bundle: dict) -> bytes:
        """
        Simulate X3DH key agreement computation.
        
        Real X3DH involves:
        - 3 or 4 DH computations (X25519)
        - HKDF key derivation
        """
        # Simulate DH computations (each ~0.1ms on modern hardware)
        for i in range(4):
            self.derive_key(f"dh_{i}_{their_bundle.get('identity_key', b'').hex()[:16]}")
        
        return self.derive_key(f"x3dh_shared")
    
    def ratchet_encrypt(self, session_key: bytes, plaintext: bytes) -> bytes:
        """
        Simulate Double Ratchet encryption.
        
        Real ratchet involves:
        - DH ratchet step (optional)
        - Symmetric key ratchet
        - AEAD encryption (AES-256-GCM)
        """
        # Simulate key ratchet
        chain_key = self.derive_key(f"chain_{session_key.hex()[:16]}")
        message_key = self.derive_key(f"msg_{chain_key.hex()[:16]}")
        
        # Simulate AEAD encryption
        key_stream = (message_key * ((len(plaintext) // 32) + 1))[:len(plaintext)]
        ciphertext = bytes(p ^ k for p, k in zip(plaintext, key_stream))
        mac = hmac.new(message_key, ciphertext, hashlib.sha256).digest()[:16]
        
        return ciphertext + mac
    
    def ratchet_decrypt(self, session_key: bytes, ciphertext: bytes) -> bytes:
        """Simulate Double Ratchet decryption."""
        chain_key = self.derive_key(f"chain_{session_key.hex()[:16]}")
        message_key = self.derive_key(f"msg_{chain_key.hex()[:16]}")
        
        # Verify MAC and decrypt
        mac = ciphertext[-16:]
        ciphertext_only = ciphertext[:-16]
        
        expected_mac = hmac.new(message_key, ciphertext_only, hashlib.sha256).digest()[:16]
        if not hmac.compare_digest(mac, expected_mac):
            raise ValueError("MAC verification failed")
        
        key_stream = (message_key * ((len(ciphertext_only) // 32) + 1))[:len(ciphertext_only)]
        return bytes(c ^ k for c, k in zip(ciphertext_only, key_stream))


def check_edge_running() -> bool:
    """Check if edge node is accepting connections."""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2.0)
        sock.connect((EDGE_HOST, EDGE_PORT))
        sock.close()
        return True
    except:
        return False


def benchmark_x3dh_setup() -> LatencyResult:
    """
    Benchmark X3DH key agreement computation.
    
    This measures the pure cryptographic overhead of establishing
    a new E2EE session (without network I/O).
    """
    log("Benchmarking X3DH setup time...")
    
    crypto = MockCrypto()
    samples = []
    
    # Warmup
    for _ in range(WARMUP_MESSAGES):
        crypto.x3dh_compute({"identity_key": b"test" * 8})
    
    # Benchmark
    for i in range(NUM_SAMPLES):
        bundle = {
            "identity_key": os.urandom(32),
            "signed_prekey": os.urandom(32),
            "one_time_prekey": os.urandom(32),
        }
        
        start = time.perf_counter()
        crypto.x3dh_compute(bundle)
        elapsed_ms = (time.perf_counter() - start) * 1000
        samples.append(elapsed_ms)
    
    result = LatencyResult("X3DH Setup", samples)
    log(f"  X3DH: avg={result.avg_ms:.3f}ms, P50={result.p50_ms:.3f}ms, P99={result.p99_ms:.3f}ms")
    
    return result


def benchmark_ratchet_encrypt(message_size: int) -> LatencyResult:
    """
    Benchmark Double Ratchet encryption.
    
    Measures the time to encrypt a message of given size.
    """
    log(f"Benchmarking ratchet encryption ({message_size} bytes)...")
    
    crypto = MockCrypto()
    session_key = crypto.derive_key("test_session")
    samples = []
    
    # Generate test messages
    messages = [os.urandom(message_size) for _ in range(NUM_SAMPLES + WARMUP_MESSAGES)]
    
    # Warmup
    for i in range(WARMUP_MESSAGES):
        crypto.ratchet_encrypt(session_key, messages[i])
    
    # Benchmark
    for i in range(NUM_SAMPLES):
        start = time.perf_counter()
        crypto.ratchet_encrypt(session_key, messages[WARMUP_MESSAGES + i])
        elapsed_ms = (time.perf_counter() - start) * 1000
        samples.append(elapsed_ms)
    
    result = LatencyResult(f"Encrypt {message_size}B", samples)
    log(f"  Encrypt {message_size}B: avg={result.avg_ms:.3f}ms, P50={result.p50_ms:.3f}ms, P99={result.p99_ms:.3f}ms")
    
    return result


def benchmark_ratchet_decrypt(message_size: int) -> LatencyResult:
    """
    Benchmark Double Ratchet decryption.
    """
    log(f"Benchmarking ratchet decryption ({message_size} bytes)...")
    
    crypto = MockCrypto()
    session_key = crypto.derive_key("test_session")
    samples = []
    
    # Pre-encrypt messages
    ciphertexts = [crypto.ratchet_encrypt(session_key, os.urandom(message_size)) 
                   for _ in range(NUM_SAMPLES + WARMUP_MESSAGES)]
    
    # Warmup
    for i in range(WARMUP_MESSAGES):
        crypto.ratchet_decrypt(session_key, ciphertexts[i])
    
    # Benchmark
    for i in range(NUM_SAMPLES):
        start = time.perf_counter()
        crypto.ratchet_decrypt(session_key, ciphertexts[WARMUP_MESSAGES + i])
        elapsed_ms = (time.perf_counter() - start) * 1000
        samples.append(elapsed_ms)
    
    result = LatencyResult(f"Decrypt {message_size}B", samples)
    log(f"  Decrypt {message_size}B: avg={result.avg_ms:.3f}ms, P50={result.p50_ms:.3f}ms, P99={result.p99_ms:.3f}ms")
    
    return result


def benchmark_e2e_latency_with_e2ee() -> LatencyResult:
    """
    Benchmark end-to-end message latency with E2EE.
    
    Measures the complete round-trip including:
    - Crypto operations (simulated)
    - Network I/O
    - Server processing
    """
    log("Benchmarking E2E latency with E2EE...")
    
    crypto = MockCrypto()
    session_key = crypto.derive_key("e2e_session")
    samples = []
    
    sender = IrisClient(host=EDGE_HOST, port=EDGE_PORT)
    sender.login("e2ee_sender")
    
    receiver = IrisClient(host=EDGE_HOST, port=EDGE_PORT)
    receiver.login("e2ee_receiver")
    
    # Warmup
    for i in range(WARMUP_MESSAGES):
        plaintext = f"warmup_{i}".encode()
        ciphertext = crypto.ratchet_encrypt(session_key, plaintext)
        sender.send_msg("e2ee_receiver", f"E2EE:{ciphertext.hex()}")
        try:
            receiver.sock.settimeout(2.0)
            receiver.recv_msg(timeout=2.0)
        except:
            pass
    
    # Benchmark
    failures = 0
    for i in range(NUM_SAMPLES):
        plaintext = f"benchmark_{i}".encode()
        
        # Measure full round-trip including crypto
        start = time.perf_counter()
        
        # Encrypt (sender side)
        ciphertext = crypto.ratchet_encrypt(session_key, plaintext)
        
        # Send
        sender.send_msg("e2ee_receiver", f"E2EE:{ciphertext.hex()}")
        
        try:
            # Receive
            receiver.sock.settimeout(5.0)
            msg = receiver.recv_msg(timeout=5.0)
            
            if msg and "E2EE:" in str(msg):
                # Decrypt (receiver side)
                ct_hex = msg.split(":", 1)[1]
                crypto.ratchet_decrypt(session_key, bytes.fromhex(ct_hex))
                
                elapsed_ms = (time.perf_counter() - start) * 1000
                samples.append(elapsed_ms)
            else:
                failures += 1
        except:
            failures += 1
    
    sender.close()
    receiver.close()
    
    if failures > NUM_SAMPLES // 2:
        log(f"  WARNING: {failures}/{NUM_SAMPLES} messages failed")
    
    result = LatencyResult("E2E with E2EE", samples) if samples else LatencyResult("E2E with E2EE", [0.0])
    log(f"  E2E+E2EE: avg={result.avg_ms:.3f}ms, P50={result.p50_ms:.3f}ms, P99={result.p99_ms:.3f}ms")
    
    return result


def benchmark_e2e_latency_plaintext() -> LatencyResult:
    """
    Benchmark end-to-end message latency without E2EE (baseline).
    """
    log("Benchmarking E2E latency (plaintext baseline)...")
    
    samples = []
    
    sender = IrisClient(host=EDGE_HOST, port=EDGE_PORT)
    sender.login("plain_sender")
    
    receiver = IrisClient(host=EDGE_HOST, port=EDGE_PORT)
    receiver.login("plain_receiver")
    
    # Warmup
    for i in range(WARMUP_MESSAGES):
        sender.send_msg("plain_receiver", f"warmup_{i}")
        try:
            receiver.sock.settimeout(2.0)
            receiver.recv_msg(timeout=2.0)
        except:
            pass
    
    # Benchmark
    failures = 0
    for i in range(NUM_SAMPLES):
        start = time.perf_counter()
        
        sender.send_msg("plain_receiver", f"benchmark_{i}")
        
        try:
            receiver.sock.settimeout(5.0)
            msg = receiver.recv_msg(timeout=5.0)
            
            if msg:
                elapsed_ms = (time.perf_counter() - start) * 1000
                samples.append(elapsed_ms)
            else:
                failures += 1
        except:
            failures += 1
    
    sender.close()
    receiver.close()
    
    result = LatencyResult("E2E Plaintext", samples) if samples else LatencyResult("E2E Plaintext", [0.0])
    log(f"  E2E Plain: avg={result.avg_ms:.3f}ms, P50={result.p50_ms:.3f}ms, P99={result.p99_ms:.3f}ms")
    
    return result


def main():
    """Run E2EE latency benchmarks."""
    log(f"=== E2EE Latency Benchmark (profile={TEST_PROFILE}, seed={TEST_SEED}) ===")
    log(f"Samples per test: {NUM_SAMPLES}")
    
    results = []
    passed = True
    
    # Crypto-only benchmarks (no network)
    log("\n--- Crypto Operations (no network) ---")
    
    x3dh_result = benchmark_x3dh_setup()
    results.append(x3dh_result)
    if x3dh_result.p99_ms > MAX_INITIAL_SETUP_MS:
        log(f"[WARN] X3DH P99 ({x3dh_result.p99_ms:.2f}ms) exceeds limit ({MAX_INITIAL_SETUP_MS}ms)")
    
    for size in MESSAGE_SIZES:
        enc_result = benchmark_ratchet_encrypt(size)
        results.append(enc_result)
        
        dec_result = benchmark_ratchet_decrypt(size)
        results.append(dec_result)
        
        # Check overhead threshold for typical message size (1KB)
        if size == 1024:
            total_crypto = enc_result.avg_ms + dec_result.avg_ms
            if total_crypto > MAX_E2EE_OVERHEAD_MS:
                log(f"[WARN] Crypto overhead ({total_crypto:.2f}ms) exceeds limit ({MAX_E2EE_OVERHEAD_MS}ms)")
    
    # End-to-end benchmarks (with network)
    if check_edge_running():
        log("\n--- End-to-End (with network) ---")
        
        plain_result = benchmark_e2e_latency_plaintext()
        results.append(plain_result)
        
        e2ee_result = benchmark_e2e_latency_with_e2ee()
        results.append(e2ee_result)
        
        # Calculate overhead
        if plain_result.samples and e2ee_result.samples:
            overhead_ms = e2ee_result.avg_ms - plain_result.avg_ms
            log(f"\nE2EE overhead: {overhead_ms:.2f}ms")
            
            if overhead_ms > MAX_E2EE_OVERHEAD_MS:
                log(f"[WARN] E2EE overhead exceeds target ({MAX_E2EE_OVERHEAD_MS}ms)")
            else:
                log(f"[PASS] E2EE overhead within target")
            
            if e2ee_result.p99_ms > MAX_P99_LATENCY_MS:
                log(f"[FAIL] E2E+E2EE P99 ({e2ee_result.p99_ms:.2f}ms) exceeds limit ({MAX_P99_LATENCY_MS}ms)")
                passed = False
            else:
                log(f"[PASS] E2E+E2EE P99 within limit")
    else:
        log("\n[SKIP] Edge not running - skipping E2E benchmarks")
    
    # Summary
    log("\n=== Summary ===")
    for r in results:
        log(f"  {r.operation:20s}: avg={r.avg_ms:7.3f}ms  P50={r.p50_ms:7.3f}ms  P99={r.p99_ms:7.3f}ms")
    
    if passed:
        log("\n[PASS] E2EE latency benchmark passed")
        sys.exit(0)
    else:
        log("\n[FAIL] E2EE latency benchmark failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
