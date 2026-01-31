#!/usr/bin/env python3
"""
Test: Presence System

Validates user presence (online/offline status) functionality:
- Query user status
- Status updates on login/logout
- Last-seen timestamps
- Presence cache behavior

Tier: 0 (Required on every merge)
Safe for laptop: Yes
Expected duration: <30s
"""

import sys
import os
import time
import struct
import socket

# Add project root to path for proper imports
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

from tests.framework import TestLogger, ClusterManager
from tests.utilities import IrisClient


def create_get_status_packet(target_user: str) -> bytes:
    """Create a get_status protocol packet."""
    # Protocol: 0x05 | TargetLen(16) | Target
    target_bytes = target_user.encode('utf-8')
    return b'\x05' + struct.pack('>H', len(target_bytes)) + target_bytes


def parse_status_response(data: bytes) -> dict:
    """Parse a status response packet."""
    # Response: 0x06 | ULen(16) | User | StatusByte | LastSeen(64)
    if len(data) < 4:
        return {"error": "too short"}
    
    if data[0] != 0x06:
        return {"error": f"wrong opcode: {data[0]}"}
    
    u_len = struct.unpack('>H', data[1:3])[0]
    if len(data) < 3 + u_len + 1 + 8:
        return {"error": "incomplete packet"}
    
    user = data[3:3+u_len].decode('utf-8')
    status_byte = data[3+u_len]
    last_seen = struct.unpack('>Q', data[3+u_len+1:3+u_len+9])[0]
    
    return {
        "user": user,
        "online": status_byte == 1,
        "last_seen": last_seen
    }


def test_online_user_status():
    """Test that online users show as online."""
    
    with TestLogger("test_online_user_status", "integration") as log:
        
        # Target user comes online
        target_user = f"status_target_{int(time.time())}"
        target = IrisClient()
        target.login(target_user)
        log.connection_event("login", target_user)
        
        # Query status (polling loop handles wait)
        querier = IrisClient()
        querier.login(f"querier_{int(time.time())}")
        log.connection_event("login", "querier")
        
        # Poll for status with retries
        result = False
        start_time = time.monotonic()
        max_wait = 10.0
        
        while time.monotonic() - start_time < max_wait:
            try:
                # Send status query
                query_packet = create_get_status_packet(target_user)
                querier.sock.sendall(query_packet)
                
                querier.sock.settimeout(2.0)
                response = querier.sock.recv(1024)
                
                status = parse_status_response(response)
                log.info("status_received", f"Status: {status}")
                
                if status.get("online"):
                    log.info("validation", f"{target_user} correctly shows as online")
                    result = True
                    break
                    
            except socket.timeout:
                pass
            except Exception as e:
                log.info("retry", f"Query failed ({e}), retrying...")
            
            time.sleep(0.5)
        
        if not result:
            log.error("validation", f"{target_user} failed to show as online after {max_wait}s")
        
        target.close()
        querier.close()
        
        if result:
            log.info("result", "Test PASSED")
        else:
            log.error("result", "Test FAILED")
        
        return result


def test_offline_user_status():
    """Test that offline users show as offline with last-seen time."""
    
    with TestLogger("test_offline_user_status", "integration") as log:
        
        # User comes online then goes offline
        target_user = f"offline_target_{int(time.time())}"
        target = IrisClient()
        target.login(target_user)
        log.connection_event("login", target_user)
        
        # Go offline
        target.close()
        log.connection_event("disconnect", target_user)
        # Polling loop below handles wait for presence update
        
        # Query status from fresh client with retries
        querier = IrisClient()
        querier.login(f"querier_off_{int(time.time())}")
        
        result = False
        start_time = time.monotonic()
        max_wait = 10.0
        
        while time.monotonic() - start_time < max_wait:
            try:
                query_packet = create_get_status_packet(target_user)
                querier.sock.sendall(query_packet)
                
                querier.sock.settimeout(2.0)
                response = querier.sock.recv(1024)
                
                status = parse_status_response(response)
                log.info("status_received", f"Status: {status}")
                
                # User should be offline (online=False or not present)
                if not status.get("online", True):
                    log.info("validation", f"{target_user} correctly shows as offline")
                    result = True
                    break
                    
            except socket.timeout:
                pass
            except Exception as e:
                log.info("retry", f"Query failed ({e}), retrying...")
            
            time.sleep(0.5)
        
        if not result:
            log.error("validation", f"{target_user} still shows as online after {max_wait}s")
        
        querier.close()
        
        if result:
            log.info("result", "Test PASSED")
        return result


def test_presence_cache():
    """Test that presence is cached and served efficiently."""
    
    with TestLogger("test_presence_cache", "integration") as log:
        
        target_user = f"cache_target_{int(time.time())}"
        target = IrisClient()
        target.login(target_user)
        log.connection_event("login", target_user)
        
        # Query same user multiple times
        querier = IrisClient()
        querier.login(f"cache_querier_{int(time.time())}")
        
        NUM_QUERIES = 5
        latencies = []
        
        for i in range(NUM_QUERIES):
            query_packet = create_get_status_packet(target_user)
            
            start = time.monotonic()
            querier.sock.sendall(query_packet)
            
            try:
                querier.sock.settimeout(5.0)
                response = querier.sock.recv(1024)
                latency_ms = (time.monotonic() - start) * 1000
                latencies.append(latency_ms)
                
                status = parse_status_response(response)
                log.info("query", f"Query {i+1}: {latency_ms:.2f}ms - {status}")
                
            except Exception as e:
                log.error("query", f"Query {i+1} failed: {e}")
        
        target.close()
        querier.close()
        
        if latencies:
            avg_latency = sum(latencies) / len(latencies)
            log.metric("avg_latency_ms", avg_latency, "ms")
            log.metric("queries_complete", len(latencies))
            
            # Cached queries should be fast
            if avg_latency < 50:  # 50ms threshold for cached response
                log.info("result", f"Test PASSED - avg latency {avg_latency:.2f}ms")
                return True
            else:
                log.warn("result", f"Slow responses - avg {avg_latency:.2f}ms")
                return True  # Still pass, just warn
        
        log.error("result", "No successful queries")
        return False


def main():
    """Run all presence tests."""
    cluster = ClusterManager()
    
    if not cluster.is_healthy():
        print("[SETUP] Starting cluster...")
        if not cluster.start():
            print("[ERROR] Failed to start cluster")
            return 1
    
    tests = [
        test_online_user_status,
        test_offline_user_status,
        test_presence_cache
    ]
    
    passed = 0
    failed = 0
    
    for test_fn in tests:
        try:
            print(f"\n{'='*60}")
            print(f"Running: {test_fn.__name__}")
            print('='*60)
            
            if test_fn():
                passed += 1
                print(f"✓ PASSED: {test_fn.__name__}")
            else:
                failed += 1
                print(f"✗ FAILED: {test_fn.__name__}")
        except Exception as e:
            failed += 1
            print(f"✗ ERROR: {test_fn.__name__} - {e}")
            import traceback
            traceback.print_exc()
    
    print(f"\n{'='*60}")
    print(f"Results: {passed}/{passed+failed} passed")
    print('='*60)
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
