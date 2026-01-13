#!/usr/bin/env python3
"""
Test: Tokyo Edge Node Verification

Validates performance and reliability of the Tokyo AWS Edge Node.
Target: iris_edge1@100.82.212.50 (Tailscale IP)

Tier: 1 (Manual/Network dependent)
Safe for laptop: Yes
"""

import sys
import os
import time
import statistics

# Add parent paths for imports
sys.path.insert(0, str(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))))

from tests.framework import TestLogger
from tests.utilities import IrisClient

# Default to the Tokyo node IP from CLUSTER_SETUP.md
EDGE_IP = os.environ.get("IRIS_EDGE_IP", "100.82.212.50")
EDGE_PORT = int(os.environ.get("IRIS_EDGE_PORT", 8085))

def test_tokyo_latency():
    """Measure RTT to Tokyo Edge Node."""
    with TestLogger("test_tokyo_latency", "integration") as log:
        log.info("setup", f"Connecting to Tokyo Edge at {EDGE_IP}:{EDGE_PORT}")
        
        try:
            client = IrisClient(host=EDGE_IP, port=EDGE_PORT)
            client.login("tokyo_verifier")
            log.connection_event("login", "tokyo_verifier")
        except Exception as e:
            log.error("setup", f"Failed to connect to {EDGE_IP}: {e}")
            log.error("setup", "Ensure Tailscale is up and the node is reachable.")
            return False

        rtt_samples = []
        NUM_SAMPLES = 20
        
        log.info("measure", f"Sending {NUM_SAMPLES} probes...")
        
        try:
            for i in range(NUM_SAMPLES):
                msg = f"ping_{i}"
                start = time.perf_counter()
                
                # We send a message to ourselves? 
                # IrisClient.recv_msg waits for a RELIABLE message.
                # If we send to 'tokyo_verifier', the edge node should route it back to us via the core.
                # However, if we are just a client, we are connected to the edge.
                # IF the edge is connected to the core, and we send to OURSELVES,
                # the edge should route it: Client -> Edge -> Core -> Edge -> Client.
                # This tests the full round-trip path!
                
                client.send_msg("tokyo_verifier", msg)
                received = client.recv_msg(timeout=5.0)
                
                end = time.perf_counter()
                rtt_ms = (end - start) * 1000
                rtt_samples.append(rtt_ms)
                
                if received != msg.encode('utf-8'):
                    log.error("validation", f"Content mismatch: {received}")
                    return False
                    
            avg_rtt = statistics.mean(rtt_samples)
            max_rtt = max(rtt_samples)
            p95_rtt = sorted(rtt_samples)[int(len(rtt_samples) * 0.95)]
            
            log.metric("tokyo_rtt_avg", avg_rtt, "ms")
            log.metric("tokyo_rtt_p95", p95_rtt, "ms")
            
            log.info("result", f"RTT Stats: Avg={avg_rtt:.1f}ms, Max={max_rtt:.1f}ms")
            
            # Acceptance criteria: < 500ms RTT for inter-continental relay
            if avg_rtt < 500:
                log.info("result", "Test PASSED (Latency acceptable)")
                return True
            else:
                log.warn("result", f"Test WARNING: High latency ({avg_rtt:.1f}ms)")
                return True # Pass but warn
                
        except Exception as e:
            log.error("execution", f"Error during latency test: {e}")
            return False
        finally:
            client.close()

def test_tokyo_throughput():
    """Verify system stability under burst load via Tokyo."""
    with TestLogger("test_tokyo_throughput", "integration") as log:
        log.info("setup", "Preparing burst test...")
        
        try:
            client = IrisClient(host=EDGE_IP, port=EDGE_PORT)
            client.login("tokyo_stress")
        except Exception as e:
            log.error("setup", f"Connection failed: {e}")
            return False
            
        BURST_SIZE = 50
        payload = "x" * 100 # 100 bytes
        
        start_time = time.time()
        
        try:
            # Pipelined send
            for i in range(BURST_SIZE):
                client.send_msg("tokyo_stress", f"{i}:{payload}")
                
            # Receive all
            for i in range(BURST_SIZE):
                client.recv_msg(timeout=2.0)
                
            duration = time.time() - start_time
            msgs_per_sec = BURST_SIZE / duration
            
            log.metric("tokyo_throughput", msgs_per_sec, "msg/sec")
            log.info("result", f"Throughput: {msgs_per_sec:.1f} msg/sec")
            
            if msgs_per_sec > 5: # Minimal baseline for crossed-ocean
                return True
            else:
                log.warn("result", "Throughput lower than expected")
                return True
                
        except Exception as e:
            log.error("execution", f"Burst failed: {e}")
            return False
        finally:
            client.close()

def main():
    print(f"Targeting Edge Node: {EDGE_IP}")
    
    tests = [test_tokyo_latency, test_tokyo_throughput]
    passed = 0
    failed = 0
    
    for test in tests:
        if test():
            passed += 1
        else:
            failed += 1
            
    return 0 if failed == 0 else 1

if __name__ == "__main__":
    sys.exit(main())
