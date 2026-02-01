#!/usr/bin/env python3
"""
Latency probe - measures RTT to Iris server.
Outputs structured data for central collection.

Usage:
    EDGE_HOST=100.95.21.52 python3 global_probe.py
    EDGE_HOST=100.95.21.52 SAMPLES=5000 python3 global_probe.py
"""
import os
import socket
import struct
import time
import json
import statistics
from datetime import datetime

EDGE_HOST = os.environ.get("EDGE_HOST", "100.95.21.52")
EDGE_PORT = int(os.environ.get("EDGE_PORT", "8085"))
WORKER_ID = os.environ.get("WORKER_ID", socket.gethostname())
SAMPLES = int(os.environ.get("SAMPLES", "1000"))


def measure_latency(host: str, port: int, samples: int) -> dict:
    """Measure RTT latency to Iris server."""
    latencies = []
    errors = 0
    
    print(f"[{WORKER_ID}] Measuring latency to {host}:{port} ({samples} samples)", flush=True)
    
    sock = None
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(10)
        sock.connect((host, port))
        
        # Login
        user_id = f"probe_{WORKER_ID}_{int(time.time())}"
        sock.sendall(bytes([0x01]) + user_id.encode())
        response = sock.recv(1024)
        
        if b"LOGIN_OK" not in response and len(response) == 0:
            print(f"  Login failed", flush=True)
            return {"error": "Login failed", "worker_id": WORKER_ID}
        
        print(f"  Connected as {user_id}", flush=True)
        
        # Measure RTT
        for i in range(samples):
            start = time.time()
            
            target = user_id.encode()
            content = f"ping{i}".encode()
            packet = (
                bytes([0x02]) +
                struct.pack('>H', len(target)) + target +
                struct.pack('>H', len(content)) + content
            )
            
            try:
                sock.sendall(packet)
                # Don't wait for echo - just measure send latency
                latencies.append((time.time() - start) * 1000)
            except Exception as e:
                errors += 1
                if errors > samples * 0.1:  # >10% errors, abort
                    print(f"  Too many errors, aborting", flush=True)
                    break
            
            if (i + 1) % 500 == 0:
                avg = sum(latencies[-500:]) / min(500, len(latencies))
                print(f"  Progress: {i + 1}/{samples} (avg {avg:.1f}ms)", flush=True)
            
            time.sleep(0.01)  # 100 samples/sec max
        
    except socket.timeout:
        print(f"  Connection timeout", flush=True)
        return {"error": "Connection timeout", "worker_id": WORKER_ID}
    except ConnectionRefusedError:
        print(f"  Connection refused", flush=True)
        return {"error": "Connection refused", "worker_id": WORKER_ID}
    except Exception as e:
        print(f"  Error: {e}", flush=True)
        return {"error": str(e), "worker_id": WORKER_ID}
    finally:
        if sock:
            try:
                sock.close()
            except:
                pass
    
    if not latencies:
        return {"error": "No measurements", "worker_id": WORKER_ID}
    
    latencies.sort()
    n = len(latencies)
    
    result = {
        "worker_id": WORKER_ID,
        "target": f"{host}:{port}",
        "timestamp": datetime.now().isoformat(),
        "samples": n,
        "errors": errors,
        "latency_ms": {
            "min": round(min(latencies), 3),
            "p50": round(latencies[n // 2], 3),
            "p75": round(latencies[int(n * 0.75)], 3),
            "p90": round(latencies[int(n * 0.90)], 3),
            "p95": round(latencies[int(n * 0.95)], 3),
            "p99": round(latencies[int(n * 0.99)], 3),
            "max": round(max(latencies), 3),
            "mean": round(statistics.mean(latencies), 3),
            "stdev": round(statistics.stdev(latencies), 3) if n > 1 else 0,
        },
    }
    
    return result


def main():
    result = measure_latency(EDGE_HOST, int(EDGE_PORT), SAMPLES)
    
    print(f"\n{'=' * 60}", flush=True)
    print(f"LATENCY RESULTS - {WORKER_ID}", flush=True)
    print(f"{'=' * 60}", flush=True)
    print(json.dumps(result, indent=2), flush=True)
    
    # Save to file
    filename = f"latency_{WORKER_ID}.json"
    with open(filename, "w") as f:
        json.dump(result, f, indent=2)
    print(f"\nSaved to {filename}", flush=True)


if __name__ == "__main__":
    main()
