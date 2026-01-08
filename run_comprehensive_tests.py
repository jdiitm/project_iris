#!/usr/bin/env python3
"""
Comprehensive Test Suite for Project Iris
Runs all tests in order of increasing intensity.
"""

import subprocess
import sys
import time
import os
from datetime import datetime

# Test definitions: (name, command, timeout_seconds, critical)
TESTS = [
    # Phase 1: Unit Tests
    ("Erlang Unit Tests (Proto)", 
     "erl -pa ebin -noinput -eval \"R = iris_proto_tests:test(), io:format('~p~n', [R]), halt(0).\"",
     30, True),
    
    ("Erlang Unit Tests (Session)", 
     "erl -pa ebin -noinput -eval \"R = iris_session_tests:test(), io:format('~p~n', [R]), halt(0).\"",
     30, True),
    
    # Phase 2: Integration Tests
    ("Online Messaging", "python3 test_iris.py", 30, True),
    ("Offline Storage", "python3 test_offline.py", 60, True),
    ("Presence System", "python3 test_presence.py", 60, False),
    
    # Phase 3: Stress Tests
    ("Benchmark (1000 msgs)", "python3 benchmark_iris.py", 60, False),
    ("Stress - Messi Hotspot", "python3 stress_messi.py", 120, False),
    ("Stress - Offline Delete", "python3 stress_offline_delete.py", 120, False),
    
    # Phase 4: Chaos Tests
    ("Chaos - Break My System", "python3 break_my_system.py", 180, False),
    ("Chaos - Kitchen Sink", "python3 kitchen_sink_chaos.py", 300, False),
    ("Chaos - Ultimate", "python3 ultimate_chaos.py", 300, False),
    
    # Phase 5: Extreme Tests
    ("Extreme - Dial Limits", "python3 extreme_dials.py", 300, False),
    ("Extreme - Offline", "python3 extreme_offline_test.py", 300, False),
]

def run_test(name, command, timeout, critical):
    """Run a single test and return (success, duration, output)"""
    print(f"\n{'='*60}")
    print(f"TEST: {name}")
    print(f"CMD:  {command}")
    print(f"{'='*60}")
    
    start = time.time()
    try:
        result = subprocess.run(
            command,
            shell=True,
            capture_output=True,
            text=True,
            timeout=timeout,
            env={**os.environ, 'ERL': '/Users/jd/.kerl/26.2/bin/erl'}
        )
        duration = time.time() - start
        
        output = result.stdout + result.stderr
        success = result.returncode == 0
        
        if success:
            print(f"✓ PASSED ({duration:.1f}s)")
        else:
            print(f"✗ FAILED (exit code: {result.returncode})")
            print(f"Output:\n{output[:500]}")
            
        return success, duration, output
        
    except subprocess.TimeoutExpired:
        duration = time.time() - start
        print(f"✗ TIMEOUT after {timeout}s")
        return False, duration, "TIMEOUT"
        
    except Exception as e:
        duration = time.time() - start
        print(f"✗ ERROR: {e}")
        return False, duration, str(e)

def ensure_cluster_running():
    """Make sure the cluster is running"""
    print("\n[*] Ensuring cluster is running...")
    result = subprocess.run(
        "./scripts/start_cluster.sh",
        shell=True,
        capture_output=True,
        text=True,
        timeout=60
    )
    if result.returncode != 0:
        print("WARNING: Cluster startup may have issues")
        print(result.stderr)
    time.sleep(3)

def main():
    print("="*60)
    print("PROJECT IRIS - COMPREHENSIVE TEST SUITE")
    print(f"Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("="*60)
    
    # Ensure cluster is running
    ensure_cluster_running()
    
    results = []
    passed = 0
    failed = 0
    skipped = 0
    
    for name, command, timeout, critical in TESTS:
        success, duration, output = run_test(name, command, timeout, critical)
        
        results.append({
            'name': name,
            'success': success,
            'duration': duration,
            'critical': critical
        })
        
        if success:
            passed += 1
        else:
            failed += 1
            if critical:
                print(f"\n!!! CRITICAL TEST FAILED: {name}")
                print("!!! Stopping test suite")
                break
    
    # Print summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    
    total = passed + failed
    print(f"\nResults: {passed}/{total} passed ({100*passed/total:.0f}%)")
    print()
    
    for r in results:
        status = "✓" if r['success'] else "✗"
        marker = "[CRITICAL]" if r['critical'] else ""
        print(f"  {status} {r['name']} ({r['duration']:.1f}s) {marker}")
    
    print()
    print(f"Finished: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()
