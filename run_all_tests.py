#!/usr/bin/env python3
import subprocess
import time
import sys
import os

def print_header(msg):
    print("\n" + "="*60)
    print(f" {msg}")
    print("="*60)

def run_cmd(cmd, exit_on_fail=True):
    print(f"[*] Running: {cmd}")
    ret = os.system(cmd)
    if ret != 0:
        print(f"[!] Command failed: {cmd}")
        if exit_on_fail:
            sys.exit(1)
    return ret

def setup():
    print_header("System Setup")
    run_cmd("make stop >/dev/null 2>&1", exit_on_fail=False)
    run_cmd("killall beam.smp >/dev/null 2>&1", exit_on_fail=False)
    run_cmd("rm -rf Mnesia*", exit_on_fail=False)
    run_cmd("make clean >/dev/null")
    run_cmd("make all >/dev/null")
    run_cmd("make start_core >/dev/null; sleep 3")
    run_cmd("make start_edge1 >/dev/null; sleep 2")
    run_cmd("make start_edge2 >/dev/null; sleep 2")

def functional_tests():
    print_header("1. Functional Tests")
    run_cmd("./test_iris.py")
    run_cmd("./test_offline.py")

def performance_tests():
    print_header("2. Performance Benchmarks")
    run_cmd("./benchmark_iris.py")

def reliability_tests():
    print_header("3. Reliability & Chaos (Break My System)")
    
    # Split Brain
    print("--- 3.1 Split Brain (Network Partition) ---")
    # We use break_my_system.py but need to adapt it since it does its own setup
    # Actually break_my_system.py does its own setup/teardown, so we should call it directly
    # and not rely on our current setup.
    run_cmd("./break_my_system.py split")
    
    # OOM / Slow Consumer
    print("--- 3.2 Slow Consumer (OOM Mitigation) ---")
    run_cmd("./break_my_system.py oom")
    
    # Disk Crusher
    print("--- 3.3 Disk Crusher (Offline Flood) ---")
    run_cmd("./break_my_system.py disk")

def main():
    start_time = time.time()
    
    try:
        # 1. Functional & Performance (Require Standard Setup)
        setup()
        functional_tests()
        performance_tests()
        run_cmd("make stop >/dev/null")
        run_cmd("killall beam.smp >/dev/null 2>&1", exit_on_fail=False)
        
        # 2. Reliability (Has Self-Contained Setup)
        reliability_tests()
        
        print_header("ALL TESTS PASSED")
        print(f"Total Duration: {time.time() - start_time:.1f}s")
        
    except KeyboardInterrupt:
        print("\n[!] Interrupted")
        sys.exit(1)

if __name__ == "__main__":
    main()
