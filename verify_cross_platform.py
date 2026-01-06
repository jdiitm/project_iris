#!/usr/bin/env python3
import sys
import subprocess
import os

def check_ram_detection():
    print("[*] Verifying RAM Detection...")
    try:
        output = subprocess.check_output("./scripts/auto_tune.sh", shell=True).decode()
        print(output)
        if "Detected Available RAM" not in output:
            print("[FAILED] Output does not contain RAM detection.")
            return False
        
        # Check if sensible on mac (should be > 4096 if machine has >8GB RAM)
        # We can't strictly assert value but we can check format.
        return True
    except subprocess.CalledProcessError as e:
        print(f"[FAILED] auto_tune.sh exited with error: {e}")
        return False

def check_ulimit():
    print("\n[*] Checking ulimit...")
    try:
        # soft limit
        import resource
        soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
        print(f"Current File Descriptor Limit: Soft={soft}, Hard={hard}")
        
        if soft < 10000:
            print("[WARNING] ulimit -n is low. High scale tests might fail.")
            print("Try running: ulimit -n 65536")
        else:
            print("[OK] ulimit seems sufficient for medium scale.")
            
        return True
    except Exception as e:
        print(f"[FAILED] Could not check ulimit: {e}")
        return False

def main():
    print("--- Cross-Platform Verification ---")
    
    checks = [
        check_ram_detection,
        check_ulimit
    ]
    
    passed = True
    for check in checks:
        if not check():
            passed = False
            
    if passed:
        print("\n[SUCCESS] Environment looks ready for basic testing.")
        sys.exit(0)
    else:
        print("\n[FAILED] Environment issues detected.")
        sys.exit(1)

if __name__ == "__main__":
    main()
