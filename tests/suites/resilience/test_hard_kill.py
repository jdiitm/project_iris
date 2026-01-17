#!/usr/bin/env python3
"""
P1-1: Hard-Kill Durability Test

This test validates that data survives process crash (kill -9).
Without this, we have no proof that the database actually works.

Test Flow:
1. Start cluster
2. Send messages to offline user (stored in Mnesia)
3. Kill core node with SIGKILL (kill -9)
4. Restart core node
5. Connect as offline user
6. Verify all messages are delivered

This is a CRITICAL production readiness test.
"""

import sys
import os
import time
import signal
import subprocess
import random
import string

# Add paths
sys.path.insert(0, str(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))

from tests.utilities import IrisClient

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..'))


def random_user():
    return ''.join(random.choices(string.ascii_lowercase, k=8))


def get_beam_pids():
    """Get all beam.smp process IDs."""
    try:
        result = subprocess.run(
            ["pgrep", "-f", "beam.smp"],
            capture_output=True, text=True
        )
        if result.returncode == 0:
            return [int(pid.strip()) for pid in result.stdout.strip().split('\n') if pid.strip()]
    except:
        pass
    return []


def kill_core_node():
    """Kill the core node with SIGKILL (kill -9)."""
    print("  Sending SIGKILL to core node...")
    
    # Get beam pids - core typically has 'iris_core' in its name
    pids = get_beam_pids()
    
    for pid in pids:
        try:
            # Check if this is a core node
            result = subprocess.run(
                ["ps", "-p", str(pid), "-o", "args="],
                capture_output=True, text=True
            )
            if "iris_core" in result.stdout:
                print(f"  Killing PID {pid} (core node)")
                os.kill(pid, signal.SIGKILL)
                return True
        except Exception as e:
            print(f"  Could not kill {pid}: {e}")
    
    # If no specific core found, kill all beam processes
    print("  No specific core node found, killing all beam processes")
    subprocess.run(["killall", "-9", "beam.smp"], capture_output=True)
    return True


def restart_core():
    """Restart the core node."""
    print("  Restarting core node...")
    
    # Use make to restart
    result = subprocess.run(
        ["make", "start_core"],
        cwd=PROJECT_ROOT,
        capture_output=True,
        timeout=60
    )
    
    # Wait for startup
    time.sleep(5)
    
    return result.returncode == 0


def restart_edge():
    """Restart the edge node."""
    print("  Restarting edge node...")
    
    result = subprocess.run(
        ["make", "start_edge1"],
        cwd=PROJECT_ROOT,
        capture_output=True,
        timeout=60
    )
    
    # Wait for startup
    time.sleep(3)
    
    return result.returncode == 0


def wait_for_port(port, timeout=30):
    """Wait for a port to be available."""
    import socket
    start = time.time()
    while time.time() - start < timeout:
        try:
            with socket.create_connection(('localhost', port), timeout=1):
                return True
        except:
            time.sleep(0.5)
    return False


def test_hard_kill_durability():
    """
    Main test: Verify messages survive kill -9.
    
    Expected: ALL messages sent before crash are delivered after restart.
    """
    print("=" * 60)
    print("P1-1: Hard-Kill Durability Test")
    print("=" * 60)
    
    sender_user = f"sender_{random_user()}"
    receiver_user = f"receiver_{random_user()}"  # Will be offline
    
    # Step 1: Send messages while receiver is OFFLINE
    print("\n[Step 1] Sending messages to offline user...")
    
    try:
        sender = IrisClient('localhost', 8085)
        sender.login(sender_user)
        
        messages = []
        for i in range(5):
            msg = f"hardkill_msg_{i}_{random_user()}"
            sender.send_msg(receiver_user, msg)
            messages.append(msg)
            print(f"  Sent: {msg}")
        
        sender.close()
        print(f"  ✓ Sent {len(messages)} messages to offline storage")
        
    except Exception as e:
        print(f"  ✗ Failed to send messages: {e}")
        return False
    
    # Give time for Mnesia to sync
    time.sleep(2)
    
    # Step 2: Kill core with SIGKILL
    print("\n[Step 2] Killing core node with SIGKILL (kill -9)...")
    
    if not kill_core_node():
        print("  ⚠ Could not kill core node")
        # Continue anyway - test can still be valuable
    
    time.sleep(2)
    
    # Step 3: Restart cluster
    print("\n[Step 3] Restarting cluster...")
    
    if not restart_core():
        print("  ⚠ Core restart command failed")
    
    if not restart_edge():
        print("  ⚠ Edge restart command failed")
    
    # Wait for edge port
    print("  Waiting for edge node...")
    if not wait_for_port(8085, timeout=30):
        print("  ✗ Edge node did not come up")
        return False
    
    print("  ✓ Cluster restarted")
    
    # Step 4: Connect as receiver and fetch messages
    print("\n[Step 4] Connecting as receiver to fetch messages...")
    
    time.sleep(3)  # Give Mnesia time to recover
    
    try:
        receiver = IrisClient('localhost', 8085)
        receiver.login(receiver_user)
        
        # Wait for offline delivery
        time.sleep(2)
        
        received = []
        for _ in range(10):  # Try to get more than expected
            try:
                msg = receiver.recv_msg(timeout=2.0)
                if msg:
                    decoded = msg.decode('utf-8') if isinstance(msg, bytes) else msg
                    received.append(decoded)
                    print(f"  Received: {decoded}")
            except:
                break
        
        receiver.close()
        
    except Exception as e:
        print(f"  ✗ Failed to receive messages: {e}")
        return False
    
    # Step 5: Validate
    print("\n[Step 5] Validating durability...")
    
    found = 0
    for orig in messages:
        if any(orig in r for r in received):
            found += 1
    
    print(f"  Recovered {found}/{len(messages)} messages")
    
    if found == len(messages):
        print("\n✓ HARD-KILL DURABILITY: PASSED")
        print("  All messages survived kill -9!")
        return True
    elif found > 0:
        print(f"\n⚠ PARTIAL DURABILITY: {found}/{len(messages)} recovered")
        print("  Some messages were lost!")
        return False
    else:
        print("\n✗ DURABILITY FAILURE: No messages recovered")
        return False


def main():
    """Run the hard-kill durability test."""
    
    print("=" * 60)
    print(" HARD-KILL DURABILITY TEST SUITE")
    print(" P1-1: Validating data survives process crash")
    print("=" * 60)
    
    # Check that cluster is running
    print("\n[Pre-check] Verifying cluster is up...")
    if not wait_for_port(8085, timeout=5):
        print("  Cluster not running. Please start cluster first.")
        print("  Run: make start")
        return 1
    
    print("  ✓ Cluster is running")
    
    # Run test
    result = test_hard_kill_durability()
    
    print("\n" + "=" * 60)
    if result:
        print(" RESULT: PASSED - Data survived kill -9")
    else:
        print(" RESULT: FAILED - Data loss detected")
    print("=" * 60)
    
    return 0 if result else 1


if __name__ == "__main__":
    sys.exit(main())
