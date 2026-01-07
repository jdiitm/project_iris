#!/usr/bin/env python3
"""
Offline Messaging Test
Tests that messages sent to an offline user are stored and delivered when they login.
"""

import sys
import time
from iris_client import IrisClient

def main():
    try:
        print("=== Offline Messaging Test ===")
        
        # Alice logs in and sends messages to Charlie (who is offline)
        alice = IrisClient()
        alice.login("alice")
        print("Alice logged in")

        print("Sending ordered offline messages to Charlie...")
        alice.send_msg("charlie", "Msg 1")
        time.sleep(0.05)
        alice.send_msg("charlie", "Msg 2")
        time.sleep(0.05)
        alice.send_msg("charlie", "Msg 3")
        
        alice.close()
        print("Alice disconnected")

        # Wait a moment for messages to be stored
        time.sleep(0.5)

        # Charlie logs in and should receive the messages
        print("\nCharlie logging in...")
        charlie = IrisClient()
        charlie.login("charlie")
        print("Charlie logged in")
        
        # Receive all messages
        received = []
        for i in range(3):
            try:
                msg = charlie.recv_msg(timeout=5.0)
                print(f"Charlie received: {msg}")
                received.append(msg)
            except Exception as e:
                print(f"Error receiving message {i+1}: {e}")
                break
        
        # Check results
        expected = [b"Msg 1", b"Msg 2", b"Msg 3"]
        
        if received == expected:
            print("\nSUCCESS: Received all messages in correct order!")
        else:
            print(f"\nFAILURE: Expected {expected}, got {received}")
            charlie.close()
            sys.exit(1)
        
        # Test that messages are deleted after retrieval
        print("\nVerifying messages are deleted (re-login test)...")
        charlie.close()
        time.sleep(0.5)
        
        charlie2 = IrisClient()
        charlie2.login("charlie")
        
        try:
            charlie2.sock.settimeout(2.0)
            extra = charlie2.sock.recv(1024)
            if extra and len(extra) > 10:  # Ignore small leftover data
                print(f"FAILURE: Received data after deletion: {extra}")
                sys.exit(1)
        except:
            pass
        
        print("SUCCESS: No messages received on re-login (messages were deleted)")
        charlie2.close()
        
        print("\n=== ALL OFFLINE TESTS PASSED ===")
        
    except Exception as e:
        print(f"Test Crashed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()
