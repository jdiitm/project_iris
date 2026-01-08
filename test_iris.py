#!/usr/bin/env python3
"""
Basic Online Messaging Test
Tests that two users can connect, login, and exchange messages.
"""

import sys
from iris_client import IrisClient

def main():
    try:
        print("=== Basic Online Messaging Test ===")
        
        # Alice connects and logs in
        alice = IrisClient()
        alice.login("alice")
        print("Alice logged in")

        # Bob connects and logs in
        bob = IrisClient()
        bob.login("bob")
        print("Bob logged in")

        # Alice sends to Bob
        print("Alice sending message to Bob...")
        alice.send_msg("bob", "Hello Bob!")
        
        # Bob receives
        msg = bob.recv_msg()
        print(f"Bob received: {msg}")
        
        if msg == b"Hello Bob!":
            print("SUCCESS: Bob received the message correctly.")
        else:
            print(f"FAILURE: Expected b'Hello Bob!', got {msg}")
            sys.exit(1)

        alice.close()
        bob.close()
        
    except Exception as e:
        print(f"Test Crashed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()
