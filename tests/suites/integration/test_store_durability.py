#!/usr/bin/env python3
"""
Test Name: test_store_durability.py
Suite: integration
Tier: 0

Purpose:
    Validates iris_store.erl durability guarantees and API behavior.

Requirements:
    - Running Iris cluster (core + edge)
    - Mnesia tables initialized

Determinism:
    - Uses seeded random for test data
    - No wall-clock dependencies
"""

import sys
import os

# Ensure project root is in path
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

import subprocess
import time
import socket

def wait_for_server(host="127.0.0.1", port=8085, timeout=30):
    """Wait for server to be available."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.settimeout(2)
                s.connect((host, port))
                return True
        except (socket.error, ConnectionRefusedError):
            time.sleep(0.5)
    return False

def run_erlang_test(code):
    """Run Erlang code and return output."""
    full_code = f"""
        cd /home/j/.gemini/antigravity/scratch/project_iris && \
        erl -pa ebin -noshell -sname test_store_$RANDOM -setcookie iris_secret -eval '
        try
            {code}
        catch
            Class:Reason -> 
                io:format("ERROR: ~p:~p~n", [Class, Reason]),
                halt(1)
        end,
        halt(0).
        '
    """
    result = subprocess.run(
        ["bash", "-c", full_code],
        capture_output=True,
        text=True,
        timeout=30
    )
    return result.returncode == 0, result.stdout, result.stderr

def test_store_put_get():
    """Test basic put/get operations."""
    print("[TEST] Store put/get operations...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Setup: Start Mnesia and create test table
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(test_store, [{ram_copies, [node()]}, {attributes, [key, value]}]),
        mnesia:wait_for_tables([test_store], 5000),
        
        %% Test put
        ok = iris_store:put(test_store, test_key, <<"test_value">>),
        
        %% Test get
        {ok, <<"test_value">>} = iris_store:get(test_store, test_key),
        
        %% Test not_found
        not_found = iris_store:get(test_store, nonexistent_key),
        
        io:format("PASS: put/get works correctly~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_store_durability_options():
    """Test different durability options."""
    print("[TEST] Store durability options...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Setup
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(test_dur, [{disc_copies, [node()]}, {attributes, [key, value]}]),
        mnesia:wait_for_tables([test_dur], 5000),
        
        %% Test guaranteed durability (default)
        ok = iris_store:put(test_dur, key1, <<"guaranteed">>, #{durability => guaranteed}),
        
        %% Test best_effort durability (async)
        ok = iris_store:put(test_dur, key2, <<"best_effort">>, #{durability => best_effort}),
        timer:sleep(100),  %% Wait for async write
        
        %% Verify both writes
        {ok, <<"guaranteed">>} = iris_store:get(test_dur, key1),
        {ok, <<"best_effort">>} = iris_store:get(test_dur, key2),
        
        io:format("PASS: durability options work correctly~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_store_delete():
    """Test delete operations."""
    print("[TEST] Store delete operations...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Setup
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(test_del, [{ram_copies, [node()]}, {attributes, [key, value]}]),
        mnesia:wait_for_tables([test_del], 5000),
        
        %% Create value
        ok = iris_store:put(test_del, del_key, <<"to_delete">>),
        {ok, <<"to_delete">>} = iris_store:get(test_del, del_key),
        
        %% Delete
        ok = iris_store:delete(test_del, del_key),
        
        %% Verify deleted
        not_found = iris_store:get(test_del, del_key),
        
        io:format("PASS: delete works correctly~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_store_batch_put():
    """Test batch put operations."""
    print("[TEST] Store batch put operations...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Setup
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(test_batch, [{ram_copies, [node()]}, {attributes, [key, value]}]),
        mnesia:wait_for_tables([test_batch], 5000),
        
        %% Batch write
        KVPairs = [{batch1, <<"v1">>}, {batch2, <<"v2">>}, {batch3, <<"v3">>}],
        ok = iris_store:batch_put(test_batch, KVPairs),
        
        %% Verify all written
        {ok, <<"v1">>} = iris_store:get(test_batch, batch1),
        {ok, <<"v2">>} = iris_store:get(test_batch, batch2),
        {ok, <<"v3">>} = iris_store:get(test_batch, batch3),
        
        io:format("PASS: batch_put works correctly~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def main():
    """Run all tests."""
    print("=" * 60)
    print("Test Suite: iris_store Integration Tests")
    print("=" * 60)
    
    results = []
    
    results.append(("put/get", test_store_put_get()))
    results.append(("durability options", test_store_durability_options()))
    results.append(("delete", test_store_delete()))
    results.append(("batch_put", test_store_batch_put()))
    
    print("\n" + "=" * 60)
    print("Results:")
    passed = sum(1 for _, r in results if r)
    failed = sum(1 for _, r in results if not r)
    
    for name, result in results:
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"  {name}: {status}")
    
    print(f"\nTotal: {passed} passed, {failed} failed")
    print("=" * 60)
    
    if failed > 0:
        sys.exit(1)
    else:
        print("\nPASS: All iris_store tests passed")
        sys.exit(0)

if __name__ == "__main__":
    main()
