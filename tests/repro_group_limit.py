#!/usr/bin/env python3
"""
Reproduction Script: Group Size Limit Inconsistency (GAP-2)

This script verifies that broadcast groups are effectively limited to 1,000 members
by iris_group.erl, despite iris_limits.erl defining the limit as 10,000.
"""

import sys
import os
import subprocess

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, PROJECT_ROOT)

def erl_eval(code):
    cmd = [
        "erl", "-noshell", "-pa", "ebin",
        "-eval", code,
        "-eval", "init:stop()."
    ]
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300,  # 5 minutes for adding 1000+ members
            cwd=PROJECT_ROOT
        )
        return result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return "[TIMEOUT]"

def run_test():
    print("=== Testing Broadcast Group Limit ===")

    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create broadcast group (no sender keys added)
    {ok, GroupId} = iris_group:create_group(<<"Broadcast Test">>, <<"admin">>),
    
    %% Add 999 members (Total 1000 with admin)
    io:format("Adding 999 members...~n"),
    Results1 = [iris_group:add_member(GroupId, 
                iolist_to_binary(io_lib:format("user_~p", [I])), 
                <<"admin">>) || I <- lists:seq(1, 999)],
    
    Success1 = length([ok || ok <- Results1]),
    io:format("Added ~p members. Total should be 1000.~n", [Success1 + 1]),
    
    %% Try to add 1001st member (user 1000)
    Result1001 = iris_group:add_member(GroupId, <<"user_1000">>, <<"admin">>),
    
    case Result1001 of
        ok -> 
            io:format("FAIL: 1001st member accepted (Limit > 1000)~n");
        {error, Reason} ->
            io:format("PASS: 1001st member rejected with: ~p~n", [Reason]);
        Other ->
            io:format("ERROR: Unexpected result: ~p~n", [Other])
    end.
    """

    output = erl_eval(code)
    print(output)

    if "PASS: 1001st member rejected" in output:
        print("\n[SUCCESS] Reproduced: Limit is 1000 (iris_group incorrect)")
        return True
    elif "FAIL: 1001st member accepted" in output:
        print("\n[FAIL] Failed to reproduce: Limit seems to be > 1000")
        return False
    else:
        print("\n[ERROR] Unknown output")
        return False

if __name__ == "__main__":
    success = run_test()
    sys.exit(0 if success else 1)
