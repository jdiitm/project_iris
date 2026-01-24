#!/usr/bin/env python3
"""
Test Name: test_region_routing.py
Suite: integration
Tier: 0

Purpose:
    Validates iris_region_router.erl configuration and routing logic.

Requirements:
    - No running cluster needed for unit tests
    - Tests region assignment and configuration

Determinism:
    - Uses deterministic hash-based routing
    - No wall-clock dependencies
"""

import sys
import os

# Ensure project root is in path
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

import subprocess

def run_erlang_test(code):
    """Run Erlang code and return output."""
    full_code = f"""
        cd {PROJECT_ROOT} && \
        erl -pa ebin -noshell -sname test_region_$RANDOM -setcookie iris_secret -eval '
        try
            {code}
        catch
            Class:Reason:Stack -> 
                io:format("ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
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

def test_default_region():
    """Test default region configuration."""
    print("[TEST] Default region configuration...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Clear config
        application:unset_env(iris_core, region_id),
        application:unset_env(iris_core, regions),
        
        %% Default should be <<"local">>
        <<"local">> = iris_region_router:get_current_region(),
        
        io:format("PASS: default region is local~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_region_configuration():
    """Test region configuration."""
    print("[TEST] Region configuration...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Set region
        application:set_env(iris_core, region_id, <<"us-east-1">>),
        <<"us-east-1">> = iris_region_router:get_current_region(),
        
        %% Set multiple regions
        application:set_env(iris_core, regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]),
        Regions = iris_region_router:get_all_regions(),
        3 = length(Regions),
        
        %% Cleanup
        application:unset_env(iris_core, region_id),
        application:unset_env(iris_core, regions),
        
        io:format("PASS: region configuration works~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_home_region_deterministic():
    """Test that home region assignment is deterministic."""
    print("[TEST] Home region determinism...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Configure regions
        application:set_env(iris_core, regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]),
        
        %% Same user should always get same region
        User = <<"alice123">>,
        Region1 = iris_region_router:get_home_region(User),
        Region2 = iris_region_router:get_home_region(User),
        Region3 = iris_region_router:get_home_region(User),
        
        %% All should be equal
        Region1 = Region2,
        Region2 = Region3,
        
        %% Cleanup
        application:unset_env(iris_core, regions),
        
        io:format("PASS: home region is deterministic~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_home_region_distribution():
    """Test that users are distributed across regions."""
    print("[TEST] Home region distribution...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Configure regions
        Regions = [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>],
        application:set_env(iris_core, regions, Regions),
        
        %% Generate 1000 users and count distribution
        Users = [list_to_binary("user_" ++ integer_to_list(I)) || I <- lists:seq(1, 1000)],
        HomeRegions = [iris_region_router:get_home_region(U) || U <- Users],
        
        %% Count per region
        Counts = lists:foldl(fun(R, Acc) ->
            maps:update_with(R, fun(V) -> V + 1 end, 1, Acc)
        end, #{}, HomeRegions),
        
        %% All regions should have some users (expect ~33% each with hash distribution)
        3 = maps:size(Counts),
        
        %% Each region should have at least 200 users (some variance expected)
        lists:foreach(fun(R) ->
            Count = maps:get(R, Counts, 0),
            true = Count > 100  %% At least 10%
        end, Regions),
        
        %% Cleanup
        application:unset_env(iris_core, regions),
        
        io:format("PASS: users are distributed across regions~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_is_local_region():
    """Test local region check."""
    print("[TEST] is_local_region check...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Set current region
        application:set_env(iris_core, region_id, <<"us-east-1">>),
        
        %% Should be true for current region
        true = iris_region_router:is_local_region(<<"us-east-1">>),
        
        %% Should be false for other regions
        false = iris_region_router:is_local_region(<<"eu-west-1">>),
        false = iris_region_router:is_local_region(<<"ap-south-1">>),
        
        %% Cleanup
        application:unset_env(iris_core, region_id),
        
        io:format("PASS: is_local_region works correctly~n")
    """)
    
    if success and "PASS" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stderr}")
        return False

def test_region_endpoints():
    """Test region endpoint management."""
    print("[TEST] Region endpoint management...")
    
    success, stdout, stderr = run_erlang_test("""
        %% Clear existing
        application:unset_env(iris_core, region_endpoints),
        
        %% Initially not found
        {error, _} = iris_region_router:get_region_endpoint(<<"us-east-1">>),
        
        %% Set endpoints
        ok = iris_region_router:set_region_endpoint(<<"us-east-1">>, [node1, node2]),
        
        %% Should now be found
        {ok, [node1, node2]} = iris_region_router:get_region_endpoint(<<"us-east-1">>),
        
        %% Update endpoints
        ok = iris_region_router:set_region_endpoint(<<"us-east-1">>, [node3]),
        {ok, [node3]} = iris_region_router:get_region_endpoint(<<"us-east-1">>),
        
        %% Cleanup
        application:unset_env(iris_core, region_endpoints),
        
        io:format("PASS: region endpoint management works~n")
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
    print("Test Suite: iris_region_router Integration Tests")
    print("=" * 60)
    
    results = []
    
    results.append(("default region", test_default_region()))
    results.append(("region configuration", test_region_configuration()))
    results.append(("home region determinism", test_home_region_deterministic()))
    results.append(("home region distribution", test_home_region_distribution()))
    results.append(("is_local_region", test_is_local_region()))
    results.append(("region endpoints", test_region_endpoints()))
    
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
        print("\nPASS: All iris_region_router tests passed")
        sys.exit(0)

if __name__ == "__main__":
    main()
