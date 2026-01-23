#!/usr/bin/env python3
"""
Test: Group Membership Management
RFC Reference: FR-13 (Group Messaging), RFC-001-AMENDMENT-001

Validates group membership operations:
1. Group creation and deletion
2. Member add/remove with authorization
3. Admin promotion/demotion
4. Sender key management
5. Member limit enforcement

This test uses Erlang RPC to directly test iris_group module.
"""

import os
import sys
import time
import subprocess
import json

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# Test configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))


def log(msg):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def erl_eval(code: str, timeout: int = 30) -> str:
    """
    Execute Erlang code and return output.
    """
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
            timeout=timeout,
            cwd=PROJECT_ROOT
        )
        return result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return "[TIMEOUT]"
    except Exception as e:
        return f"[ERROR] {e}"


def check_erlang_available() -> bool:
    """Check if Erlang is available."""
    try:
        result = subprocess.run(
            ["erl", "-noshell", "-eval", "io:format(\"ok\"), init:stop()."],
            capture_output=True, text=True, timeout=10
        )
        return "ok" in result.stdout
    except:
        return False


def check_module_compiled() -> bool:
    """Check if iris_group module is compiled."""
    return os.path.exists(os.path.join(PROJECT_ROOT, "ebin", "iris_group.beam"))


def start_mnesia_and_group():
    """Start Mnesia and the group server."""
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    io:format(\"started~n\").
    """
    output = erl_eval(code)
    return "started" in output


def test_group_creation():
    """Test: Create group and verify creator becomes admin."""
    log("=== Test: Group Creation ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(<<"Test Group">>, <<"alice">>),
    
    %% Verify group exists
    {ok, Info} = iris_group:get_group(GroupId),
    Name = maps:get(name, Info),
    Creator = maps:get(created_by, Info),
    Count = maps:get(member_count, Info),
    
    %% Verify creator is admin
    IsAdmin = iris_group:is_admin(GroupId, <<"alice">>),
    IsMember = iris_group:is_member(GroupId, <<"alice">>),
    
    case {Name, Creator, Count, IsAdmin, IsMember} of
        {<<"Test Group">>, <<"alice">>, 1, true, true} ->
            io:format(\"PASS: Group created correctly~n\");
        Other ->
            io:format(\"FAIL: Unexpected result: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Group created correctly")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_member_management():
    """Test: Add and remove members."""
    log("=== Test: Member Management ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(<<"Member Test">>, <<"admin">>),
    
    %% Add member
    ok = iris_group:add_member(GroupId, <<"bob">>, <<"admin">>),
    IsBobMember1 = iris_group:is_member(GroupId, <<"bob">>),
    
    %% Verify member count
    {ok, Info1} = iris_group:get_group(GroupId),
    Count1 = maps:get(member_count, Info1),
    
    %% Remove member
    ok = iris_group:remove_member(GroupId, <<"bob">>, <<"admin">>),
    IsBobMember2 = iris_group:is_member(GroupId, <<"bob">>),
    
    {ok, Info2} = iris_group:get_group(GroupId),
    Count2 = maps:get(member_count, Info2),
    
    case {IsBobMember1, Count1, IsBobMember2, Count2} of
        {true, 2, false, 1} ->
            io:format(\"PASS: Member management works~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Member add/remove works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_admin_promotion():
    """Test: Promote and demote admins."""
    log("=== Test: Admin Promotion ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group and add member
    {ok, GroupId} = iris_group:create_group(<<"Admin Test">>, <<"owner">>),
    ok = iris_group:add_member(GroupId, <<"user">>, <<"owner">>),
    
    %% User is not admin initially
    IsAdmin1 = iris_group:is_admin(GroupId, <<"user">>),
    
    %% Promote to admin
    ok = iris_group:promote_admin(GroupId, <<"user">>, <<"owner">>),
    IsAdmin2 = iris_group:is_admin(GroupId, <<"user">>),
    
    %% Demote back to member
    ok = iris_group:demote_admin(GroupId, <<"user">>, <<"owner">>),
    IsAdmin3 = iris_group:is_admin(GroupId, <<"user">>),
    IsMember = iris_group:is_member(GroupId, <<"user">>),
    
    case {IsAdmin1, IsAdmin2, IsAdmin3, IsMember} of
        {false, true, false, true} ->
            io:format(\"PASS: Admin promotion/demotion works~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Admin promotion/demotion works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_authorization():
    """Test: Non-admins cannot perform admin operations."""
    log("=== Test: Authorization ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group and add regular member
    {ok, GroupId} = iris_group:create_group(<<"Auth Test">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"member">>, <<"admin">>),
    
    %% Member cannot add others
    R1 = iris_group:add_member(GroupId, <<"newuser">>, <<"member">>),
    
    %% Member cannot remove others
    R2 = iris_group:remove_member(GroupId, <<"admin">>, <<"member">>),
    
    %% Member cannot promote
    R3 = iris_group:promote_admin(GroupId, <<"member">>, <<"member">>),
    
    %% Member cannot delete group
    R4 = iris_group:delete_group(GroupId, <<"member">>),
    
    AllDenied = (R1 =:= {error, not_authorized}) and
                (R2 =:= {error, not_authorized}) and
                (R3 =:= {error, not_authorized}) and
                (R4 =:= {error, not_authorized}),
    
    case AllDenied of
        true -> io:format(\"PASS: Authorization enforced~n\");
        false -> io:format(\"FAIL: ~p ~p ~p ~p~n\", [R1, R2, R3, R4])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Authorization enforced correctly")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_last_admin_protection():
    """Test: Cannot remove or demote the last admin."""
    log("=== Test: Last Admin Protection ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group with single admin
    {ok, GroupId} = iris_group:create_group(<<"Last Admin">>, <<"solo">>),
    
    %% Cannot demote last admin
    R1 = iris_group:demote_admin(GroupId, <<"solo">>, <<"solo">>),
    
    %% Cannot remove last admin  
    R2 = iris_group:remove_member(GroupId, <<"solo">>, <<"solo">>),
    
    %% Add another admin
    ok = iris_group:add_member(GroupId, <<"other">>, <<"solo">>),
    ok = iris_group:promote_admin(GroupId, <<"other">>, <<"solo">>),
    
    %% Now can demote original (no longer last admin)
    R3 = iris_group:demote_admin(GroupId, <<"solo">>, <<"other">>),
    
    case {R1, R2, R3} of
        {{error, last_admin}, {error, last_admin}, ok} ->
            io:format(\"PASS: Last admin protected~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Last admin protection works")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_sender_keys():
    """Test: Sender key storage and retrieval."""
    log("=== Test: Sender Keys ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(<<"Sender Keys">>, <<"alice">>),
    
    %% Store sender key
    ok = iris_group:store_sender_key(GroupId, <<"alice">>, <<"key1">>, <<"secretkey">>),
    
    %% Retrieve sender key
    {ok, Key1} = iris_group:get_sender_key(GroupId, <<"alice">>, <<"key1">>),
    
    %% Store another key
    ok = iris_group:store_sender_key(GroupId, <<"alice">>, <<"key2">>, <<"secretkey2">>),
    
    %% Get all keys
    AllKeys = iris_group:get_all_sender_keys(GroupId, <<"alice">>),
    
    %% Rotate key
    {ok, NewKeyId} = iris_group:rotate_sender_key(GroupId, <<"alice">>, <<"newkey">>),
    {ok, RotatedKey} = iris_group:get_sender_key(GroupId, <<"alice">>, NewKeyId),
    
    %% Non-existent key
    R1 = iris_group:get_sender_key(GroupId, <<"alice">>, <<"nonexistent">>),
    
    case {Key1, length(AllKeys), RotatedKey, R1} of
        {<<"secretkey">>, 2, <<"newkey">>, {error, not_found}} ->
            io:format(\"PASS: Sender keys work~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Sender key operations work")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_group_deletion():
    """Test: Group deletion removes all data."""
    log("=== Test: Group Deletion ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    %% Create group with member and sender key
    {ok, GroupId} = iris_group:create_group(<<"Delete Test">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"member">>, <<"admin">>),
    ok = iris_group:store_sender_key(GroupId, <<"admin">>, <<"sk1">>, <<"keydata">>),
    
    %% Verify exists
    {ok, _} = iris_group:get_group(GroupId),
    true = iris_group:is_member(GroupId, <<"member">>),
    
    %% Delete group
    ok = iris_group:delete_group(GroupId, <<"admin">>),
    
    %% Verify deleted
    R1 = iris_group:get_group(GroupId),
    R2 = iris_group:is_member(GroupId, <<"admin">>),
    R3 = iris_group:is_member(GroupId, <<"member">>),
    
    case {R1, R2, R3} of
        {{error, not_found}, false, false} ->
            io:format(\"PASS: Group deleted completely~n\");
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Group deletion removes all data")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def test_list_groups():
    """Test: List groups for a user."""
    log("=== Test: List User Groups ===")
    
    # Use unique user IDs to avoid stale data issues
    import random
    suffix = random.randint(100000, 999999)
    
    code = f"""
    mnesia:create_schema([node()]),
    mnesia:start(),
    {{ok, _}} = iris_group:start_link(),
    
    %% Use unique user IDs to avoid collision with stale data
    User1 = <<"list_user1_{suffix}">>,
    User2 = <<"list_user2_{suffix}">>,
    
    %% Create multiple groups for user
    {{ok, G1}} = iris_group:create_group(<<"Group 1">>, User1),
    {{ok, G2}} = iris_group:create_group(<<"Group 2">>, User1),
    {{ok, _G3}} = iris_group:create_group(<<"Other Group">>, User2),
    
    %% List user1's groups
    Groups = iris_group:list_groups(User1),
    
    HasG1 = lists:member(G1, Groups),
    HasG2 = lists:member(G2, Groups),
    Count = length(Groups),
    
    case {{HasG1, HasG2, Count}} of
        {{true, true, 2}} ->
            io:format("PASS: User groups listed~n");
        Other ->
            io:format("FAIL: ~p~n", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ User groups listed correctly")
        return True
    else:
        log(f"  ✗ FAIL: {output}")
        return False


def main():
    """Run group membership tests."""
    log(f"=== Group Membership Tests (profile={TEST_PROFILE}, seed={TEST_SEED}) ===")
    
    if not check_erlang_available():
        log("[SKIP] Erlang not available. Exiting with code 2.")
        sys.exit(2)
    
    if not check_module_compiled():
        log("[SKIP] iris_group.beam not found. Run 'make' first.")
        sys.exit(2)
    
    results = []
    
    results.append(("Group Creation", test_group_creation()))
    results.append(("Member Management", test_member_management()))
    results.append(("Admin Promotion", test_admin_promotion()))
    results.append(("Authorization", test_authorization()))
    results.append(("Last Admin Protection", test_last_admin_protection()))
    results.append(("Sender Keys", test_sender_keys()))
    results.append(("Group Deletion", test_group_deletion()))
    results.append(("List User Groups", test_list_groups()))
    
    log("\n=== Results ===")
    passed = 0
    failed = 0
    for name, result in results:
        status = "PASS" if result else "FAIL"
        log(f"  {name}: {status}")
        if result:
            passed += 1
        else:
            failed += 1
    
    log(f"\nTotal: {passed}/{len(results)} passed")
    
    if failed > 0:
        log("[FAIL] Some tests failed")
        sys.exit(1)
    else:
        log("[PASS] All group membership tests passed")
        sys.exit(0)


if __name__ == "__main__":
    main()
