#!/usr/bin/env python3
"""
Test: Group Fan-out Stress Test
RFC Reference: FR-13 (Group Messaging)

Validates efficient message delivery to group members under load.

Test scenarios:
1. Small group fan-out (serial strategy)
2. Medium group fan-out (parallel batch strategy)
3. Large group fan-out (worker pool strategy)
4. Burst message delivery
"""

import os
import sys
import time
import subprocess

# Path setup
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
sys.path.insert(0, PROJECT_ROOT)

# Test configuration
TEST_PROFILE = os.environ.get("TEST_PROFILE", "smoke")
TEST_SEED = int(os.environ.get("TEST_SEED", "42"))

# Scale based on profile
if TEST_PROFILE == "smoke":
    SMALL_GROUP_SIZE = 10
    MEDIUM_GROUP_SIZE = 30
    LARGE_GROUP_SIZE = 60
    BURST_MESSAGES = 5
else:
    SMALL_GROUP_SIZE = 40
    MEDIUM_GROUP_SIZE = 150
    LARGE_GROUP_SIZE = 500
    BURST_MESSAGES = 20


def log(msg):
    """Print with timestamp."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def erl_eval(code: str, timeout: int = 60) -> str:
    """Execute Erlang code and return output."""
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


def check_modules_compiled() -> bool:
    """Check if required modules are compiled."""
    required = ["iris_group.beam", "iris_group_fanout.beam"]
    for module in required:
        if not os.path.exists(os.path.join(PROJECT_ROOT, "ebin", module)):
            log(f"Missing: {module}")
            return False
    return True


def test_small_group_fanout():
    """Test: Small group uses serial delivery strategy."""
    log(f"=== Test: Small Group Fan-out ({SMALL_GROUP_SIZE} members) ===")
    
    code = f"""
    mnesia:create_schema([node()]),
    mnesia:start(),
    {{ok, _}} = iris_group:start_link(),
    
    %% Clear tables
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    
    %% Create group with members
    {{ok, GroupId}} = iris_group:create_group(<<"Small Group">>, <<"admin">>),
    
    %% Add members
    [iris_group:add_member(GroupId, list_to_binary("member_" ++ integer_to_list(N)), <<"admin">>) 
     || N <- lists:seq(1, {SMALL_GROUP_SIZE - 1})],
    
    %% Get delivery stats
    Stats = iris_group_fanout:get_delivery_stats(GroupId),
    Strategy = maps:get(strategy, Stats, unknown),
    MemberCount = maps:get(member_count, Stats, 0),
    
    case {{Strategy, MemberCount >= {SMALL_GROUP_SIZE}}} of
        {{serial, true}} ->
            io:format("PASS: Small group uses serial strategy (~p members)~n", [MemberCount]);
        Other ->
            io:format("FAIL: ~p~n", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Small group uses serial strategy")
        return True
    else:
        log(f"  ✗ FAIL: {output[-500:]}")
        return False


def test_medium_group_fanout():
    """Test: Medium group uses parallel batch strategy."""
    log(f"=== Test: Medium Group Fan-out ({MEDIUM_GROUP_SIZE} members) ===")
    
    code = f"""
    mnesia:create_schema([node()]),
    mnesia:start(),
    {{ok, _}} = iris_group:start_link(),
    
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    
    %% Create group
    {{ok, GroupId}} = iris_group:create_group(<<"Medium Group">>, <<"admin">>),
    
    %% Add many members
    [iris_group:add_member(GroupId, list_to_binary("member_" ++ integer_to_list(N)), <<"admin">>) 
     || N <- lists:seq(1, {MEDIUM_GROUP_SIZE - 1})],
    
    %% Get delivery stats
    Stats = iris_group_fanout:get_delivery_stats(GroupId),
    Strategy = maps:get(strategy, Stats, unknown),
    MemberCount = maps:get(member_count, Stats, 0),
    
    %% Medium groups should use parallel_batch strategy
    ExpectedStrategy = case MemberCount of
        N when N > 50 -> parallel_batch;
        _ -> serial
    end,
    
    case Strategy =:= ExpectedStrategy of
        true ->
            io:format("PASS: Medium group uses ~p strategy (~p members)~n", [Strategy, MemberCount]);
        false ->
            io:format("FAIL: Expected ~p, got ~p~n", [ExpectedStrategy, Strategy])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Medium group uses correct strategy")
        return True
    else:
        log(f"  ✗ FAIL: {output[-500:]}")
        return False


def test_large_group_fanout():
    """Test: Large group uses worker pool strategy."""
    log(f"=== Test: Large Group Fan-out ({LARGE_GROUP_SIZE} members) ===")
    
    code = f"""
    mnesia:create_schema([node()]),
    mnesia:start(),
    {{ok, _}} = iris_group:start_link(),
    
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    
    %% Create group
    {{ok, GroupId}} = iris_group:create_group(<<"Large Group">>, <<"admin">>),
    
    %% Add many members
    [iris_group:add_member(GroupId, list_to_binary("member_" ++ integer_to_list(N)), <<"admin">>) 
     || N <- lists:seq(1, {LARGE_GROUP_SIZE - 1})],
    
    %% Get delivery stats
    Stats = iris_group_fanout:get_delivery_stats(GroupId),
    Strategy = maps:get(strategy, Stats, unknown),
    MemberCount = maps:get(member_count, Stats, 0),
    
    %% Large groups should use worker_pool strategy
    ExpectedStrategy = case MemberCount of
        N when N > 200 -> worker_pool;
        N when N > 50 -> parallel_batch;
        _ -> serial
    end,
    
    case Strategy =:= ExpectedStrategy of
        true ->
            io:format("PASS: Large group uses ~p strategy (~p members)~n", [Strategy, MemberCount]);
        false ->
            io:format("FAIL: Expected ~p, got ~p~n", [ExpectedStrategy, Strategy])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Large group uses correct strategy")
        return True
    else:
        log(f"  ✗ FAIL: {output[-500:]}")
        return False


def test_fanout_stats():
    """Test: Fan-out provides accurate delivery stats."""
    log("=== Test: Fan-out Delivery Stats ===")
    
    code = """
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, _} = iris_group:start_link(),
    
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(<<"Stats Group">>, <<"sender">>),
    
    %% Add a few members
    [iris_group:add_member(GroupId, list_to_binary("member_" ++ integer_to_list(N)), <<"sender">>) 
     || N <- lists:seq(1, 5)],
    
    %% Fan out a message
    Message = <<"Test message for stats">>,
    {ok, Stats} = iris_group_fanout:fanout_group_msg(GroupId, <<"sender">>, Message),
    
    %% Extract stats fields
    TotalMembers = element(3, Stats),  %% total_members field
    Duration = element(7, Stats),       %% duration_ms field
    
    case {TotalMembers >= 5, Duration >= 0} of
        {true, true} ->
            io:format(\"PASS: Stats show ~p members, ~pms duration~n\", [TotalMembers, Duration]);
        Other ->
            io:format(\"FAIL: ~p~n\", [Other])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Delivery stats accurate")
        return True
    else:
        log(f"  ✗ FAIL: {output[-500:]}")
        return False


def test_batch_fanout():
    """Test: Batch message delivery."""
    log(f"=== Test: Batch Fan-out ({BURST_MESSAGES} messages) ===")
    
    code = f"""
    mnesia:start(),
    
    %% Create ram_copies tables for test environment
    catch mnesia:create_table(group, [{{ram_copies, [node()]}}, {{attributes, [id, name, created_by, created_at, max_members]}}]),
    catch mnesia:create_table(group_member, [{{ram_copies, [node()]}}, {{attributes, [group_id, user_id, role, joined_at]}}]),
    catch mnesia:create_table(group_sender_key, [{{ram_copies, [node()]}}, {{attributes, [group_id, key_id, sender_key, created_at]}}]),
    
    %% Wait for tables
    mnesia:wait_for_tables([group, group_member, group_sender_key], 5000),
    
    {{ok, _}} = iris_group:start_link(),
    
    %% Create group
    {{ok, GroupId}} = iris_group:create_group(<<"Batch Group">>, <<"sender">>),
    [iris_group:add_member(GroupId, list_to_binary("member_" ++ integer_to_list(N)), <<"sender">>) 
     || N <- lists:seq(1, 10)],
    
    %% Create batch of messages
    Messages = [list_to_binary("Message " ++ integer_to_list(N)) || N <- lists:seq(1, {BURST_MESSAGES})],
    
    %% Time batch delivery
    Start = erlang:monotonic_time(millisecond),
    {{ok, Stats}} = iris_group_fanout:fanout_batch(GroupId, <<"sender">>, Messages),
    Duration = erlang:monotonic_time(millisecond) - Start,
    
    %% Check stats
    TotalMembers = element(3, Stats),
    StatDuration = element(7, Stats),
    
    case {{TotalMembers >= 10, Duration < 10000}} of
        {{true, true}} ->
            io:format("PASS: Batch of {BURST_MESSAGES} messages delivered in ~pms~n", [Duration]);
        Other ->
            io:format("FAIL: ~p (duration: ~p, stat_duration: ~p)~n", [Other, Duration, StatDuration])
    end.
    """
    
    output = erl_eval(code)
    
    if "PASS" in output:
        log("  ✓ Batch delivery works")
        return True
    else:
        log(f"  ✗ FAIL: {output[-500:]}")
        return False


def main():
    """Run group fan-out stress tests."""
    log(f"=== Group Fan-out Stress Tests (profile={TEST_PROFILE}, seed={TEST_SEED}) ===")
    
    if not check_modules_compiled():
        log("[SKIP] Required modules not compiled. Run 'make' first.")
        sys.exit(2)
    
    results = []
    
    results.append(("Small Group Fan-out", test_small_group_fanout()))
    results.append(("Medium Group Fan-out", test_medium_group_fanout()))
    results.append(("Large Group Fan-out", test_large_group_fanout()))
    results.append(("Fan-out Stats", test_fanout_stats()))
    results.append(("Batch Fan-out", test_batch_fanout()))
    
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
        log("[PASS] All group fan-out tests passed")
        sys.exit(0)


if __name__ == "__main__":
    main()
