#!/usr/bin/env python3
import sys
import os
import time
import socket
import subprocess

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, PROJECT_ROOT)

def run_erlang_test(code):
    full_code = f"""
        cd {PROJECT_ROOT} && \
        erl -pa ebin -noshell -sname test_oom_$RANDOM -setcookie iris_secret -eval '
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
    return result.returncode == 0, result.stdout

def test_oom_kill():
    print("[TEST] Verifying OOM Kill Switch...")
    
    success, stdout = run_erlang_test("""
        %% Start Ingress Guard
        iris_ingress_guard:start_link(),
        
        %% Start Edge Conn process directly
        {ok, Pid} = iris_edge_conn:start_link(undefined),
        
        %% Check max_heap_size flag
        {max_heap_size, Flags} = process_info(Pid, max_heap_size),
        Expected = #{size => 50000, kill => true},
        
        IsSubset = maps:fold(fun(K, V, Acc) -> 
            Acc andalso (maps:get(K, Flags, undefined) == V)
        end, true, Expected),
        
        case IsSubset of
            true -> 
                 io:format("PASS: max_heap_size set correctly to ~p~n", [Expected]);
            false -> 
                 io:format("FAIL: max_heap_size ~p does not match expected ~p~n", [Flags, Expected]),
                 exit(fail)
        end,

        %% Force memory growth to trigger kill
        %% We send a massive binary to the process loop
        LargeBin = binary:copy(<<1>>, 1024 * 1024), %% 1MB
        Pid ! {tcp, undefined, LargeBin},
        
        timer:sleep(100),
        
        %% Verify process is dead
        case is_process_alive(Pid) of
            true -> 
                io:format("FAIL: Process survived > max_heap_size~n"),
                exit(survived);
            false ->
                io:format("PASS: Process was killed by system~n")
        end
    """)
    
    if success and "PASS: Process was killed" in stdout:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL: {stdout}")
        return False

if __name__ == "__main__":
    if test_oom_kill():
        sys.exit(0)
    else:
        sys.exit(1)
