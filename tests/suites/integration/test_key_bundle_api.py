#!/usr/bin/env python3
"""
Key Bundle API Integration Tests

Tests the iris_keys module API locally (single-node Erlang VM).
Validates basic key bundle operations:
- Upload and fetch
- OPK consumption
- OPK exhaustion with SPK fallback

NOTE: These tests do NOT exercise distributed durability.
For distributed durability under node failure, see:
  tests/suites/chaos_dist/test_key_bundle_durability.py
"""

import subprocess
import sys
import os
import random
import string

# Determinism: seed from environment
TEST_SEED = int(os.environ.get("TEST_SEED", 42))
random.seed(TEST_SEED)

# Add project root to sys.path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(current_dir, "../../.."))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Test configuration
TIMEOUT = 30


def generate_user_id():
    """Generate a unique test user ID."""
    return f"test_user_{''.join(random.choices(string.ascii_lowercase, k=8))}"


def run_erlang_command(code, timeout=TIMEOUT):
    """Run Erlang code and return output."""
    full_code = f"""
        cd {project_root} && \\
        erl -pa ebin -noshell -sname test_keys_$RANDOM -setcookie iris_secret -eval '
        try
            application:ensure_all_started(mnesia),
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
        timeout=timeout
    )
    return result.returncode == 0, result.stdout, result.stderr


def check_iris_keys_available():
    """Check if iris_keys module is compiled and available."""
    success, stdout, stderr = run_erlang_command(
        'case code:which(iris_keys) of non_existing -> io:format("NOT_FOUND"), halt(1); _ -> io:format("OK") end'
    )
    return success and "OK" in stdout


def test_key_bundle_upload():
    """Test basic key bundle upload."""
    print("\n1. Testing basic key bundle upload...")

    user_id = generate_user_id()

    # Create and upload a key bundle
    code = f'''
        UserId = <<"{user_id}">>,
        {{IK, IKPriv}} = crypto:generate_key(ecdh, x25519),
        {{SPK, _SPKPriv}} = crypto:generate_key(ecdh, x25519),
        Sig = iris_x3dh:sign_prekey(SPK, IKPriv),
        OPKs = [element(1, crypto:generate_key(ecdh, x25519)) || _ <- lists:seq(1, 10)],

        Bundle = #{{
            identity_key => IK,
            signed_prekey => SPK,
            signed_prekey_signature => Sig,
            one_time_prekeys => OPKs
        }},

        %% Start iris_keys if not running
        case whereis(iris_keys) of
            undefined -> iris_keys:start_link();
            _ -> ok
        end,

        case iris_keys:upload_bundle(UserId, Bundle) of
            ok ->
                io:format("UPLOAD_OK~n"),
                %% Verify fetch
                case iris_keys:fetch_bundle(UserId, false) of
                    {{ok, Fetched}} ->
                        case maps:get(identity_key, Fetched) =:= IK of
                            true -> io:format("FETCH_OK~n");
                            false -> io:format("FETCH_MISMATCH~n")
                        end;
                    Error -> io:format("FETCH_ERROR: ~p~n", [Error])
                end;
            Error -> io:format("UPLOAD_ERROR: ~p~n", [Error])
        end
    '''

    success, stdout, stderr = run_erlang_command(code)

    if success and "UPLOAD_OK" in stdout and "FETCH_OK" in stdout:
        print("   PASS: Key bundle upload and fetch working")
        return True
    else:
        print(f"   FAIL: Key bundle test failed")
        print(f"     stdout: {stdout}")
        print(f"     stderr: {stderr}")
        return False


def test_opk_consumption():
    """Test that OPK consumption works correctly."""
    print("\n2. Testing OPK consumption...")

    user_id = generate_user_id()

    code = f'''
        UserId = <<"{user_id}">>,
        {{IK, IKPriv}} = crypto:generate_key(ecdh, x25519),
        {{SPK, _SPKPriv}} = crypto:generate_key(ecdh, x25519),
        Sig = iris_x3dh:sign_prekey(SPK, IKPriv),
        OPKs = [element(1, crypto:generate_key(ecdh, x25519)) || _ <- lists:seq(1, 5)],

        Bundle = #{{
            identity_key => IK,
            signed_prekey => SPK,
            signed_prekey_signature => Sig,
            one_time_prekeys => OPKs
        }},

        case whereis(iris_keys) of
            undefined -> iris_keys:start_link();
            _ -> ok
        end,

        iris_keys:upload_bundle(UserId, Bundle),

        %% Fetch and consume OPK
        {{ok, Fetched1}} = iris_keys:fetch_bundle(UserId, true),
        OPK1 = maps:get(one_time_prekey, Fetched1),
        Remaining1 = maps:get(prekeys_remaining, Fetched1),

        case {{OPK1 =/= undefined, Remaining1 == 4}} of
            {{true, true}} -> io:format("CONSUME_OK~n");
            _ -> io:format("CONSUME_FAIL: OPK=~p, Remaining=~p~n", [OPK1, Remaining1])
        end,

        %% Verify count decreased
        {{ok, Count}} = iris_keys:get_prekey_count(UserId),
        case Count of
            4 -> io:format("COUNT_OK~n");
            _ -> io:format("COUNT_FAIL: ~p~n", [Count])
        end
    '''

    success, stdout, stderr = run_erlang_command(code)

    if success and "CONSUME_OK" in stdout and "COUNT_OK" in stdout:
        print("   PASS: OPK consumption working correctly")
        return True
    else:
        print(f"   FAIL: OPK consumption test failed")
        print(f"     stdout: {stdout}")
        return False


def test_opk_exhaustion_fallback():
    """Test that OPK exhaustion gracefully falls back to SPK."""
    print("\n3. Testing OPK exhaustion fallback...")

    user_id = generate_user_id()

    code = f'''
        UserId = <<"{user_id}">>,
        {{IK, IKPriv}} = crypto:generate_key(ecdh, x25519),
        {{SPK, _SPKPriv}} = crypto:generate_key(ecdh, x25519),
        Sig = iris_x3dh:sign_prekey(SPK, IKPriv),
        OPKs = [element(1, crypto:generate_key(ecdh, x25519))],  %% Only 1 OPK

        Bundle = #{{
            identity_key => IK,
            signed_prekey => SPK,
            signed_prekey_signature => Sig,
            one_time_prekeys => OPKs
        }},

        case whereis(iris_keys) of
            undefined -> iris_keys:start_link();
            _ -> ok
        end,

        iris_keys:upload_bundle(UserId, Bundle),

        %% Consume the only OPK
        {{ok, _}} = iris_keys:fetch_bundle(UserId, true),

        %% Now fetch again - should return with spk_fallback_mode=true
        {{ok, Fetched}} = iris_keys:fetch_bundle(UserId, true),
        OPK = maps:get(one_time_prekey, Fetched),
        FallbackMode = maps:get(spk_fallback_mode, Fetched, false),

        case {{OPK, FallbackMode}} of
            {{undefined, true}} -> io:format("FALLBACK_OK~n");
            _ -> io:format("FALLBACK_FAIL: OPK=~p, Fallback=~p~n", [OPK, FallbackMode])
        end
    '''

    success, stdout, stderr = run_erlang_command(code)

    if success and "FALLBACK_OK" in stdout:
        print("   PASS: OPK exhaustion fallback working")
        return True
    else:
        print(f"   FAIL: OPK exhaustion test failed")
        print(f"     stdout: {stdout}")
        return False


def main():
    print("\n" + "=" * 60)
    print("Key Bundle API Integration Tests")
    print("=" * 60)

    # Check prerequisites
    if not check_iris_keys_available():
        print("\nFAIL: iris_keys module not available")
        print("      Compile with: erlc -o ebin src/iris_keys.erl")
        sys.exit(1)

    print("[Check] iris_keys module available")

    # Run tests
    results = []

    results.append(("Key bundle upload", test_key_bundle_upload()))
    results.append(("OPK consumption", test_opk_consumption()))
    results.append(("OPK exhaustion fallback", test_opk_exhaustion_fallback()))

    # Summary
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)

    passed = sum(1 for _, r in results if r)
    total = len(results)

    for name, result in results:
        status = "PASS" if result else "FAIL"
        print(f"  {status}: {name}")

    print(f"\n  Total: {passed}/{total} tests passed")

    if passed == total:
        print("\nPASS: Key bundle API tests verified")
        sys.exit(0)
    else:
        print(f"\nFAIL: {total - passed} tests failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
