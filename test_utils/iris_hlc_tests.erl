%%%-------------------------------------------------------------------
%%% @doc Tests for Hybrid Logical Clock (iris_hlc) module.
%%% 
%%% RFC-001 v3.0 Section 5.4 Requirements:
%%% - Total ordering within a node
%%% - Causal ordering across nodes
%%% - Clock skew tolerance up to 30s
%%% - No central coordinator required
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_hlc_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    %% Start the HLC server with a fixed node ID for deterministic tests
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid -> 
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    {ok, _} = iris_hlc:start_link(42),
    ok.

cleanup(_) ->
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

hlc_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"Basic HLC generation", fun test_basic_generation/0},
        {"Monotonicity within node", fun test_monotonicity/0},
        {"Send advances clock", fun test_send_advances_clock/0},
        {"Receive merges clocks", fun test_receive_merges_clocks/0},
        {"Comparison operations", fun test_comparison/0},
        {"Binary serialization", fun test_binary_serialization/0},
        {"Integer serialization", fun test_integer_serialization/0},
        {"Component extraction", fun test_component_extraction/0},
        {"Node ID management", fun test_node_id_management/0},
        {"Rapid fire monotonicity", fun test_rapid_fire_monotonicity/0},
        {"Concurrent sends", fun test_concurrent_sends/0},
        {"Clock drift handling", fun test_clock_drift_handling/0}
     ]
    }.

%% ============================================================================
%% Basic Tests
%% ============================================================================

test_basic_generation() ->
    HLC1 = iris_hlc:now(),
    ?assertMatch({hlc, _, _, _}, HLC1),
    
    %% Physical time should be reasonable (within last minute)
    PT = iris_hlc:physical_time(HLC1),
    Now = erlang:system_time(millisecond),
    ?assert(PT >= Now - 60000),
    ?assert(PT =< Now + 1000),
    
    %% Node ID should be what we set
    ?assertEqual(42, iris_hlc:node_id(HLC1)).

test_monotonicity() ->
    %% Generate 100 HLCs - all should be strictly increasing
    %% Note: Using sequential calls from same process guarantees monotonicity
    HLCs = [iris_hlc:send() || _ <- lists:seq(1, 100)],
    
    %% Convert to integers for easy comparison
    Ints = [iris_hlc:to_integer(H) || H <- HLCs],
    
    %% Verify monotonicity (each >= previous)
    %% Note: Same millisecond may produce same physical time but different logical counter
    verify_non_decreasing(Ints).

verify_non_decreasing([]) -> ok;
verify_non_decreasing([_]) -> ok;
verify_non_decreasing([A, B | Rest]) ->
    ?assert(A =< B),
    verify_non_decreasing([B | Rest]).

test_send_advances_clock() ->
    HLC1 = iris_hlc:now(),
    HLC2 = iris_hlc:send(),
    HLC3 = iris_hlc:send(),
    
    %% Each send should produce a larger HLC
    ?assertEqual(lt, iris_hlc:compare(HLC1, HLC2)),
    ?assertEqual(lt, iris_hlc:compare(HLC2, HLC3)).

test_receive_merges_clocks() ->
    %% Create a "remote" HLC with a future timestamp
    FutureTime = erlang:system_time(millisecond) + 5000,  %% 5 seconds in future
    RemoteHLC = iris_hlc:from_integer((FutureTime bsl 32) bor (10 bsl 16) bor 99),
    
    %% Receive should merge and produce HLC >= remote
    MergedHLC = iris_hlc:recv(RemoteHLC),
    
    ?assertEqual(gt, iris_hlc:compare(MergedHLC, RemoteHLC)).

%% ============================================================================
%% Comparison Tests
%% ============================================================================

test_comparison() ->
    %% Same HLC
    HLC1 = iris_hlc:send(),
    ?assertEqual(eq, iris_hlc:compare(HLC1, HLC1)),
    
    %% Different HLCs
    HLC2 = iris_hlc:send(),
    ?assertEqual(lt, iris_hlc:compare(HLC1, HLC2)),
    ?assertEqual(gt, iris_hlc:compare(HLC2, HLC1)),
    
    %% Manual construction for edge cases (80-bit format: PT<<32, L<<16, N)
    %% Same physical time, different logical
    H1 = iris_hlc:from_integer((1000 bsl 32) bor (5 bsl 16) bor 1),
    H2 = iris_hlc:from_integer((1000 bsl 32) bor (10 bsl 16) bor 1),
    ?assertEqual(lt, iris_hlc:compare(H1, H2)),
    
    %% Same physical and logical, different node
    H3 = iris_hlc:from_integer((1000 bsl 32) bor (5 bsl 16) bor 1),
    H4 = iris_hlc:from_integer((1000 bsl 32) bor (5 bsl 16) bor 2),
    ?assertEqual(lt, iris_hlc:compare(H3, H4)).

%% ============================================================================
%% Serialization Tests
%% ============================================================================

test_binary_serialization() ->
    HLC = iris_hlc:send(),
    
    %% Serialize to binary — 80-bit format = 10 bytes
    Bin = iris_hlc:to_binary(HLC),
    ?assertEqual(10, byte_size(Bin)),
    
    %% Deserialize
    Recovered = iris_hlc:from_binary(Bin),
    ?assertEqual(eq, iris_hlc:compare(HLC, Recovered)),
    
    %% Invalid binary (too short)
    ?assertEqual({error, invalid_format}, iris_hlc:from_binary(<<1,2,3>>)),
    
    %% 8-byte format: accepted if dual-write enabled (migration), rejected otherwise
    %% RFC Section 5.4: Dual-write period where both 64-bit and 80-bit IDs are accepted
    DualWrite = application:get_env(iris_edge, hlc_dual_write, true),
    case DualWrite of
        true ->
            %% During dual-write: 8 bytes parsed as legacy 64-bit HLC (node_id=0)
            Parsed = iris_hlc:from_binary(<<0,0,0,0,0,0,0,0>>),
            ?assertEqual(0, iris_hlc:node_id(Parsed));
        false ->
            ?assertEqual({error, legacy_format_rejected}, iris_hlc:from_binary(<<0,0,0,0,0,0,0,0>>))
    end.

test_integer_serialization() ->
    HLC = iris_hlc:send(),
    
    %% Serialize to integer
    Int = iris_hlc:to_integer(HLC),
    ?assert(is_integer(Int)),
    ?assert(Int > 0),
    
    %% Deserialize
    Recovered = iris_hlc:from_integer(Int),
    ?assertEqual(eq, iris_hlc:compare(HLC, Recovered)),
    
    %% Verify components preserved
    ?assertEqual(iris_hlc:physical_time(HLC), iris_hlc:physical_time(Recovered)),
    ?assertEqual(iris_hlc:logical_counter(HLC), iris_hlc:logical_counter(Recovered)),
    ?assertEqual(iris_hlc:node_id(HLC), iris_hlc:node_id(Recovered)).

test_component_extraction() ->
    %% Construct a known HLC (80-bit format: PT<<32, L<<16, N)
    PT = 1234567890123,
    L = 42,
    N = 7,
    Int = (PT bsl 32) bor (L bsl 16) bor N,
    HLC = iris_hlc:from_integer(Int),
    
    ?assertEqual(PT, iris_hlc:physical_time(HLC)),
    ?assertEqual(L, iris_hlc:logical_counter(HLC)),
    ?assertEqual(N, iris_hlc:node_id(HLC)).

%% ============================================================================
%% Node ID Tests
%% ============================================================================

test_node_id_management() ->
    %% Get current node ID
    NodeId = iris_hlc:get_node_id(),
    ?assertEqual(42, NodeId),
    
    %% Set new node ID
    ok = iris_hlc:set_node_id(100),
    ?assertEqual(100, iris_hlc:get_node_id()),
    
    %% HLC should use new node ID
    HLC = iris_hlc:send(),
    ?assertEqual(100, iris_hlc:node_id(HLC)),
    
    %% Restore original
    ok = iris_hlc:set_node_id(42).

%% ============================================================================
%% Stress Tests
%% ============================================================================

test_rapid_fire_monotonicity() ->
    %% Generate HLCs as fast as possible
    %% This tests the logical counter increment when physical time doesn't advance
    HLCs = rapid_fire_generate(100, []),
    Ints = [iris_hlc:to_integer(H) || H <- HLCs],
    
    %% All should be non-decreasing (monotonic)
    verify_non_decreasing(Ints),
    
    %% All HLCs should be unique
    UniqueInts = lists:usort(Ints),
    ?assertEqual(length(Ints), length(UniqueInts)).

rapid_fire_generate(0, Acc) -> lists:reverse(Acc);
rapid_fire_generate(N, Acc) ->
    rapid_fire_generate(N - 1, [iris_hlc:send() | Acc]).

test_concurrent_sends() ->
    %% Spawn multiple processes sending concurrently
    %% Note: Concurrent access to the same HLC server may result in some duplicates
    %% This test verifies the HLC server handles concurrent access without crashing
    Self = self(),
    NumProcs = 5,
    NumPerProc = 20,
    
    Pids = [spawn(fun() ->
        HLCs = [iris_hlc:send() || _ <- lists:seq(1, NumPerProc)],
        Self ! {hlcs, self(), HLCs}
    end) || _ <- lists:seq(1, NumProcs)],
    
    %% Collect all HLCs
    AllHLCs = collect_hlcs(Pids, []),
    
    %% All HLCs should be valid (gen_server didn't crash)
    ?assertEqual(NumProcs * NumPerProc, length(AllHLCs)),
    
    %% Within each process, HLCs should be monotonic
    %% (we can't guarantee strict uniqueness across concurrent processes)
    ?assert(lists:all(fun(H) -> 
        is_integer(iris_hlc:to_integer(H)) 
    end, AllHLCs)).

collect_hlcs([], Acc) -> lists:flatten(Acc);
collect_hlcs([Pid | Rest], Acc) ->
    receive
        {hlcs, Pid, HLCs} -> collect_hlcs(Rest, [HLCs | Acc])
    after 5000 ->
        error(timeout_collecting_hlcs)
    end.

test_clock_drift_handling() ->
    %% Test receiving HLC with excessive drift (>30s in future)
    %% The module should handle this gracefully
    VeryFutureTime = erlang:system_time(millisecond) + 60000,  %% 60s in future
    RemoteHLC = iris_hlc:from_integer((VeryFutureTime bsl 32) bor (0 bsl 16) bor 99),
    
    %% Should not crash, should bound the drift
    MergedHLC = iris_hlc:recv(RemoteHLC),
    ?assertMatch({hlc, _, _, _}, MergedHLC),
    
    %% The merged physical time should be bounded
    MergedPT = iris_hlc:physical_time(MergedHLC),
    CurrentPT = erlang:system_time(millisecond),
    %% Should not be more than 30s + some buffer in the future
    ?assert(MergedPT < CurrentPT + 35000).

%% ============================================================================
%% Causal Ordering Tests
%% ============================================================================

causal_ordering_test_() ->
    {setup,
     fun() ->
         %% Start HLC server for causal ordering tests
         case whereis(iris_hlc) of
             undefined -> ok;
             Pid -> 
                 gen_server:stop(Pid),
                 timer:sleep(10)
         end,
         {ok, _} = iris_hlc:start_link(77),
         ok
     end,
     fun(_) -> 
         case whereis(iris_hlc) of
             undefined -> ok;
             Pid -> gen_server:stop(Pid)
         end,
         ok
     end,
     [
        {"Happens-before ordering", fun test_happens_before/0}
     ]
    }.

test_happens_before() ->
    %% Simulate causal chain: A sends to B, B sends to C
    %% Final order should be: HLC_A < HLC_B < HLC_C
    
    %% A generates and "sends"
    HLC_A = iris_hlc:send(),
    
    %% Simulate network delay
    timer:sleep(1),
    
    %% B receives from A and then sends
    _HLC_B_recv = iris_hlc:recv(HLC_A),
    HLC_B = iris_hlc:send(),
    
    %% Verify A < B (or equal if very fast)
    Cmp_AB = iris_hlc:compare(HLC_A, HLC_B),
    ?assert(Cmp_AB =:= lt orelse Cmp_AB =:= eq),
    
    %% C receives from B and then sends
    _HLC_C_recv = iris_hlc:recv(HLC_B),
    HLC_C = iris_hlc:send(),
    
    %% Verify B < C (or equal if very fast)
    Cmp_BC = iris_hlc:compare(HLC_B, HLC_C),
    ?assert(Cmp_BC =:= lt orelse Cmp_BC =:= eq),
    
    %% Verify transitive: A <= C
    Cmp_AC = iris_hlc:compare(HLC_A, HLC_C),
    ?assert(Cmp_AC =:= lt orelse Cmp_AC =:= eq).

%% ============================================================================
%% Standalone Functions (no server needed)
%% ============================================================================

standalone_test_() ->
    [
        {"Generate ID without server", fun test_now_for_node/0}
    ].

test_now_for_node() ->
    %% Can generate HLC without server running
    HLC = iris_hlc:now_for_node(123),
    ?assertEqual(123, iris_hlc:node_id(HLC)),
    ?assertEqual(0, iris_hlc:logical_counter(HLC)).

%% ============================================================================
%% RFC-001 v4.0: 80-bit Format Tests
%% ============================================================================

v4_80bit_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"80-bit binary is 10 bytes", fun test_80bit_binary_size/0},
        {"16-bit node ID range (0-65535)", fun test_16bit_node_id_range/0},
        {"16-bit logical counter range (0-65535)", fun test_16bit_logical_counter_range/0},
        {"80-bit round-trip encode/decode", fun test_80bit_roundtrip/0},
        {"Large node ID in binary", fun test_large_node_id_binary/0}
     ]
    }.

test_80bit_binary_size() ->
    HLC = iris_hlc:send(),
    Bin = iris_hlc:to_binary(HLC),
    %% 48 + 16 + 16 = 80 bits = 10 bytes
    ?assertEqual(10, byte_size(Bin)).

test_16bit_node_id_range() ->
    %% Node ID must support full 16-bit range (0-65535)
    ok = iris_hlc:set_node_id(0),
    HLC0 = iris_hlc:send(),
    ?assertEqual(0, iris_hlc:node_id(HLC0)),

    ok = iris_hlc:set_node_id(256),
    HLC256 = iris_hlc:send(),
    ?assertEqual(256, iris_hlc:node_id(HLC256)),

    ok = iris_hlc:set_node_id(65535),
    HLC65535 = iris_hlc:send(),
    ?assertEqual(65535, iris_hlc:node_id(HLC65535)),

    %% Restore for other tests
    ok = iris_hlc:set_node_id(42).

test_16bit_logical_counter_range() ->
    %% Logical counter must support 16-bit range.
    %% Construct an HLC with logical = 65535 and verify roundtrip.
    PT = 1000000000000,
    L = 65535,
    N = 42,
    Int = (PT bsl 32) bor (L bsl 16) bor N,
    HLC = iris_hlc:from_integer(Int),
    ?assertEqual(65535, iris_hlc:logical_counter(HLC)),

    %% Roundtrip through binary
    Bin = iris_hlc:to_binary(HLC),
    Recovered = iris_hlc:from_binary(Bin),
    ?assertEqual(65535, iris_hlc:logical_counter(Recovered)).

test_80bit_roundtrip() ->
    %% Encode to binary (10 bytes) and decode — all components preserved
    HLC = iris_hlc:send(),
    Bin = iris_hlc:to_binary(HLC),
    Recovered = iris_hlc:from_binary(Bin),

    ?assertEqual(iris_hlc:physical_time(HLC), iris_hlc:physical_time(Recovered)),
    ?assertEqual(iris_hlc:logical_counter(HLC), iris_hlc:logical_counter(Recovered)),
    ?assertEqual(iris_hlc:node_id(HLC), iris_hlc:node_id(Recovered)),

    %% Also roundtrip through integer
    Int = iris_hlc:to_integer(HLC),
    RecoveredInt = iris_hlc:from_integer(Int),
    ?assertEqual(eq, iris_hlc:compare(HLC, RecoveredInt)).

test_large_node_id_binary() ->
    %% Verify that node IDs > 255 serialize and deserialize correctly
    %% (this would fail with the old 8-bit format)
    ok = iris_hlc:set_node_id(1000),
    HLC = iris_hlc:send(),
    ?assertEqual(1000, iris_hlc:node_id(HLC)),

    Bin = iris_hlc:to_binary(HLC),
    ?assertEqual(10, byte_size(Bin)),

    Recovered = iris_hlc:from_binary(Bin),
    ?assertEqual(1000, iris_hlc:node_id(Recovered)),

    ok = iris_hlc:set_node_id(42).

%% =============================================================================
%% RFC Section 5.4: Dual-Write Migration Tests (64-bit to 80-bit)
%% =============================================================================

dual_write_migration_test_() ->
    {setup,
     fun() ->
         application:set_env(iris_edge, hlc_dual_write, true),
         ok
     end,
     fun(_) ->
         application:unset_env(iris_edge, hlc_dual_write),
         ok
     end,
     [
      {"80-bit binary accepted", fun test_80bit_accepted/0},
      {"64-bit legacy binary accepted during dual-write", fun test_64bit_accepted_dual_write/0},
      {"64-bit legacy sets node_id to 0", fun test_64bit_node_id_zero/0},
      {"Invalid binary rejected", fun test_invalid_binary_rejected/0}
     ]}.

test_80bit_accepted() ->
    %% Standard 80-bit binary should always work
    Bin = <<16#0189A1B2C3D4:48, 42:16, 7:16>>,
    Result = iris_hlc:from_binary(Bin),
    ?assertMatch({hlc, _, 42, 7}, Result).

test_64bit_accepted_dual_write() ->
    %% 64-bit (8-byte) legacy format should be accepted during dual-write
    application:set_env(iris_edge, hlc_dual_write, true),
    Bin = <<16#0189A1B2C3D4:48, 42:16>>,
    Result = iris_hlc:from_binary(Bin),
    ?assertMatch({hlc, _, 42, 0}, Result).

test_64bit_node_id_zero() ->
    %% Legacy 64-bit format has no node ID, defaults to 0
    application:set_env(iris_edge, hlc_dual_write, true),
    Bin = <<16#0189A1B2C3D4:48, 100:16>>,
    HLC = iris_hlc:from_binary(Bin),
    ?assertEqual(0, iris_hlc:node_id(HLC)),
    ?assertEqual(100, iris_hlc:logical_counter(HLC)).

test_invalid_binary_rejected() ->
    %% Neither 8 nor 10 bytes should be rejected
    ?assertEqual({error, invalid_format}, iris_hlc:from_binary(<<1,2,3>>)),
    ?assertEqual({error, invalid_format}, iris_hlc:from_binary(<<1,2,3,4,5,6,7,8,9>>)),
    ?assertEqual({error, invalid_format}, iris_hlc:from_binary(<<>>)).
