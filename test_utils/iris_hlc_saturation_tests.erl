-module(iris_hlc_saturation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% HLC logical counter saturation
%% =============================================================================
%% RFC Section 1.1: "Messages have strictly monotonic IDs (HLC)"
%% When logical counter reaches MAX_LOGICAL (65535), the HLC MUST NOT produce
%% duplicate timestamps. It must either wait for wall clock to advance or
%% return an error.
%% =============================================================================

setup() ->
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    {ok, _} = iris_hlc:start_link(42),
    ok.

cleanup(_) ->
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

hlc_saturation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"Strict monotonicity under rapid generation", fun test_strict_monotonicity_rapid/0},
        {"No duplicate timestamps under burst", fun test_no_duplicates_burst/0},
        {"Counter overflow returns error or advances", fun test_counter_overflow_handling/0}
     ]
    }.

%% ---------------------------------------------------------------------------
%% Test: 500 rapid sends produce strictly monotonic (unique) timestamps
%% ---------------------------------------------------------------------------
test_strict_monotonicity_rapid() ->
    HLCs = [iris_hlc:send() || _ <- lists:seq(1, 500)],
    Ints = [iris_hlc:to_integer(H) || H <- HLCs],
    %% All must be strictly increasing (no duplicates)
    UniqueInts = lists:usort(Ints),
    ?assertEqual(length(Ints), length(UniqueInts),
                 "HLC timestamps must be strictly unique under rapid generation").

%% ---------------------------------------------------------------------------
%% Test: Under burst, all timestamps unique even approaching counter limit
%% ---------------------------------------------------------------------------
test_no_duplicates_burst() ->
    %% Generate enough to stress the logical counter within one millisecond.
    %% gen_server serialization + Erlang scheduling means we won't hit 65535,
    %% but this confirms uniqueness under load.
    N = 1000,
    HLCs = [iris_hlc:send() || _ <- lists:seq(1, N)],
    Ints = [iris_hlc:to_integer(H) || H <- HLCs],
    UniqueInts = lists:usort(Ints),
    ?assertEqual(N, length(UniqueInts),
                 "All HLC timestamps must be unique under burst generation").

%% ---------------------------------------------------------------------------
%% Test: When logical counter is at MAX_LOGICAL, next send must not duplicate.
%% This directly tests the overflow handling path.
%% ---------------------------------------------------------------------------
test_counter_overflow_handling() ->
    %% Directly set state to have logical counter at MAX_LOGICAL (65535)
    %% by sending a remote HLC that forces the counter near max.
    PT = erlang:system_time(millisecond) + 5000,  %% Future time to prevent wall clock advance
    %% Create remote HLC with logical = 65534
    RemoteHLC = iris_hlc:from_integer((PT bsl 32) bor (65534 bsl 16) bor 99),
    
    %% Receive it -- local counter becomes max(local_L, remote_L) + 1 = 65535
    MergedHLC = iris_hlc:recv(RemoteHLC),
    L1 = iris_hlc:logical_counter(MergedHLC),
    ?assertEqual(65535, L1),
    
    %% Now send -- this is the critical test.
    %% Counter is at 65535, wall clock hasn't advanced past PT + 5000.
    %% The implementation MUST produce a unique, greater-than timestamp.
    NextHLC = iris_hlc:send(),
    ?assert(iris_hlc:compare(MergedHLC, NextHLC) =:= lt,
            "HLC after overflow must be strictly greater than previous").
