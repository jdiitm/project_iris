-module(iris_test_utils).

%% =============================================================================
%% AUDIT MITIGATION V2 — P1-1: Test Determinism Utilities
%% =============================================================================
%%
%% Event-driven assertion helpers to replace timer:sleep() in tests.
%% These poll a condition with exponential backoff, eliminating
%% timing-dependent test failures.
%% =============================================================================

-export([wait_until/2, wait_until/3]).

%% @doc Wait until Fun() returns true, or timeout after TimeoutMs.
%% Polls with exponential backoff starting at 5ms, capped at 100ms.
%% Returns ok on success, {error, timeout} on timeout.
-spec wait_until(fun(() -> boolean()), pos_integer()) -> ok | {error, timeout}.
wait_until(Fun, TimeoutMs) ->
    wait_until(Fun, TimeoutMs, 5).

%% @doc Wait until Fun() returns true, or timeout after TimeoutMs.
%% InitialIntervalMs sets the initial polling interval.
-spec wait_until(fun(() -> boolean()), pos_integer(), pos_integer()) -> ok | {error, timeout}.
wait_until(Fun, TimeoutMs, InitialIntervalMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    do_wait_until(Fun, Deadline, InitialIntervalMs).

do_wait_until(Fun, Deadline, IntervalMs) ->
    case Fun() of
        true -> ok;
        _ ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true -> {error, timeout};
                false ->
                    timer:sleep(min(IntervalMs, 100)),
                    NextInterval = min(IntervalMs * 2, 100),
                    do_wait_until(Fun, Deadline, NextInterval)
            end
    end.
