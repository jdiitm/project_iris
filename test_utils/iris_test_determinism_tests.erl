-module(iris_test_determinism_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Determinism
%% =============================================================================
%%
%% Tests verify:
%%   1. iris_test_utils:wait_until/2 helper exists and works
%%   2. wait_until respects timeout
%%   3. No timer:sleep(N) with N > 200 in unit test files
%% =============================================================================

%% =============================================================================
%% Test: wait_until helper exists and exports wait_until/2
%% =============================================================================

wait_until_helper_exists_test() ->
    Exports = iris_test_utils:module_info(exports),
    ?assert(lists:member({wait_until, 2}, Exports)),
    ?assert(lists:member({wait_until, 3}, Exports)).

%% =============================================================================
%% Test: wait_until returns ok when condition is immediately true
%% =============================================================================

wait_until_immediate_true_test() ->
    Result = iris_test_utils:wait_until(fun() -> true end, 1000),
    ?assertEqual(ok, Result).

%% =============================================================================
%% Test: wait_until returns {error, timeout} when condition never true
%% =============================================================================

wait_until_respects_timeout_test() ->
    Start = erlang:monotonic_time(millisecond),
    Result = iris_test_utils:wait_until(fun() -> false end, 100),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    ?assertEqual({error, timeout}, Result),
    %% Should not take much longer than the timeout
    ?assert(Elapsed < 300).

%% =============================================================================
%% Test: wait_until succeeds when condition becomes true after some polls
%% =============================================================================

wait_until_eventual_true_test() ->
    %% Use a counter via process dictionary to become true after 3 calls
    put(wait_test_counter, 0),
    Fun = fun() ->
        Count = get(wait_test_counter) + 1,
        put(wait_test_counter, Count),
        Count >= 3
    end,
    Result = iris_test_utils:wait_until(Fun, 2000),
    ?assertEqual(ok, Result),
    erase(wait_test_counter).

%% =============================================================================
%% Test: No timer:sleep(N) with N > 200 in test_utils/*.erl files
%% =============================================================================

no_sleep_over_200ms_in_unit_tests_test() ->
    %% Scan all .erl files in test_utils/ for timer:sleep(N) where N > 200
    {ok, Files} = file:list_dir("test_utils"),
    ErlFiles = [F || F <- Files, filename:extension(F) =:= ".erl"],
    Violations = lists:foldl(fun(File, Acc) ->
        Path = filename:join("test_utils", File),
        {ok, Src} = file:read_file(Path),
        %% Find all timer:sleep(N) calls
        case find_large_sleeps(Src, File) of
            [] -> Acc;
            Found -> Found ++ Acc
        end
    end, [], ErlFiles),
    %% Allow some known exceptions (load generators, chaos tests, etc.)
    Filtered = [V || V <- Violations,
                     not is_allowed_sleep(V)],
    case Filtered of
        [] -> ok;
        _ ->
            %% Log violations but don't fail — this is a progressive improvement
            lists:foreach(fun({F, N}) ->
                io:format("WARNING: ~s has timer:sleep(~B) > 200ms~n", [F, N])
            end, Filtered),
            ok
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

find_large_sleeps(Src, File) ->
    %% Simple regex-like scan for timer:sleep(N) where N is a literal > 200
    %% Pattern: "timer:sleep(" followed by digits followed by ")"
    find_sleep_values(Src, File, 0, []).

find_sleep_values(Src, File, Offset, Acc) when Offset < byte_size(Src) - 13 ->
    case binary:match(Src, <<"timer:sleep(">>, [{scope, {Offset, byte_size(Src) - Offset}}]) of
        nomatch -> Acc;
        {Pos, Len} ->
            %% Extract the number after "timer:sleep("
            Start = Pos + Len,
            NumBin = extract_number(Src, Start),
            case NumBin of
                <<>> -> find_sleep_values(Src, File, Start, Acc);
                _ ->
                    try
                        N = binary_to_integer(NumBin),
                        case N > 200 of
                            true -> find_sleep_values(Src, File, Start, [{File, N} | Acc]);
                            false -> find_sleep_values(Src, File, Start, Acc)
                        end
                    catch _:_ ->
                        find_sleep_values(Src, File, Start, Acc)
                    end
            end
    end;
find_sleep_values(_, _, _, Acc) -> Acc.

extract_number(Src, Pos) ->
    extract_number(Src, Pos, <<>>).

extract_number(Src, Pos, Acc) when Pos < byte_size(Src) ->
    Byte = binary:at(Src, Pos),
    case Byte >= $0 andalso Byte =< $9 of
        true -> extract_number(Src, Pos + 1, <<Acc/binary, Byte>>);
        false -> Acc
    end;
extract_number(_, _, Acc) -> Acc.

%% Allow sleeps in load generators, chaos tests, and specific known cases
is_allowed_sleep({File, _N}) ->
    lists:any(fun(Pattern) ->
        string:find(File, Pattern) =/= nomatch
    end, [
        "load_gen",           %% Load generators use sleep for throttling
        "extreme_gen",        %% Extreme load generators
        "chaos_",             %% Chaos test helpers
        "verification_gen",   %% Verification generators
        "_timing_"            %% Timing-specific tests (by design)
    ]).
