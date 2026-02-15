-module(iris_compression_detect_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Zstd Runtime Detection Tests
%% =============================================================================
%%
%% Tests verify:
%% - available_algorithms/0 always includes zlib
%% - available_algorithms/0 dynamically detects zstd NIF presence
%% - iris_session uses dynamic detection (not hardcoded compression list)
%% - available_algorithms/0 is exported
%% =============================================================================

iris_compression_detect_test_() ->
    [
     {"available_algorithms/0 is exported",
      fun test_function_exported/0},
     {"zlib is always available",
      fun test_zlib_always_available/0},
     {"available_algorithms returns only binaries",
      fun test_returns_binaries/0},
     {"zstd inclusion depends on NIF .so presence",
      fun test_zstd_detection/0},
     {"iris_session uses dynamic capabilities (not hardcoded list)",
      fun test_session_dynamic_capabilities/0},
     {"if zstd reported available, compress must not crash",
      fun test_zstd_load_verification/0},
     {"source checks NIF loadability, not just file existence",
      fun test_source_checks_nif_load/0}
    ].

test_function_exported() ->
    Exports = iris_compression:module_info(exports),
    ?assert(lists:member({available_algorithms, 0}, Exports)).

test_zlib_always_available() ->
    Algos = iris_compression:available_algorithms(),
    ?assert(lists:member(<<"zlib">>, Algos)).

test_returns_binaries() ->
    Algos = iris_compression:available_algorithms(),
    ?assert(is_list(Algos)),
    lists:foreach(fun(A) ->
        ?assert(is_binary(A))
    end, Algos).

test_zstd_detection() ->
    Algos = iris_compression:available_algorithms(),
    %% Check if the NIF .so actually exists on disk
    NifExists = case code:priv_dir(iris_edge) of
        {error, _} -> false;
        PrivDir ->
            filelib:is_file(filename:join(PrivDir, "iris_zstd_nif.so"))
    end,
    case NifExists of
        true ->
            ?assert(lists:member(<<"zstd">>, Algos));
        false ->
            ?assertNot(lists:member(<<"zstd">>, Algos))
    end.

test_session_dynamic_capabilities() ->
    %% Verify the source code calls iris_compression:available_algorithms()
    %% and does NOT have a hardcoded zstd in SERVER_CAPABILITIES
    {ok, Src} = file:read_file("src/iris_session.erl"),
    ?assert(binary:match(Src, <<"iris_compression:available_algorithms()">>) =/= nomatch),
    %% The old hardcoded line should NOT exist
    ?assertEqual(nomatch, binary:match(Src, <<"[<<\"zlib\">>, <<\"zstd\">>, <<\"e2ee\">>">>)).

test_zstd_load_verification() ->
    %% If zstd is reported as available, it must actually work (not crash with undef)
    Algos = iris_compression:available_algorithms(),
    case lists:member(<<"zstd">>, Algos) of
        true ->
            %% Must not crash — should return {ok, _} or {error, _}
            Result = iris_compression:compress(zstd, <<"test data for compression">>),
            case Result of
                {ok, _} -> ok;
                {error, Reason} ->
                    %% If available but fails, detection is broken
                    ?assertEqual(should_not_fail_if_available, Reason)
            end;
        false ->
            %% zstd not available — fine, nothing to verify
            ok
    end.

test_source_checks_nif_load() ->
    %% Source should try loading the NIF, not just check file existence
    {ok, Src} = file:read_file("src/iris_compression.erl"),
    %% Must have actual NIF invocation in the detection path
    ?assert(binary:match(Src, <<"zstd_nif_available">>) =/= nomatch),
    %% Should have persistent_term or try-catch based detection
    HasTryDetect = (binary:match(Src, <<"try_zstd_nif">>) =/= nomatch) orelse
                   (binary:match(Src, <<"persistent_term">>) =/= nomatch) orelse
                   (binary:match(Src, <<"iris_zstd_nif:compress">>) =/= nomatch),
    ?assert(HasTryDetect).
