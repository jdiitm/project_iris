-module(iris_compression_detect_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Zstd Runtime Detection Tests
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
     {"AUDIT: available_algorithms/0 is exported",
      fun test_function_exported/0},
     {"AUDIT: zlib is always available",
      fun test_zlib_always_available/0},
     {"AUDIT: available_algorithms returns only binaries",
      fun test_returns_binaries/0},
     {"AUDIT: zstd inclusion depends on NIF .so presence",
      fun test_zstd_detection/0},
     {"AUDIT: iris_session uses dynamic capabilities (not hardcoded list)",
      fun test_session_dynamic_capabilities/0}
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
