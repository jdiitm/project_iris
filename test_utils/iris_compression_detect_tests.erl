-module(iris_compression_detect_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Dynamic Compression Capability Detection Tests (Audit Mitigation)
%%
%% Validates:
%%   - available_algorithms/0 always includes <<"zlib">>
%%   - available_algorithms/0 only includes <<"zstd">> when NIF is present
%%   - iris_session uses dynamic detection (not hardcoded macro)
%% =============================================================================

compression_detect_test_() ->
    [
     {"available_algorithms includes zlib", fun check_zlib_always_present/0},
     {"available_algorithms detects zstd presence", fun check_zstd_detection/0},
     {"server capabilities use dynamic detection", fun check_dynamic_capabilities/0}
    ].

%% zlib is a core OTP module — always available.
check_zlib_always_present() ->
    Algos = iris_compression:available_algorithms(),
    ?assert(lists:member(<<"zlib">>, Algos)).

%% zstd should be in the list if and only if the NIF .so is loadable.
%% In the test environment, the NIF is likely NOT built, so zstd should
%% be absent. Either way, verify the function returns a proper list.
check_zstd_detection() ->
    Algos = iris_compression:available_algorithms(),
    ?assert(is_list(Algos)),
    %% If zstd NIF is not available (typical in tests), verify it's excluded
    case code:priv_dir(iris_edge) of
        {error, _} ->
            ?assertNot(lists:member(<<"zstd">>, Algos));
        PrivDir ->
            NifPath = filename:join(PrivDir, "iris_zstd_nif.so"),
            case filelib:is_file(NifPath) of
                true ->
                    ?assert(lists:member(<<"zstd">>, Algos));
                false ->
                    ?assertNot(lists:member(<<"zstd">>, Algos))
            end
    end.

%% Verify iris_session uses iris_compression:available_algorithms()
%% instead of a hardcoded macro by checking the source code.
check_dynamic_capabilities() ->
    %% Read iris_session.erl source and verify it calls available_algorithms
    {ok, Source} = file:read_file("src/iris_session.erl"),
    ?assert(binary:match(Source, <<"iris_compression:available_algorithms">>) =/= nomatch).
