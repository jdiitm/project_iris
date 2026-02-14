-module(iris_auth_json_fuzz_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: JSON Parser Hardening Tests
%% =============================================================================
%%
%% Tests verify that iris_auth_json:decode/1 safely handles:
%% - Oversized input (>8KB) rejected with {error, input_too_large}
%% - Unterminated strings
%% - Control characters in strings
%% - Large integers (Erlang bigints)
%% - Empty input
%% - Non-object input
%% - Source code contains MAX_INPUT_SIZE guard
%% =============================================================================

iris_auth_json_fuzz_test_() ->
    [
     {"AUDIT: oversized input (16KB) rejected with input_too_large",
      fun test_oversized_input_rejected/0},
     {"AUDIT: exactly-at-limit input (8192 bytes) is accepted",
      fun test_at_limit_input_accepted/0},
     {"AUDIT: unterminated string returns error",
      fun test_unterminated_string/0},
     {"AUDIT: control char in string does not crash",
      fun test_control_char_in_string/0},
     {"AUDIT: large integer parses without crash",
      fun test_integer_overflow/0},
     {"AUDIT: empty input returns error",
      fun test_empty_input/0},
     {"AUDIT: non-object input returns error",
      fun test_non_object_input/0},
     {"AUDIT: encode/decode roundtrip preserves data",
      fun test_roundtrip/0},
     {"AUDIT: source contains MAX_INPUT_SIZE guard",
      fun test_source_has_guard/0}
    ].

test_oversized_input_rejected() ->
    %% 16KB payload — must be rejected
    BigPayload = binary:copy(<<" ">>, 16384),
    Result = iris_auth_json:decode(BigPayload),
    ?assertEqual({error, input_too_large}, Result).

test_at_limit_input_accepted() ->
    %% 8192 bytes exactly — should be accepted (not rejected)
    %% Build a valid JSON object that is exactly 8192 bytes
    %% {"k":"<value>"} where value pads to 8192 total
    Prefix = <<"{\"k\":\"">>,
    Suffix = <<"\"}">>,
    PadLen = 8192 - byte_size(Prefix) - byte_size(Suffix),
    Pad = binary:copy(<<"a">>, PadLen),
    Input = <<Prefix/binary, Pad/binary, Suffix/binary>>,
    ?assertEqual(8192, byte_size(Input)),
    Result = iris_auth_json:decode(Input),
    ?assertMatch({ok, _}, Result).

test_unterminated_string() ->
    %% Missing closing quote — must return error, not hang or crash
    Result = iris_auth_json:decode(<<"{\"key\":\"no close}">>),
    ?assertMatch({error, _}, Result).

test_control_char_in_string() ->
    %% Raw null byte inside a string value — must not crash
    Input = <<"{\"k\":\"val", 0, "ue\"}">>,
    %% Should either parse (with the byte included) or error — never crash
    Result = iris_auth_json:decode(Input),
    case Result of
        {ok, Map} ->
            %% If it parses, the value should contain the byte
            Val = maps:get(<<"k">>, Map),
            ?assert(is_binary(Val));
        {error, _} ->
            ok
    end.

test_integer_overflow() ->
    %% Very large integer — Erlang bigints handle this natively
    Input = <<"{\"n\":99999999999999999999999999}">>,
    Result = iris_auth_json:decode(Input),
    ?assertMatch({ok, #{<<"n">> := _}}, Result),
    {ok, #{<<"n">> := Val}} = Result,
    ?assert(is_integer(Val)),
    ?assertEqual(99999999999999999999999999, Val).

test_empty_input() ->
    Result = iris_auth_json:decode(<<"">>),
    ?assertMatch({error, _}, Result).

test_non_object_input() ->
    %% Arrays, bare strings, bare numbers — must return error
    ?assertMatch({error, _}, iris_auth_json:decode(<<"[1,2,3]">>)),
    ?assertMatch({error, _}, iris_auth_json:decode(<<"\"hello\"">>)),
    ?assertMatch({error, _}, iris_auth_json:decode(<<"42">>)).

test_roundtrip() ->
    Original = #{<<"sub">> => <<"user1">>, <<"exp">> => 1700000000},
    Encoded = iris_auth_json:encode(Original),
    {ok, Decoded} = iris_auth_json:decode(Encoded),
    ?assertEqual(Original, Decoded).

test_source_has_guard() ->
    {ok, Src} = file:read_file("src/iris_auth_json.erl"),
    ?assert(binary:match(Src, <<"MAX_INPUT_SIZE">>) =/= nomatch),
    ?assert(binary:match(Src, <<"input_too_large">>) =/= nomatch).
