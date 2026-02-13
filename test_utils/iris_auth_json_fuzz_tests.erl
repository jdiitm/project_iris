-module(iris_auth_json_fuzz_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% JSON Parser Hardening Tests (Audit Mitigation)
%%
%% Validates that iris_auth_json:decode/1 handles:
%%   - Oversized input (>8KB) — must be explicitly rejected
%%   - Unterminated strings
%%   - Control characters in strings
%%   - Integer overflow (Erlang bigints should handle this)
%%   - Empty input
%% =============================================================================

json_fuzz_test_() ->
    [
     {"oversized input rejected", fun check_oversized_input/0},
     {"unterminated string rejected", fun check_unterminated_string/0},
     {"control char in string", fun check_control_char/0},
     {"integer overflow handled", fun check_integer_overflow/0},
     {"empty input rejected", fun check_empty_input/0}
    ].

%% A 16KB JSON input must be rejected with {error, input_too_large}.
check_oversized_input() ->
    %% Build a valid-looking JSON > 8192 bytes
    BigValue = list_to_binary(lists:duplicate(16000, $a)),
    BigJson = <<"{\"key\":\"", BigValue/binary, "\"}">>,
    ?assert(byte_size(BigJson) > 8192),
    Result = iris_auth_json:decode(BigJson),
    ?assertMatch({error, input_too_large}, Result).

%% Missing closing quote must return an error, not crash.
check_unterminated_string() ->
    Result = iris_auth_json:decode(<<"{\"key\":\"no close}">>),
    ?assertMatch({error, _}, Result).

%% Raw NUL byte inside a JSON string value.
check_control_char() ->
    Bin = <<"{\"key\":\"hello\x00world\"}">>,
    %% Either parse it or return an error — must not crash
    Result = iris_auth_json:decode(Bin),
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% Very large integer — Erlang bigints handle this natively.
check_integer_overflow() ->
    Bin = <<"{\"n\":99999999999999999999999999}">>,
    Result = iris_auth_json:decode(Bin),
    ?assertMatch({ok, #{<<"n">> := _}}, Result).

%% Empty binary must return an error.
check_empty_input() ->
    Result = iris_auth_json:decode(<<"">>),
    ?assertMatch({error, _}, Result).
