-module(iris_auth_json_depth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% JSON Parser Depth Limit Tests
%% =============================================================================
%%
%% Tests verify that iris_auth_json:decode/1 rejects deeply nested JSON
%% to prevent stack exhaustion attacks (depth bomb). A crafted payload like
%% {"a":{"a":{"a":...}}} within 8KB can exhaust the call stack without a
%% depth guard.
%% =============================================================================

iris_auth_json_depth_test_() ->
    [
     {"100-level nested JSON is rejected",
      fun test_deeply_nested_rejected/0},
     {"JSON at max allowed depth is accepted",
      fun test_at_depth_limit_accepted/0},
     {"source has MAX_DEPTH guard",
      fun test_source_has_depth_guard/0}
    ].

%% Build a deeply nested JSON object: {"a":{"a":{"a":...}}}
build_nested_json(0) -> <<"{}">>;
build_nested_json(Depth) ->
    Inner = build_nested_json(Depth - 1),
    <<"{\"a\":", Inner/binary, "}">>.

test_deeply_nested_rejected() ->
    %% 100-level nesting -- must be rejected with max_depth_exceeded
    Input = build_nested_json(100),
    ?assert(byte_size(Input) =< 8192),  %% within size limit
    Result = iris_auth_json:decode(Input),
    ?assertMatch({error, _}, Result).

test_at_depth_limit_accepted() ->
    %% At the max allowed depth (32) -- should parse successfully
    Input = build_nested_json(32),
    Result = iris_auth_json:decode(Input),
    ?assertMatch({ok, _}, Result).

test_source_has_depth_guard() ->
    {ok, Src} = file:read_file("src/iris_auth_json.erl"),
    ?assert(binary:match(Src, <<"MAX_DEPTH">>) =/= nomatch).
