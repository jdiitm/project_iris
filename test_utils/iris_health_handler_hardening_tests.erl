-module(iris_health_handler_hardening_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Health Handler Hardening Tests
%% =============================================================================
%%
%% Tests verify that the HTTP health handler:
%% - Returns 404 for unknown paths
%% - Handles trailing slashes gracefully
%% - Ignores query strings
%% - Has path normalization in source
%% =============================================================================

iris_health_handler_hardening_test_() ->
    [
     {"AUDIT: unknown path returns 404",
      fun test_unknown_path_returns_404/0},
     {"AUDIT: trailing slash on /health is handled",
      fun test_trailing_slash_handled/0},
     {"AUDIT: query string on /health is ignored",
      fun test_query_string_ignored/0},
     {"AUDIT: source has path normalization",
      fun test_source_has_path_normalization/0}
    ].

test_unknown_path_returns_404() ->
    %% The dispatch function already returns 404 for unknown paths
    %% Verify it works with the dispatch function directly
    {Status, _, _} = iris_health_handler:dispatch(<<"/unknown">>, #{}),
    ?assertEqual(404, Status).

test_trailing_slash_handled() ->
    %% /health/ should be treated the same as /health
    {Status, _, _} = iris_health_handler:dispatch(<<"/health/">>, #{}),
    ?assertEqual(200, Status).

test_query_string_ignored() ->
    %% /health?foo=bar should be treated the same as /health
    {Status, _, _} = iris_health_handler:dispatch(<<"/health?foo=bar">>, #{}),
    ?assertEqual(200, Status).

test_source_has_path_normalization() ->
    {ok, Src} = file:read_file("src/iris_health_handler.erl"),
    %% Source should have normalize_path or equivalent
    ?assert(binary:match(Src, <<"normalize_path">>) =/= nomatch).
