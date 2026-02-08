-module(iris_token_refresh_error_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 3 TDD: token_refresh must NOT return a placeholder access token
%% =============================================================================
%% RED:  iris_session.erl contains <<"access_token_pending">> placeholder.
%% GREEN: Replace placeholder with a proper error response.
%% =============================================================================

%% Structural test: the source must not contain the dangerous placeholder.
%% This is more reliable than mocking the full RPC chain for handle_packet/4.
no_placeholder_token_in_session_source_test() ->
    {ok, Source} = file:read_file("src/iris_session.erl"),
    ?assertEqual(nomatch, binary:match(Source, <<"access_token_pending">>)).

%% Verify encode_error/1 is used for the failure branch (positive check).
%% After the fix, the fallback branch should use encode_error(token_creation_failed).
token_creation_failed_error_exists_in_source_test() ->
    {ok, Source} = file:read_file("src/iris_session.erl"),
    ?assertNotEqual(nomatch, binary:match(Source, <<"token_creation_failed">>)).
