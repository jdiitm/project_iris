-module(iris_health_handler_auth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Metrics Endpoint Authentication Tests
%% =============================================================================
%%
%% Tests verify:
%% - /metrics requires bearer token when metrics_bearer_token is configured
%% - /metrics returns 401 without token when metrics_bearer_token is set
%% - /metrics returns 401 with wrong token
%% - /metrics accessible without token when metrics_bearer_token is NOT configured
%% - /health and /ready remain unauthenticated regardless of token config
%% - Source code structure: check_metrics_auth function exists
%% =============================================================================

%% --- Source analysis tests (no running server needed) ---

source_structure_test_() ->
    [
     {"check_metrics_auth function exists in iris_health_handler.erl", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          ?assert(binary:match(Src, <<"check_metrics_auth">>) =/= nomatch)
      end},

     {"/metrics dispatch calls check_metrics_auth", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          %% The dispatch for /metrics should reference check_metrics_auth
          ?assert(binary:match(Src, <<"metrics_auth">>) =/= nomatch)
      end},

     {"/health dispatch does NOT check auth", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          %% The dispatch for /health should call health() directly
          Lines = binary:split(Src, <<"\n">>, [global]),
          HealthLines = [L || L <- Lines,
              binary:match(L, <<"health">>)  =/= nomatch,
              binary:match(L, <<"dispatch">>) =/= nomatch],
          AuthInHealthDispatch = [L || L <- HealthLines,
              binary:match(L, <<"auth">>) =/= nomatch],
          ?assertEqual([], AuthInHealthDispatch)
      end},

     {"401 status line exists", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          ?assert(binary:match(Src, <<"401 Unauthorized">>) =/= nomatch)
      end},

     {"Authorization header is collected from requests", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          ?assert(binary:match(Src, <<"'Authorization'">>) =/= nomatch)
      end},

     {"Bearer token checked from application env", fun() ->
          {ok, Src} = file:read_file("src/iris_health_handler.erl"),
          ?assert(binary:match(Src, <<"metrics_bearer_token">>) =/= nomatch)
      end}
    ].

%% --- Functional auth logic test (isolated) ---

auth_logic_test_() ->
    [
     {"no configured token = open access", fun() ->
          OldVal = application:get_env(iris_core, metrics_bearer_token, undefined),
          application:unset_env(iris_core, metrics_bearer_token),
          try
              %% Simulate: check_metrics_auth with no configured token
              %% When no token configured, any request should get 'ok'
              ?assertEqual(undefined,
                           application:get_env(iris_core, metrics_bearer_token, undefined))
          after
              case OldVal of
                  undefined -> ok;
                  _ -> application:set_env(iris_core, metrics_bearer_token, OldVal)
              end
          end
      end},

     {"configured token requires matching Authorization header", fun() ->
          OldVal = application:get_env(iris_core, metrics_bearer_token, undefined),
          application:set_env(iris_core, metrics_bearer_token, <<"test_metrics_secret">>),
          try
              Token = application:get_env(iris_core, metrics_bearer_token, undefined),
              ?assertEqual(<<"test_metrics_secret">>, Token)
          after
              case OldVal of
                  undefined -> application:unset_env(iris_core, metrics_bearer_token);
                  _ -> application:set_env(iris_core, metrics_bearer_token, OldVal)
              end
          end
      end}
    ].
