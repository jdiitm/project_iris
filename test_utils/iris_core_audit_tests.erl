-module(iris_core_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_core.erl (TDD RED phase)
%% =============================================================================
%%
%% Tests cover:
%% - 4.3: Production mode must reject default cookie 'iris_secret'
%%
%% Note: erlang:set_cookie/2 fails on non-distributed nodes (nonode@nohost),
%% so we test via validate_production_cookie/1 which accepts the cookie atom.
%% =============================================================================

%% =============================================================================
%% 4.3: Cookie Enforcement
%% =============================================================================

cookie_enforcement_test_() ->
    [
     {"production mode rejects default cookie iris_secret", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          try
              ?assertEqual({error, default_cookie_in_production},
                           iris_core:validate_production_cookie(iris_secret))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end},

     {"development mode allows default cookie", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, development),
          try
              ?assertEqual(ok, iris_core:validate_production_cookie(iris_secret))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end},

     {"production mode allows non-default cookie", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          try
              ?assertEqual(ok,
                           iris_core:validate_production_cookie(secure_prod_cookie))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end}
    ].
