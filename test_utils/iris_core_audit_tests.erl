-module(iris_core_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_core.erl
%% =============================================================================
%%
%% Tests cover:
%% - 4.3: Production mode must reject default cookie 'iris_secret'
%% - 5.4: Bare spawns in init/1 must be replaced with supervised workers
%% - 4.2: CP consistency mode must be fatal in production
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

%% =============================================================================
%% 5.4: Supervised Cluster Join (no bare spawns in init/1)
%% =============================================================================

cluster_join_worker_test_() ->
    [
     {"iris_cluster_join_worker module exists and exports start_link", fun() ->
          Exports = iris_cluster_join_worker:module_info(exports),
          ?assert(lists:member({start_link, 1}, Exports))
      end},

     {"iris_cluster_join_worker is a gen_server", fun() ->
          Exports = iris_cluster_join_worker:module_info(exports),
          ?assert(lists:member({init, 1}, Exports)),
          ?assert(lists:member({handle_info, 2}, Exports))
      end},

     {"cluster join worker accepts cluster_join task", fun() ->
          %% Should not crash when started with cluster_join task
          %% (will just find no peers in unit test and log standalone mode)
          {ok, Pid} = iris_cluster_join_worker:start_link(cluster_join),
          ?assert(is_pid(Pid)),
          gen_server:stop(Pid)
      end},

     {"cluster join worker accepts region_wiring task", fun() ->
          {ok, Pid} = iris_cluster_join_worker:start_link(region_wiring),
          ?assert(is_pid(Pid)),
          gen_server:stop(Pid)
      end}
    ].

%% =============================================================================
%% 4.2: CP Consistency Mode Hard-Fail
%% =============================================================================

cp_mode_test_() ->
    [
     {"production mode rejects CP consistency mode", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          application:set_env(iris_core, consistency_mode, cp),
          try
              ?assertEqual({error, cp_not_implemented},
                           iris_core:validate_consistency_mode())
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end,
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end},

     {"development mode allows CP with warning", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, deployment_mode, development),
          application:set_env(iris_core, consistency_mode, cp),
          try
              ?assertEqual(ok, iris_core:validate_consistency_mode())
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end,
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end},

     {"hardened_ap mode always succeeds", fun() ->
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, consistency_mode, hardened_ap),
          try
              ?assertEqual(ok, iris_core:validate_consistency_mode())
          after
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end}
    ].
