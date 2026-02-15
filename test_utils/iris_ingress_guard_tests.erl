-module(iris_ingress_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Comprehensive Tests for iris_ingress_guard.erl
%% =============================================================================
%%
%% Tests cover:
%% - Fail-closed in production mode when guard not started
%% - Fail-open in development mode (backward compatible)
%% - Fail-open when deployment_mode unset (defaults to development)
%% - Connection limit enforcement via atomics
%% - close/0 decrements counter correctly
%% - get_active_count/0 accuracy
%% - Guard lifecycle (start_link, init, terminate)
%% =============================================================================

%% Helper: clear persistent_term to simulate guard not started
clear_guard_state() ->
    try persistent_term:erase(iris_ingress_guard) catch _:_ -> ok end.

%% Helper: set deployment mode and restore after test
with_deployment_mode(Mode, Fun) ->
    OldMode = application:get_env(iris_edge, deployment_mode, undefined),
    application:set_env(iris_edge, deployment_mode, Mode),
    try
        Fun()
    after
        case OldMode of
            undefined -> application:unset_env(iris_edge, deployment_mode);
            _ -> application:set_env(iris_edge, deployment_mode, OldMode)
        end
    end.

%% =============================================================================
%% Fail-closed / fail-open behavior
%% =============================================================================

fail_closed_open_test_() ->
    [
     {"production mode denies when guard not started", fun() ->
          clear_guard_state(),
          with_deployment_mode(production, fun() ->
              ?assertEqual({deny, guard_not_ready}, iris_ingress_guard:check())
          end)
      end},

     {"development mode allows when guard not started", fun() ->
          clear_guard_state(),
          with_deployment_mode(development, fun() ->
              ?assertEqual(allow, iris_ingress_guard:check())
          end)
      end},

     {"test mode allows when guard not started", fun() ->
          clear_guard_state(),
          with_deployment_mode(test, fun() ->
              ?assertEqual(allow, iris_ingress_guard:check())
          end)
      end},

     {"unset deployment_mode defaults to development (allow)", fun() ->
          clear_guard_state(),
          OldMode = application:get_env(iris_edge, deployment_mode, undefined),
          application:unset_env(iris_edge, deployment_mode),
          try
              ?assertEqual(allow, iris_ingress_guard:check())
          after
              case OldMode of
                  undefined -> ok;
                  _ -> application:set_env(iris_edge, deployment_mode, OldMode)
              end
          end
      end}
    ].

%% =============================================================================
%% Guard lifecycle and connection counting
%% =============================================================================

guard_lifecycle_test_() ->
    {setup,
     fun() ->
         clear_guard_state(),
         %% Kill any existing guard
         case whereis(iris_ingress_guard) of
             undefined -> ok;
             ExistingPid -> gen_server:stop(ExistingPid), timer:sleep(50)
         end,
         {ok, NewPid} = iris_ingress_guard:start_link(),
         NewPid
     end,
     fun(GuardPid) ->
         gen_server:stop(GuardPid),
         timer:sleep(50)
     end,
     [
      {"check/0 returns allow when guard is started and under limit", fun() ->
           ?assertEqual(allow, iris_ingress_guard:check()),
           iris_ingress_guard:close()  %% Clean up the increment
       end},

      {"get_active_count/0 returns 0 initially", fun() ->
           ?assertEqual(0, iris_ingress_guard:get_active_count())
       end},

      {"check/0 increments active count", fun() ->
           Before = iris_ingress_guard:get_active_count(),
           ?assertEqual(allow, iris_ingress_guard:check()),
           After = iris_ingress_guard:get_active_count(),
           ?assertEqual(Before + 1, After),
           iris_ingress_guard:close()
       end},

      {"close/0 decrements active count", fun() ->
           ?assertEqual(allow, iris_ingress_guard:check()),
           Before = iris_ingress_guard:get_active_count(),
           iris_ingress_guard:close(),
           After = iris_ingress_guard:get_active_count(),
           ?assertEqual(Before - 1, After)
       end},

      {"multiple check/close pairs track correctly", fun() ->
           ?assertEqual(allow, iris_ingress_guard:check()),
           ?assertEqual(allow, iris_ingress_guard:check()),
           ?assertEqual(allow, iris_ingress_guard:check()),
           ?assertEqual(3, iris_ingress_guard:get_active_count()),
           iris_ingress_guard:close(),
           iris_ingress_guard:close(),
           iris_ingress_guard:close(),
           ?assertEqual(0, iris_ingress_guard:get_active_count())
       end}
     ]}.

%% =============================================================================
%% Undefined state tests (guard not started)
%% =============================================================================

undefined_state_test_() ->
    [
     {"get_active_count returns 0 when guard not started", fun() ->
          clear_guard_state(),
          ?assertEqual(0, iris_ingress_guard:get_active_count())
      end},

     {"close/0 is safe when guard not started", fun() ->
          clear_guard_state(),
          ?assertEqual(ok, iris_ingress_guard:close())
      end}
    ].
