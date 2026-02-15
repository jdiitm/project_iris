-module(iris_session_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_session.erl
%% =============================================================================
%%
%% Tests cover:
%% - 4.4: Queue depth estimation error must be observable (metric + log)
%% - 3.1/6.3: User block enforcement in send path
%% =============================================================================

%% =============================================================================
%% 4.4: Queue Depth Error Observability
%% =============================================================================

queue_depth_error_metric_test_() ->
    [
     {"estimate_remaining returns -1 and increments metric on error", fun() ->
          %% Ensure metrics ETS table exists for this test
          Table = iris_metrics_table,
          case ets:info(Table) of
              undefined -> ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
              _ -> ok
          end,
          %% Reset the metric counter
          ets:insert(Table, {queue_depth_estimate_error, 0}),
          %% Call with non-existent node to trigger the badrpc path
          Result = iris_session:estimate_remaining_messages(
                       'nonexistent_node@nowhere', <<"test_user">>, 5),
          ?assertEqual(-1, Result),
          [{_, After}] = ets:lookup(Table, queue_depth_estimate_error),
          ?assertEqual(1, After)
      end},

     {"estimate_remaining returns correct value on success", fun() ->
          %% When Depth is an integer and NextCursor is an integer,
          %% the result should be max(0, Depth - NextCursor)
          %% We can't easily test the success path with a real RPC,
          %% but we can test calculate_remaining/2 directly
          ?assertEqual(5, iris_session:calculate_remaining(10, 5)),
          ?assertEqual(0, iris_session:calculate_remaining(3, 10)),
          ?assertEqual(0, iris_session:calculate_remaining(5, 5))
      end}
    ].

%% =============================================================================
%% 3.1/6.3: User Block Enforcement
%% =============================================================================

block_enforcement_test_() ->
    [
     {"check_block_status returns ok when user_blocks table absent (feature not deployed)", fun() ->
          %% When the user_blocks Mnesia table doesn't exist, the blocking feature
          %% is not deployed. No blocks exist to enforce, so messages are allowed.
          %% Fail-closed only applies to transient failures of a deployed feature.
          Result = iris_session:check_block_status(<<"sender">>, <<"recipient">>),
          ?assertEqual(ok, Result)
      end},

     {"check_block_status function is exported", fun() ->
          Exports = iris_session:module_info(exports),
          ?assert(lists:member({check_block_status, 2}, Exports))
      end}
    ].

%% =============================================================================
%% H-2 AUDIT: Empty username must be rejected at login
%% =============================================================================
%% RFC does not define behavior for empty usernames. Accepting them creates
%% phantom entries in ETS/Mnesia keyed by <<>> which corrupt data structures.

empty_username_login_test_() ->
    {setup,
     fun() ->
         %% Create required ETS tables
         case ets:info(local_presence_v2) of
             undefined -> ets:new(local_presence_v2, [named_table, public, set]);
             _ -> ets:delete_all_objects(local_presence_v2)
         end,
         case ets:info(presence_cache) of
             undefined -> ets:new(presence_cache, [named_table, public, set]);
             _ -> ets:delete_all_objects(presence_cache)
         end,
         %% Ensure iris_auth_failed_logins ETS exists for check_login_rate
         case ets:info(iris_auth_failed_logins) of
             undefined ->
                 ets:new(iris_auth_failed_logins, [set, named_table, public,
                     {read_concurrency, true}, {write_concurrency, true}]);
             _ -> ok
         end,
         ok
     end,
     fun(_) ->
         catch ets:delete_all_objects(local_presence_v2),
         catch ets:delete_all_objects(presence_cache),
         ok
     end,
     [
      {"Login with empty username is rejected", fun() ->
           %% Empty login data -> parse_login_data returns {<<>>, undefined}
           %% After fix: must return an error/close the connection
           Result = iris_session:handle_packet({login, <<>>}, undefined, self(), tcp),
           %% Should get a close action (connection rejected)
           case Result of
               {ok, _User, Actions} ->
                   ?assert(lists:member(close, Actions));
               {error, _} ->
                   ?assert(true)
           end
       end}
     ]}.

%% =============================================================================
%% H-3 AUDIT: authenticate must fail-closed when iris_auth is down
%% =============================================================================
%% If iris_auth gen_server is not running but auth is enabled, authenticate/2
%% must return {error, auth_unavailable}, not ok.

auth_bypass_when_down_test_() ->
    {setup,
     fun() ->
         %% Ensure iris_auth is NOT running
         case whereis(iris_auth) of
             undefined -> ok;
             Pid -> gen_server:stop(Pid)
         end,
         %% But auth IS enabled
         application:set_env(iris_edge, auth_enabled, true),
         %% Ensure ETS tables exist
         case ets:info(local_presence_v2) of
             undefined -> ets:new(local_presence_v2, [named_table, public, set]);
             _ -> ets:delete_all_objects(local_presence_v2)
         end,
         case ets:info(presence_cache) of
             undefined -> ets:new(presence_cache, [named_table, public, set]);
             _ -> ets:delete_all_objects(presence_cache)
         end,
         case ets:info(iris_auth_failed_logins) of
             undefined ->
                 ets:new(iris_auth_failed_logins, [set, named_table, public,
                     {read_concurrency, true}, {write_concurrency, true}]);
             _ -> ok
         end,
         ok
     end,
     fun(_) ->
         application:unset_env(iris_edge, auth_enabled),
         catch ets:delete_all_objects(local_presence_v2),
         catch ets:delete_all_objects(presence_cache),
         ok
     end,
     [
      {"Auth bypass returns error when iris_auth is down and auth enabled", fun() ->
           %% iris_auth is not registered, auth_enabled is true
           ?assertEqual(undefined, whereis(iris_auth)),
           %% Try to login with a token
           Result = iris_session:handle_packet({login, <<"user:fake_token">>}, undefined, self(), tcp),
           %% Should NOT successfully login -- must get error/close
           case Result of
               {ok, User, Actions} ->
                   %% If it returned a user, it must also close (auth failed)
                   ?assert(lists:member(close, Actions) orelse User =:= undefined);
               {error, _} ->
                   ?assert(true)
           end
       end}
     ]}.
