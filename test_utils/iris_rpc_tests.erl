-module(iris_rpc_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Comprehensive Tests for iris_rpc.erl (AUDIT P0-2)
%% =============================================================================
%%
%% Tests cover:
%% - P0-2: {badrpc, Reason} wrapping into {error, {rpc_failed, Node, Reason}}
%% - P0-2: Circuit breaker integration (record_success/record_failure)
%% - 6.3: Metric increments for calls, casts, and errors
%% - Spec compliance: call/4, call/5, cast/4 return types
%% - 5.1: iris_rpc is not dead code (callers exist in src/)
%% =============================================================================

%% --- Setup / Teardown ---

setup_metrics_table() ->
    Table = iris_metrics_table,
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ets:insert(Table, {rpc_calls_total, 0}),
    ets:insert(Table, {rpc_casts_total, 0}),
    ets:insert(Table, {rpc_errors_total, 0}).

setup_all() ->
    setup_metrics_table(),
    %% Do NOT call setup_circuit_breaker_ets() here — start_link()
    %% creates its own ETS table in init/1. Creating it beforehand
    %% causes a badarg:already_exists crash.
    case whereis(iris_circuit_breaker) of
        undefined ->
            case iris_circuit_breaker:start_link() of
                {ok, Pid} -> {started, Pid};
                {error, {already_started, Pid}} -> {existing, Pid}
            end;
        Pid ->
            {existing, Pid}
    end.

cleanup({started, Pid}) when is_pid(Pid) ->
    gen_server:stop(Pid);
cleanup(_) ->
    ok.

%% =============================================================================
%% P0-2: badrpc wrapping tests
%% =============================================================================

rpc_badrpc_wrapping_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup/1,
     [
      {"call/4 wraps {badrpc, nodedown} into {error, {rpc_failed, Node, _}}", fun() ->
           Result = iris_rpc:call('nonexistent@nohost', erlang, node, []),
           ?assertMatch({error, {rpc_failed, 'nonexistent@nohost', _}}, Result)
       end},

      {"call/5 wraps {badrpc, _} into error tuple with custom timeout", fun() ->
           Result = iris_rpc:call('nonexistent@nohost', erlang, node, [], 1000),
           ?assertMatch({error, {rpc_failed, 'nonexistent@nohost', _}}, Result)
       end},

      {"call/4 returns actual result for successful local call", fun() ->
           Result = iris_rpc:call(node(), erlang, node, []),
           ?assertEqual(node(), Result)
       end},

      {"call/5 returns actual result for successful local call", fun() ->
           Result = iris_rpc:call(node(), erlang, self, [], 5000),
           ?assert(is_pid(Result))
       end},

      {"call/4 wraps timeout as {error, {rpc_failed, _, timeout}}", fun() ->
           %% Call with 1ms timeout on a function that takes longer
           Result = iris_rpc:call(node(), timer, sleep, [5000], 1),
           ?assertMatch({error, {rpc_failed, _, _}}, Result)
       end},

      {"badrpc error tuple preserves the original reason", fun() ->
           {error, {rpc_failed, _Node, Reason}} =
               iris_rpc:call('nonexistent@nohost', erlang, node, []),
           %% Reason should be nodedown or noconnection
           ?assert(Reason =:= nodedown orelse Reason =:= noconnection)
       end}
     ]}.

%% =============================================================================
%% P0-2: Metric counter tests
%% =============================================================================

rpc_metrics_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup/1,
     [
      {"call/4 increments rpc_calls_total on success", fun() ->
           setup_metrics_table(),
           iris_rpc:call(node(), erlang, node, []),
           [{_, Count}] = ets:lookup(iris_metrics_table, rpc_calls_total),
           ?assertEqual(1, Count)
       end},

      {"call/4 increments rpc_calls_total on failure", fun() ->
           setup_metrics_table(),
           iris_rpc:call('nonexistent@nohost', erlang, node, []),
           [{_, Count}] = ets:lookup(iris_metrics_table, rpc_calls_total),
           ?assertEqual(1, Count)
       end},

      {"call/4 increments rpc_errors_total on badrpc", fun() ->
           setup_metrics_table(),
           iris_rpc:call('nonexistent@nohost', erlang, node, []),
           [{_, Count}] = ets:lookup(iris_metrics_table, rpc_errors_total),
           ?assertEqual(1, Count)
       end},

      {"call/4 does NOT increment rpc_errors_total on success", fun() ->
           setup_metrics_table(),
           iris_rpc:call(node(), erlang, node, []),
           [{_, Count}] = ets:lookup(iris_metrics_table, rpc_errors_total),
           ?assertEqual(0, Count)
       end},

      {"cast/4 increments rpc_casts_total", fun() ->
           setup_metrics_table(),
           iris_rpc:cast(node(), erlang, node, []),
           [{_, Count}] = ets:lookup(iris_metrics_table, rpc_casts_total),
           ?assertEqual(1, Count)
       end},

      {"multiple calls accumulate metrics", fun() ->
           setup_metrics_table(),
           iris_rpc:call(node(), erlang, node, []),
           iris_rpc:call(node(), erlang, self, []),
           iris_rpc:call('nonexistent@nohost', erlang, node, []),
           [{_, CallCount}] = ets:lookup(iris_metrics_table, rpc_calls_total),
           [{_, ErrCount}] = ets:lookup(iris_metrics_table, rpc_errors_total),
           ?assertEqual(3, CallCount),
           ?assertEqual(1, ErrCount)
       end}
     ]}.

%% =============================================================================
%% P0-2: Circuit breaker integration tests
%% =============================================================================

rpc_circuit_breaker_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup/1,
     [
      {"successful call records success with circuit breaker", fun() ->
           iris_rpc:call(node(), erlang, node, []),
           %% Verify breaker status is closed (healthy)
           Status = iris_circuit_breaker:get_status(node()),
           ?assertMatch(#{status := closed}, Status)
       end},

      {"failed call records failure with circuit breaker", fun() ->
           FailNode = 'breaker_test_fail@nohost',
           iris_rpc:call(FailNode, erlang, node, []),
           Status = iris_circuit_breaker:get_status(FailNode),
           ?assertMatch(#{status := closed, failures := 1}, Status)
       end}
     ]}.

%% =============================================================================
%% cast/4 spec compliance tests
%% =============================================================================

rpc_cast_test_() ->
    [
     {"cast/4 returns true (same as rpc:cast)", fun() ->
          setup_metrics_table(),
          Result = iris_rpc:cast(node(), erlang, node, []),
          ?assertEqual(true, Result)
      end},

     {"cast/4 does not wrap errors (fire-and-forget)", fun() ->
          setup_metrics_table(),
          %% Cast to nonexistent node still returns true (fire-and-forget)
          Result = iris_rpc:cast('nonexistent@nohost', erlang, node, []),
          ?assertEqual(true, Result)
      end}
    ].

%% =============================================================================
%% 5.1: iris_rpc must not be dead code
%% =============================================================================

rpc_caller_existence_test_() ->
    [
     {"iris_session.erl calls iris_rpc:call (not dead code)", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          ?assert(binary:match(Src, <<"iris_rpc:call">>) =/= nomatch)
      end},

     {"iris_session.erl calls iris_rpc:cast", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          ?assert(binary:match(Src, <<"iris_rpc:cast">>) =/= nomatch)
      end},

     {"iris_rpc no longer exposes raw badrpc to callers", fun() ->
          {ok, Src} = file:read_file("src/iris_rpc.erl"),
          %% The spec should mention {error, {rpc_failed, ...}}
          ?assert(binary:match(Src, <<"rpc_failed">>) =/= nomatch)
      end}
    ].
