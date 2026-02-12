-module(iris_rpc_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_rpc.erl (TDD RED phase)
%% =============================================================================
%%
%% Tests cover:
%% - 6.3: RPC wrapper must increment metrics on call/cast
%%
%% =============================================================================

setup_metrics_table() ->
    Table = iris_metrics_table,
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end,
    ets:insert(Table, {rpc_calls_total, 0}),
    ets:insert(Table, {rpc_casts_total, 0}).

rpc_call_test_() ->
    [
     {"iris_rpc:call/4 increments rpc_calls_total metric", fun() ->
          setup_metrics_table(),
          _Result = iris_rpc:call(node(), erlang, node, []),
          [{_, Count}] = ets:lookup(iris_metrics_table, rpc_calls_total),
          ?assertEqual(1, Count)
      end},

     {"iris_rpc:call/4 delegates to rpc:call and returns correct result", fun() ->
          setup_metrics_table(),
          Expected = rpc:call(node(), erlang, node, []),
          Result = iris_rpc:call(node(), erlang, node, []),
          ?assertEqual(Expected, Result)
      end},

     {"iris_rpc:call/5 accepts timeout parameter", fun() ->
          setup_metrics_table(),
          Result = iris_rpc:call(node(), erlang, node, [], 5000),
          ?assertEqual(node(), Result)
      end},

     {"iris_rpc:cast/4 increments rpc_casts_total metric", fun() ->
          setup_metrics_table(),
          iris_rpc:cast(node(), erlang, node, []),
          [{_, Count}] = ets:lookup(iris_metrics_table, rpc_casts_total),
          ?assertEqual(1, Count)
      end}
    ].
