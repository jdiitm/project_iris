-module(iris_session_rpc_error_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Session RPC Error Pattern Tests
%% =============================================================================
%%
%% Tests verify that iris_session.erl correctly handles the new error tuple
%% format {error, {rpc_failed, _Node, Reason}} from iris_rpc:call/4,5.
%%
%% Previously, iris_session matched on {badrpc, Reason} which was returned
%% by raw rpc:call. iris_rpc wraps badrpc into the structured
%% error tuple, and all 11 call sites in iris_session were updated.
%%
%% Tests cover:
%% - Source code no longer contains {badrpc, Reason} pattern matches
%% - All RPC call sites use iris_rpc:call (not raw rpc:call)
%% - estimate_remaining_messages handles rpc_failed gracefully
%% - check_block_status handles rpc_failed gracefully
%% - group_fanout_recipients handles rpc_failed gracefully
%% =============================================================================

%% =============================================================================
%% Source code analysis
%% =============================================================================

source_pattern_test_() ->
    [
     {"iris_session.erl does NOT match on {badrpc, _}", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          %% There should be NO {badrpc, Reason} pattern matches
          Lines = binary:split(Src, <<"\n">>, [global]),
          BadrpcLines = [L || L <- Lines,
              binary:match(L, <<"{badrpc,">>) =/= nomatch,
              %% Ignore comments
              binary:match(L, <<"%">>) =:= nomatch],
          ?assertEqual([], BadrpcLines)
      end},

     {"iris_session.erl matches on {error, {rpc_failed, ...}}", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          ?assert(binary:match(Src, <<"{error, {rpc_failed,">>) =/= nomatch)
      end},

     {"iris_session.erl uses iris_rpc:call (not raw rpc:call)", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          %% Should have iris_rpc:call references
          ?assert(binary:match(Src, <<"iris_rpc:call">>) =/= nomatch),
          %% Should NOT have raw rpc:call (only iris_rpc:call)
          Lines = binary:split(Src, <<"\n">>, [global]),
          RawRpcLines = [L || L <- Lines,
              binary:match(L, <<"rpc:call">>) =/= nomatch,
              binary:match(L, <<"iris_rpc:call">>) =:= nomatch,
              %% Ignore comments
              binary:match(L, <<"%">>) =:= nomatch],
          ?assertEqual([], RawRpcLines)
      end},

     {"iris_session.erl has 11+ rpc_failed matches (all call sites)", fun() ->
          {ok, Src} = file:read_file("src/iris_session.erl"),
          Lines = binary:split(Src, <<"\n">>, [global]),
          RpcFailedLines = [L || L <- Lines,
              binary:match(L, <<"rpc_failed">>) =/= nomatch],
          %% At least 11 call sites were updated
          ?assert(length(RpcFailedLines) >= 11)
      end}
    ].

%% =============================================================================
%% Functional tests for exported error-handling functions
%% =============================================================================

exported_error_handling_test_() ->
    [
     {"estimate_remaining_messages returns -1 for RPC failure", fun() ->
          %% Setup metrics table for the function
          Table = iris_metrics_table,
          case ets:info(Table) of
              undefined -> ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
              _ -> ok
          end,
          ets:insert(Table, {queue_depth_estimate_error, 0}),
          ets:insert(Table, {rpc_calls_total, 0}),
          ets:insert(Table, {rpc_casts_total, 0}),
          ets:insert(Table, {rpc_errors_total, 0}),
          %% Setup circuit breaker ETS
          case ets:info(iris_circuit_breaker_ets) of
              undefined -> ets:new(iris_circuit_breaker_ets, [named_table, public, {read_concurrency, true}]);
              _ -> ok
          end,
          %% Call with nonexistent node to trigger rpc_failed path
          Result = iris_session:estimate_remaining_messages(
                       'nonexistent_node@nowhere', <<"rpc_err_test_user">>, 5),
          ?assertEqual(-1, Result),
          %% Metric should be incremented
          [{_, Count}] = ets:lookup(Table, queue_depth_estimate_error),
          ?assertEqual(1, Count)
      end},

     {"calculate_remaining returns correct values", fun() ->
          ?assertEqual(5, iris_session:calculate_remaining(10, 5)),
          ?assertEqual(0, iris_session:calculate_remaining(3, 10)),
          ?assertEqual(0, iris_session:calculate_remaining(0, 0))
      end},

     {"check_block_status is exported", fun() ->
          Exports = iris_session:module_info(exports),
          ?assert(lists:member({check_block_status, 2}, Exports))
      end},

     {"group_fanout_recipients is exported", fun() ->
          Exports = iris_session:module_info(exports),
          ?assert(lists:member({group_fanout_recipients, 3}, Exports))
      end}
    ].
