-module(iris_session_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_session.erl (TDD RED phase)
%% =============================================================================
%%
%% Tests cover:
%% - 4.4: Queue depth estimation error must be observable (metric + log)
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
