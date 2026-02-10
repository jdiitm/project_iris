-module(iris_reconciliation_batch_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% G-2: Reconciliation OOM Prevention Tests (RFC Section 7.1.1)
%%
%% The current merge_offline_msg_from/1 uses rpc:call(Node, mnesia,
%% dirty_match_object, [{offline_msg, '_', '_', '_'}]) which fetches ALL
%% offline messages into the caller's RAM in one shot. With 1M+ messages,
%% this causes OOM.
%%
%% RED: iris_core must export reconcile_batch/2 for cursor-based iteration.
%%      The current code only exports reconcile_after_partition/0.
%% GREEN: Implement cursor-based batched reconciliation.
%% =============================================================================

iris_reconciliation_batch_test_() ->
    [
     {"reconcile_batch/2 is exported for cursor-based reconciliation",
      fun test_reconcile_batch_exported/0}
    ].

test_reconcile_batch_exported() ->
    %% iris_core must export reconcile_batch/2 which accepts a RemoteNode
    %% and a batch size, and performs cursor-based iteration instead of
    %% fetching all records at once.
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({reconcile_batch, 2}, Exports)).
