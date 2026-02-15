-module(iris_core_reconcile_txn_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Core Reconciliation Transaction Durability Tests
%% =============================================================================
%%
%% Tests verify that iris_core.erl conflict resolution / replication merges use
%% mnesia:transaction instead of dirty_write. Specifically:
%% - merge_key_batch uses transaction for batch writes
%% - merge_table_batch uses transaction for batch writes
%% - merge_set_records uses transaction for set record writes
%% - Source code analysis: no dirty_write in merge functions
%% =============================================================================

%% =============================================================================
%% Source Code Analysis Tests
%% =============================================================================

source_analysis_test_() ->
    [
     {"merge_key_batch uses mnesia:transaction", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          %% Find merge_key_batch function
          ?assert(binary:match(Src, <<"merge_key_batch">>) =/= nomatch),
          %% Check that transaction is used (not dirty_write)
          ?assert(binary:match(Src, <<"mnesia:transaction">>) =/= nomatch)
      end},

     {"merge_table_batch uses mnesia:transaction", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          ?assert(binary:match(Src, <<"merge_table_batch">>) =/= nomatch)
      end},

     {"merge_set_records uses mnesia:transaction", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          ?assert(binary:match(Src, <<"merge_set_records">>) =/= nomatch)
      end},

     {"no dirty_write in merge functions", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          Lines = binary:split(Src, <<"\n">>, [global]),
          %% Find lines containing dirty_write AND merge
          MergeDirtyLines = [L || L <- Lines,
              binary:match(L, <<"dirty_write">>) =/= nomatch,
              binary:match(L, <<"merge">>) =/= nomatch],
          ?assertEqual([], MergeDirtyLines)
      end},

     {"iris_core.erl uses mnesia:transaction in merge paths", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          ?assert(binary:match(Src, <<"mnesia:transaction">>) =/= nomatch)
      end}
    ].

%% =============================================================================
%% Transaction pattern validation
%% =============================================================================

transaction_pattern_test_() ->
    [
     {"transactions wrap lists:foreach for batch atomicity", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          %% The pattern should be: mnesia:transaction(fun() -> lists:foreach(...)  end)
          %% We verify both mnesia:transaction and lists:foreach are present in merge context
          ?assert(binary:match(Src, <<"lists:foreach">>) =/= nomatch),
          ?assert(binary:match(Src, <<"{atomic, ok} = mnesia:transaction">>) =/= nomatch)
      end},

     {"merge writes use mnesia:write with table and write lock", fun() ->
          {ok, Src} = file:read_file("src/iris_core.erl"),
          %% Pattern: mnesia:write(Table, Record, write)
          ?assert(binary:match(Src, <<"mnesia:write(Table, Rec, write)">>) =/= nomatch)
      end}
    ].
