-module(iris_offline_storage).
-export([store/3, store_batch/3, retrieve/2]).
-export([store_sync/3]).  %% Direct sync_transaction mode (for critical paths)

%% Mnesia table definition (created in iris_core:init_db/0):
%% {offline_msg, User, Timestamp, Msg}

%% =============================================================================
%% Store with Write-Ahead Log + Batched Sync Transaction
%% =============================================================================
%% This provides:
%% 1. Immediate durability via disk_log WAL (~1ms)
%% 2. Batched Mnesia sync_transaction (amortizes the ~3ms cost)
%% 3. Crash recovery via WAL replay
%% =============================================================================

store(User, Msg, Count) ->
    %% Use durable batcher if available, fallback to direct sync
    case whereis(iris_durable_batcher_1) of
        undefined ->
            %% Batcher not started - use direct sync_transaction
            store_sync(User, Msg, Count);
        _Pid ->
            %% Use WAL-backed batcher for optimal latency
            iris_durable_batcher:store(User, Msg, Count)
    end.

%% Direct sync_transaction mode - guaranteed durable but slower
store_sync(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    
    F = fun() ->
        mnesia:write({offline_msg, Key, Timestamp, Msg})
    end,
    
    %% CRITICAL: sync_transaction waits for replication to ALL disc_copies nodes
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} ->
            logger:error("Offline store failed for ~p: ~p", [User, Reason]),
            {error, Reason}
    end.

store_batch(User, Msgs, Count) ->
    %% Use durable batcher if available, fallback to direct sync
    case whereis(iris_durable_batcher_1) of
        undefined ->
            %% Batcher not started - use direct sync_transaction
            store_batch_sync(User, Msgs, Count);
        _Pid ->
            %% Use WAL-backed batcher for optimal latency
            iris_durable_batcher:store_batch(User, Msgs, Count, #{})
    end.

%% Direct sync_transaction mode for batch - guaranteed durable but slower
store_batch_sync(User, Msgs, Count) ->
    Timestamp = os:system_time(millisecond),
    %% Group messages by Bucket
    BucketedMsgs = lists:foldl(fun(Msg, Acc) ->
        Bucket = erlang:phash2(Msg, Count),
        orddict:append(Bucket, Msg, Acc)
    end, orddict:new(), Msgs),
    
    F = fun() ->
        lists:foreach(fun({Bucket, Batch}) ->
             Key = {User, Bucket},
             mnesia:write({offline_msg, Key, Timestamp, Batch})
        end, orddict:to_list(BucketedMsgs))
    end,
    
    %% CRITICAL: sync_transaction for durability
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} ->
            logger:error("Offline batch store failed for ~p: ~p", [User, Reason]),
            {error, Reason}
    end.

retrieve(User, Count) ->
    %% Read messages from all buckets
    F = fun() ->
        %% Iterate all buckets 0..Count-1
        Lists = lists:map(fun(ID) ->
            Key = {User, ID},
            Msgs = mnesia:read(offline_msg, Key, write),
            mnesia:delete({offline_msg, Key}),
            Msgs
        end, lists:seq(0, Count - 1)),
        lists:append(Lists)
    end,
    
    case mnesia:activity(transaction, F) of
        {atomic, Records} ->
            sort_and_extract(Records);
        Records when is_list(Records) ->
            sort_and_extract(Records);
        Error ->
            logger:error("Error retrieving offline msgs: ~p", [Error]),
            []
    end.

sort_and_extract(Records) ->
    Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
    RawMsgs = [Msg || {_, _, _, Msg} <- Sorted],
    lists:flatten(RawMsgs).
