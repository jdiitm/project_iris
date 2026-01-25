-module(iris_offline_storage).
-export([store/3, store_batch/3, retrieve/2]).
-export([store_sync/3]).  %% Direct sync_transaction mode (for critical paths)
-export([store_durable/3]).  %% AUDIT FIX: Guaranteed durable - ACK only after persistence
%% PRINCIPAL_AUDIT_REPORT: Lockfree cursor-based retrieval (Hard Stop #2)
-export([retrieve_cursor/3, delete_confirmed/4, retrieve_lockfree/2, delete_all_async/2]).

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

%% =============================================================================
%% AUDIT FIX: Guaranteed Durable Store (RFC NFR-6, NFR-8)
%% =============================================================================
%% This function MUST be used when the caller needs to ACK to the client.
%% It guarantees:
%% 1. Message is written to Mnesia with sync_transaction
%% 2. Function returns ONLY after write is confirmed durable
%% 3. If this function returns 'ok', the message WILL survive any single node failure
%%
%% Use cases:
%% - Offline message storage before sending ACK to sender
%% - Any path where RPO=0 is required
%% =============================================================================
store_durable(User, Msg, Count) ->
    %% ALWAYS use sync_transaction path (bypass batcher)
    %% The batcher provides better latency but may ACK before Mnesia commit
    Result = store_sync(User, Msg, Count),
    case Result of
        ok ->
            %% Log at debug level for durability auditing
            logger:debug("Durable store confirmed for user ~p", [User]),
            ok;
        {error, Reason} ->
            %% CRITICAL: Do not ACK to client if this fails
            logger:error("DURABILITY FAILURE for ~p: ~p - DO NOT ACK", [User, Reason]),
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

%% =============================================================================
%% Cursor-Based Retrieval (Per PRINCIPAL_AUDIT_REPORT Hard Stop #2)
%% =============================================================================
%% This provides lockfree retrieval using dirty reads.
%% Messages are deleted async AFTER delivery is confirmed.
%% Usage pattern:
%%   {Msgs, Cursor} = retrieve_cursor(User, Count, 0),
%%   ... deliver Msgs to client ...
%%   ... on ACK: delete_cursor(User, Count, 0, Cursor) ...
%% =============================================================================

%% @doc Retrieve a batch of messages without global lock (dirty read)
%% Returns {Messages, NextCursor} where NextCursor is used for pagination.
%% Messages are NOT deleted - caller must confirm delivery then call delete_confirmed/3.
-spec retrieve_cursor(binary(), integer(), integer()) -> {list(), integer()}.
retrieve_cursor(User, Count, Cursor) ->
    %% Calculate batch range (e.g., buckets Cursor to Cursor+BatchSize)
    BatchSize = min(10, Count - Cursor),  %% Max 10 buckets per batch
    EndCursor = Cursor + BatchSize,
    
    %% Dirty read (lockfree) from buckets
    Msgs = lists:flatmap(fun(ID) ->
        Key = {User, ID},
        case mnesia:dirty_read(offline_msg, Key) of
            [] -> [];
            Records -> [Msg || {_, _, _, Msg} <- Records]
        end
    end, lists:seq(Cursor, EndCursor - 1)),
    
    NextCursor = if
        EndCursor >= Count -> done;
        true -> EndCursor
    end,
    
    {Msgs, NextCursor}.

%% @doc Delete messages after delivery is confirmed (async, fire-and-forget)
%% Call this AFTER client ACKs receipt of messages from retrieve_cursor.
-spec delete_confirmed(binary(), integer(), integer(), integer()) -> ok.
delete_confirmed(User, _Count, FromCursor, ToCursor) ->
    %% Spawn async delete to not block the caller
    spawn(fun() ->
        lists:foreach(fun(ID) ->
            Key = {User, ID},
            mnesia:dirty_delete(offline_msg, Key)
        end, lists:seq(FromCursor, ToCursor - 1))
    end),
    ok.

%% @doc Retrieve all messages using lockfree cursor-based approach
%% This is a convenience wrapper that retrieves everything without holding locks.
-spec retrieve_lockfree(binary(), integer()) -> list().
retrieve_lockfree(User, Count) ->
    %% Collect all messages using dirty reads
    AllMsgs = lists:flatmap(fun(ID) ->
        Key = {User, ID},
        case mnesia:dirty_read(offline_msg, Key) of
            [] -> [];
            Records -> Records
        end
    end, lists:seq(0, Count - 1)),
    
    %% Sort and extract (don't delete - let caller confirm first)
    sort_and_extract(AllMsgs).

%% @doc Delete all offline messages for a user (async, for cleanup)
-spec delete_all_async(binary(), integer()) -> ok.
delete_all_async(User, Count) ->
    spawn(fun() ->
        lists:foreach(fun(ID) ->
            mnesia:dirty_delete(offline_msg, {User, ID})
        end, lists:seq(0, Count - 1))
    end),
    ok.

sort_and_extract(Records) ->
    Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
    RawMsgs = [Msg || {_, _, _, Msg} <- Sorted],
    lists:flatten(RawMsgs).
