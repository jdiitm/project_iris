-module(iris_offline_storage).
-export([store/3, store_batch/3, retrieve/2]).
-export([store_sync/3]).  %% Direct sync_transaction mode (for critical paths)
-export([store_durable/3]).  %% AUDIT FIX: Guaranteed durable - ACK only after persistence
-export([store_with_seq/3]).  %% AUDIT FIX: Store with client-provided sequence for FIFO
%% PRINCIPAL_AUDIT_REPORT: Lockfree cursor-based retrieval (Hard Stop #2)
-export([retrieve_cursor/3, delete_confirmed/4, retrieve_lockfree/2, delete_all_async/2]).
-export([notify_push/2]).  %% AUDIT M12: Push notification hook

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
    %% AUDIT V2 P0-2: Backpressure — reject writes when Mnesia memory exceeds threshold
    case check_memory_backpressure() of
        ok ->
            %% Use durable batcher if available, fallback to direct sync
            case whereis(iris_durable_batcher_1) of
                undefined ->
                    %% Batcher not started - use direct sync_transaction
                    store_sync(User, Msg, Count);
                _Pid ->
                    %% Use WAL-backed batcher for optimal latency
                    iris_durable_batcher:store(User, Msg, Count)
            end;
        {error, memory_pressure} ->
            {error, memory_pressure}
    end.

%% Direct sync_transaction mode - guaranteed durable but slower
store_sync(User, Msg, Count) ->
    %% CRITICAL: Use HLC for proper message ordering (RFC FR-5)
    %% os:system_time(millisecond) has insufficient precision for rapid-fire messages
    Timestamp = case whereis(iris_hlc) of
        undefined ->
            %% Fallback to nanoseconds if HLC not started
            os:system_time(nanosecond);
        _Pid ->
            %% Use HLC for guaranteed total ordering
            iris_hlc:to_integer(iris_hlc:send())
    end,
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
    %% AUDIT V2 P0-2: Backpressure check before durable write
    case check_memory_backpressure() of
        {error, memory_pressure} ->
            {error, memory_pressure};
        ok ->
            do_store_durable(User, Msg, Count)
    end.

do_store_durable(User, Msg, Count) ->
    %% ALWAYS use sync_transaction path (bypass batcher)
    %% The batcher provides better latency but may ACK before Mnesia commit
    Result = store_sync(User, Msg, Count),
    case Result of
        ok ->
            %% Log at debug level for durability auditing
            logger:debug("Durable store confirmed for user ~p", [User]),
            %% AUDIT M12: Invoke push notification hook for offline users
            notify_push(User, Msg),
            ok;
        {error, Reason} ->
            %% CRITICAL: Do not ACK to client if this fails
            logger:error("DURABILITY FAILURE for ~p: ~p - DO NOT ACK", [User, Reason]),
            {error, Reason}
    end.

%% =============================================================================
%% AUDIT FIX: Store with client-provided sequence number (RFC FR-5)
%% =============================================================================
%% This function uses the client-provided sequence number as the timestamp,
%% guaranteeing FIFO ordering regardless of parallel processing or clock drift.
%% =============================================================================
store_with_seq(User, Msg, SeqNo) ->
    %% Use sequence number directly as timestamp (guaranteed ordering)
    Timestamp = SeqNo,
    BucketID = 0,  %% Single bucket for sequenced messages to preserve order
    Key = {User, BucketID},
    
    F = fun() ->
        mnesia:write({offline_msg, Key, Timestamp, Msg})
    end,
    
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} ->
            logger:error("Sequenced store failed for ~p: ~p", [User, Reason]),
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
    %% CRITICAL: Use HLC for proper message ordering (RFC FR-5)
    Timestamp = case whereis(iris_hlc) of
        undefined ->
            os:system_time(nanosecond);
        _Pid ->
            iris_hlc:to_integer(iris_hlc:send())
    end,
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
    %% AUDIT FIX 2.4: Deprecation metric — use retrieve_cursor/3 instead.
    iris_metrics:inc(offline_retrieve_deprecated_calls),
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
%% RFC FR-5: Messages are sorted by Timestamp to guarantee FIFO ordering even when
%% messages are spread across multiple buckets due to content-based hashing.
-spec retrieve_cursor(binary(), integer(), integer()) -> {list(), integer()}.
retrieve_cursor(User, Count, Cursor) ->
    %% Calculate batch range (e.g., buckets Cursor to Cursor+BatchSize)
    BatchSize = min(10, Count - Cursor),  %% Max 10 buckets per batch
    EndCursor = Cursor + BatchSize,
    
    %% Dirty read (lockfree) from buckets - keep full records for sorting
    Records = lists:flatmap(fun(ID) ->
        Key = {User, ID},
        case mnesia:dirty_read(offline_msg, Key) of
            [] -> [];
            Recs -> Recs
        end
    end, lists:seq(Cursor, EndCursor - 1)),
    
    %% RFC FR-5 FIX: Sort by Timestamp (3rd element) to ensure FIFO ordering
    %% This is critical because messages are bucketed by content hash, not arrival order.
    %% Without sorting, messages would be returned in bucket-ID order, breaking FIFO.
    Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
    Msgs = [Msg || {_, _, _, Msg} <- Sorted],
    
    NextCursor = if
        EndCursor >= Count -> done;
        true -> EndCursor
    end,
    
    {Msgs, NextCursor}.

%% @doc Delete messages after delivery is confirmed (async, fire-and-forget)
%% Call this AFTER client ACKs receipt of messages from retrieve_cursor.
-spec delete_confirmed(binary(), integer(), integer(), integer()) -> ok.
delete_confirmed(User, _Count, FromCursor, ToCursor) ->
    %% B-3 AUDIT MITIGATION: Monitored spawn for async delete
    iris_async:spawn_monitored(offline_msg_delete, fun() ->
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
    %% B-3 AUDIT MITIGATION: Monitored spawn for async cleanup
    iris_async:spawn_monitored(offline_msg_cleanup, fun() ->
        lists:foreach(fun(ID) ->
            mnesia:dirty_delete(offline_msg, {User, ID})
        end, lists:seq(0, Count - 1))
    end),
    ok.

sort_and_extract(Records) ->
    %% Sort by timestamp (SeqNo) for FIFO ordering (RFC FR-5)
    Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
    RawMsgs = [Msg || {_, _, _, Msg} <- Sorted],
    lists:flatten(RawMsgs).

%% =============================================================================
%% AUDIT M12: Push Notification Hook
%% =============================================================================
%% Configurable hook for push notifications (APNS/FCM).
%% Default is no-op. Configure via:
%%   application:set_env(iris_core, push_hook, fun(User, Msg) -> ... end)
%% or implement a module with push_notify/2 and set:
%%   application:set_env(iris_core, push_hook, {Module, Function})
%% =============================================================================

%% =============================================================================
%% AUDIT V2 P0-2: Memory Backpressure Check
%% =============================================================================
%% Rejects offline message writes when Mnesia memory exceeds the configured
%% alarm threshold. This prevents OOM crashes from unbounded message growth.

check_memory_backpressure() ->
    try iris_mnesia_guard:is_memory_ok() of
        ok -> ok;
        {error, memory_pressure} ->
            try iris_metrics:inc(offline_store_backpressure_rejects)
            catch C1:R1 ->
                logger:warning("~p: metrics inc(backpressure_rejects) failed ~p:~p", [?MODULE, C1, R1]),
                ok
            end,
            logger:warning("Offline store rejected: Mnesia memory pressure"),
            {error, memory_pressure}
    catch
        _:_ ->
            %% Guard module not available — permissive
            ok
    end.

-spec notify_push(binary(), binary()) -> ok.
notify_push(User, Msg) ->
    case application:get_env(iris_core, push_hook, undefined) of
        undefined ->
            ok;  %% No push hook configured — no-op
        {Module, Function} ->
            try Module:Function(User, Msg)
            catch Class:Error ->
                logger:warning("Push notification hook failed: ~p:~p", [Class, Error])
            end,
            ok;
        Fun when is_function(Fun, 2) ->
            try Fun(User, Msg)
            catch Class:Error ->
                logger:warning("Push notification hook failed: ~p:~p", [Class, Error])
            end,
            ok
    end.
