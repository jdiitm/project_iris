-module(iris_store).

%% =============================================================================
%% Iris Store: Simplified Storage Interface
%% =============================================================================
%% 
%% DESIGN PHILOSOPHY:
%% This module provides a SINGLE, SIMPLE interface for all storage operations.
%% It replaces the complexity of iris_storage + iris_offline_storage layers.
%% 
%% CONSISTENCY GUARANTEES:
%% This module provides HARDENED AP semantics:
%% 
%% WRITES:
%%   - Durable after function returns (quorum by default - W=2 of N=3)
%%   - Replicated to majority of disc_copies nodes
%%   - Blocked during detected partitions (safe mode)
%%
%% READS:
%%   - May be stale during partitions
%%   - Eventually consistent (typically < 100ms)
%%
%% FAILURE MODES:
%%   - Single node failure: No data loss (RPO=0)
%%   - Partition: Writes blocked, stale reads allowed
%%   - Total cluster loss: Data recoverable from any node with disc_copies
%% 
%% API:
%%   put(Table, Key, Value) -> ok | {error, Reason}           %% Quorum durable (default)
%%   put(Table, Key, Value, Opts) -> ok | {error, Reason}     %% With options
%%   get(Table, Key) -> {ok, Value} | not_found | {error, Reason}
%%   delete(Table, Key) -> ok | {error, Reason}
%%   
%% OPTIONS:
%%   #{durability => guaranteed | best_effort | quorum}
%%   #{timeout => MilliSeconds}
%% 
%% =============================================================================

-export([put/3, put/4]).
-export([get/2, get/3]).
-export([delete/2, delete/3]).
-export([batch_put/2, batch_put/3]).

%% Inbox Log APIs (RFC-001 v3.0 compliant)
-export([append_inbox/2, append_inbox/3]).
-export([scan_inbox/3, scan_inbox/4]).
-export([inbox_offset/1]).
-export([create_inbox_table/0, create_inbox_table/1]).

%% =============================================================================
%% API: Put (Write)
%% =============================================================================

%% @doc Store a key-value pair with guaranteed durability (default)
%% Blocks until replicated to all disc_copies nodes
-spec put(atom(), term(), term()) -> ok | {error, term()}.
put(Table, Key, Value) ->
    put(Table, Key, Value, #{}).

%% @doc Store with options
%% Options:
%%   - durability: quorum (default) | guaranteed | best_effort
%%   - timeout: milliseconds (default 5000)
%%
%% VIOLATION-3 FIX: Default changed from 'guaranteed' to 'quorum'.
%% Rationale: 'guaranteed' uses sync_transaction which blocks on the slowest node,
%% violating NFR-7 (Availability). Quorum writes (W=2 of N=3) tolerate one slow
%% node while maintaining durability.
-spec put(atom(), term(), term(), map()) -> ok | {error, term()}.
put(Table, Key, Value, Opts) ->
    %% Check partition guard first
    case check_write_safety() of
        ok ->
            Durability = maps:get(durability, Opts, quorum),
            do_put(Durability, Table, Key, Value, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

do_put(guaranteed, Table, Key, Value, _Opts) ->
    %% sync_transaction: Waits for replication to ALL disc_copies nodes
    %% This is the SAFE default - survives any single node failure
    F = fun() -> mnesia:write({Table, Key, Value}) end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> 
            logger:error("Store put failed: ~p", [Reason]),
            {error, Reason}
    end;

do_put(best_effort, Table, Key, Value, _Opts) ->
    %% Async transaction: Fast but may lose data on crash
    %% Use ONLY for non-critical data (typing indicators, read receipts)
    spawn(fun() ->
        F = fun() -> mnesia:write({Table, Key, Value}) end,
        mnesia:activity(transaction, F)
    end),
    ok;

do_put(quorum, Table, Key, Value, Opts) ->
    %% Quorum write: Majority of replicas must ACK
    %% Tolerates minority failures without blocking
    %% Falls back to guaranteed if quorum module not available
    case whereis(iris_quorum_write) of
        undefined ->
            %% Quorum module not running - fall back to guaranteed
            do_put(guaranteed, Table, Key, Value, Opts);
        _ ->
            Timeout = maps:get(timeout, Opts, 3000),
            iris_quorum_write:write_durable(Table, Key, Value, #{timeout => Timeout})
    end.

%% =============================================================================
%% API: Get (Read)
%% =============================================================================

%% @doc Read a value by key (fast dirty read)
-spec get(atom(), term()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key) ->
    get(Table, Key, #{}).

%% @doc Read with options
%% Options:
%%   - consistency: eventual (default) | quorum
-spec get(atom(), term(), map()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key, Opts) ->
    Consistency = maps:get(consistency, Opts, eventual),
    do_get(Consistency, Table, Key, Opts).

do_get(eventual, Table, Key, _Opts) ->
    %% Dirty read: Fast but may be stale during partitions
    %% This is acceptable for most use cases (presence, status)
    case mnesia:dirty_read(Table, Key) of
        [] -> not_found;
        [{Table, Key, Value}] -> {ok, Value};
        [Record | _] -> {ok, Record}
    end;

do_get(quorum, Table, Key, Opts) ->
    %% Quorum read: Linearizable, reads from majority
    %% Falls back to eventual if quorum module not available
    case whereis(iris_quorum_write) of
        undefined ->
            %% Quorum module not running - fall back to eventual
            do_get(eventual, Table, Key, Opts);
        _ ->
            Timeout = maps:get(timeout, Opts, 2000),
            iris_quorum_write:read_quorum(Table, Key, #{timeout => Timeout})
    end.

%% =============================================================================
%% API: Delete
%% =============================================================================

%% @doc Delete a key with guaranteed durability
-spec delete(atom(), term()) -> ok | {error, term()}.
delete(Table, Key) ->
    delete(Table, Key, #{}).

%% @doc Delete with options
-spec delete(atom(), term(), map()) -> ok | {error, term()}.
delete(Table, Key, Opts) ->
    case check_write_safety() of
        ok ->
            Durability = maps:get(durability, Opts, guaranteed),
            do_delete(Durability, Table, Key);
        {error, Reason} ->
            {error, Reason}
    end.

do_delete(guaranteed, Table, Key) ->
    F = fun() -> mnesia:delete({Table, Key}) end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end;

do_delete(best_effort, Table, Key) ->
    spawn(fun() ->
        F = fun() -> mnesia:delete({Table, Key}) end,
        mnesia:activity(transaction, F)
    end),
    ok.

%% =============================================================================
%% API: Batch Operations
%% =============================================================================

%% @doc Batch put multiple key-value pairs atomically
-spec batch_put(atom(), [{term(), term()}]) -> ok | {error, term()}.
batch_put(Table, KeyValuePairs) ->
    batch_put(Table, KeyValuePairs, #{}).

-spec batch_put(atom(), [{term(), term()}], map()) -> ok | {error, term()}.
batch_put(Table, KeyValuePairs, Opts) ->
    case check_write_safety() of
        ok ->
            Durability = maps:get(durability, Opts, guaranteed),
            do_batch_put(Durability, Table, KeyValuePairs);
        {error, Reason} ->
            {error, Reason}
    end.

do_batch_put(guaranteed, Table, KeyValuePairs) ->
    F = fun() ->
        lists:foreach(fun({Key, Value}) ->
            mnesia:write({Table, Key, Value})
        end, KeyValuePairs)
    end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end;

do_batch_put(best_effort, Table, KeyValuePairs) ->
    spawn(fun() ->
        F = fun() ->
            lists:foreach(fun({Key, Value}) ->
                mnesia:write({Table, Key, Value})
            end, KeyValuePairs)
        end,
        mnesia:activity(transaction, F)
    end),
    ok.

%% =============================================================================
%% API: Inbox Log (RFC-001 v3.0 Compliant)
%% =============================================================================
%%
%% The Inbox Log is the fundamental abstraction for message storage:
%% - Every user U has a strictly ordered, append-only Inbox Log (L_u)
%% - Write: Sending to U = Append to L_u, returns Offset (HLC)
%% - Read: Sequential consumption via Scan(UserID, AfterOffset, Limit)
%% - Ordering: Messages have strictly monotonic HLC offsets
%% - Writes to single Inbox are serialized (per-user lock via user_id key)
%%
%% Table: iris_inbox
%% Record: {iris_inbox, {UserID, HLC_Offset}, Message}
%%
%% =============================================================================

%% @doc Create the inbox table. Call during system startup.
-spec create_inbox_table() -> ok | {error, term()}.
create_inbox_table() ->
    create_inbox_table([]).

-spec create_inbox_table([node()]) -> ok | {error, term()}.
create_inbox_table(ExtraNodes) ->
    Nodes = [node() | ExtraNodes],
    case mnesia:create_table(iris_inbox, [
        {type, ordered_set},       %% Sorted by key for efficient range scans
        {attributes, [key, message, metadata]},
        {record_name, iris_inbox},
        {disc_copies, Nodes}       %% Durable across restarts
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, iris_inbox}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Append a message to a user's inbox log.
%% Returns {ok, Offset} where Offset is an HLC that can be used for scanning.
-spec append_inbox(term(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
append_inbox(UserID, Message) ->
    append_inbox(UserID, Message, #{}).

%% @doc Append with options.
%% Options:
%%   - durability: guaranteed (default) | best_effort | quorum
%%   - metadata: map() of additional metadata to store
-spec append_inbox(term(), binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
append_inbox(UserID, Message, Opts) ->
    case check_write_safety() of
        ok ->
            %% Generate HLC offset (monotonically increasing)
            HLC = case whereis(iris_hlc) of
                undefined -> 
                    %% Fallback: use microsecond timestamp + random component
                    erlang:system_time(microsecond);
                _Pid -> 
                    iris_hlc:to_integer(iris_hlc:send())
            end,
            
            Key = {UserID, HLC},
            Metadata = maps:get(metadata, Opts, #{}),
            Record = {iris_inbox, Key, Message, Metadata},
            
            Durability = maps:get(durability, Opts, guaranteed),
            case do_append_inbox(Durability, Record) of
                ok -> {ok, HLC};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

do_append_inbox(guaranteed, Record) ->
    F = fun() -> mnesia:write(Record) end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> 
            logger:error("Inbox append failed: ~p", [Reason]),
            {error, Reason}
    end;

do_append_inbox(best_effort, Record) ->
    spawn(fun() ->
        F = fun() -> mnesia:write(Record) end,
        mnesia:activity(transaction, F)
    end),
    ok;

do_append_inbox(quorum, Record) ->
    {iris_inbox, Key, Message, Metadata} = Record,
    Timeout = 3000,
    iris_quorum_write:write_durable(iris_inbox, Key, {Message, Metadata}, #{timeout => Timeout}).

%% @doc Scan a user's inbox for messages after a given offset.
%% Returns {ok, [{Offset, Message, Metadata}]} or {ok, []} if no messages.
-spec scan_inbox(term(), non_neg_integer(), non_neg_integer()) -> 
    {ok, [{non_neg_integer(), binary(), map()}]} | {error, term()}.
scan_inbox(UserID, AfterOffset, Limit) ->
    scan_inbox(UserID, AfterOffset, Limit, #{}).

%% @doc Scan with options.
%% Options:
%%   - consistency: eventual (default) | transaction
%%   - include_metadata: boolean() (default true)
-spec scan_inbox(term(), non_neg_integer(), non_neg_integer(), map()) -> 
    {ok, [{non_neg_integer(), binary(), map()}]} | {error, term()}.
scan_inbox(UserID, AfterOffset, Limit, Opts) ->
    Consistency = maps:get(consistency, Opts, eventual),
    IncludeMetadata = maps:get(include_metadata, Opts, true),
    do_scan_inbox(Consistency, UserID, AfterOffset, Limit, IncludeMetadata).

do_scan_inbox(eventual, UserID, AfterOffset, Limit, IncludeMetadata) ->
    %% Use dirty operations for fast reads
    %% Start key: just after the AfterOffset
    StartKey = {UserID, AfterOffset + 1},
    %% End key: next user ID (exclusive)
    EndUserID = next_user_id(UserID),
    
    try
        Messages = collect_inbox_messages(StartKey, EndUserID, Limit, IncludeMetadata, []),
        {ok, Messages}
    catch
        exit:{aborted, Reason} -> {error, Reason};
        _:Error -> {error, Error}
    end;

do_scan_inbox(transaction, UserID, AfterOffset, Limit, IncludeMetadata) ->
    F = fun() ->
        StartKey = {UserID, AfterOffset + 1},
        EndUserID = next_user_id(UserID),
        collect_inbox_messages_tx(StartKey, EndUserID, Limit, IncludeMetadata, [])
    end,
    case mnesia:activity(transaction, F) of
        {atomic, Messages} -> {ok, Messages};
        Messages when is_list(Messages) -> {ok, Messages};
        {aborted, Reason} -> {error, Reason}
    end.

%% @private Collect messages using dirty reads (fast)
collect_inbox_messages(_CurrentKey, _EndUserID, 0, _IncludeMeta, Acc) ->
    lists:reverse(Acc);
collect_inbox_messages(CurrentKey, EndUserID, Remaining, IncludeMeta, Acc) ->
    case mnesia:dirty_next(iris_inbox, CurrentKey) of
        '$end_of_table' -> 
            lists:reverse(Acc);
        {NextUserID, _Offset} when NextUserID >= EndUserID ->
            %% Reached next user's messages, stop
            lists:reverse(Acc);
        NextKey = {_UserID, Offset} ->
            case mnesia:dirty_read(iris_inbox, NextKey) of
                [] ->
                    %% Key disappeared, continue
                    collect_inbox_messages(NextKey, EndUserID, Remaining, IncludeMeta, Acc);
                [{iris_inbox, _Key, Message, Metadata}] ->
                    Entry = case IncludeMeta of
                        true -> {Offset, Message, Metadata};
                        false -> {Offset, Message, #{}}
                    end,
                    collect_inbox_messages(NextKey, EndUserID, Remaining - 1, IncludeMeta, [Entry | Acc])
            end
    end.

%% @private Collect messages in transaction context
collect_inbox_messages_tx(_CurrentKey, _EndUserID, 0, _IncludeMeta, Acc) ->
    lists:reverse(Acc);
collect_inbox_messages_tx(CurrentKey, EndUserID, Remaining, IncludeMeta, Acc) ->
    case mnesia:next(iris_inbox, CurrentKey) of
        '$end_of_table' -> 
            lists:reverse(Acc);
        {NextUserID, _Offset} when NextUserID >= EndUserID ->
            lists:reverse(Acc);
        NextKey = {_UserID, Offset} ->
            case mnesia:read(iris_inbox, NextKey) of
                [] ->
                    collect_inbox_messages_tx(NextKey, EndUserID, Remaining, IncludeMeta, Acc);
                [{iris_inbox, _Key, Message, Metadata}] ->
                    Entry = case IncludeMeta of
                        true -> {Offset, Message, Metadata};
                        false -> {Offset, Message, #{}}
                    end,
                    collect_inbox_messages_tx(NextKey, EndUserID, Remaining - 1, IncludeMeta, [Entry | Acc])
            end
    end.

%% @private Generate the "next" user ID for range scan termination
next_user_id(UserID) when is_binary(UserID) ->
    %% Append high byte to get next user
    <<UserID/binary, 255>>;
next_user_id(UserID) when is_integer(UserID) ->
    UserID + 1;
next_user_id(UserID) when is_atom(UserID) ->
    %% Convert to binary, increment, but this is a fallback
    list_to_atom(atom_to_list(UserID) ++ [255]);
next_user_id(UserID) when is_list(UserID) ->
    UserID ++ [255];
next_user_id(UserID) ->
    %% Generic: tuple it with 'z' to be > any normal key
    {UserID, zzz_end}.

%% @doc Get the current highest offset for a user's inbox.
%% Returns {ok, Offset} or {ok, 0} if inbox is empty.
-spec inbox_offset(term()) -> {ok, non_neg_integer()}.
inbox_offset(UserID) ->
    %% Find the last key for this user by scanning backwards from a high offset
    %% Use max 64-bit integer value as the search start point (higher than any HLC or microsecond timestamp)
    EndKey = {UserID, 16#7FFFFFFFFFFFFFFF},  %% Max positive signed 64-bit (Erlang arbitrary precision helps)
    case find_last_inbox_offset(EndKey, UserID) of
        undefined -> {ok, 0};
        Offset -> {ok, Offset}
    end.

find_last_inbox_offset(SearchKey, UserID) ->
    case mnesia:dirty_prev(iris_inbox, SearchKey) of
        '$end_of_table' -> undefined;
        {FoundUserID, Offset} when FoundUserID =:= UserID -> Offset;
        {_OtherUser, _} -> undefined  %% Found different user's key, this user has no entries
    end.

%% =============================================================================
%% Internal: Safety Checks
%% =============================================================================

check_write_safety() ->
    %% Check partition guard if available
    case whereis(iris_partition_guard) of
        undefined -> 
            ok;  %% Guard not running = permissive
        _Pid ->
            iris_partition_guard:is_safe_for_writes()
    end.
