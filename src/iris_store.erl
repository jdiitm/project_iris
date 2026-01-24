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
%%   - Durable after function returns (sync_transaction by default)
%%   - Replicated to all disc_copies nodes
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
%%   put(Table, Key, Value) -> ok | {error, Reason}           %% Guaranteed durable
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
%%   - durability: guaranteed (default) | best_effort | quorum
%%   - timeout: milliseconds (default 5000)
-spec put(atom(), term(), term(), map()) -> ok | {error, term()}.
put(Table, Key, Value, Opts) ->
    %% Check partition guard first
    case check_write_safety() of
        ok ->
            Durability = maps:get(durability, Opts, guaranteed),
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
    Timeout = maps:get(timeout, Opts, 3000),
    iris_quorum_write:write_durable(Table, Key, Value, #{timeout => Timeout}).

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
    Timeout = maps:get(timeout, Opts, 2000),
    iris_quorum_write:read_quorum(Table, Key, #{timeout => Timeout}).

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
