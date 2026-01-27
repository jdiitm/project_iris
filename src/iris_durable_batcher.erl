-module(iris_durable_batcher).
-behaviour(gen_server).

%% =============================================================================
%% Write-Ahead Log (WAL) + Batched Sync Transaction
%% =============================================================================
%% Design Principles:
%% 1. Immediate durability: disk_log sync before ACK to client (~1ms)
%% 2. Batched replication: N messages = 1 sync_transaction vs N transactions
%% 3. Crash recovery: Replay uncommitted WAL entries on restart
%% 4. Sharded: 8 batcher processes for parallelism (phash2 by user)
%% =============================================================================

-export([start_link/1, store/3, store_batch/4]).
-export([get_stats/0, force_flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(POOL_SIZE, 8).
-define(BATCH_SIZE, 1000).
-define(FLUSH_INTERVAL_MS, 500).
%% P1-H6 FIX: WAL directory is now configurable via iris_core.wal_directory
%% Default changed from /tmp (often tmpfs) to data/wal (persistent)
-define(DEFAULT_WAL_DIR, "data/wal").

-record(state, {
    shard_id :: integer(),
    wal_log :: disk_log:log() | undefined,
    pending = [] :: list(),          %% [{User, Key, Timestamp, Msg, SeqNo}]
    pending_count = 0 :: integer(),
    seq_no = 0 :: integer(),
    timer_ref :: reference() | undefined,
    %% Stats
    writes_wal = 0 :: integer(),
    writes_mnesia = 0 :: integer(),
    batch_count = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

%% Start a specific shard (called by supervisor)
start_link(ShardId) ->
    Name = shard_name(ShardId),
    gen_server:start_link({local, Name}, ?MODULE, [ShardId], []).

%% Store a message with write-ahead durability
%% Returns immediately after WAL sync (not after Mnesia replication)
-spec store(binary(), binary(), integer()) -> ok | {error, term()}.
store(User, Msg, BucketCount) ->
    ShardId = select_shard(User),
    gen_server:call(shard_name(ShardId), {store, User, Msg, BucketCount}, 10000).

%% Store batch with write-ahead durability
-spec store_batch(binary(), [binary()], integer(), map()) -> ok | {error, term()}.
store_batch(User, Msgs, BucketCount, _Opts) ->
    ShardId = select_shard(User),
    gen_server:call(shard_name(ShardId), {store_batch, User, Msgs, BucketCount}, 30000).

%% Force flush all pending batches (for graceful shutdown)
force_flush() ->
    [gen_server:call(shard_name(I), force_flush, 30000) || I <- lists:seq(1, ?POOL_SIZE)],
    ok.

%% Get aggregated stats from all shards
get_stats() ->
    Stats = [get_shard_stats(I) || I <- lists:seq(1, ?POOL_SIZE)],
    aggregate_stats(Stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([ShardId]) ->
    process_flag(trap_exit, true),
    
    %% P1-H6 FIX: Get WAL directory from config (with validation)
    WalDir = get_wal_directory(),
    
    %% Ensure WAL directory exists
    ok = filelib:ensure_dir(WalDir ++ "/"),
    
    %% P1-H6 FIX: Validate WAL directory is on persistent storage
    validate_wal_storage(WalDir, ShardId),
    
    %% Open disk_log for this shard
    LogName = list_to_atom("iris_wal_" ++ integer_to_list(ShardId)),
    LogFile = WalDir ++ "/shard_" ++ integer_to_list(ShardId) ++ ".wal",
    
    WalLog = case disk_log:open([
        {name, LogName},
        {file, LogFile},
        {type, halt},
        {format, internal},
        {mode, read_write}
    ]) of
        {ok, Log} -> 
            logger:info("WAL shard ~p opened at ~s", [ShardId, LogFile]),
            Log;
        {repaired, Log, _Recovered, _Bad} -> 
            logger:warning("WAL shard ~p repaired on open at ~s", [ShardId, LogFile]),
            Log;
        {error, Reason} ->
            logger:error("Failed to open WAL for shard ~p at ~s: ~p", [ShardId, LogFile, Reason]),
            undefined
    end,
    
    %% Replay any uncommitted entries from previous crash
    self() ! replay_wal,
    
    %% Start flush timer
    TRef = erlang:send_after(?FLUSH_INTERVAL_MS, self(), flush),
    
    {ok, #state{
        shard_id = ShardId,
        wal_log = WalLog,
        timer_ref = TRef
    }}.

%% P1-H6 FIX: Get configurable WAL directory
get_wal_directory() ->
    case application:get_env(iris_core, wal_directory) of
        {ok, Dir} when is_list(Dir) -> Dir;
        {ok, Dir} when is_binary(Dir) -> binary_to_list(Dir);
        _ -> ?DEFAULT_WAL_DIR
    end.

%% P1-H6 FIX: Validate WAL directory is suitable for durability
validate_wal_storage(WalDir, ShardId) ->
    %% Check if directory is on tmpfs (RAM-only filesystem)
    %% This is a best-effort check - may not work on all systems
    case ShardId of
        1 ->
            %% Only log warning once (from shard 1)
            case is_tmpfs(WalDir) of
                true ->
                    logger:warning("======================================================="),
                    logger:warning("WARNING: WAL directory appears to be on tmpfs!"),
                    logger:warning("Path: ~s", [WalDir]),
                    logger:warning(""),
                    logger:warning("tmpfs is RAM-only and does not survive reboots."),
                    logger:warning("This defeats the purpose of write-ahead logging."),
                    logger:warning(""),
                    logger:warning("Configure iris_core.wal_directory to a persistent path."),
                    logger:warning("=======================================================");
                false ->
                    ok;
                unknown ->
                    logger:info("WAL directory: ~s (could not verify persistence)", [WalDir])
            end;
        _ ->
            ok
    end.

%% Check if path is on tmpfs (best effort)
is_tmpfs(Path) ->
    %% Try to detect tmpfs via /proc/mounts on Linux
    try
        case file:read_file("/proc/mounts") of
            {ok, Content} ->
                Lines = string:split(binary_to_list(Content), "\n", all),
                AbsPath = filename:absname(Path),
                check_tmpfs_mounts(AbsPath, Lines);
            {error, _} ->
                unknown
        end
    catch
        _:_ -> unknown
    end.

check_tmpfs_mounts(_Path, []) ->
    false;
check_tmpfs_mounts(Path, [Line | Rest]) ->
    case string:split(Line, " ", all) of
        [_, MountPoint, FsType | _] when FsType == "tmpfs" ->
            case string:prefix(Path, MountPoint) of
                nomatch -> check_tmpfs_mounts(Path, Rest);
                _ -> true
            end;
        _ ->
            check_tmpfs_mounts(Path, Rest)
    end.

handle_call({store, User, Msg, BucketCount}, _From, State) ->
    case do_wal_write(User, Msg, BucketCount, State) of
        {ok, NewState} ->
            %% Check if we should flush immediately
            FinalState = maybe_flush(NewState),
            {reply, ok, FinalState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({store_batch, User, Msgs, BucketCount}, _From, State) ->
    case do_wal_write_batch(User, Msgs, BucketCount, State) of
        {ok, NewState} ->
            FinalState = maybe_flush(NewState),
            {reply, ok, FinalState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(force_flush, _From, State) ->
    NewState = do_flush(State),
    {reply, ok, NewState};

handle_call(get_stats_local, _From, State) ->
    Stats = #{
        shard_id => State#state.shard_id,
        pending_count => State#state.pending_count,
        writes_wal => State#state.writes_wal,
        writes_mnesia => State#state.writes_mnesia,
        batch_count => State#state.batch_count
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State) ->
    NewState = do_flush(State),
    %% Reset timer
    TRef = erlang:send_after(?FLUSH_INTERVAL_MS, self(), flush),
    {noreply, NewState#state{timer_ref = TRef}};

handle_info(replay_wal, State) ->
    NewState = replay_uncommitted(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Flush any pending writes
    do_flush(State),
    %% Close WAL
    case State#state.wal_log of
        undefined -> ok;
        Log -> disk_log:close(Log)
    end,
    ok.

%% =============================================================================
%% Internal: Write-Ahead Log
%% =============================================================================

do_wal_write(User, Msg, BucketCount, State = #state{wal_log = undefined}) ->
    %% No WAL - fall back to direct Mnesia (less safe but works)
    do_direct_mnesia_write(User, Msg, BucketCount),
    {ok, State#state{writes_mnesia = State#state.writes_mnesia + 1}};

do_wal_write(User, Msg, BucketCount, State = #state{wal_log = Log, seq_no = SeqNo}) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, BucketCount),
    Key = {User, BucketID},
    NewSeqNo = SeqNo + 1,
    
    Entry = {pending, NewSeqNo, Key, Timestamp, Msg},
    
    %% CRITICAL: Sync write to disk - survives crash
    case disk_log:log(Log, Entry) of
        ok ->
            case disk_log:sync(Log) of
                ok ->
                    NewPending = [{Key, Timestamp, Msg, NewSeqNo} | State#state.pending],
                    {ok, State#state{
                        pending = NewPending,
                        pending_count = State#state.pending_count + 1,
                        seq_no = NewSeqNo,
                        writes_wal = State#state.writes_wal + 1
                    }};
                {error, Reason} ->
                    logger:error("WAL sync failed: ~p", [Reason]),
                    {error, wal_sync_failed}
            end;
        {error, Reason} ->
            logger:error("WAL write failed: ~p", [Reason]),
            {error, wal_write_failed}
    end.

do_wal_write_batch(User, Msgs, BucketCount, State = #state{wal_log = undefined}) ->
    %% No WAL - direct Mnesia
    lists:foreach(fun(Msg) ->
        do_direct_mnesia_write(User, Msg, BucketCount)
    end, Msgs),
    {ok, State#state{writes_mnesia = State#state.writes_mnesia + length(Msgs)}};

do_wal_write_batch(User, Msgs, BucketCount, State = #state{wal_log = Log, seq_no = SeqNo}) ->
    Timestamp = os:system_time(millisecond),
    
    %% Build entries
    {Entries, FinalSeqNo, NewPending} = lists:foldl(fun(Msg, {AccE, AccSeq, AccP}) ->
        BucketID = erlang:phash2(Msg, BucketCount),
        Key = {User, BucketID},
        NextSeq = AccSeq + 1,
        Entry = {pending, NextSeq, Key, Timestamp, Msg},
        PendingEntry = {Key, Timestamp, Msg, NextSeq},
        {[Entry | AccE], NextSeq, [PendingEntry | AccP]}
    end, {[], SeqNo, State#state.pending}, Msgs),
    
    %% Write all entries
    case disk_log:log_terms(Log, lists:reverse(Entries)) of
        ok ->
            case disk_log:sync(Log) of
                ok ->
                    {ok, State#state{
                        pending = NewPending,
                        pending_count = State#state.pending_count + length(Msgs),
                        seq_no = FinalSeqNo,
                        writes_wal = State#state.writes_wal + length(Msgs)
                    }};
                {error, _} ->
                    {error, wal_sync_failed}
            end;
        {error, _} ->
            {error, wal_write_failed}
    end.

do_direct_mnesia_write(User, Msg, BucketCount) ->
    Timestamp = os:system_time(millisecond),
    BucketID = erlang:phash2(Msg, BucketCount),
    Key = {User, BucketID},
    F = fun() -> mnesia:write({offline_msg, Key, Timestamp, Msg}) end,
    mnesia:activity(sync_transaction, F).

%% =============================================================================
%% Internal: Batched Mnesia Flush
%% =============================================================================

maybe_flush(State = #state{pending_count = Count}) when Count >= ?BATCH_SIZE ->
    do_flush(State);
maybe_flush(State) ->
    State.

do_flush(State = #state{pending = [], pending_count = 0}) ->
    State;  %% Nothing to flush

do_flush(State = #state{pending = Pending, wal_log = Log}) ->
    %% Batch write to Mnesia with sync_transaction
    F = fun() ->
        lists:foreach(fun({Key, Timestamp, Msg, _SeqNo}) ->
            mnesia:write({offline_msg, Key, Timestamp, Msg})
        end, Pending)
    end,
    
    case mnesia:activity(sync_transaction, F) of
        ok ->
            mark_committed(Log, Pending),
            State#state{
                pending = [],
                pending_count = 0,
                writes_mnesia = State#state.writes_mnesia + length(Pending),
                batch_count = State#state.batch_count + 1
            };
        {atomic, _} ->
            mark_committed(Log, Pending),
            State#state{
                pending = [],
                pending_count = 0,
                writes_mnesia = State#state.writes_mnesia + length(Pending),
                batch_count = State#state.batch_count + 1
            };
        {aborted, Reason} ->
            logger:error("Mnesia batch write aborted: ~p", [Reason]),
            State;  %% Keep pending for retry
        Error ->
            logger:error("Mnesia batch write error: ~p", [Error]),
            State
    end.

mark_committed(undefined, _Pending) ->
    ok;
mark_committed(Log, Pending) ->
    %% Mark entries as committed in WAL
    Entries = [{committed, SeqNo} || {_Key, _Ts, _Msg, SeqNo} <- Pending],
    disk_log:log_terms(Log, Entries),
    ok.

%% =============================================================================
%% Internal: Crash Recovery
%% =============================================================================

replay_uncommitted(State = #state{wal_log = undefined}) ->
    State;

replay_uncommitted(State = #state{wal_log = Log, shard_id = ShardId}) ->
    logger:info("Replaying WAL for shard ~p...", [ShardId]),
    
    %% Read all entries
    {PendingMap, MaxSeqNo} = read_all_wal_entries(Log, #{}, 0),
    
    %% Filter out committed entries
    UncommittedEntries = maps:to_list(PendingMap),
    
    case UncommittedEntries of
        [] ->
            logger:info("WAL shard ~p: no uncommitted entries", [ShardId]),
            %% Truncate WAL since all committed
            disk_log:truncate(Log),
            State#state{seq_no = MaxSeqNo};
        _ ->
            logger:info("WAL shard ~p: replaying ~p uncommitted entries", 
                       [ShardId, length(UncommittedEntries)]),
            
            %% Replay to Mnesia
            F = fun() ->
                lists:foreach(fun({SeqNo, {Key, Timestamp, Msg}}) ->
                    mnesia:write({offline_msg, Key, Timestamp, Msg})
                end, UncommittedEntries)
            end,
            
            case mnesia:activity(sync_transaction, F) of
                ok -> ok;
                {atomic, _} -> ok;
                Error ->
                    logger:error("WAL replay failed: ~p", [Error])
            end,
            
            %% Truncate WAL after successful replay
            disk_log:truncate(Log),
            State#state{seq_no = MaxSeqNo}
    end.

read_all_wal_entries(Log, PendingMap, MaxSeq) ->
    read_wal_chunk(Log, disk_log:chunk(Log, start), PendingMap, MaxSeq).

read_wal_chunk(_Log, eof, PendingMap, MaxSeq) ->
    {PendingMap, MaxSeq};
read_wal_chunk(_Log, {error, _Reason}, PendingMap, MaxSeq) ->
    {PendingMap, MaxSeq};
read_wal_chunk(Log, {Cont, Terms}, PendingMap, MaxSeq) ->
    {NewMap, NewMax} = lists:foldl(fun
        ({pending, SeqNo, Key, Timestamp, Msg}, {Map, Max}) ->
            {maps:put(SeqNo, {Key, Timestamp, Msg}, Map), max(SeqNo, Max)};
        ({committed, SeqNo}, {Map, Max}) ->
            {maps:remove(SeqNo, Map), max(SeqNo, Max)};
        (_Other, Acc) ->
            Acc
    end, {PendingMap, MaxSeq}, Terms),
    read_wal_chunk(Log, disk_log:chunk(Log, Cont), NewMap, NewMax).

%% =============================================================================
%% Internal: Helpers
%% =============================================================================

shard_name(ShardId) ->
    list_to_atom("iris_durable_batcher_" ++ integer_to_list(ShardId)).

select_shard(User) ->
    (erlang:phash2(User, ?POOL_SIZE) + 1).

get_shard_stats(ShardId) ->
    try gen_server:call(shard_name(ShardId), get_stats_local, 1000)
    catch _:_ -> #{writes_wal => 0, writes_mnesia => 0, batch_count => 0, pending_count => 0}
    end.

aggregate_stats(StatsList) ->
    lists:foldl(fun(S, Acc) ->
        #{
            writes_wal => maps:get(writes_wal, S, 0) + maps:get(writes_wal, Acc, 0),
            writes_mnesia => maps:get(writes_mnesia, S, 0) + maps:get(writes_mnesia, Acc, 0),
            batch_count => maps:get(batch_count, S, 0) + maps:get(batch_count, Acc, 0),
            total_pending => maps:get(pending_count, S, 0) + maps:get(total_pending, Acc, 0)
        }
    end, #{writes_wal => 0, writes_mnesia => 0, batch_count => 0, total_pending => 0}, StatsList).
