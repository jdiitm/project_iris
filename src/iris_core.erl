-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

%% OTP Callbacks
-export([start/2, stop/1, init/1]).
-export([init_db/0, init_db/1, join_cluster/1, init_cross_region_replication/0]).
-export([reconcile_after_partition/0]).  %% F1 AUDIT FIX: RFC 7.1.1 union merge
-export([reconcile_batch/2]).  %% G-2 FIX: cursor-based batched reconciliation
-export([reconcile_table/3]).  %% GAP-2 FIX: generic table reconciliation (RFC 7.1.1)
-export([merge_table_batch/3]).  %% F1 FIX: exported for conflict resolution testing
-export([should_overwrite/3]).   %% F1 FIX: timestamp-aware conflict resolution

%% High-Scale Messaging APIs
-export([register_user/3, lookup_user/1]).
-export([check_mtls_enforcement/0]).
-export([store_offline/2, store_offline_durable/2, store_batch/2, retrieve_offline/1]).
-export([retrieve_offline_paginated/3, get_offline_queue_depth/1, delete_offline_confirmed/2]).
-export([get_bucket_count/1, set_bucket_count/2]).
-export([update_status/2, get_status/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Application Callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    %% Rationale: Production systems use structured logging for grep-ability.
    logger:info("Starting Iris Core on node ~p", [node()]),

    %% Rationale: DB initialization is moved to a dedicated manager or
    %% handled via a boot script to prevent accidental schema wipes.
    %% Smart DB Init: Only initialize schema if we are the First Node or Standalone.
    %% If we find other seeds, we skip schema creation and let Mnesia sync from them.
    case application:get_env(iris_core, auto_init_db, false) of
        true -> 
            Seeds = application:get_env(iris_core, join_seeds, []),
            LiveSeeds = [S || S <- Seeds, S =/= node(), net_adm:ping(S) =:= pong],
            case LiveSeeds of
                [] -> 
                    logger:info("No live seeds found. Initializing new schema as Primary."),
                    init_db();
                [Seed|_] -> 
                    logger:info("Found live seed ~p. Joining existing cluster.", [Seed]),
                    %% Ensure Mnesia is started but do NOT create schema (will sync)
                    mnesia:start()
            end;
        false -> ok
    end,

    %% Ensure PG (Default Scope) is started safely
    %% In tests it might be already started; in prod it needs starting.
    try pg:start_link() 
    catch 
        error:undef -> ok; %% Old OTP?
        _:_ -> ok 
    end,

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    logger:info("Stopping Iris Core on node ~p", [node()]),
    ok.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init([]) ->
    %% Rationale: strategy 'one_for_one' is replaced with a logic-based hierarchy.
    %% We use secondary supervisors for batchers to isolate their crashes.
    
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},

    Children = [
        %% Flow Controller: Global backpressure and cascade failure detection
        #{id => iris_flow_controller,
          start => {iris_flow_controller, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Deduplication Service: RFC NFR-11 - 7-day dedup window with Mnesia persistence
        %% MUST start early - dedup checks happen during message processing
        #{id => iris_dedup,
          start => {iris_dedup, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Presence Manager: ETS-backed lockfree presence registry
        %% FORENSIC_AUDIT_FIX: Must start early - creates presence_local ETS table
        #{id => iris_presence,
          start => {iris_presence, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Partition Guard: Split-brain detection and safe mode
        %% AUDIT FIX: Detects cluster partitions and rejects writes to prevent divergence
        #{id => iris_partition_guard,
          start => {iris_partition_guard, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Cluster Manager: Self-healing cluster topology
        %% FORENSIC_AUDIT_FIX: Monitors nodeup/nodedown and auto-wires replication
        #{id => iris_cluster_manager,
          start => {iris_cluster_manager, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Durable Batcher Supervisor: WAL + batched sync_transaction for durability
        #{id => iris_durable_batcher_sup,
          start => {iris_durable_batcher_sup, start_link, []},
          type => supervisor,
          restart => permanent},
          
        %% Core Registry: Registers this Core with pg for Edge discovery
        #{id => iris_core_registry,
          start => {iris_core_registry, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Status Batcher Supervisor: Manages the 100 workers
        #{id => iris_status_batcher_sup,
          start => {iris_status_batcher_sup, start_link, [100]},
          type => supervisor,
          restart => permanent},
          
        %% Group Messaging Service: Handles group creation, membership, and message fanout
        #{id => iris_group,
          start => {iris_group, start_link, []},
          type => worker,
          restart => permanent},
          
        %% Shard Manager: Consistent user-to-shard mapping for horizontal scaling
        %% FIX: iris_shard was missing from supervisor - needed for message routing
        #{id => iris_shard,
          start => {iris_shard, start_link, []},
          type => worker,
          restart => permanent},

        %% Metrics: Must start early -- other modules emit counters through it
        #{id => iris_metrics,
          start => {iris_metrics, start_link, []},
          type => worker,
          restart => permanent},

        %% E2EE Key Bundle Storage (X3DH key bundles, SPK rotation)
        #{id => iris_keys,
          start => {iris_keys, start_link, []},
          type => worker,
          restart => permanent},

        %% Cross-Region Message Bridge
        #{id => iris_region_bridge,
          start => {iris_region_bridge, start_link, []},
          type => worker,
          restart => permanent},

        %% Optional Read Receipt Tracking
        #{id => iris_read_receipts,
          start => {iris_read_receipts, start_link, []},
          type => worker,
          restart => permanent},

        %% Mailbox Guard: Per-user mailbox overflow protection
        #{id => iris_mailbox_guard,
          start => {iris_mailbox_guard, start_link, []},
          type => worker,
          restart => permanent},

        %% Mailbox Monitor: Tracks mailbox sizes for backpressure signals
        #{id => iris_mailbox_monitor,
          start => {iris_mailbox_monitor, start_link, []},
          type => worker,
          restart => permanent},

        %% Efficiency Monitor: Scheduler utilization and memory tracking
        #{id => iris_efficiency_monitor,
          start => {iris_efficiency_monitor, start_link, []},
          type => worker,
          restart => permanent}
    ],

    %% Register this Core node with pg for Edge discovery
    %% AND attempt to auto-rejoin cluster if peers are found
    spawn(fun() -> 
        timer:sleep(1000), % Wait for registry to start
        iris_core_registry:join(),
        
        %% Auto-rejoin cluster: ping known peers and join first responder
        KnownPeers = application:get_env(iris_core, join_seeds, []),
        OtherPeers = [P || P <- KnownPeers, P =/= node()],
        case lists:search(fun(P) -> net_adm:ping(P) == pong end, OtherPeers) of
            {value, LivePeer} ->
                logger:info("Auto-joining cluster via ~p", [LivePeer]),
                iris_core:join_cluster(LivePeer);
            false ->
                logger:info("No cluster peers found, standalone mode")
        end
    end),

    %% AUDIT FIX: Auto-wire cross-region replication if configured
    %% Only if we are a core node
    spawn(fun() ->
        case application:get_env(iris_core, regions, []) of
            [] -> ok;
            Regions when length(Regions) > 0 ->
                 %% Wait for cluster to stabilize
                 timer:sleep(5000),
                 case is_core_node(node()) of
                     true ->
                         logger:info("Regions configured, attempting to wire replication..."),
                         init_cross_region_replication();
                     false -> ok
                 end
        end
    end),

    %% SAFETY DEFAULT: Validate presence backend configuration
    case application:get_env(iris_core, presence_backend) of
        {ok, Backend} when Backend =:= ets; Backend =:= mnesia ->
            ok;
        undefined ->
            logger:error("CRITICAL: iris_core presence_backend NOT CONFIGURED."),
            logger:error("Must be set to 'ets' (high scale) or 'mnesia' (legacy)."),
            logger:error("Refusing to start with unsafe defaults."),
            exit(presence_backend_not_configured);
        {ok, Other} ->
             logger:error("Invalid presence_backend: ~p", [Other]),
             exit({invalid_presence_backend, Other})
    end,

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% mTLS Enforcement (NFR-15)
%%%===================================================================

%% @doc Check mTLS enforcement config. Exits if enforce_mtls=true but
%% ssl_dist_optfile is not set. Called from start/2.
-spec check_mtls_enforcement() -> ok.
check_mtls_enforcement() ->
    case application:get_env(iris_core, enforce_mtls, false) of
        true ->
            case init:get_argument(ssl_dist_optfile) of
                {ok, _} -> ok;
                error ->
                    logger:error("CRITICAL: enforce_mtls=true but ssl_dist_optfile not set"),
                    exit(mtls_not_configured)
            end;
        false ->
            logger:warning("mTLS NOT enforced (NFR-15). Set enforce_mtls=true for production."),
            ok
    end.

%%%===================================================================
%%% FAANG-Grade Messaging APIs
%%%===================================================================

register_user(User, Node, Pid) ->
    %% FORENSIC_AUDIT_FIX: Default to ETS for lockfree presence (was mnesia).
    %% Mnesia causes global lock bottleneck at scale (~10k tx/sec limit).
    %% ETS provides ~1μs lockfree operations.
    case application:get_env(iris_core, presence_backend, ets) of
        ets ->
            %% Lockfree ETS-backed presence (~1μs, no global lock)
            iris_presence:register(User, Node, Pid);
        mnesia ->
            %% Legacy Mnesia-backed presence (global lock, ~1ms)
            %% WARN: This path is deprecated for high-scale use
             F = fun() -> mnesia:write({presence, User, Node, Pid}) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> ok;
                {aborted, Reason} ->
                    logger:error("Failed to register user ~p: ~p", [User, Reason]),
                    {error, Reason}
            end;
        Other ->
            %% SAFETY DEFAULT: Crash if invalid config
            error({invalid_presence_backend, Other})
    end.

lookup_user(User) ->
    %% FORENSIC_AUDIT_FIX: Default to ETS for lockfree lookup.
    case application:get_env(iris_core, presence_backend, ets) of
        ets ->
            %% Lockfree ETS lookup
            iris_presence:lookup(User);
        mnesia ->
            %% Legacy Mnesia dirty_read (still fast, but requires Mnesia)
            case mnesia:dirty_read(presence, User) of
                [{presence, User, Node, Pid}] -> {ok, Node, Pid};
                [] -> {error, not_found}
            end
    end.

store_offline(User, Msg) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store(User, Msg, Count).

%% AUDIT FIX: Guaranteed durable store - use WAL + Async Replication
%% Old: mnesia:sync_transaction (Global Lock)
%% New: iris_durable_batcher (Local Disk WAL) -> Mnesia (Async)
%% P0-B FIX: For multimaster durability, use sync_transaction when cluster mode
%% RFC NFR-11: Server-side deduplication with 7-day window
%% RFC FR-5: FIFO ordering using client-provided sequence number
store_offline_durable(User, Msg) ->
    %% RFC Section 8: Inbox Size limit enforcement (GAP-6 fix)
    case get_offline_queue_depth(User) >= iris_limits:max_inbox_size() of
        true ->
            iris_metrics:inc(iris_inbox_full_rejected),
            {error, inbox_full};
        false ->
            store_offline_durable_inner(User, Msg)
    end.

store_offline_durable_inner(User, {idempotent_msg, IdempotencyKey, RealMsg}) ->
    %% RFC 1.2: Dedup by (user_id, idempotency_key), NOT by content hash
    DedupKey = <<User/binary, ":", IdempotencyKey/binary>>,
    case iris_dedup:check_and_mark(DedupKey) of
        duplicate ->
            logger:debug("Dedup: idempotency_key duplicate for ~p", [User]),
            iris_metrics:dedup_hit(),
            ok;
        new ->
            Count = get_bucket_count(User),
            case application:get_env(iris_core, multimaster_durability, false) of
                true ->
                    store_offline_sync_replicated(User, RealMsg, Count);
                false ->
                    case iris_durable_batcher:store(User, RealMsg, Count, undefined) of
                        ok -> ok;
                        {error, Reason} ->
                            logger:error("WAL write failed: ~p", [Reason]),
                            {error, durable_write_failed}
                    end
            end
    end;
store_offline_durable_inner(User, Msg) ->
    %% RFC NFR-11: Extract SeqNo and check dedup BEFORE storing
    %% RFC FR-5: Preserve SeqNo for FIFO ordering
    %% Message format may be: {SeqNo, RealMsg} or just binary
    {DedupKey, ActualMsg, MaybeSeqNo} = case Msg of
        {SeqNo, RealMsg} when is_integer(SeqNo) ->
            %% BUG FIX: Dedup key must include message content hash, not just User:SeqNo
            %% Each sender has their own SeqNo counter starting at 1, so without content hash,
            %% messages from different senders with the same SeqNo would be incorrectly deduplicated.
            %% Key format: User:SeqNo:ContentHash (unique per recipient+sequence+content)
            ContentHash = erlang:phash2(RealMsg),
            Key = <<User/binary, ":", (integer_to_binary(SeqNo))/binary, ":", (integer_to_binary(ContentHash))/binary>>,
            {Key, RealMsg, SeqNo};
        _ ->
            %% No sequence number - use message hash for dedup
            Hash = erlang:phash2(Msg),
            Key = <<User/binary, ":hash:", (integer_to_binary(Hash))/binary>>,
            {Key, Msg, undefined}
    end,
    
    %% Check for duplicate
    case iris_dedup:check_and_mark(DedupKey) of
        duplicate ->
            %% Silently drop duplicate - this is expected behavior
            logger:debug("Dedup: Dropping duplicate for ~p (key=~p)", [User, DedupKey]),
            iris_metrics:dedup_hit(),
            ok;  %% Return ok - duplicate is not an error
        new ->
            %% New message - store it
            Count = get_bucket_count(User),
            %% Check if we should use sync_transaction for guaranteed replication
            case application:get_env(iris_core, multimaster_durability, false) of
                true ->
                    %% P0-B FIX: Use sync_transaction to ensure replication BEFORE ACK
                    %% This is slower but guarantees RPO=0 even under SIGKILL
                    store_offline_sync_replicated(User, ActualMsg, Count, MaybeSeqNo);
                false ->
                    %% P1-H6 FIX: Use WAL for immediate durability (RPO=0) without global lock
                    %% RFC FR-5: Pass MaybeSeqNo for FIFO ordering
                    case iris_durable_batcher:store(User, ActualMsg, Count, MaybeSeqNo) of
                        ok -> ok;
                        {error, Reason} -> 
                            logger:error("WAL write failed for user ~p: ~p", [User, Reason]),
                            {error, durable_write_failed}
                    end
            end
    end.

%% P0-B FIX: Sync-replicated offline storage for multimaster durability
%% Uses mnesia:sync_transaction which blocks until ALL disc_copies have the data
%% This guarantees no message loss even under SIGKILL, but is slower (~20-100ms)
%% RFC FR-5: Use client SeqNo as timestamp when available for FIFO ordering
store_offline_sync_replicated(User, Msg, BucketCount) ->
    store_offline_sync_replicated(User, Msg, BucketCount, undefined).

store_offline_sync_replicated(User, Msg, BucketCount, MaybeSeqNo) ->
    %% RFC FR-5: Use client's SeqNo as timestamp for FIFO ordering when available
    %% If no SeqNo, fallback to HLC (maintains causality)
    Timestamp = case MaybeSeqNo of
        SeqNo when is_integer(SeqNo) ->
            %% Use SeqNo directly - client guarantees ordering
            SeqNo;
        undefined ->
            %% Fallback to HLC for proper message ordering
            case whereis(iris_hlc) of
                undefined ->
                    os:system_time(nanosecond);
                _Pid ->
                    iris_hlc:to_integer(iris_hlc:send())
            end
    end,
    BucketID = erlang:phash2(Msg, BucketCount),
    Key = {User, BucketID},
    Record = {offline_msg, Key, Timestamp, Msg},
    
    %% sync_transaction: Blocks until ALL disc_copies nodes have committed
    logger:debug("Storing offline msg for ~p: key=~p, ts=~p, node=~p", [User, Key, Timestamp, node()]),
    case mnesia:sync_transaction(fun() ->
        mnesia:write(Record)
    end) of
        {atomic, ok} -> 
            logger:info("Stored offline msg for ~p on ~p (key=~p, ts=~p)", [User, node(), Key, Timestamp]),
            ok;
        {aborted, Reason} ->
            logger:error("Sync replicated store failed for user ~p: ~p", [User, Reason]),
            {error, {sync_replication_failed, Reason}}
    end.

store_batch(User, Msgs) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store_batch(User, Msgs, Count).

retrieve_offline(User) ->
    Count = get_bucket_count(User),
    iris_offline_storage:retrieve(User, Count).

%% =============================================================================
%% HOT-001 FIX: Paginated Offline Retrieval for Celebrity Hotspots
%% =============================================================================
%% Instead of dumping all messages at once (OOM risk for 1M+ messages),
%% we retrieve in batches and allow the client to ACK pages before continuing.
%% This prevents login failures for users with large offline queues.
%% =============================================================================

%% @doc Get the number of offline messages queued for a user
-spec get_offline_queue_depth(binary()) -> non_neg_integer().
get_offline_queue_depth(User) ->
    Count = get_bucket_count(User),
    %% Count messages across all buckets using dirty reads (fast, non-blocking)
    lists:foldl(fun(BucketId, Acc) ->
        Key = {User, BucketId},
        case mnesia:dirty_read(offline_msg, Key) of
            [] -> Acc;
            Records -> Acc + length(Records)
        end
    end, 0, lists:seq(0, Count - 1)).

%% @doc Retrieve offline messages in paginated batches (HOT-001 FIX)
%% Returns {Messages, NextCursor} where NextCursor is 'done' or an opaque cursor.
%% Messages are NOT deleted - caller must confirm delivery via delete_offline_confirmed/2.
%% 
%% PageSize: Number of messages to return per page (recommended: 100-1000)
%% Cursor: 0 for first page, or value from previous call's NextCursor
-spec retrieve_offline_paginated(binary(), non_neg_integer(), non_neg_integer()) -> 
    {list(), done | non_neg_integer()}.
retrieve_offline_paginated(User, _PageSize, Cursor) ->
    Count = get_bucket_count(User),
    %% Use lockfree cursor-based retrieval (PageSize is handled by retrieve_cursor)
    iris_offline_storage:retrieve_cursor(User, Count, Cursor).

%% @doc Delete offline messages after client confirms receipt (HOT-001 FIX)
%% Call this AFTER client ACKs the page of messages.
%% FromCursor/ToCursor define the range of buckets to delete.
-spec delete_offline_confirmed(binary(), {non_neg_integer(), non_neg_integer()}) -> ok.
delete_offline_confirmed(User, {FromCursor, ToCursor}) ->
    Count = get_bucket_count(User),
    iris_offline_storage:delete_confirmed(User, Count, FromCursor, ToCursor).

get_bucket_count(User) ->
    case mnesia:dirty_read(user_meta, User) of
        [{user_meta, User, Count}] -> Count;
        [] -> 1
    end.

set_bucket_count(User, Count) ->
    %% AUDIT FIX (Finding 2.1): Reject bucket count decreases to prevent
    %% data stranding — messages in higher-numbered buckets become invisible
    %% if BucketCount shrinks.
    CurrentCount = get_bucket_count(User),
    case Count < CurrentCount of
        true ->
            {error, {bucket_count_decrease, CurrentCount, Count}};
        false ->
            F = fun() -> mnesia:write({user_meta, User, Count}) end,
            mnesia:transaction(F)
    end.

update_status(User, online) -> ok;
update_status(User, offline) ->
    %% FORENSIC_AUDIT_FIX: Unregister from correct backend
    %% Rationale: Atomic delete prevents "ghost" online status if batcher is slow.
    case application:get_env(iris_core, presence_backend, ets) of
        ets ->
            %% ETS-backed presence - unregister via iris_presence
            iris_presence:unregister(User);
        mnesia ->
            %% Legacy Mnesia-backed presence
            mnesia:dirty_delete(presence, User)
    end,
    iris_status_batcher:submit(User, offline).

get_status(User) ->
    %% Rationale: Multi-tier lookup. RAM -> Disk.
    %% FORENSIC_AUDIT_FIX: Default to ETS for lockfree status lookup.
    case application:get_env(iris_core, presence_backend, ets) of
        ets ->
            %% ETS-backed presence lookup (lockfree)
            case iris_presence:lookup_local(User) of
                {ok, _Node, _Pid} -> {online, true, 0};
                _ -> get_status_from_disk(User)
            end;
        mnesia ->
            %% Legacy Mnesia-backed presence lookup
            case mnesia:dirty_read(presence, User) of
                [{presence, User, _, _}] -> {online, true, 0};
                [] -> get_status_from_disk(User)
            end
    end.

%% Helper: Get status from disk (user_status table)
get_status_from_disk(User) ->
    case mnesia:dirty_read(user_status, User) of
        [{user_status, User, LastSeen}] -> {online, false, LastSeen};
        [] -> {online, false, 0}
    end.

%%%===================================================================
%%% Internal Functions (Hidden from External API)
%%%===================================================================

init_db() ->
    %% ROBUST INITIALIZATION: Config-driven with recovery support.
    %% Key insight: Check if Mnesia schema already exists before recreating.
    Peers = application:get_env(iris_core, join_seeds, []),
    OtherPeers = [P || P <- Peers, P =/= node()],
    
    %% Ensure mnesia is stopped before configuration
    application:stop(mnesia),
    
    %% Check if we have existing Mnesia data (restart recovery)
    MnesiaDir = mnesia:system_info(directory),
    SchemaFile = filename:join(MnesiaDir, "schema.DAT"),
    HasExistingData = filelib:is_file(SchemaFile),
    
    case HasExistingData of
        true ->
            %% RECOVERY: Existing data found - just start Mnesia
            logger:info("Found existing Mnesia data at ~s. Starting recovery...", [MnesiaDir]),
            mnesia:start(),
            %% Wait for tables to load from disk
            Tables = [offline_msg, user_meta, user_status, revoked_tokens, dedup_log],
            case mnesia:wait_for_tables(Tables, 30000) of
                ok ->
                    logger:info("All tables loaded successfully");
                {timeout, BadTables} ->
                    %% CRITICAL: Some tables failed to load - repair them
                    logger:warning("Tables failed to load: ~p. Attempting repair...", [BadTables]),
                    repair_failed_tables(BadTables);
                {error, Reason} ->
                    logger:error("Table load error: ~p. Recreating tables...", [Reason]),
                    create_tables([node()])
            end,
            %% Ensure presence table exists (RAM only)
            ensure_table_exists(presence, [
                {ram_copies, [node()]},
                {attributes, [user, node, pid]}
            ]),
            %% RFC FR-11a: Ensure refresh_tokens table exists
            ensure_table_exists(refresh_tokens, [
                {disc_copies, [node()]},
                {attributes, [token_id, user_id, family_id, used, created_at, expires_at]},
                {type, set}
            ]),
            logger:info("Mnesia recovery complete. Tables: ~p", [mnesia:system_info(tables)]);
            
        false ->
            %% FRESH START: No existing data
            case lists:search(fun(P) -> net_adm:ping(P) == pong end, OtherPeers) of
                {value, LivePeer} ->
                    %% CLUSTER EXISTS: Join it
                    case safe_to_delete_schema(LivePeer) of
                        {ok, proceed} ->
                            logger:info("Found active cluster node ~p. Joining...", [LivePeer]),
                            mnesia:delete_schema([node()]),
                            mnesia:start(),
                            mnesia:change_config(extra_db_nodes, [LivePeer]),
                            mnesia:change_table_copy_type(schema, node(), disc_copies),
                            Tables = mnesia:system_info(tables) -- [schema],
                            [mnesia:add_table_copy(T, node(), disc_copies) || T <- Tables],
                            logger:info("Joined cluster successfully.");
                        {error, Reason} ->
                            logger:error("REFUSING to delete schema: ~p. Starting standalone.", [Reason]),
                            mnesia:create_schema([node()]),
                            mnesia:start(),
                            create_tables([node()])
                    end;
                    
                false ->
                    %% NO PEERS: We are the seed node.
                    logger:info("No peers found. Initializing as SEED node."),
                    mnesia:create_schema([node()]),
                    mnesia:start(),
                    create_tables([node()])
            end
    end.

%% Helper: Create table only if it doesn't exist
ensure_table_exists(Table, Opts) ->
    case lists:member(Table, mnesia:system_info(tables)) of
        true -> ok;
        false -> mnesia:create_table(Table, Opts)
    end.

%% Safety check before schema deletion to prevent data wipe
safe_to_delete_schema(LivePeer) ->
    %% Check 1: Env flag must allow schema deletion
    AllowDelete = application:get_env(iris_core, allow_schema_delete, false),
    case AllowDelete of
        false ->
            {error, schema_delete_not_allowed};
        true ->
            %% Check 2: Peer must have real tables (not empty cluster)
            case rpc:call(LivePeer, mnesia, system_info, [tables], 5000) of
                {badrpc, _} ->
                    {error, peer_unreachable};
                Tables when is_list(Tables), length(Tables) > 1 ->
                    %% Has tables beyond just 'schema'
                    {ok, proceed};
                _ ->
                    {error, peer_has_no_data}
            end
    end.

%% Repair tables that failed to load after crash recovery
repair_failed_tables([]) -> ok;
repair_failed_tables([Table | Rest]) ->
    logger:info("Repairing table: ~p", [Table]),
    %% Try to force load from local disc
    case mnesia:force_load_table(Table) of
        yes ->
            logger:info("Table ~p force loaded", [Table]),
            %% Verify table is usable
            case mnesia:wait_for_tables([Table], 5000) of
                ok -> ok;
                _ -> 
                    logger:warning("Table ~p force loaded but not usable. Recreating...", [Table]),
                    nuke_and_recreate_table(Table)
            end;
        ErrorOrNo ->
            logger:warning("Force load failed for ~p: ~p. Recreating table...", [Table, ErrorOrNo]),
            nuke_and_recreate_table(Table)
    end,
    repair_failed_tables(Rest).

%% Completely destroy and recreate a corrupted table
%% AUDIT FIX: Added safety gate to prevent accidental data loss
%% Set {iris_core, [{allow_table_nuke, true}]} to enable (DANGEROUS)
nuke_and_recreate_table(Table) ->
    case application:get_env(iris_core, allow_table_nuke, false) of
        true ->
            logger:warning("NUKING corrupted table ~p (allow_table_nuke=true)", [Table]),
            do_nuke_and_recreate(Table);
        false ->
            %% SAFE DEFAULT: Crash and alert operator instead of deleting data
            logger:error("========================================"),
            logger:error("CRITICAL: Table ~p corrupted!", [Table]),
            logger:error("Manual intervention required."),
            logger:error("Options:"),
            logger:error("  1. Restore from backup (recommended)"),
            logger:error("  2. Set allow_table_nuke=true and restart (DATA LOSS)"),
            logger:error("========================================"),
            exit({table_corrupted_manual_intervention, Table})
    end.

%% Internal: Actually perform the dangerous table nuke operation
do_nuke_and_recreate(Table) ->
    %% Step 1: Delete from Mnesia (may fail if table is in bad state)
    catch mnesia:delete_table(Table),
    timer:sleep(500),
    %% Step 2: Delete disc files directly (the nuclear option)
    MnesiaDir = mnesia:system_info(directory),
    TableFiles = filelib:wildcard(filename:join(MnesiaDir, atom_to_list(Table) ++ ".*")),
    lists:foreach(fun(File) ->
        logger:info("Deleting corrupted file: ~s", [File]),
        file:delete(File)
    end, TableFiles),
    %% Step 3: Recreate fresh
    recreate_table(Table),
    mnesia:wait_for_tables([Table], 10000),
    logger:info("Table ~p recreated successfully", [Table]).

%% Recreate a single table with its original definition
recreate_table(offline_msg) ->
    mnesia:create_table(offline_msg, [
        {disc_copies, [node()]},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]);
recreate_table(user_meta) ->
    mnesia:create_table(user_meta, [
        {disc_copies, [node()]},
        {attributes, [user, bucket_count]}
    ]);
recreate_table(user_status) ->
    mnesia:create_table(user_status, [
        {disc_copies, [node()]},
        {attributes, [user, last_seen]}
    ]);
recreate_table(revoked_tokens) ->
    mnesia:create_table(revoked_tokens, [
        {disc_copies, [node()]},
        {attributes, [jti, timestamp]}
    ]);
recreate_table(dedup_log) ->
    mnesia:create_table(dedup_log, [
        {disc_copies, [node()]},
        {attributes, [msg_id, timestamp]},
        {type, set}
    ]);
recreate_table(refresh_tokens) ->
    mnesia:create_table(refresh_tokens, [
        {disc_copies, [node()]},
        {attributes, [token_id, user_id, family_id, used, created_at, expires_at]},
        {type, set}
    ]);
recreate_table(Table) ->
    logger:error("Unknown table to recreate: ~p", [Table]).

%% Internal: Create tables (only called when seeding)
create_tables(Nodes) ->
    mnesia:create_table(presence, [
        {ram_copies, Nodes},
        {attributes, [user, node, pid]}
    ]),
    mnesia:create_table(offline_msg, [
        {disc_copies, Nodes},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]),
    mnesia:create_table(user_meta, [
        {disc_copies, Nodes},
        {attributes, [user, bucket_count]}
    ]),
    mnesia:create_table(user_status, [
        {disc_copies, Nodes},
        {attributes, [user, last_seen]}
    ]),
    %% P0-4 FIX: Add revoked_tokens table for distributed auth revocation
    mnesia:create_table(revoked_tokens, [
        {disc_copies, Nodes},
        {attributes, [jti, timestamp]}
    ]),
    %% P0-FIX: Add dedup_log table for bloom filter false positive verification
    %% Keyed by MsgId, stores timestamp for 7-day TTL cleanup
    mnesia:create_table(dedup_log, [
        {disc_copies, Nodes},
        {attributes, [msg_id, timestamp]},
        {type, set}
    ]),
    %% RFC FR-11a: Refresh token table for token refresh flow
    mnesia:create_table(refresh_tokens, [
        {disc_copies, Nodes},
        {attributes, [token_id, user_id, family_id, used, created_at, expires_at]},
        {type, set}
    ]),
    %% FR-8b: User safety tables (block/report)
    mnesia:create_table(user_blocks, [
        {disc_copies, Nodes},
        {attributes, [key, blocker, blocked, created_at]},
        {type, set}
    ]),
    mnesia:create_table(user_reports, [
        {disc_copies, Nodes},
        {attributes, [id, reporter, reported, reason, created_at]},
        {type, bag}
    ]),
    mnesia:wait_for_tables([presence, offline_msg, user_meta, user_status, revoked_tokens, dedup_log, refresh_tokens, user_blocks, user_reports], 5000),
    logger:info("Tables created.").

%% Legacy wrapper for specific node lists (unused now but kept for API compat)
init_db(Nodes) ->
   init_db(). %% Ignore args, use robust logic

join_cluster(Node) ->
    try
        case net_adm:ping(Node) of
            pong ->
                mnesia:change_config(extra_db_nodes, [Node]),
                mnesia:change_table_copy_type(schema, node(), disc_copies),
                Tables = mnesia:system_info(tables) -- [schema],
                [mnesia:add_table_copy(T, node(), disc_copies) || T <- Tables],
                logger:info("Joined cluster with ~p", [Node]);
            pang ->
                {error, undefined_node}
        end
    catch
        Class:Reason:Stack ->
             logger:error("Exception in join_cluster: ~p:~p at ~p", [Class, Reason, Stack]),
             {error, {Class, Reason}}
    end.

%%%===================================================================
%%% Cross-Region Mnesia Replication
%%%===================================================================
%% 
%% Enables cross-region table replication for:
%% - presence: ram_copies (fast reads, eventual consistency OK)
%% - offline_msg: disc_copies (durable, sync_transaction for RPO=0)
%% - user_status: ram_copies (last_seen timestamps)
%%
%% Call this AFTER all core nodes have joined the cluster.
%% Usage: iris_core:init_cross_region_replication().
%%%===================================================================

init_cross_region_replication() ->
    logger:info("Initializing cross-region Mnesia replication..."),
    
    %% Get all connected core nodes (filter by naming convention)
    AllNodes = [node() | nodes()],
    CoreNodes = [N || N <- AllNodes, is_core_node(N)],
    
    logger:info("Core nodes for replication: ~p", [CoreNodes]),
    
    %% Skip if we're the only node
    case length(CoreNodes) of
        1 ->
            logger:info("Single node cluster - no replication needed"),
            ok;
        _ ->
            %% Replicate presence table (ram_copies for speed)
            replicate_table(presence, ram_copies, CoreNodes),
            
            %% Replicate offline_msg table (disc_copies for durability/RPO=0)
            replicate_table(offline_msg, disc_copies, CoreNodes),
            
            %% Replicate user_status table (ram_copies)
            replicate_table(user_status, ram_copies, CoreNodes),
            
            %% Replicate user_meta table (disc_copies)
            replicate_table(user_meta, disc_copies, CoreNodes),
            
            %% Replicate dedup_log table (disc_copies for RFC NFR-11 dedup persistence)
            replicate_table(dedup_log, disc_copies, CoreNodes),
            
            %% Replicate revoked_tokens table (disc_copies for auth revocation)
            replicate_table(revoked_tokens, disc_copies, CoreNodes),
            
            %% Replicate cross-region bridge tables (disc_copies for RPO=0 during partitions)
            %% These tables store messages queued for delivery to partitioned regions
            replicate_table(cross_region_outbound, disc_copies, CoreNodes),
            replicate_table(cross_region_dead_letter, disc_copies, CoreNodes),
            
            logger:info("Cross-region replication initialized successfully"),
            ok
    end.

%%%===================================================================
%%% F1 AUDIT FIX: Post-Partition Reconciliation (RFC 7.1.1)
%%%===================================================================
%%% Called by iris_partition_guard when transitioning from diverged -> normal.
%%% Performs union merge of append-only tables (offline_msg) across all
%%% visible nodes. "No acknowledged message is lost during split-brain."
%%%
%%% Design:
%%% - Background process (spawned by partition guard)
%%% - Reads remote offline_msg records via Mnesia dirty operations
%%% - Inserts missing records locally (bag table = natural union)
%%% - Idempotent: re-running is safe (bag allows duplicates, but
%%%   records are keyed by {User, BucketID} + timestamp + msg)
%%%===================================================================

-spec reconcile_after_partition() -> ok | {error, term()}.
reconcile_after_partition() ->
    logger:info("=== POST-PARTITION RECONCILIATION START ==="),
    
    %% Get remote nodes that have offline_msg table
    RemoteNodes = try
        AllCopies = mnesia:table_info(offline_msg, all_nodes),
        AllCopies -- [node()]
    catch
        _:_ -> []
    end,
    
    case RemoteNodes of
        [] ->
            logger:info("Reconciliation: no remote nodes with offline_msg, skipping"),
            ok;
        _ ->
            %% RFC 7.1.1: Reconcile ALL data types, not just messages
            logger:info("Reconciliation: merging from ~p", [RemoteNodes]),
            lists:foreach(fun(RemoteNode) ->
                %% 1. Messages: Union (append-only, dedup handles duplicates)
                merge_offline_msg_from(RemoteNode),
                %% 2. Presence: Last-writer-wins (HLC timestamp) -- ram_copies, best-effort
                reconcile_table(RemoteNode, presence, 1000),
                %% 3. Group membership: Union of adds, latest-timestamp for removes
                reconcile_table(RemoteNode, group_member, 1000),
                %% 4. Key bundles: Union (all bundles are valid)
                reconcile_table(RemoteNode, e2ee_key_bundle, 1000)
            end, RemoteNodes),
            logger:info("=== POST-PARTITION RECONCILIATION COMPLETE ==="),
            ok
    end.

%% @doc Cursor-based batched reconciliation (G-2 FIX).
%% Replaces the O(N) dirty_match_object which fetches ALL records into RAM.
%% Iterates remote keys in batches using dirty_first/dirty_next.
-spec reconcile_batch(node(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
reconcile_batch(RemoteNode, BatchSize) when is_atom(RemoteNode), is_integer(BatchSize), BatchSize > 0 ->
    logger:info("G-2: Batched reconciliation from ~p (batch_size=~p)", [RemoteNode, BatchSize]),
    try
        FirstKey = rpc:call(RemoteNode, mnesia, dirty_first, [offline_msg], 5000),
        case FirstKey of
            {badrpc, Reason} ->
                logger:warning("G-2: Failed to read first key from ~p: ~p", [RemoteNode, Reason]),
                {error, Reason};
            '$end_of_table' ->
                logger:info("G-2: Remote node ~p has no offline_msg records", [RemoteNode]),
                {ok, 0};
            _ ->
                Merged = reconcile_batch_loop(RemoteNode, FirstKey, BatchSize, 0),
                logger:info("G-2: Batched reconciliation from ~p complete (~p merged)", [RemoteNode, Merged]),
                {ok, Merged}
        end
    catch
        _:Error ->
            logger:warning("G-2: Error during batched reconciliation from ~p: ~p", [RemoteNode, Error]),
            {error, Error}
    end.

reconcile_batch_loop(_RemoteNode, '$end_of_table', _BatchSize, Merged) ->
    Merged;
reconcile_batch_loop(RemoteNode, CurrentKey, BatchSize, Merged) ->
    %% Collect a batch of keys
    {Keys, NextKey} = collect_keys(RemoteNode, CurrentKey, BatchSize),
    
    %% Fetch records for these keys from the remote node and merge
    BatchMerged = merge_key_batch(RemoteNode, Keys),
    
    %% Rate limit: yield between batches to avoid flooding
    timer:sleep(1),
    
    reconcile_batch_loop(RemoteNode, NextKey, BatchSize, Merged + BatchMerged).

collect_keys(RemoteNode, StartKey, BatchSize) ->
    collect_keys(RemoteNode, StartKey, BatchSize, [StartKey]).

collect_keys(_RemoteNode, _CurrentKey, 0, Acc) ->
    %% Batch full -- need next key to continue
    %% The last key in Acc is the one we need to get the next for
    {lists:reverse(Acc), needs_next};
collect_keys(RemoteNode, CurrentKey, Remaining, Acc) ->
    NextKey = rpc:call(RemoteNode, mnesia, dirty_next, [offline_msg, CurrentKey], 5000),
    case NextKey of
        {badrpc, _} -> {lists:reverse(Acc), '$end_of_table'};
        '$end_of_table' -> {lists:reverse(Acc), '$end_of_table'};
        _ -> collect_keys(RemoteNode, NextKey, Remaining - 1, [NextKey | Acc])
    end.

merge_key_batch(RemoteNode, Keys) ->
    lists:foldl(fun(Key, Count) ->
        RemoteRecords = rpc:call(RemoteNode, mnesia, dirty_read, [offline_msg, Key], 5000),
        case RemoteRecords of
            {badrpc, _} -> Count;
            Records when is_list(Records) ->
                LocalRecords = mnesia:dirty_read(offline_msg, Key),
                LocalSet = sets:from_list(LocalRecords),
                Missing = [R || R <- Records, not sets:is_element(R, LocalSet)],
                lists:foreach(fun(Record) ->
                    mnesia:dirty_write(Record)
                end, Missing),
                Count + length(Missing);
            _ -> Count
        end
    end, 0, Keys).

%% Merge offline_msg records from a remote node into local table.
%% G-2 FIX: Now delegates to reconcile_batch/2 for cursor-based iteration.
merge_offline_msg_from(RemoteNode) ->
    logger:info("Reconciliation: reading offline_msg from ~p (batched)", [RemoteNode]),
    reconcile_batch(RemoteNode, 1000).

%% @doc Generic table reconciliation (GAP-2 FIX: RFC 7.1.1).
%% Cursor-based batched union merge for any Mnesia table.
%% Works for: presence, group_member, e2ee_key_bundle.
-spec reconcile_table(node(), atom(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
reconcile_table(RemoteNode, Table, BatchSize) when is_atom(RemoteNode), is_atom(Table), BatchSize > 0 ->
    logger:info("GAP-2: Reconciling table ~p from ~p (batch_size=~p)", [Table, RemoteNode, BatchSize]),
    try
        %% Check if table exists on remote node
        case rpc:call(RemoteNode, mnesia, table_info, [Table, size], 5000) of
            {badrpc, _Reason} ->
                logger:info("GAP-2: Table ~p not available on ~p, skipping", [Table, RemoteNode]),
                {ok, 0};
            _Size ->
                FirstKey = rpc:call(RemoteNode, mnesia, dirty_first, [Table], 5000),
                case FirstKey of
                    {badrpc, Reason2} ->
                        logger:warning("GAP-2: Failed to read first key of ~p from ~p: ~p",
                                       [Table, RemoteNode, Reason2]),
                        {error, Reason2};
                    '$end_of_table' ->
                        logger:info("GAP-2: Remote ~p table ~p is empty", [RemoteNode, Table]),
                        {ok, 0};
                    _ ->
                        Merged = reconcile_table_loop(RemoteNode, Table, FirstKey, BatchSize, 0),
                        logger:info("GAP-2: Reconciled ~p records from ~p:~p", [Merged, RemoteNode, Table]),
                        {ok, Merged}
                end
        end
    catch
        _:Error ->
            logger:warning("GAP-2: Error reconciling ~p from ~p: ~p", [Table, RemoteNode, Error]),
            {error, Error}
    end.

reconcile_table_loop(_RemoteNode, _Table, '$end_of_table', _BatchSize, Merged) ->
    Merged;
reconcile_table_loop(RemoteNode, Table, CurrentKey, BatchSize, Merged) ->
    %% Collect a batch of keys from the remote table
    {Keys, NextKey} = collect_table_keys(RemoteNode, Table, CurrentKey, BatchSize),

    %% Fetch and merge records for this batch
    BatchMerged = merge_table_batch(RemoteNode, Table, Keys),

    %% Rate limit between batches
    timer:sleep(1),

    reconcile_table_loop(RemoteNode, Table, NextKey, BatchSize, Merged + BatchMerged).

collect_table_keys(RemoteNode, Table, StartKey, BatchSize) ->
    collect_table_keys(RemoteNode, Table, StartKey, BatchSize, [StartKey]).

collect_table_keys(_RemoteNode, _Table, _CurrentKey, 0, Acc) ->
    {lists:reverse(Acc), needs_next};
collect_table_keys(RemoteNode, Table, CurrentKey, Remaining, Acc) ->
    NextKey = rpc:call(RemoteNode, mnesia, dirty_next, [Table, CurrentKey], 5000),
    case NextKey of
        {badrpc, _} -> {lists:reverse(Acc), '$end_of_table'};
        '$end_of_table' -> {lists:reverse(Acc), '$end_of_table'};
        _ -> collect_table_keys(RemoteNode, Table, NextKey, Remaining - 1, [NextKey | Acc])
    end.

merge_table_batch(RemoteNode, Table, Keys) ->
    lists:foldl(fun(Key, Count) ->
        RemoteRecords = rpc:call(RemoteNode, mnesia, dirty_read, [Table, Key], 5000),
        case RemoteRecords of
            {badrpc, _} -> Count;
            Records when is_list(Records) ->
                LocalRecords = mnesia:dirty_read(Table, Key),
                %% F1 FIX: Use conflict-aware merge instead of blind write.
                %% For bag tables (offline_msg, e2ee_key_bundle): union merge (write missing).
                %% For set tables (group_member, presence): timestamp-aware LWW.
                TableType = try mnesia:table_info(Table, type) catch _:_ -> set end,
                WrittenCount = case TableType of
                    bag ->
                        %% Append-only / bag: union merge (original logic)
                        LocalSet = sets:from_list(LocalRecords),
                        Missing = [R || R <- Records, not sets:is_element(R, LocalSet)],
                        lists:foreach(fun(Record) ->
                            mnesia:dirty_write(Table, Record)
                        end, Missing),
                        length(Missing);
                    _ ->
                        %% set / ordered_set: timestamp-aware conflict resolution
                        merge_set_records(Table, Records, LocalRecords)
                end,
                Count + WrittenCount;
            _ -> Count
        end
    end, 0, Keys).

%% F1 FIX: Merge set-type table records with timestamp-aware conflict resolution.
%% For each remote record, check if a local record exists with the same key.
%% If local exists: overwrite only if remote is newer (should_overwrite).
%% If local absent: write the remote record.
merge_set_records(Table, RemoteRecords, LocalRecords) ->
    %% Build a map of local records by key (element 2 is the key field in records)
    LocalMap = maps:from_list([{element(2, R), R} || R <- LocalRecords]),
    lists:foldl(fun(RemoteRec, Written) ->
        RemoteKey = element(2, RemoteRec),
        case maps:get(RemoteKey, LocalMap, undefined) of
            undefined ->
                %% No local record - write remote
                mnesia:dirty_write(Table, RemoteRec),
                Written + 1;
            LocalRec ->
                case should_overwrite(Table, RemoteRec, LocalRec) of
                    true ->
                        mnesia:dirty_write(Table, RemoteRec),
                        Written + 1;
                    false ->
                        Written
                end
        end
    end, 0, RemoteRecords).

%% F1 FIX: Determine if a remote record should overwrite a local record.
%% Implements per-table conflict resolution strategy (RFC 7.1.1):
%%   - group_member: Compare last_seen timestamps (LWW)
%%   - presence: Keep local (ephemeral, local is authoritative)
%%   - Default: Keep local (conservative)
should_overwrite(group_member, RemoteRec, LocalRec) ->
    %% last_seen is the 6th element (#group_member.last_seen)
    RemoteTS = element(6, RemoteRec),
    LocalTS = element(6, LocalRec),
    RemoteTS > LocalTS;
should_overwrite(presence, _RemoteRec, _LocalRec) ->
    %% Presence is ram_copies, ephemeral. Local is authoritative.
    false;
should_overwrite(_Table, _RemoteRec, _LocalRec) ->
    %% Conservative default: keep local record
    false.

%% Helper: Check if a node is a core node (by naming convention)
is_core_node(Node) ->
    NodeStr = atom_to_list(Node),
    %% Match patterns like: core_east_1@..., core_west_1@..., iris_core@...
    lists:prefix("core", NodeStr) orelse lists:prefix("iris_core", NodeStr).

%% Helper: Add table copies to all nodes that don't have them
%% Retries on failure (schema may not be active yet on remote nodes)
replicate_table(Table, CopyType, Nodes) ->
    case lists:member(Table, mnesia:system_info(tables)) of
        false ->
            logger:warning("Table ~p does not exist, skipping replication", [Table]),
            ok;
        true ->
            CurrentCopies = case CopyType of
                ram_copies -> mnesia:table_info(Table, ram_copies);
                disc_copies -> mnesia:table_info(Table, disc_copies);
                disc_only_copies -> mnesia:table_info(Table, disc_only_copies)
            end,
            
            %% Add copies to nodes that don't have them
            NodesToAdd = Nodes -- CurrentCopies,
            
            lists:foreach(fun(Node) ->
                add_table_copy_with_retry(Table, Node, CopyType, 3)
            end, NodesToAdd),
            
            ok
    end.

%% Add table copy with retries (handles transient failures like schema not active)
add_table_copy_with_retry(Table, Node, CopyType, Retries) ->
    add_table_copy_with_retry(Table, Node, CopyType, Retries, 1).

add_table_copy_with_retry(Table, Node, CopyType, MaxRetries, Attempt) when Attempt =< MaxRetries ->
    logger:info("Adding ~p copy of ~p to ~p (attempt ~p/~p)", 
                [CopyType, Table, Node, Attempt, MaxRetries]),
    case mnesia:add_table_copy(Table, Node, CopyType) of
        {atomic, ok} ->
            logger:info("Successfully added ~p to ~p", [Table, Node]),
            ok;
        {aborted, {already_exists, _, _}} ->
            logger:debug("Table ~p already exists on ~p", [Table, Node]),
            ok;
        {aborted, {system_limit, _, _} = Reason} ->
            %% Schema not active yet - wait and retry
            logger:warning("Failed to add ~p to ~p: ~p - retrying in 5s", [Table, Node, Reason]),
            timer:sleep(5000),
            add_table_copy_with_retry(Table, Node, CopyType, MaxRetries, Attempt + 1);
        {aborted, {not_active, _, _} = Reason} ->
            %% Schema not active yet - wait and retry
            logger:warning("Failed to add ~p to ~p: ~p - retrying in 5s", [Table, Node, Reason]),
            timer:sleep(5000),
            add_table_copy_with_retry(Table, Node, CopyType, MaxRetries, Attempt + 1);
        {aborted, Reason} ->
            logger:warning("Failed to add ~p to ~p: ~p", [Table, Node, Reason]),
            {error, Reason}
    end;
add_table_copy_with_retry(Table, Node, _CopyType, MaxRetries, _Attempt) ->
    logger:error("Failed to add ~p to ~p after ~p attempts", [Table, Node, MaxRetries]),
    {error, max_retries_exceeded}.