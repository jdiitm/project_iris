-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

%% OTP Callbacks
-export([start/2, stop/1, init/1]).
-export([init_db/0, init_db/1, join_cluster/1, init_cross_region_replication/0]).
-export([reconcile_after_partition/0]).  %% RFC 7.1.1 union merge
-export([reconcile_batch/2]).  %% Cursor-based batched reconciliation
-export([reconcile_table/3]).  %% Generic table reconciliation (RFC 7.1.1)
-export([merge_table_batch/3]).  %% Exported for conflict resolution testing
-export([should_overwrite/3]).   %% Timestamp-aware conflict resolution

%% High-Scale Messaging APIs
-export([register_user/3, lookup_user/1]).
-export([check_mtls_enforcement/0, check_mtls_enforcement/1]).
-export([validate_production_cookie/0, validate_production_cookie/1]).
-export([is_core_node/1]).  %% Exported for iris_cluster_join_worker
-export([validate_consistency_mode/0]).  %% CP mode hard-fail
-export([validate_metrics_token/0]).    %% Production metrics token validation
-export([make_dedup_key/2]).  %% Testable dedup key generation
-export([nuke_and_recreate_table/1]).  %% Exported for testing production guard
-export([force_load_isolated/1]).  %% Exported for H-2 production guard testing
-export([store_offline/2, store_offline_durable/2, store_batch/2, retrieve_offline/1]).
-export([retrieve_offline_paginated/3, get_offline_queue_depth/1, delete_offline_confirmed/2]).
-export([get_bucket_count/1, set_bucket_count/2]).
-export([update_status/2, get_status/1]).
-export([table_spec/1]).  %% Exported for scalability test assertions
-export([cleanup_expired_entries/0]).  %% TTL cleanup for dedup/revoked/refresh
-export([validate_compression_startup/0]).  %% Compression check at startup
-export([reload_tls_config/0]).  %% SEC-01: Runtime TLS certificate rotation

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Application Callbacks
%%%===================================================================

-spec start(atom(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Rationale: Production systems use structured logging for grep-ability.
    logger:info("Starting Iris Core on node ~p", [node()]),

    %% Validate consistency mode (fatal in production for CP)
    case validate_consistency_mode() of
        ok -> ok;
        {error, cp_not_implemented} ->
            init:stop(1),
            exit(cp_not_implemented)
    end,

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
        error:undef -> ok; %% Old OTP without pg module
        error:{already_started, _} -> ok;
        Class:Reason ->
            logger:warning("pg:start_link() failed: ~p:~p (non-fatal)", [Class, Reason]),
            ok
    end,

    %% Validate replication_factor
    case application:get_env(iris_core, replication_factor, 3) of
        RF when is_integer(RF), RF > 0 -> ok;
        BadRF ->
            logger:error("FATAL: replication_factor=~p is invalid (must be > 0)", [BadRF]),
            init:stop(1),
            exit(invalid_replication_factor)
    end,

    %% Validate critical config. Fatal in production mode.
    case application:get_env(iris_core, deployment_mode, development) of
        production ->
            case application:get_env(iris_core, expected_cluster_nodes, []) of
                [] ->
                    logger:error("FATAL: expected_cluster_nodes is empty in production mode -- "
                                 "split-brain is undetectable. Set expected_cluster_nodes in config."),
                    init:stop(1),
                    exit(expected_cluster_nodes_empty);
                _ -> ok
            end;
        _ ->
            case application:get_env(iris_core, expected_cluster_nodes, []) of
                [] -> logger:warning("PRODUCTION WARNING: expected_cluster_nodes is empty -- "
                                     "partition guard is DISABLED, split-brain is undetectable");
                _ -> ok
            end
    end,
    case application:get_env(iris_edge, core_nodes, []) of
        [] -> logger:warning("PRODUCTION WARNING: core_nodes is empty -- "
                             "edge nodes cannot route messages to core");
        _ -> ok
    end,

    %% Reject default cookie in production mode
    case validate_production_cookie() of
        ok -> ok;
        {error, default_cookie_in_production} ->
            init:stop(1),
            exit(default_cookie_in_production)
    end,

    %% Reject placeholder metrics token in production mode
    case validate_metrics_token() of
        ok -> ok;
        {error, TokenErr} ->
            init:stop(1),
            exit(TokenErr)
    end,

    %% Verify mTLS is configured if enforce_mtls=true
    check_mtls_enforcement(),

    %% Check compression algorithm availability
    validate_compression_startup(),

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec stop(term()) -> ok.
stop(_State) ->
    logger:info("Stopping Iris Core on node ~p", [node()]),
    ok.

%% @doc Validate that the default cookie is not used in production.
-spec validate_production_cookie() -> ok | {error, default_cookie_in_production}.
validate_production_cookie() ->
    validate_production_cookie(erlang:get_cookie()).

-spec validate_production_cookie(atom()) -> ok | {error, default_cookie_in_production}.
validate_production_cookie(Cookie) ->
    case application:get_env(iris_core, deployment_mode, development) of
        production ->
            case Cookie of
                iris_secret ->
                    logger:error("FATAL: Default cookie 'iris_secret' in production mode. "
                                 "Set IRIS_COOKIE or COOKIE= for cluster security."),
                    {error, default_cookie_in_production};
                _ -> ok
            end;
        _ -> ok
    end.

%% @doc Validate metrics_bearer_token in production mode.
%% Rejects placeholder value and missing token.
-spec validate_metrics_token() -> ok | {error, placeholder_metrics_token | metrics_token_missing}.
validate_metrics_token() ->
    case application:get_env(iris_core, deployment_mode, development) of
        production ->
            case application:get_env(iris_core, metrics_bearer_token, undefined) of
                undefined ->
                    logger:error("FATAL: metrics_bearer_token not set in production mode. "
                                 "Set metrics_bearer_token in config or remove metrics endpoint."),
                    {error, metrics_token_missing};
                <<"REPLACE_WITH_METRICS_TOKEN">> ->
                    logger:error("FATAL: metrics_bearer_token is still the default placeholder. "
                                 "Set a real token in production config."),
                    {error, placeholder_metrics_token};
                _Token ->
                    ok
            end;
        _ ->
            ok
    end.

%% Validate consistency mode.
%% CP is not implemented — fatal in ALL deployment modes (no silent fallback).
%% Unknown values are also rejected to prevent typos from degrading to AP.
-spec validate_consistency_mode() -> ok | {error, cp_not_implemented | {unknown_consistency_mode, term()}}.
validate_consistency_mode() ->
    case application:get_env(iris_core, consistency_mode, hardened_ap) of
        hardened_ap ->
            ok;
        cp ->
            logger:error("FATAL: consistency_mode=cp is NOT IMPLEMENTED. "
                         "Use hardened_ap. See docs/rfc/consistency-modes.md"),
            {error, cp_not_implemented};
        Unknown ->
            logger:error("FATAL: Unknown consistency_mode=~p. "
                         "Only hardened_ap is supported.", [Unknown]),
            {error, {unknown_consistency_mode, Unknown}}
    end.

%% Validate compression at startup.
%% Logs a warning if zstd is unavailable so operators know bandwidth
%% efficiency is degraded by 30-50%. Always returns ok (non-fatal).
-spec validate_compression_startup() -> ok.
validate_compression_startup() ->
    Algos = iris_compression:available_algorithms(),
    case lists:member(<<"zstd">>, Algos) of
        true ->
            logger:info("Compression: zstd + zlib available"),
            ok;
        false ->
            logger:warning("WARNING: zstd compression NOT available. "
                           "Only zlib is active. Bandwidth efficiency degraded 30-50%. "
                           "Install libzstd-dev and rebuild NIF to enable zstd."),
            ok
    end.

%% SEC-01: Runtime TLS certificate rotation.
%% Clears the OTP SSL PEM cache so the next TLS handshake reads fresh
%% cert/key files from disk. No listener restart required.
-spec reload_tls_config() -> ok.
reload_tls_config() ->
    ssl:clear_pem_cache(),
    iris_metrics:inc(tls_config_reload_count),
    logger:info("TLS PEM cache cleared — next handshake will use updated certificates"),
    ok.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Rationale: strategy 'one_for_one' is replaced with a logic-based hierarchy.
    %% We use secondary supervisors for batchers to isolate their crashes.
    
    %% rest_for_one ensures foundation services restart dependents
    %%
    %% Restart Intensity Risk Documentation
    %% ---------------------------------------------------
    %% The current rest_for_one strategy means that if an early child (e.g.
    %% iris_metrics) crashes, ALL subsequent children are restarted in order.
    %% With intensity=10/period=60, up to 10 cascading restarts per minute
    %% are tolerated before the supervisor itself terminates.
    %%
    %% Known risks:
    %%  1. A flapping foundation service triggers cascading restarts of ALL
    %%     higher-tier children (batchers, cluster join worker, etc.)
    %%  2. 10 restarts/60s may be too aggressive for production — consider
    %%     reducing to 5/60 after burn-in monitoring.
    %%
    %% Future work: Split into tiered supervisors (foundation_sup, messaging_sup,
    %% cluster_sup) so that a crash in messaging does not cascade into cluster
    %% infrastructure. See: OTP Design Principles — Supervisor Behaviour.
    %% Reduced from 10 to 7 restarts per 60s.
    %% rest_for_one means EVERY upstream crash restarts ALL downstream children,
    %% so we need more headroom than one_for_one supervisors. 7/60 balances:
    %%   - Cascade prevention (30% fewer restarts before supervisor gives up)
    %%   - Burst tolerance (survives transient storms under load)
    %% Edge supervisor uses 5/60 (one_for_one = no cascade risk).
    SupFlags = #{strategy => rest_for_one,
                 intensity => 7,
                 period => 60},

    %% Tiered sub-supervisors: each tier uses one_for_one internally,
    %% so a crash in one tier does not cascade into another.
    %% rest_for_one at the top level ensures foundation starts first.
    Children = [
        %% Tier 1: Foundation (metrics, health, mnesia_guard, flow_controller, dedup, presence, partition_guard)
        #{id => iris_foundation_sup,
          start => {iris_foundation_sup, start_link, []},
          type => supervisor,
          restart => permanent},

        %% Tier 2: Messaging (group, shard, keys, region_bridge, read_receipts, mailbox_guard, mailbox_monitor, efficiency_monitor)
        #{id => iris_messaging_sup,
          start => {iris_messaging_sup, start_link, []},
          type => supervisor,
          restart => permanent},

        %% Tier 3: Cluster (cluster_manager, durable_batcher_sup, core_registry, status_batcher_sup, join_worker, region_wiring)
        #{id => iris_cluster_sup,
          start => {iris_cluster_sup, start_link, []},
          type => supervisor,
          restart => permanent}
    ],

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
    SslConfigured = case init:get_argument(ssl_dist_optfile) of
        {ok, _} -> true;
        error -> false
    end,
    check_mtls_enforcement(SslConfigured).

%% @doc Testable variant: accepts whether SSL distribution is configured.
-spec check_mtls_enforcement(boolean()) -> ok.
check_mtls_enforcement(SslConfigured) ->
    Env = application:get_env(iris_core, env, undefined),
    Default = case Env of
        production -> true;
        _          -> false
    end,
    case application:get_env(iris_core, enforce_mtls, Default) of
        true ->
            case SslConfigured of
                true -> ok;
                false ->
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

-spec register_user(binary(), node(), pid()) -> ok | {error, term()}.
register_user(User, Node, Pid) ->
    %% Default to ETS for lockfree presence (was mnesia).
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

-spec lookup_user(binary()) -> {ok, node(), pid()} | not_found.
lookup_user(User) ->
    %% Default to ETS for lockfree lookup.
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

-spec store_offline(binary(), binary()) -> ok | {error, term()}.
store_offline(User, Msg) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store(User, Msg, Count).

%% Guaranteed durable store - use WAL + Async Replication
%% Old: mnesia:sync_transaction (Global Lock)
%% New: iris_durable_batcher (Local Disk WAL) -> Mnesia (Async)
%% For multimaster durability, use sync_transaction when cluster mode
%% RFC NFR-11: Server-side deduplication with 7-day window
%% RFC FR-5: FIFO ordering using client-provided sequence number
-spec store_offline_durable(binary(), binary()) -> ok | {error, term()}.
store_offline_durable(User, Msg) ->
    %% Mnesia memory backpressure gate (prevents OOM).
    %% This MUST be the first check — before inbox depth or any Mnesia read.
    case iris_mnesia_guard:is_memory_ok() of
        {error, memory_pressure} ->
            iris_metrics:inc(offline_store_backpressure_rejects),
            logger:warning("store_offline_durable: rejected due to memory pressure "
                           "(user=~s)", [User]),
            {error, memory_pressure};
        ok ->
            %% RFC Section 8: Inbox Size limit enforcement
            Depth = get_offline_queue_depth(User),
            MaxInbox = iris_limits:max_inbox_size(),
            case Depth >= MaxInbox of
                true ->
                    iris_metrics:inc(iris_inbox_full_rejected),
                    {error, inbox_full};
                false ->
                    %% Soft warning at 95% capacity — alert operators
                    %% before hard rejection so they can intervene (e.g. nudge user
                    %% to come online, or raise the limit).
                    case Depth >= trunc(MaxInbox * 0.95) of
                        true ->
                            logger:warning("inbox_near_capacity: user=~s depth=~B limit=~B (~B%)",
                                           [User, Depth, MaxInbox, trunc(Depth * 100 / MaxInbox)]),
                            iris_metrics:inc(inbox_near_capacity);
                        false ->
                            ok
                    end,
                    store_offline_durable_inner(User, Msg)
            end
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
            {make_dedup_key(User, Msg), RealMsg, SeqNo};
        _ ->
            {make_dedup_key(User, Msg), Msg, undefined}
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
                    %% Use sync_transaction to ensure replication BEFORE ACK
                    %% This is slower but guarantees RPO=0 even under SIGKILL
                    store_offline_sync_replicated(User, ActualMsg, Count, MaybeSeqNo);
                false ->
                    %% Use WAL for immediate durability (RPO=0) without global lock
                    %% RFC FR-5: Pass MaybeSeqNo for FIFO ordering
                    case iris_durable_batcher:store(User, ActualMsg, Count, MaybeSeqNo) of
                        ok -> ok;
                        {error, Reason} -> 
                            logger:error("WAL write failed for user ~p: ~p", [User, Reason]),
                            {error, durable_write_failed}
                    end
            end
    end.

%% Strong-hash dedup key generation (replaces phash2)
%% Uses truncated SHA-256 (64-bit) for collision resistance.
%% At 300 msgs/user: phash2 collision ~1:65K, sha256-64bit ~1:4B.
-spec make_dedup_key(binary(), term()) -> binary().
make_dedup_key(User, {SeqNo, RealMsg}) when is_integer(SeqNo) ->
    HashBin = binary:part(crypto:hash(sha256, term_to_binary(RealMsg)), 0, 8),
    HexHash = binary:encode_hex(HashBin),
    <<User/binary, ":", (integer_to_binary(SeqNo))/binary, ":", HexHash/binary>>;
make_dedup_key(User, Msg) ->
    HashBin = binary:part(crypto:hash(sha256, term_to_binary(Msg)), 0, 8),
    HexHash = binary:encode_hex(HashBin),
    <<User/binary, ":hash:", HexHash/binary>>.

%% Sync-replicated offline storage for multimaster durability
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
%% Paginated Offline Retrieval for Celebrity Hotspots
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

%% @doc Retrieve offline messages in paginated batches
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

%% @doc Delete offline messages after client confirms receipt
%% Call this AFTER client ACKs the page of messages.
%% FromCursor/ToCursor define the range of buckets to delete.
-spec delete_offline_confirmed(binary(), {non_neg_integer(), non_neg_integer()}) -> ok.
delete_offline_confirmed(User, {FromCursor, ToCursor}) ->
    Count = get_bucket_count(User),
    iris_offline_storage:delete_confirmed(User, Count, FromCursor, ToCursor).

get_bucket_count(User) ->
    HotResult = try mnesia:dirty_read(user_meta, User)
                catch C1:R1 ->
                    logger:warning("~p: dirty_read(user_meta, ~p) failed ~p:~p", [?MODULE, User, C1, R1]),
                    []
                end,
    case HotResult of
        [{user_meta, User, Count, _LastMod}] ->
            iris_storage_tier:touch(User),
            Count;
        [{user_meta, User, Count}] ->
            iris_storage_tier:touch(User),
            Count;  %% Legacy records
        _ ->
            %% Fall back to cold tier
            case iris_storage_tier:read_through(user_meta, user_meta_cold, User) of
                {ok, {_, User, Count, _LastMod}} ->
                    %% Promote back to hot tier for future fast access
                    iris_storage_tier:promote(user_meta, user_meta_cold, User),
                    Count;
                {ok, {_, User, Count}} ->
                    iris_storage_tier:promote(user_meta, user_meta_cold, User),
                    Count;
                not_found ->
                    1
            end
    end.

set_bucket_count(User, Count) ->
    %% Reject bucket count decreases to prevent
    %% data stranding — messages in higher-numbered buckets become invisible
    %% if BucketCount shrinks.
    CurrentCount = get_bucket_count(User),
    case Count < CurrentCount of
        true ->
            {error, {bucket_count_decrease, CurrentCount, Count}};
        false ->
            F = fun() -> mnesia:write({user_meta, User, Count, os:system_time(second)}) end,
            mnesia:transaction(F)
    end.

%% @doc Update a user's presence status.
%%
%% update_status(_User, online) is intentionally a no-op.
%% Online status is established exclusively via register_user/3, which
%% atomically writes the presence record with the user's node and pid.
%% Calling update_status(User, online) without a pid/node would create
%% an incomplete presence record, so we deliberately skip it here.
%% Callers wanting to mark a user as online MUST use register_user/3.
update_status(_User, online) -> ok;
update_status(User, offline) ->
    %% Unregister from correct backend
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
    %% Default to ETS for lockfree status lookup.
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
%% Return {error, not_found} for users that never existed
%% instead of an ambiguous zero-timestamp tuple.
-spec get_status_from_disk(binary()) -> {online, false, non_neg_integer()} | {error, not_found}.
get_status_from_disk(User) ->
    case mnesia:dirty_read(user_status, User) of
        [{user_status, User, LastSeen}] -> {online, false, LastSeen};
        [] -> {error, not_found}
    end.

%%%===================================================================
%%% Internal Functions (Hidden from External API)
%%%===================================================================

-spec init_db() -> ok.
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
%% Check for live peers before force_load_table to prevent
%% data divergence when peers hold newer data.
repair_failed_tables([]) -> ok;
repair_failed_tables([Table | Rest]) ->
    logger:info("Repairing table: ~p", [Table]),
    %% Check if peers have a copy we can sync from
    ActiveReplicas = mnesia:table_info(Table, active_replicas) -- [node()],
    case ActiveReplicas of
        [Peer | _] ->
            %% Peer has data — try to add a copy from the peer instead of force-loading
            logger:info("Table ~p: peer ~p has active replica, syncing from peer", [Table, Peer]),
            case mnesia:add_table_copy(Table, node(), disc_copies) of
                {atomic, ok} ->
                    logger:info("Table ~p synced from peer ~p", [Table, Peer]);
                {aborted, {already_exists, _, _}} ->
                    %% Already have a copy, just wait for sync
                    mnesia:wait_for_tables([Table], 10000);
                {aborted, Reason} ->
                    logger:warning("Table ~p sync from peer failed: ~p, falling back to force_load",
                                   [Table, Reason]),
                    force_load_isolated(Table)
            end;
        [] ->
            %% No peers available — we are isolated, force_load is our only option
            iris_metrics:inc(force_load_table_events),
            force_load_isolated(Table)
    end,
    repair_failed_tables(Rest).

%% Force-load a table when no peers are available (isolated node).
%% Emit metric + divergence warning since data may be stale.
%% H-2: Blocked in production to prevent loading stale/divergent data.
force_load_isolated(Table) ->
    case application:get_env(iris_core, deployment_mode, development) of
        production ->
            logger:error("BLOCKED: force_load_table(~p) refused in production. "
                         "Requires peer with active replica or manual backup restore.", [Table]),
            {error, blocked_in_production};
        _ ->
            force_load_isolated_inner(Table)
    end.

force_load_isolated_inner(Table) ->
    logger:warning("DATA DIVERGENCE RISK: force_load_table(~p) with no active peers. "
                   "Local data may be stale or divergent.", [Table]),
    case mnesia:force_load_table(Table) of
        yes ->
            logger:info("Table ~p force loaded (isolated)", [Table]),
            case mnesia:wait_for_tables([Table], 5000) of
                ok -> ok;
                _ ->
                    logger:warning("Table ~p force loaded but not usable. Recreating...", [Table]),
                    nuke_and_recreate_table(Table)
            end;
        ErrorOrNo ->
            logger:warning("Force load failed for ~p: ~p. Recreating table...", [Table, ErrorOrNo]),
            nuke_and_recreate_table(Table)
    end.

%% Completely destroy and recreate a corrupted table
%% Safety gate to prevent accidental data loss
%% Production mode blocks nuke unconditionally
nuke_and_recreate_table(Table) ->
    case application:get_env(iris_core, deployment_mode, development) of
        production ->
            logger:error("BLOCKED: nuke_and_recreate_table(~p) refused in production mode. "
                         "Restore from backup instead.", [Table]),
            exit({nuke_blocked_in_production, Table});
        _ -> ok
    end,
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
    try mnesia:delete_table(Table)
    catch Class:Reason ->
        logger:warning("mnesia:delete_table(~p) during nuke failed: ~p:~p (expected if table corrupted)",
                       [Table, Class, Reason])
    end,
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

%% =============================================================================
%% Single source of truth for Mnesia table definitions.
%% Returns {StorageType, Options} so callers only supply the node list.
%% =============================================================================
table_spec(presence) ->
    {ram_copies, [{attributes, [user, node, pid]}]};
table_spec(offline_msg) ->
    {disc_only_copies, [{attributes, [key, timestamp, msg]}, {type, bag}]};
table_spec(user_meta) ->
    {disc_copies, [{attributes, [user, bucket_count, last_modified]}]};
table_spec(user_status) ->
    {disc_copies, [{attributes, [user, last_seen]}]};
table_spec(revoked_tokens) ->
    {disc_copies, [{attributes, [jti, timestamp]}]};
table_spec(dedup_log) ->
    {disc_only_copies, [{attributes, [msg_id, timestamp]}, {type, set}]};
table_spec(refresh_tokens) ->
    {disc_copies, [{attributes, [token_id, user_id, family_id, used, created_at, expires_at]}, {type, set}]};
table_spec(user_blocks) ->
    {disc_copies, [{attributes, [key, blocker, blocked, created_at]}, {type, set}]};
table_spec(user_reports) ->
    {disc_copies, [{attributes, [id, reporter, reported, reason, created_at]}, {type, bag}]};

%% Cold-tier overflow for user_meta.
%% disc_only_copies keeps data on disc only (no keys in RAM).
%% Same schema as user_meta so records can be moved transparently.
table_spec(user_meta_cold) ->
    {disc_only_copies, [{attributes, [user, bucket_count, last_modified]}]}.

%% ---------------------------------------------------------------------------
%% TTL Cleanup — Purge expired entries from dedup_log, revoked_tokens,
%% and refresh_tokens. Called periodically (1-hour interval) or on demand.
%% Entries older than 7 days are purged from dedup_log and revoked_tokens.
%% Expired refresh_tokens are purged based on their expires_at field.
%% ---------------------------------------------------------------------------
-define(TTL_SECONDS, 7 * 86400).  %% 7 days

%% Return {ok, TotalDeleted} instead of ok, so callers see cleanup activity.
-spec cleanup_expired_entries() -> {ok, non_neg_integer()}.
cleanup_expired_entries() ->
    Cutoff = os:system_time(second) - ?TTL_SECONDS,
    N1 = cleanup_table_by_timestamp(dedup_log, 3, Cutoff),
    N2 = cleanup_table_by_timestamp(revoked_tokens, 3, Cutoff),
    N3 = cleanup_refresh_tokens_expired(),
    {ok, N1 + N2 + N3}.

%% Delete all records from Table where element at Position is < Cutoff.
%% Returns count of deleted records; logs on error instead of swallowing.
cleanup_table_by_timestamp(Table, TimestampPos, Cutoff) ->
    try
        case mnesia:transaction(fun() ->
            mnesia:foldl(fun(Record, Acc) ->
                case element(TimestampPos, Record) of
                    TS when is_integer(TS), TS < Cutoff ->
                        mnesia:delete_object(Record),
                        Acc + 1;
                    _ ->
                        Acc
                end
            end, 0, Table)
        end) of
            {atomic, Count} -> Count;
            {aborted, Reason} ->
                logger:warning("cleanup_table_by_timestamp(~p) aborted: ~p", [Table, Reason]),
                0
        end
    catch
        Class:Error ->
            logger:warning("cleanup_table_by_timestamp(~p) error: ~p:~p", [Table, Class, Error]),
            0
    end.

%% Delete refresh_tokens where expires_at is in the past.
%% Returns count of deleted records.
cleanup_refresh_tokens_expired() ->
    Now = os:system_time(second),
    try
        case mnesia:transaction(fun() ->
            mnesia:foldl(fun(Record, Acc) ->
                %% refresh_tokens: {refresh_tokens, token_id, user_id, family_id, used, created_at, expires_at}
                ExpiresAt = element(7, Record),
                case is_integer(ExpiresAt) andalso ExpiresAt < Now of
                    true ->
                        mnesia:delete_object(Record),
                        Acc + 1;
                    false -> Acc
                end
            end, 0, refresh_tokens)
        end) of
            {atomic, Count} -> Count;
            {aborted, Reason} ->
                logger:warning("cleanup_refresh_tokens_expired aborted: ~p", [Reason]),
                0
        end
    catch
        Class:Error ->
            logger:warning("cleanup_refresh_tokens_expired error: ~p:~p", [Class, Error]),
            0
    end.

%% Recreate a single table with its original definition (recovery path)
recreate_table(Table) ->
    case erlang:function_exported(?MODULE, table_spec, 1) andalso
         (catch table_spec(Table)) of
        {StorageType, Opts} ->
            mnesia:create_table(Table, [{StorageType, [node()]} | Opts]);
        _ ->
            logger:error("Unknown table to recreate: ~p", [Table])
    end.

%% Internal: Create tables (only called when seeding)
create_tables(Nodes) ->
    AllTables = [presence, offline_msg, user_meta, user_meta_cold, user_status,
                 revoked_tokens, dedup_log, refresh_tokens,
                 user_blocks, user_reports],
    lists:foreach(fun(Table) ->
        {StorageType, Opts} = table_spec(Table),
        mnesia:create_table(Table, [{StorageType, Nodes} | Opts])
    end, AllTables),
    mnesia:wait_for_tables(AllTables, 5000),
    logger:info("Tables created.").

%% Legacy wrapper for specific node lists (unused now but kept for API compat)
-spec init_db([node()]) -> ok.
init_db(_Nodes) ->
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
            
            %% Replicate offline_msg table (disc_only_copies for durability/RPO=0)
            replicate_table(offline_msg, disc_only_copies, CoreNodes),
            
            %% Replicate user_status table (ram_copies)
            replicate_table(user_status, ram_copies, CoreNodes),
            
            %% Replicate user_meta table (disc_copies)
            replicate_table(user_meta, disc_copies, CoreNodes),
            
            %% Replicate dedup_log table (disc_only_copies for RFC NFR-11 dedup persistence)
            replicate_table(dedup_log, disc_only_copies, CoreNodes),
            
            %% Replicate revoked_tokens table (disc_copies for auth revocation)
            replicate_table(revoked_tokens, disc_copies, CoreNodes),
            
            %% Replicate cross-region bridge tables (disc_copies for RPO=0 during partitions)
            %% These tables store messages queued for delivery to partitioned regions
            replicate_table(cross_region_outbound, disc_copies, CoreNodes),
            replicate_table(cross_region_dead_letter, disc_copies, CoreNodes),
            
            %% Replicate e2ee_key_bundle table (disc_copies for durability)
            %% Key bundles MUST survive node failure (99.999% durability, same as messages).
            %% Without replication, a SIGKILL of the primary loses all key bundles.
            replicate_table(e2ee_key_bundle, disc_copies, CoreNodes),
            
            %% Replicate key_contact table (disc_copies for key change notifications)
            replicate_table(key_contact, disc_copies, CoreNodes),
            
            logger:info("Cross-region replication initialized successfully"),
            ok
    end.

%%%===================================================================
%%% Post-Partition Reconciliation (RFC 7.1.1)
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
    %% Refuse reconciliation while still diverged
    case iris_partition_guard:get_status() of
        #{mode := diverged} ->
            logger:warning("Reconciliation aborted: partition guard still in diverged mode"),
            {error, no_quorum};
        _ ->
            do_reconcile_after_partition()
    end.

do_reconcile_after_partition() ->
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

%% @doc Cursor-based batched reconciliation.
%% Replaces the O(N) dirty_match_object which fetches ALL records into RAM.
%% Iterates remote keys in batches using dirty_first/dirty_next.
-spec reconcile_batch(node(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
reconcile_batch(RemoteNode, BatchSize) when is_atom(RemoteNode), is_integer(BatchSize), BatchSize > 0 ->
    logger:info("Batched reconciliation from ~p (batch_size=~p)", [RemoteNode, BatchSize]),
    try
        FirstKey = rpc:call(RemoteNode, mnesia, dirty_first, [offline_msg], 5000),
        case FirstKey of
            {badrpc, Reason} ->
                logger:warning("Failed to read first key from ~p: ~p", [RemoteNode, Reason]),
                {error, Reason};
            '$end_of_table' ->
                logger:info("Remote node ~p has no offline_msg records", [RemoteNode]),
                {ok, 0};
            _ ->
                Merged = reconcile_batch_loop(RemoteNode, FirstKey, BatchSize, 0),
                logger:info("Batched reconciliation from ~p complete (~p merged)", [RemoteNode, Merged]),
                {ok, Merged}
        end
    catch
        _:Error ->
            logger:warning("Error during batched reconciliation from ~p: ~p", [RemoteNode, Error]),
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
                %% Transaction for reconciliation durability
                case Missing of
                    [] -> ok;
                    _ ->
                        {atomic, ok} = mnesia:transaction(fun() ->
                            lists:foreach(fun(Record) ->
                                mnesia:write(Record)
                            end, Missing)
                        end)
                end,
                Count + length(Missing);
            _ -> Count
        end
    end, 0, Keys).

%% Merge offline_msg records from a remote node into local table.
%% Now delegates to reconcile_batch/2 for cursor-based iteration.
merge_offline_msg_from(RemoteNode) ->
    logger:info("Reconciliation: reading offline_msg from ~p (batched)", [RemoteNode]),
    reconcile_batch(RemoteNode, 1000).

%% @doc Generic table reconciliation (RFC 7.1.1).
%% Cursor-based batched union merge for any Mnesia table.
%% Works for: presence, group_member, e2ee_key_bundle.
-spec reconcile_table(node(), atom(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
reconcile_table(RemoteNode, Table, BatchSize) when is_atom(RemoteNode), is_atom(Table), BatchSize > 0 ->
    logger:info("Reconciling table ~p from ~p (batch_size=~p)", [Table, RemoteNode, BatchSize]),
    try
        %% Check if table exists on remote node
        case rpc:call(RemoteNode, mnesia, table_info, [Table, size], 5000) of
            {badrpc, _Reason} ->
                logger:info("Table ~p not available on ~p, skipping", [Table, RemoteNode]),
                {ok, 0};
            _Size ->
                FirstKey = rpc:call(RemoteNode, mnesia, dirty_first, [Table], 5000),
                case FirstKey of
                    {badrpc, Reason2} ->
                        logger:warning("Failed to read first key of ~p from ~p: ~p",
                                       [Table, RemoteNode, Reason2]),
                        {error, Reason2};
                    '$end_of_table' ->
                        logger:info("Remote ~p table ~p is empty", [RemoteNode, Table]),
                        {ok, 0};
                    _ ->
                        Merged = reconcile_table_loop(RemoteNode, Table, FirstKey, BatchSize, 0),
                        logger:info("Reconciled ~p records from ~p:~p", [Merged, RemoteNode, Table]),
                        {ok, Merged}
                end
        end
    catch
        _:Error ->
            logger:warning("Error reconciling ~p from ~p: ~p", [Table, RemoteNode, Error]),
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
                %% Use conflict-aware merge instead of blind write.
                %% For bag tables (offline_msg): union merge (write missing).
                %% For set tables: timestamp-aware LWW (see should_overwrite/3).
                %% e2ee_key_bundle is set type, uses updated_at LWW.
                TableType = try mnesia:table_info(Table, type)
                            catch Class:Reason ->
                                logger:warning("iris_core: table_info(~p, type) failed: ~p:~p, defaulting to set", [Table, Class, Reason]),
                                set
                            end,
                WrittenCount = case TableType of
                    bag ->
                        %% Append-only / bag: union merge (original logic)
                        %% Transaction for reconciliation durability
                        LocalSet = sets:from_list(LocalRecords),
                        Missing = [R || R <- Records, not sets:is_element(R, LocalSet)],
                        case Missing of
                            [] -> ok;
                            _ ->
                                {atomic, ok} = mnesia:transaction(fun() ->
                                    lists:foreach(fun(Record) ->
                                        mnesia:write(Table, Record, write)
                                    end, Missing)
                                end)
                        end,
                        length(Missing);
                    _ ->
                        %% set / ordered_set: timestamp-aware conflict resolution
                        merge_set_records(Table, Records, LocalRecords)
                end,
                Count + WrittenCount;
            _ -> Count
        end
    end, 0, Keys).

%% Merge set-type table records with timestamp-aware conflict resolution.
%% For each remote record, check if a local record exists with the same key.
%% If local exists: overwrite only if remote is newer (should_overwrite).
%% If local absent: write the remote record.
merge_set_records(Table, RemoteRecords, LocalRecords) ->
    %% Build a map of local records by key (element 2 is the key field in records)
    LocalMap = maps:from_list([{element(2, R), R} || R <- LocalRecords]),
    %% Collect records to write, then write in a single transaction
    ToWrite = lists:filter(fun(RemoteRec) ->
        RemoteKey = element(2, RemoteRec),
        case maps:get(RemoteKey, LocalMap, undefined) of
            undefined -> true;
            LocalRec -> should_overwrite(Table, RemoteRec, LocalRec)
        end
    end, RemoteRecords),
    case ToWrite of
        [] -> 0;
        _ ->
            {atomic, ok} = mnesia:transaction(fun() ->
                lists:foreach(fun(Rec) ->
                    mnesia:write(Table, Rec, write)
                end, ToWrite)
            end),
            length(ToWrite)
    end.

%% Determine if a remote record should overwrite a local record.
%% Implements per-table conflict resolution strategy (RFC 7.1.1):
%%   - group_member: Compare last_seen timestamps (LWW)
%%   - user_status: Compare last_seen timestamps (LWW)
%%   - user_meta: Compare last_modified timestamps (LWW)
%%   - presence: Keep local (ephemeral, local is authoritative)
%%   - Default: Keep local (conservative)
should_overwrite(group_member, RemoteRec, LocalRec) ->
    %% last_seen is the 6th element (#group_member.last_seen)
    RemoteTS = element(6, RemoteRec),
    LocalTS = element(6, LocalRec),
    RemoteTS > LocalTS;
should_overwrite(user_status, RemoteRec, LocalRec) ->
    %% LWW: last_seen is element 3: {user_status, User, LastSeen}
    element(3, RemoteRec) > element(3, LocalRec);
should_overwrite(user_meta, RemoteRec, LocalRec) ->
    %% LWW: last_modified is element 4: {user_meta, User, BucketCount, LastModified}
    %% Guard against legacy records without last_modified (tuple_size == 3)
    case {tuple_size(RemoteRec), tuple_size(LocalRec)} of
        {4, 4} -> element(4, RemoteRec) > element(4, LocalRec);
        {4, 3} -> true;   %% New-format remote beats legacy local
        _ -> false         %% Legacy or equal — keep local
    end;
should_overwrite(presence, _RemoteRec, _LocalRec) ->
    %% Presence is ram_copies, ephemeral. Local is authoritative.
    false;
should_overwrite(e2ee_key_bundle, RemoteRec, LocalRec) ->
    %% RFC 7.1.1 says "Key bundles: Union (all bundles are valid)."
    %% Since e2ee_key_bundle is a set table (one record per user_id),
    %% "union" means keep the record with the most recent updated_at.
    %% updated_at is the 9th element of the key_bundle record.
    RemoteUpdatedAt = element(9, RemoteRec),
    LocalUpdatedAt = element(9, LocalRec),
    RemoteUpdatedAt > LocalUpdatedAt;
should_overwrite(_Table, _RemoteRec, _LocalRec) ->
    %% Conservative default: keep local record
    false.

%% Helper: Check if a node is a core node.
%% Config-based role assignment with naming convention fallback.
%% Supports K8s, IP-based naming, and any non-standard naming schema.
is_core_node(Node) ->
    case application:get_env(iris_core, node_role) of
        {ok, core} -> true;
        {ok, _Other} -> false;
        undefined ->
            %% Fallback: naming convention (backward compat)
            NodeStr = atom_to_list(Node),
            lists:prefix("core", NodeStr) orelse lists:prefix("iris_core", NodeStr)
    end.

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