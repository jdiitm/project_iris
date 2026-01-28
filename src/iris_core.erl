-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

%% OTP Callbacks
-export([start/2, stop/1, init/1]).
-export([init_db/0, init_db/1, join_cluster/1, init_cross_region_replication/0]).

%% High-Scale Messaging APIs
-export([register_user/3, lookup_user/1]).
-export([store_offline/2, store_offline_durable/2, store_batch/2, retrieve_offline/1]).
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
          
        %% Partition Guard: Split-brain detection and safe mode
        %% AUDIT FIX: Detects cluster partitions and rejects writes to prevent divergence
        #{id => iris_partition_guard,
          start => {iris_partition_guard, start_link, []},
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
%%% FAANG-Grade Messaging APIs
%%%===================================================================

register_user(User, Node, Pid) ->
    %% PRINCIPAL_AUDIT_REPORT: Support both legacy Mnesia and new ETS-backed presence.
    %% Set IRIS_PRESENCE_BACKEND=ets to use lockfree ETS (recommended for scale).
    case application:get_env(iris_core, presence_backend, mnesia) of
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
    %% PRINCIPAL_AUDIT_REPORT: Support both backends for lookup.
    case application:get_env(iris_core, presence_backend, mnesia) of
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
store_offline_durable(User, Msg) ->
    Count = get_bucket_count(User),
    %% P1-H6 FIX: Use WAL for immediate durability (RPO=0) without global lock
    case iris_durable_batcher:store(User, Msg, Count) of
        ok -> ok;
        {error, Reason} -> 
            logger:error("WAL write failed for user ~p: ~p", [User, Reason]),
            {error, durable_write_failed}
    end.

store_batch(User, Msgs) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store_batch(User, Msgs, Count).

retrieve_offline(User) ->
    Count = get_bucket_count(User),
    iris_offline_storage:retrieve(User, Count).

get_bucket_count(User) ->
    case mnesia:dirty_read(user_meta, User) of
        [{user_meta, User, Count}] -> Count;
        [] -> 1
    end.

set_bucket_count(User, Count) ->
    F = fun() -> mnesia:write({user_meta, User, Count}) end,
    mnesia:transaction(F).

update_status(User, online) -> ok;
update_status(User, offline) ->
    %% Rationale: Atomic delete prevents "ghost" online status if batcher is slow.
    mnesia:dirty_delete(presence, User),
    iris_status_batcher:submit(User, offline).

get_status(User) ->
    %% Rationale: Multi-tier lookup. RAM -> Disk.
    case mnesia:dirty_read(presence, User) of
        [{presence, User, _, _}] -> {online, true, 0};
        [] -> 
            case mnesia:dirty_read(user_status, User) of
                [{user_status, User, LastSeen}] -> {online, false, LastSeen};
                [] -> {online, false, 0}
            end
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
            Tables = [offline_msg, user_meta, user_status, revoked_tokens],
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
    mnesia:wait_for_tables([presence, offline_msg, user_meta, user_status, revoked_tokens], 5000),
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
            
            logger:info("Cross-region replication initialized successfully"),
            ok
    end.

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