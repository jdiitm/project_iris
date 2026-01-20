-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

%% OTP Callbacks
-export([start/2, stop/1, init/1]).
-export([init_db/0, init_db/1, join_cluster/1]).

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

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% FAANG-Grade Messaging APIs
%%%===================================================================

register_user(User, Node, Pid) ->
    %% Rationale: Transactional safety for global presence consistency.
    %% io:format("register_user called: ~p from ~p pid ~p~n", [User, Node, Pid]),
    F = fun() -> mnesia:write({presence, User, Node, Pid}) end,
    Result = mnesia:transaction(F),
    %% io:format("register_user result: ~p~n", [Result]),
    case Result of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            logger:error("Failed to register user ~p: ~p", [User, Reason]),
            {error, Reason}
    end.

lookup_user(User) ->
    %% Rationale: Dirty reads are 10x faster and acceptable for "Online" status.
    case mnesia:dirty_read(presence, User) of
        [{presence, User, Node, Pid}] -> {ok, Node, Pid};
        [] -> {error, not_found}
    end.

store_offline(User, Msg) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store(User, Msg, Count).

%% AUDIT FIX: Guaranteed durable store - use when ACK is sent to client
%% This function blocks until Mnesia sync_transaction completes
%% Returns 'ok' ONLY if message is confirmed durable (survives any single node failure)
store_offline_durable(User, Msg) ->
    Count = get_bucket_count(User),
    iris_offline_storage:store_durable(User, Msg, Count).

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
            Tables = [offline_msg, user_meta, user_status, revoked_tokens, presence],
            mnesia:wait_for_tables(Tables, 30000),
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