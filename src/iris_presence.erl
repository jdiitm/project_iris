-module(iris_presence).

%% =============================================================================
%% ETS-Backed Regional Presence Manager
%% =============================================================================
%% Replace mnesia:transaction for presence with lockfree ETS.
%%
%% Design:
%% - Local writes to ETS (lockfree, ~1μs)
%% - Async broadcast to other regions via gen_server:cast
%% - Eventual consistency acceptable (user appears online within 1s)
%% - Heartbeat-based expiration (users offline after 30s no heartbeat)
%%
%% This eliminates the global Mnesia lock bottleneck that limited
%% the system to ~10,000 tx/sec.
%% =============================================================================

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/3, unregister/1, lookup/1, lookup_local/1, heartbeat/1]).
-export([get_all_local/0, get_stats/0]).
-export([broadcast_update/3, broadcast_removal/1]).
%% PS-2: Presence privacy (RFC-001 v4.0 FR-8a)
-export([set_privacy/2, lookup_with_privacy/2, add_contact/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, presence_local).
-define(PRIVACY_TABLE, presence_privacy).     %% PS-2: {UserId, Level}
-define(CONTACTS_TABLE, presence_contacts).   %% PS-2: {UserId, ContactId}
-define(HEARTBEAT_INTERVAL_MS, 10000).  %% 10 seconds
-define(EXPIRY_THRESHOLD_MS, 30000).    %% 30 seconds without heartbeat = offline
-define(CLEANUP_INTERVAL_MS, 5000).     %% Cleanup check every 5 seconds

-record(state, {
    cleanup_timer :: reference() | undefined,
    stats = #{} :: map()
}).

-record(presence_entry, {
    user :: binary(),
    node :: node(),
    pid :: pid(),
    timestamp :: integer(),  %% erlang:system_time(millisecond)
    last_heartbeat :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a user's presence (lockfree ETS insert)
%% This is the hot path - must be as fast as possible
-spec register(binary(), node(), pid()) -> ok | {error, table_unavailable}.
register(User, Node, Pid) ->
    Now = erlang:system_time(millisecond),
    Entry = #presence_entry{
        user = User,
        node = Node,
        pid = Pid,
        timestamp = Now,
        last_heartbeat = Now
    },
    %% Lockfree ETS insert - O(1), ~1μs
    %% Defensive: if the table is temporarily unavailable (supervisor restart
    %% window under rest_for_one), return error instead of crashing the caller.
    try
        true = ets:insert(?ETS_TABLE, {User, Entry}),
        iris_async:spawn_monitored(presence_broadcast_update, fun() -> broadcast_update(User, Node, Pid) end),
        ok
    catch
        error:badarg ->
            logger:warning("presence_local table unavailable, registration deferred for ~p", [User]),
            {error, table_unavailable}
    end.

%% @doc Unregister a user's presence
-spec unregister(binary()) -> ok.
unregister(User) ->
    try ets:delete(?ETS_TABLE, User) catch error:badarg -> ok end,
    iris_async:spawn_monitored(presence_broadcast_removal, fun() -> broadcast_removal(User) end),
    ok.


%% @doc Lookup a user's presence (Cluster-aware: RPC to Shard Owner)
%% Strategy: Check local (fast/consistent for local users) -> Check Shard Owner (authoritative)
%% FIX: Falls back to all cluster nodes when shard has no assigned nodes
-spec lookup(binary()) -> {ok, node(), pid()} | {error, not_found | expired}.
lookup(User) ->
    %% 1. Local Optimization (Read your own write)
    case lookup_local(User) of
        {ok, Node, Pid} -> {ok, Node, Pid};
        _ ->
            %% 2. Remote Lookup (RPC to Shard Owner)
            ShardId = iris_shard:get_shard(User),
            ShardNodes = iris_shard:get_shard_nodes(ShardId),
            case ShardNodes of
                [] ->
                    %% FIX: Fallback to all cluster nodes when shard is unassigned
                    %% This handles cases where not all shards have assigned nodes
                    AllNodes = get_all_cluster_nodes(),
                    lookup_any_node(AllNodes, User);
                Nodes ->
                    lookup_any_node(Nodes, User)
            end
    end.

%% @doc Get all nodes in the cluster (for fallback when shard is unassigned)
get_all_cluster_nodes() ->
    %% Get Mnesia db_nodes as authoritative cluster membership
    %% Mnesia may not be started yet during early startup
    try mnesia:system_info(running_db_nodes) of
        Nodes when is_list(Nodes) -> 
            %% Exclude self since we already checked local
            [N || N <- Nodes, N =/= node()];
        _ -> 
            %% Fallback to nodes()
            nodes()
    catch C:R ->
        logger:warning("Presence remote node lookup failed (~p:~p), using nodes()", [C, R]),
        nodes()
    end.

%% @doc Lookup user on specific nodes (try until success)
lookup_any_node([], _User) -> {error, not_found};
lookup_any_node([Node | Rest], User) ->
    if Node =:= node() ->
           %% Already checked local in step 1, but technically we could check again or skip.
           %% For simplicity, we just skip (assuming step 1 was sufficient)
           lookup_any_node(Rest, User);
       true ->
           %% Remote RPC
           case rpc:call(Node, ?MODULE, lookup_local, [User], 1000) of
               {ok, N, P} -> {ok, N, P};
               _ -> lookup_any_node(Rest, User)
           end
    end.

%% @doc Lookup a user's presence (Local ETS only)
-spec lookup_local(binary()) -> {ok, node(), pid()} | {error, not_found | expired}.
lookup_local(User) ->
    try
        case ets:lookup(?ETS_TABLE, User) of
            [{User, Entry}] ->
                %% Check if entry is expired
                Now = erlang:system_time(millisecond),
                Age = Now - Entry#presence_entry.last_heartbeat,
                if
                    Age > ?EXPIRY_THRESHOLD_MS ->
                        %% Entry expired - remove it
                        ets:delete(?ETS_TABLE, User),
                        {error, expired};
                    true ->
                        {ok, Entry#presence_entry.node, Entry#presence_entry.pid}
                end;
            [] ->
                {error, not_found}
        end
    catch
        error:badarg -> {error, not_found}
    end.

%% @doc Update heartbeat timestamp for a user
-spec heartbeat(binary()) -> ok | {error, not_found}.
heartbeat(User) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?ETS_TABLE, User) of
        [{User, Entry}] ->
            NewEntry = Entry#presence_entry{last_heartbeat = Now},
            ets:insert(?ETS_TABLE, {User, NewEntry}),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc Get all local presence entries (for debugging/sync)
-spec get_all_local() -> [{binary(), node(), pid()}].
get_all_local() ->
    ets:foldl(fun({User, Entry}, Acc) ->
        [{User, Entry#presence_entry.node, Entry#presence_entry.pid} | Acc]
    end, [], ?ETS_TABLE).

%% @doc Get presence stats
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Broadcast presence update to other nodes
-spec broadcast_update(binary(), node(), pid()) -> ok.
broadcast_update(User, Node, Pid) ->
    %% Shard-aware routing (Limit broadcast to shard owners)
    ShardId = iris_shard:get_shard(User),
    Members = iris_shard:get_shard_nodes(ShardId),
    logger:info("DEBUG: broadcast_update ~p -> Shard ~p -> Nodes ~p", [User, ShardId, Members]),
    
    %% Cast to valid members (fire-and-forget, async)
    lists:foreach(fun(Member) ->
        gen_server:cast({?SERVER, Member}, {presence_update, User, Node, Pid})
    end, Members),
    ok.

%% @doc Broadcast presence removal to other nodes
-spec broadcast_removal(binary()) -> ok.
broadcast_removal(User) ->
    ShardId = iris_shard:get_shard(User),
    Members = iris_shard:get_shard_nodes(ShardId),
    
    lists:foreach(fun(Member) ->
        gen_server:cast({?SERVER, Member}, {presence_remove, User})
    end, Members),
    ok.

%% =============================================================================
%% PS-2: Presence Privacy API (RFC-001 v4.0 FR-8a)
%% =============================================================================

%% @doc Set privacy level for a user: everyone | contacts | nobody
-spec set_privacy(binary(), everyone | contacts | nobody) -> ok.
set_privacy(UserId, Level) when Level =:= everyone; Level =:= contacts; Level =:= nobody ->
    ets:insert(?PRIVACY_TABLE, {UserId, Level}),
    ok.

%% @doc Add a contact for privacy-aware presence queries
-spec add_contact(binary(), binary()) -> ok.
add_contact(UserId, ContactId) ->
    ets:insert(?CONTACTS_TABLE, {UserId, ContactId}),
    ok.

%% @doc Lookup presence with privacy check
-spec lookup_with_privacy(binary(), binary()) -> {ok, term()} | {ok, unavailable}.
lookup_with_privacy(UserId, RequesterId) ->
    Level = case ets:lookup(?PRIVACY_TABLE, UserId) of
        [{UserId, L}] -> L;
        [] -> everyone
    end,
    case Level of
        everyone ->
            lookup(UserId);
        nobody ->
            {ok, unavailable};
        contacts ->
            IsContact = case ets:match(?CONTACTS_TABLE, {UserId, RequesterId}) of
                [_|_] -> true;
                [] -> false
            end,
            case IsContact of
                true -> lookup(UserId);
                false -> {ok, unavailable}
            end
    end.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Create public ETS table for lockfree access
    %% - public: any process can read/write
    %% - set: key-value with unique keys
    %% - {write_concurrency, true}: optimized for concurrent writes
    %% - {read_concurrency, true}: optimized for concurrent reads
    %% Defensive: reuse table if it survived a restart (e.g. via rest_for_one cascade).
    %% Without this, ets:new crashes with badarg if the table already exists from
    %% a previous instance that was stopped but whose table was reclaimed.
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [
                named_table,
                public,
                set,
                {keypos, 1},
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end,

    %% PS-2: Privacy level table {UserId, Level :: everyone|contacts|nobody}
    case ets:info(?PRIVACY_TABLE) of
        undefined ->
            ets:new(?PRIVACY_TABLE, [named_table, public, set,
                                     {read_concurrency, true}]);
        _ -> ok
    end,
    %% PS-2: Contacts table {UserId, ContactId}
    case ets:info(?CONTACTS_TABLE) of
        undefined ->
            ets:new(?CONTACTS_TABLE, [named_table, public, bag,
                                      {read_concurrency, true}]);
        _ -> ok
    end,
    
    %% Start cleanup timer
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    
    logger:info("iris_presence started with ETS-backed lockfree presence"),
    
    {ok, #state{
        cleanup_timer = TimerRef,
        stats = #{
            registers => 0,
            unregisters => 0,
            lookups => 0,
            expirations => 0,
            broadcasts_sent => 0,
            broadcasts_received => 0
        }
    }}.

handle_call(get_stats, _From, State) ->
    %% Add current table size to stats
    TableSize = ets:info(?ETS_TABLE, size),
    Stats = maps:merge(State#state.stats, #{table_size => TableSize}),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({presence_update, User, Node, Pid}, State) ->
    %% Received broadcast from another node - update local ETS
    Now = erlang:system_time(millisecond),
    Entry = #presence_entry{
        user = User,
        node = Node,
        pid = Pid,
        timestamp = Now,
        last_heartbeat = Now
    },
    ets:insert(?ETS_TABLE, {User, Entry}),
    
    %% Update stats
    NewStats = maps:update_with(broadcasts_received, fun(V) -> V + 1 end, 1, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast({presence_remove, User}, State) ->
    %% Received removal broadcast from another node
    ets:delete(?ETS_TABLE, User),
    NewStats = maps:update_with(broadcasts_received, fun(V) -> V + 1 end, 1, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    %% Periodic cleanup of expired entries
    Now = erlang:system_time(millisecond),
    Threshold = Now - ?EXPIRY_THRESHOLD_MS,
    
    %% Find and delete expired entries
    Expired = ets:foldl(fun({User, Entry}, Acc) ->
        if
            Entry#presence_entry.last_heartbeat < Threshold ->
                [User | Acc];
            true ->
                Acc
        end
    end, [], ?ETS_TABLE),
    
    %% Delete expired entries
    lists:foreach(fun(User) ->
        ets:delete(?ETS_TABLE, User)
    end, Expired),
    
    %% Update stats
    ExpiredCount = length(Expired),
    NewStats = if
        ExpiredCount > 0 ->
            maps:update_with(expirations, fun(V) -> V + ExpiredCount end, ExpiredCount, State#state.stats);
        true ->
            State#state.stats
    end,
    
    if
        ExpiredCount > 0 ->
            logger:debug("Cleaned up ~p expired presence entries", [ExpiredCount]);
        true ->
            ok
    end,
    
    %% Reschedule cleanup
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    
    {noreply, State#state{cleanup_timer = TimerRef, stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel cleanup timer
    case State#state.cleanup_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================


