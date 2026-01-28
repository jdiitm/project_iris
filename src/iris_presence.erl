-module(iris_presence).

%% =============================================================================
%% ETS-Backed Regional Presence Manager
%% =============================================================================
%% Per PRINCIPAL_AUDIT_REPORT.md Hard Stop #1:
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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, presence_local).
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
-spec register(binary(), node(), pid()) -> ok.
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
    true = ets:insert(?ETS_TABLE, {User, Entry}),
    %% Async broadcast to other nodes (fire-and-forget)
    spawn(fun() -> broadcast_update(User, Node, Pid) end),
    ok.

%% @doc Unregister a user's presence
-spec unregister(binary()) -> ok.
unregister(User) ->
    ets:delete(?ETS_TABLE, User),
    spawn(fun() -> broadcast_removal(User) end),
    ok.

%% @doc Lookup a user's presence (Cluster-aware: RPC to Shard Owner)
-spec lookup(binary()) -> {ok, node(), pid()} | {error, not_found | expired}.
lookup(User) ->
    %% 1. Determine Shard Owner
    ShardId = iris_shard:get_shard(User),
    Nodes = iris_shard:get_shard_nodes(ShardId),
    lookup_any_node(Nodes, User).

%% @doc Lookup user on specific nodes (try until success)
lookup_any_node([], _User) -> {error, not_found};
lookup_any_node([Node | Rest], User) ->
    if Node =:= node() ->
           %% Local optimization
           case lookup_local(User) of
               {error, not_found} -> lookup_any_node(Rest, User);
               Result -> Result
           end;
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
    %% AUDIT FIX: Shard-aware routing (Limit broadcast to shard owners)
    ShardId = iris_shard:get_shard(User),
    Members = iris_shard:get_shard_nodes(ShardId),
    
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
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Create public ETS table for lockfree access
    %% - public: any process can read/write
    %% - set: key-value with unique keys
    %% - {write_concurrency, true}: optimized for concurrent writes
    %% - {read_concurrency, true}: optimized for concurrent reads
    ?ETS_TABLE = ets:new(?ETS_TABLE, [
        named_table,
        public,
        set,
        {keypos, 1},
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    
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

%% =============================================================================
%% Internal Functions
%% =============================================================================


