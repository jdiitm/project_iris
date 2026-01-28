-module(iris_shard).

%% =============================================================================
%% User Sharding Module
%% =============================================================================
%% Purpose: Consistent user-to-shard mapping for horizontal scaling.
%% Design:
%% 1. Configurable shard count (power of 2 recommended)
%% 2. Consistent hashing using phash2 (stable across node restarts)
%% 3. Virtual nodes for better distribution
%% 4. Shard membership discovery via pg groups
%% =============================================================================

-export([start_link/0]).
-export([get_shard/1, get_shard_node/1, get_shard_nodes/1, jump_hash/2]).
-export([get_shard_count/0, set_shard_count/1]).
-export([join_shard/1, leave_shard/1]).
-export([get_all_shards/0, get_local_shards/0]).
-export([get_shard_stats/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_SHARD_COUNT, 4096).       %% Default number of shards
-define(PG_SCOPE, iris_shards).         %% pg scope for shard groups
-define(SHARD_PREFIX, iris_shard_).     %% Prefix for shard group names

-record(state, {
    shard_count :: integer(),
    local_shards = [] :: list(),        %% Shards this node owns
    shard_map = #{} :: map()            %% Shard ID => [Nodes]
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get shard ID for a user (0 to shard_count-1)
-spec get_shard(binary()) -> integer().
get_shard(User) when is_binary(User) ->
    ShardCount = get_shard_count(),
    %% AUDIT2 FIX: consistent hashing (Jump Hash) instead of modulo
    Hash = erlang:phash2(User),
    jump_hash(Hash, ShardCount);
get_shard(User) when is_list(User) ->
    get_shard(list_to_binary(User)).

%% Jump Consistent Hash (Google 2014)
%% Maps a key to a bucket in [0, NumBuckets-1]
%% Stable mapping: Increasing NumBuckets only moves 1/N keys
jump_hash(Key, NumBuckets) ->
    jump_hash_loop(Key, NumBuckets, -1, 0).

jump_hash_loop(_Key, NumBuckets, B, J) when J >= NumBuckets ->
    B;
jump_hash_loop(Key, NumBuckets, _B, J) ->
    NewB = J,
    %% Linear Congruential Generator step
    Key1 = (Key * 2862933555777941757 + 1) band 16#FFFFFFFFFFFFFFFF,
    %% J = (B + 1) * (2^31 / ((Key >> 33) + 1))
    %% Note: Erlang / is float division
    NextJFloat = (NewB + 1) * (2147483648.0 / ((Key1 bsr 33) + 1)),
    NextJ = trunc(NextJFloat),
    jump_hash_loop(Key1, NumBuckets, NewB, NextJ).

%% @doc Get the primary node for a user's shard
-spec get_shard_node(binary()) -> {ok, node()} | {error, no_nodes}.
get_shard_node(User) ->
    ShardId = get_shard(User),
    case get_shard_nodes(ShardId) of
        [] -> {error, no_nodes};
        [Primary | _] -> {ok, Primary}
    end.

%% @doc Get all nodes serving a specific shard
-spec get_shard_nodes(integer()) -> [node()].
get_shard_nodes(ShardId) ->
    GroupName = shard_group_name(ShardId),
    case pg:get_members(?PG_SCOPE, GroupName) of
        [] -> 
            %% No members in pg, try local lookup
            gen_server:call(?SERVER, {get_shard_nodes, ShardId});
        Members ->
            %% AUDIT2 FIX (Issue 9): Randomize node selection to prevent load bias
            %% lists:usort always picked first (alphabetical), causing uneven load
            Nodes = lists:usort([node(Pid) || Pid <- Members]),
            shuffle_nodes(Nodes)
    end.

%% @doc Get current shard count
-spec get_shard_count() -> integer().
get_shard_count() ->
    case application:get_env(iris_core, shard_count) of
        {ok, Count} -> Count;
        undefined -> ?DEFAULT_SHARD_COUNT
    end.

%% @doc Set shard count (requires restart to take effect)
-spec set_shard_count(integer()) -> ok.
set_shard_count(Count) when Count > 0 ->
    application:set_env(iris_core, shard_count, Count),
    gen_server:cast(?SERVER, {update_shard_count, Count}).

%% @doc Join this node to a shard (node will serve this shard)
-spec join_shard(integer()) -> ok.
join_shard(ShardId) ->
    gen_server:call(?SERVER, {join_shard, ShardId}).

%% @doc Leave a shard (stop serving this shard)
-spec leave_shard(integer()) -> ok.
leave_shard(ShardId) ->
    gen_server:call(?SERVER, {leave_shard, ShardId}).

%% @doc Get all shards and their serving nodes
-spec get_all_shards() -> map().
get_all_shards() ->
    gen_server:call(?SERVER, get_all_shards).

%% @doc Get shards this node is serving
-spec get_local_shards() -> [integer()].
get_local_shards() ->
    gen_server:call(?SERVER, get_local_shards).

%% @doc Get shard distribution statistics
-spec get_shard_stats() -> map().
get_shard_stats() ->
    gen_server:call(?SERVER, get_shard_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Start pg scope if not already started
    case pg:start(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    
    ShardCount = get_shard_count(),
    
    %% Auto-join shards based on node name hash (for even distribution)
    LocalShards = calculate_local_shards(ShardCount),
    
    %% Join calculated shards
    lists:foreach(fun(ShardId) ->
        join_shard_internal(ShardId)
    end, LocalShards),
    
    {ok, #state{
        shard_count = ShardCount,
        local_shards = LocalShards
    }}.

handle_call({get_shard_nodes, ShardId}, _From, State) ->
    Nodes = get_shard_nodes_internal(ShardId),
    {reply, Nodes, State};

handle_call({join_shard, ShardId}, _From, State = #state{local_shards = Local}) ->
    join_shard_internal(ShardId),
    NewLocal = lists:usort([ShardId | Local]),
    {reply, ok, State#state{local_shards = NewLocal}};

handle_call({leave_shard, ShardId}, _From, State = #state{local_shards = Local}) ->
    leave_shard_internal(ShardId),
    NewLocal = lists:delete(ShardId, Local),
    {reply, ok, State#state{local_shards = NewLocal}};

handle_call(get_all_shards, _From, State = #state{shard_count = Count}) ->
    AllShards = lists:foldl(fun(ShardId, Acc) ->
        Nodes = get_shard_nodes_internal(ShardId),
        maps:put(ShardId, Nodes, Acc)
    end, #{}, lists:seq(0, Count - 1)),
    {reply, AllShards, State};

handle_call(get_local_shards, _From, State = #state{local_shards = Local}) ->
    {reply, Local, State};

handle_call(get_shard_stats, _From, State = #state{shard_count = Count, local_shards = Local}) ->
    AllShards = lists:foldl(fun(ShardId, Acc) ->
        Nodes = get_shard_nodes_internal(ShardId),
        maps:put(ShardId, length(Nodes), Acc)
    end, #{}, lists:seq(0, Count - 1)),
    
    NodeCounts = maps:values(AllShards),
    EmptyShards = length([C || C <- NodeCounts, C == 0]),
    
    %% AUDIT2 FIX (Issue 6): Report shard coverage health
    CoverageHealth = case EmptyShards of
        0 -> healthy;
        N when N < 5 -> degraded;
        _ -> critical
    end,
    
    Stats = #{
        shard_count => Count,
        local_shards => length(Local),
        empty_shards => EmptyShards,
        coverage_health => CoverageHealth,  %% AUDIT2: Added health indicator
        distribution => AllShards,
        avg_nodes_per_shard => safe_avg(NodeCounts)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_shard_count, Count}, State) ->
    {noreply, State#state{shard_count = Count}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{local_shards = Local}) ->
    %% Leave all shards on shutdown
    lists:foreach(fun(ShardId) ->
        leave_shard_internal(ShardId)
    end, Local),
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

shard_group_name(ShardId) ->
    list_to_atom(atom_to_list(?SHARD_PREFIX) ++ integer_to_list(ShardId)).

join_shard_internal(ShardId) ->
    GroupName = shard_group_name(ShardId),
    pg:join(?PG_SCOPE, GroupName, self()).

leave_shard_internal(ShardId) ->
    GroupName = shard_group_name(ShardId),
    pg:leave(?PG_SCOPE, GroupName, self()).

get_shard_nodes_internal(ShardId) ->
    GroupName = shard_group_name(ShardId),
    case pg:get_members(?PG_SCOPE, GroupName) of
        [] -> [];
        Members -> 
            %% AUDIT2 FIX: Use shuffle for internal lookups too
            Nodes = lists:usort([node(Pid) || Pid <- Members]),
            shuffle_nodes(Nodes)
    end.

%% AUDIT2 FIX (Issue 9): Randomize node order for load balancing
shuffle_nodes([]) -> [];
shuffle_nodes([Single]) -> [Single];
shuffle_nodes(Nodes) ->
    %% Fisher-Yates shuffle for even distribution
    Tagged = [{rand:uniform(), N} || N <- Nodes],
    [N || {_, N} <- lists:sort(Tagged)].

%% Calculate which shards this node should own based on node name
calculate_local_shards(ShardCount) ->
    %% Use node name hash to determine primary shard
    NodeName = atom_to_binary(node(), utf8),
    PrimaryShard = erlang:phash2(NodeName, ShardCount),
    
    %% For redundancy, also claim adjacent shards
    %% This gives each node ~3 shards by default
    AdjacentCount = 2,
    ShardIds = [PrimaryShard | 
                [(PrimaryShard + I) rem ShardCount || I <- lists:seq(1, AdjacentCount)]],
    
    lists:usort(ShardIds).

safe_avg([]) -> 0.0;
safe_avg(List) ->
    round((lists:sum(List) / length(List)) * 10) / 10.
