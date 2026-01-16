-module(iris_storage).

%% =============================================================================
%% Storage Abstraction Layer
%% =============================================================================
%% Purpose: Abstract storage backend for horizontal scaling.
%% Backends:
%% 1. mnesia (default) - Distributed Erlang database
%% 2. ets_cluster - Sharded ETS with cross-node replication
%% 3. redis - External Redis cluster
%% 4. cassandra - Apache Cassandra (future)
%% =============================================================================

-export([start_link/0]).
-export([put/3, put/4]).
-export([get/2, get/3]).
-export([delete/2]).
-export([get_backend/0, set_backend/1]).
-export([get_stats/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_BACKEND, mnesia).

-record(state, {
    backend :: atom(),
    stats = #{} :: map()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Store a key-value pair
-spec put(atom(), term(), term()) -> ok | {error, term()}.
put(Table, Key, Value) ->
    put(Table, Key, Value, #{}).

-spec put(atom(), term(), term(), map()) -> ok | {error, term()}.
put(Table, Key, Value, Opts) ->
    Backend = get_backend(),
    do_put(Backend, Table, Key, Value, Opts).

%% @doc Retrieve a value by key
-spec get(atom(), term()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key) ->
    get(Table, Key, #{}).

-spec get(atom(), term(), map()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key, Opts) ->
    Backend = get_backend(),
    do_get(Backend, Table, Key, Opts).

%% @doc Delete a key
-spec delete(atom(), term()) -> ok | {error, term()}.
delete(Table, Key) ->
    Backend = get_backend(),
    do_delete(Backend, Table, Key).

%% @doc Get current storage backend
-spec get_backend() -> atom().
get_backend() ->
    case application:get_env(iris_core, storage_backend) of
        {ok, Backend} -> Backend;
        undefined -> ?DEFAULT_BACKEND
    end.

%% @doc Set storage backend
-spec set_backend(atom()) -> ok.
set_backend(Backend) when Backend == mnesia; Backend == ets_cluster; Backend == redis ->
    application:set_env(iris_core, storage_backend, Backend),
    gen_server:cast(?SERVER, {set_backend, Backend}).

%% @doc Get storage statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    Backend = get_backend(),
    init_backend(Backend),
    {ok, #state{backend = Backend}}.

handle_call(get_stats, _From, State = #state{backend = Backend}) ->
    Stats = get_backend_stats(Backend),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_backend, Backend}, State) ->
    init_backend(Backend),
    {noreply, State#state{backend = Backend}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Backend: Mnesia
%% =============================================================================

init_backend(mnesia) ->
    %% Mnesia is initialized by iris_core
    ok;
init_backend(ets_cluster) ->
    %% ETS tables are created per-table
    ok;
init_backend(redis) ->
    %% Would initialize Redis connection pool
    ok.

do_put(mnesia, Table, Key, Value, Opts) ->
    Durability = maps:get(durability, Opts, sync),
    F = fun() ->
        mnesia:write({Table, Key, Value})
    end,
    case Durability of
        sync ->
            case mnesia:activity(sync_transaction, F) of
                ok -> ok;
                {atomic, _} -> ok;
                {aborted, Reason} -> {error, Reason}
            end;
        async ->
            mnesia:activity(async_dirty, F),
            ok
    end;

do_put(ets_cluster, Table, Key, Value, _Opts) ->
    %% Write locally and replicate
    ensure_ets_table(Table),
    ets:insert(Table, {Key, Value}),
    %% Async replicate to other nodes
    replicate_to_peers(Table, {put, Key, Value}),
    ok;

do_put(redis, Table, Key, Value, Opts) ->
    %% Would use Redis client
    Ttl = maps:get(ttl, Opts, 0),
    RedisKey = redis_key(Table, Key),
    logger:debug("Redis SET ~s = ~p (TTL: ~p)", [RedisKey, Value, Ttl]),
    ok.

do_get(mnesia, Table, Key, _Opts) ->
    F = fun() ->
        mnesia:read({Table, Key})
    end,
    case mnesia:activity(transaction, F) of
        [] -> not_found;
        [{Table, Key, Value}] -> {ok, Value};
        {aborted, Reason} -> {error, Reason}
    end;

do_get(ets_cluster, Table, Key, _Opts) ->
    ensure_ets_table(Table),
    case ets:lookup(Table, Key) of
        [] -> not_found;
        [{Key, Value}] -> {ok, Value}
    end;

do_get(redis, Table, Key, _Opts) ->
    RedisKey = redis_key(Table, Key),
    logger:debug("Redis GET ~s", [RedisKey]),
    not_found.  %% Placeholder

do_delete(mnesia, Table, Key) ->
    F = fun() ->
        mnesia:delete({Table, Key})
    end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end;

do_delete(ets_cluster, Table, Key) ->
    ensure_ets_table(Table),
    ets:delete(Table, Key),
    replicate_to_peers(Table, {delete, Key}),
    ok;

do_delete(redis, Table, Key) ->
    RedisKey = redis_key(Table, Key),
    logger:debug("Redis DEL ~s", [RedisKey]),
    ok.

%% =============================================================================
%% ETS Cluster Helpers
%% =============================================================================

ensure_ets_table(Table) ->
    case ets:info(Table) of
        undefined ->
            ets:new(Table, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end.

replicate_to_peers(Table, Operation) ->
    %% Get peer nodes from discovery
    Peers = case whereis(iris_discovery) of
        undefined -> nodes();
        _ -> iris_discovery:get_nodes(iris_storage)
    end,
    
    %% Async replicate
    lists:foreach(fun(Node) ->
        spawn(fun() ->
            rpc:cast(Node, ?MODULE, apply_replication, [Table, Operation])
        end)
    end, Peers -- [node()]).

%% Called via RPC on peer nodes
apply_replication(Table, {put, Key, Value}) ->
    ensure_ets_table(Table),
    ets:insert(Table, {Key, Value});
apply_replication(Table, {delete, Key}) ->
    ensure_ets_table(Table),
    ets:delete(Table, Key).

%% =============================================================================
%% Redis Helpers
%% =============================================================================

redis_key(Table, Key) when is_atom(Table), is_binary(Key) ->
    <<(atom_to_binary(Table, utf8))/binary, ":", Key/binary>>;
redis_key(Table, Key) ->
    redis_key(Table, term_to_binary(Key)).

%% =============================================================================
%% Stats
%% =============================================================================

get_backend_stats(mnesia) ->
    #{
        backend => mnesia,
        tables => mnesia:system_info(tables),
        running_nodes => mnesia:system_info(running_db_nodes),
        held_locks => length(mnesia:system_info(held_locks))
    };
get_backend_stats(ets_cluster) ->
    AllTables = ets:all(),
    #{
        backend => ets_cluster,
        table_count => length(AllTables),
        memory_bytes => lists:sum([ets:info(T, memory) * erlang:system_info(wordsize) 
                                   || T <- AllTables, is_atom(T)])
    };
get_backend_stats(redis) ->
    #{
        backend => redis,
        connected => false  %% Placeholder
    }.
