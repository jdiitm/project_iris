-module(iris_storage).

%% =============================================================================
%% Storage Abstraction Layer (LEGACY - Use iris_store for new code)
%% =============================================================================
%% 
%% NOTE: This module is maintained for backwards compatibility.
%% For NEW code, use iris_store.erl which provides:
%%   - Simpler API
%%   - Clear durability options (guaranteed | best_effort | quorum)
%%   - Partition-aware writes
%%   - Better documentation
%% 
%% This module now ONLY supports Mnesia backend (the only production backend).
%% Redis and ETS cluster backends have been removed to reduce complexity.
%% 
%% =============================================================================

-export([start_link/0]).
-export([put/3, put/4]).
-export([get/2, get/3]).
-export([delete/2]).
-export([get_backend/0]).
-export([get_stats/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
    stats = #{} :: map()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Store a key-value pair (uses Mnesia sync_transaction)
-spec put(atom(), term(), term()) -> ok | {error, term()}.
put(Table, Key, Value) ->
    put(Table, Key, Value, #{}).

-spec put(atom(), term(), term(), map()) -> ok | {error, term()}.
put(Table, Key, Value, _Opts) ->
    %% CRITICAL: Always use sync_transaction for durability
    F = fun() -> mnesia:write({Table, Key, Value}) end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Retrieve a value by key (dirty read for speed)
-spec get(atom(), term()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key) ->
    get(Table, Key, #{}).

-spec get(atom(), term(), map()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key, _Opts) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> not_found;
        [{Table, Key, Value}] -> {ok, Value};
        [Record | _] -> {ok, Record}
    end.

%% @doc Delete a key
-spec delete(atom(), term()) -> ok | {error, term()}.
delete(Table, Key) ->
    F = fun() -> mnesia:delete({Table, Key}) end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get current storage backend (always mnesia now)
-spec get_backend() -> mnesia.
get_backend() ->
    mnesia.

%% @doc Get storage statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        backend => mnesia,
        tables => mnesia:system_info(tables),
        running_nodes => mnesia:system_info(running_db_nodes),
        held_locks => length(mnesia:system_info(held_locks))
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
