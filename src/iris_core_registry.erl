-module(iris_core_registry).
-behaviour(gen_server).

%% API
-export([start_link/0, join/0, leave/0]).
-export([get_core/0, get_core_for_user/1, get_all_cores/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PG_GROUP, iris_core_nodes).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register this node as a Core node (call from iris_core startup)
-spec join() -> ok.
join() ->
    gen_server:call(?SERVER, join_pg).

%% @doc Unregister this node as a Core node
-spec leave() -> ok.
leave() ->
    pg:leave(?PG_GROUP, self()),
    logger:info("Core node ~p left registry", [node()]),
    ok.

%% @doc Get any available Core node (for general queries)
-spec get_core() -> {ok, node()} | {error, no_cores_available}.
get_core() ->
    case get_all_cores() of
        [] -> 
            %% Fallback to legacy discovery for backward compatibility
            legacy_core_node();
        Cores -> 
            %% Pick random Core for load distribution
            Index = rand:uniform(length(Cores)),
            {ok, lists:nth(Index, Cores)}
    end.

%% @doc Get Core node for specific user (consistent hashing for cache locality)
-spec get_core_for_user(binary()) -> {ok, node()} | {error, no_cores_available}.
get_core_for_user(User) ->
    case get_all_cores() of
        [] -> 
            legacy_core_node();
        Cores ->
            %% Consistent hash to same Core for same user
            %% This improves cache hit rates on Core's local Mnesia
            SortedCores = lists:sort(Cores),
            Index = (erlang:phash2(User, length(SortedCores))) + 1,
            {ok, lists:nth(Index, SortedCores)}
    end.

%% @doc Get list of all available Core nodes
-spec get_all_cores() -> [node()].
get_all_cores() ->
    case pg:get_members(?PG_GROUP) of
        Pids when is_list(Pids) ->
            %% Extract unique nodes from pids
            lists:usort([node(Pid) || Pid <- Pids]);
        _ ->
            []
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Ensure pg scope exists
    case pg:start_link(?PG_GROUP) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, #{}}.

handle_call(join_pg, _From, State) ->
    pg:join(?PG_GROUP, self()),
    pg:join(iris_shards, self()), %% CRITICAL: Join sharding group
    logger:info("Core node ~p joined registry (Groups: ~p, iris_shards)", [node(), ?PG_GROUP]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route_remote, User, Msg}, State) ->
    %% Receive message from Edge Router and store offline
    %% Spawning to avoid blocking this registry process
    %% DURABILITY FIX: Use store_offline_durable for RPO=0 guarantee
    spawn(fun() -> 
        try iris_core:store_offline_durable(User, Msg)
        catch E:R -> logger:error("Failed to store routed msg: ~p:~p", [E, R])
        end
    end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Fallback for when pg is not available or no cores registered
legacy_core_node() ->
    case node() of
        'nonode@nohost' ->
            {error, no_cores_available};
        _ ->
            %% FIXED: Do not assume Core is on the same host (Tokyo vs Bangalore)
            %% Scan connected nodes for anything looking like a core
            Connected = nodes(connected),
            case [N || N <- Connected, string:str(atom_to_list(N), "iris_core") > 0] of
                 [Core|_] -> {ok, Core};
                 [] -> 
                      %% Emergency Fallback: Try Configured IPs + Local Derived
                      LocalCore = local_derived_core(),
                      ConfigNodes = application:get_env(iris_edge, core_nodes, []),
                      Candidates = [LocalCore | ConfigNodes],
                      
                      case lists:search(fun(N) -> net_adm:ping(N) == pong end, Candidates) of
                          {value, LiveCore} -> {ok, LiveCore};
                          false -> {error, no_cores_available}
                      end
            end
    end.

local_derived_core() ->
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core"
    end,
    list_to_atom(CoreName ++ "@" ++ Host).
