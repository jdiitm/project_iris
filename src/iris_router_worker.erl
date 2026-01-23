-module(iris_router_worker).
-export([start_link/1, get_stats/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATS_COUNTER, iris_router_stats).
-define(IDX_MSGS, 1).
-define(IDX_TIME, 2).

%% Dynamic Core node discovery with failover
get_core_node() ->
    case iris_core_registry:get_core() of
        {ok, Node} -> Node;
        {error, _} -> legacy_core_node()
    end.

get_core_for_user(User) ->
    case iris_core_registry:get_core_for_user(User) of
        {ok, Node} -> Node;
        {error, _} -> legacy_core_node()
    end.

legacy_core_node() ->
    %% FIXED: Scan connected nodes for actual Core IP
    Connected = nodes(connected),
    case [N || N <- Connected, string:str(atom_to_list(N), "iris_core") > 0] of
         [Core|_] -> Core;
         [] -> 
             %% Local Fallback for Tests/Single-Node
             LocalCore = local_derived_core(),
             
             %% Configured Nodes from sys.config + Local Derived
             ConfigNodes = application:get_env(iris_edge, core_nodes, []),
             Candidates = [LocalCore | ConfigNodes],
             
             case lists:search(fun(N) -> net_adm:ping(N) == pong end, Candidates) of
                 {value, LiveCore} -> LiveCore;
                 false -> 
                     error(no_core_available)
             end
    end.

%% Helper to derive Core node name from local Edge name (for tests)
local_derived_core() ->
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core"
    end,
    list_to_atom(CoreName ++ "@" ++ Host).

%% Get all known Core nodes (from pg registry + configured fallbacks)
get_all_known_cores() ->
    %% Start with pg-registered cores
    PGCores = case iris_core_registry:get_all_cores() of
        {ok, List} -> List;
        _ -> []
    end,
    %% Add configured fallbacks if not already present
    ConfigNodes = application:get_env(iris_edge, core_nodes, []),
    
    %% Add derived local core (for tests)
    LocalCore = local_derived_core(),
    
    %% Merge: pg cores first, then derived, then configured
    lists:usort(PGCores ++ [LocalCore] ++ [C || C <- ConfigNodes, not lists:member(C, PGCores)]).

%% Route to Core with automatic failover on circuit_open
%% Tries each Core in order until one succeeds or all fail
route_to_core_with_failover(_User, _Msg, []) ->
    %% No cores left to try - all circuits open or all failed
    logger:error("Router: ALL Core circuits open! Message dropped."),
    ok;
route_to_core_with_failover(User, Msg, [CoreNode | RestCores]) ->
    case iris_circuit_breaker:call(CoreNode, iris_core, lookup_user, [User]) of
        {ok, Node, Pid} ->
            IsLocal = (Node == node()),
            IsAlive = if IsLocal -> is_process_alive(Pid); true -> true end,
            
            if IsAlive ->
                %% io:format("[DELIVER] Msg to ~s -> Pid ~p on ~p~n", [User, Pid, Node]),
                Pid ! {deliver_msg, Msg};
            true ->
                logger:warning("Router: Local PID dead for ~p. Storing offline.", [User]),
                store_offline_with_failover(User, Msg, [CoreNode | RestCores])
            end;
        {error, not_found} ->
            logger:info("Router: Storing offline msg for ~s", [User]),
            store_offline_with_failover(User, Msg, [CoreNode | RestCores]);
        {error, circuit_open} ->
            %% FAILOVER: Try next Core instead of dropping!
            logger:warning("Router: Circuit open for ~p. Trying failover...", [CoreNode]),
            route_to_core_with_failover(User, Msg, RestCores);
        {badrpc, _Reason} ->
            %% Network error - try next core
            logger:warning("Router: RPC failed to ~p. Trying failover...", [CoreNode]),
            route_to_core_with_failover(User, Msg, RestCores);
        {error, Reason} ->
            logger:error("Circuit Breaker Error ~p: ~p", [User, Reason]),
            route_to_core_with_failover(User, Msg, RestCores)
    end.

%% Store offline with failover (try cores in order)
store_offline_with_failover(_User, _Msg, []) ->
    logger:error("Router: Cannot store offline - all Cores unavailable!"),
    ok;
store_offline_with_failover(User, Msg, [CoreNode | RestCores]) ->
    %% DURABILITY FIX: Use store_offline_durable for RPO=0 guarantee
    case iris_circuit_breaker:call(CoreNode, iris_core, store_offline_durable, [User, Msg]) of
        {error, circuit_open} ->
            store_offline_with_failover(User, Msg, RestCores);
        {badrpc, _} ->
            store_offline_with_failover(User, Msg, RestCores);
        _ ->
            ok  %% Success or other result
    end.

%% Start with ID (integer)
start_link(Id) ->
    Name = list_to_atom("iris_router_" ++ integer_to_list(Id)),
    gen_server:start_link({local, Name}, ?MODULE, [], []).

get_stats(Id) ->
    Name = list_to_atom("iris_router_" ++ integer_to_list(Id)),
    try gen_server:call(Name, get_stats)
    catch _:_ -> {0, 0} end.

init([]) ->
    Ref = counters:new(2, []),
    {ok, Ref}.

handle_call(get_stats, _From, Ref) ->
    Msgs = counters:get(Ref, ?IDX_MSGS),
    Time = counters:get(Ref, ?IDX_TIME),
    Avg = case Msgs of 0 -> 0; _ -> Time / Msgs end,
    {reply, {Msgs, Avg}, Ref};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast({route, User, Msg, StartTime}, Ref) ->
    %% Get Core node for this user (consistent hashing for cache locality)
    PrimaryCore = get_core_for_user(User),
    
    %% Build list of all known cores for failover
    AllCores = get_all_known_cores(),
    %% Put primary first, then others
    OrderedCores = [PrimaryCore | lists:delete(PrimaryCore, AllCores)],
    
    %% Optimization: Local Switching Search
    case ets:lookup(local_presence_v2, User) of
        [{User, LocalPid}] ->
            %% FOUND LOCAL: Bypass Core RPC entirely
            %% logger:info("Router: CACHE HIT for ~p", [User]),
            LocalPid ! {deliver_msg, Msg};
        [] ->
            %% NOT LOCAL: Try cores with failover
            logger:warning("Router: CACHE MISS for ~p - Fallback to RPC", [User]),
            route_to_core_with_failover(User, Msg, OrderedCores)
    end,
    End = os:system_time(microsecond),
    Diff = End - StartTime,
    counters:add(Ref, ?IDX_MSGS, 1),
    counters:add(Ref, ?IDX_TIME, Diff),
    {noreply, Ref};

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
