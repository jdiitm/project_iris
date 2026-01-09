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
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core"
    end,
    list_to_atom(CoreName ++ "@" ++ Host).

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
    CoreNode = get_core_for_user(User),
    
    %% Optimization: Local Switching Search
    case ets:lookup(local_presence, User) of
        [{User, LocalPid}] ->
            %% FOUND LOCAL: Bypass Core RPC entirely
            LocalPid ! {deliver_msg, Msg};
        [] ->
            %% NOT LOCAL: Fallback to Global Core RPC via Circuit Breaker
            case iris_circuit_breaker:call(CoreNode, iris_core, lookup_user, [User]) of
                {ok, Node, Pid} ->
                    IsLocal = (Node == node()),
                    IsAlive = if IsLocal -> is_process_alive(Pid); true -> true end,
                    
                    if IsAlive ->
                        Pid ! {deliver_msg, Msg};
                    true ->
                        logger:warning("Router: Local PID dead for ~p. Storing offline.", [User]),
                        iris_circuit_breaker:call(CoreNode, iris_core, store_offline, [User, Msg])
                    end;
                {error, not_found} ->
                    logger:info("Router: Storing offline msg for ~s", [User]),
                    iris_circuit_breaker:call(CoreNode, iris_core, store_offline, [User, Msg]);
                {error, circuit_open} ->
                    logger:error("Router: Circuit open for ~p. Dropping message.", [CoreNode]),
                    %% TODO: Store locally in emergency buffer?
                    ok;
                {badrpc, Reason} ->
                    logger:error("RPC Error routing to ~p: ~p", [User, Reason]);
                {error, Reason} ->
                    logger:error("Circuit Breaker Error ~p: ~p", [User, Reason])
            end
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
