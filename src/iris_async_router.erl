-module(iris_async_router).
-behaviour(gen_server).

%% =============================================================================
%% Planetary Scale Async Router (Partitioned)
%% =============================================================================
%% Key Design Principles:
%% 1. Partitioned Worker Pool (8 Shards) to saturate Multi-Core CPUs.
%% 2. Consistent Hashing via phash2 for sticky user routing.
%% 3. Stats Aggregation across the pool.
%% =============================================================================

-export([start_link/1, route/2, route_async/2]).
-export([register_local/2, unregister_local/1]).
-export([get_local_count/0, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(LOCAL_PRESENCE, local_presence_v2).
-define(POOL_SIZE, 8).

-record(state, {
    shard_id :: integer(),
    local_count = 0 :: integer(),
    routed_local = 0 :: integer(),
    routed_remote = 0 :: integer(),
    start_time :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

%% Start a specific shard (Called by Supervisor)
start_link(ShardId) ->
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    gen_server:start_link({local, Name}, ?MODULE, [ShardId], []).

%% Route a message - Sharded by User ID
-spec route(binary(), binary()) -> ok.
route(User, Msg) ->
    ShardId = (erlang:phash2(User, ?POOL_SIZE) + 1),
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    gen_server:cast(Name, {route, User, Msg}).

-spec route_async(binary(), binary()) -> ok.
route_async(User, Msg) ->
    route(User, Msg).

%% Register is global (ETS is public), but we can track count locally if needed.
%% For now, we don't track per-shard local count strictly, as ETS is the source of truth.
register_local(User, Pid) ->
    true = ets:insert(?LOCAL_PRESENCE, {User, Pid}),
    ok.

unregister_local(User) ->
    true = ets:delete(?LOCAL_PRESENCE, User),
    ok.

get_local_count() ->
    ets:info(?LOCAL_PRESENCE, size).

%% Aggregate Stats from all shards
get_stats() ->
    Shards = lists:seq(1, ?POOL_SIZE),
    StatsList = [call_shard_stats(I) || I <- Shards],
    aggregate_stats(StatsList).

call_shard_stats(ShardId) ->
    Name = list_to_atom("iris_async_router_" ++ integer_to_list(ShardId)),
    try gen_server:call(Name, get_stats_local, 100)
    catch _:_ -> #{routed_local => 0, routed_remote => 0}
    end.

aggregate_stats(StatsList) ->
    lists:foldl(fun(S, Acc) ->
        #{
            routed_local => maps:get(routed_local, S, 0) + maps:get(routed_local, Acc, 0),
            routed_remote => maps:get(routed_remote, S, 0) + maps:get(routed_remote, Acc, 0),
            local_users => get_local_count(), %% Global ETS count
            uptime_seconds => maps:get(uptime_seconds, S, 0) %% Just take one, roughly same
        }
    end, #{routed_local => 0, routed_remote => 0}, StatsList).


%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([ShardId]) ->
    %% Only Shard 1 creates the table (race condition simple fix)
    if ShardId =:= 1 ->
        case ets:info(?LOCAL_PRESENCE) of
            undefined ->
                ets:new(?LOCAL_PRESENCE, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]);
            _ -> ok
        end;
       true -> ok
    end,
    {ok, #state{shard_id = ShardId, start_time = erlang:system_time(second)}}.

handle_call(get_stats_local, _From, State) ->
    Reply = #{
        shard_id => State#state.shard_id,
        routed_local => State#state.routed_local,
        routed_remote => State#state.routed_remote,
        uptime_seconds => erlang:system_time(second) - State#state.start_time
    },
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({route, User, Msg}, State) ->
    %% CRITICAL PATH - Sharded
    case ets:lookup(?LOCAL_PRESENCE, User) of
        [{User, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid ! {deliver_msg, Msg},
                    {noreply, State#state{routed_local = State#state.routed_local + 1}};
                false ->
                    %% Stale entry
                    ets:delete(?LOCAL_PRESENCE, User),
                    route_to_remote(User, Msg, State)
            end;
        [] ->
            route_to_remote(User, Msg, State)
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal
%% =============================================================================

route_to_remote(User, Msg, State) ->
    %% Remote Routing Logic (PG or RPC) - Unchanged
    %% We assume shards are started on Core nodes too? No, this is Edge Router.
    %% Edge Router -> Remote Core. 
    %% Using pg:get_members(iris_shards)
    Members = pg:get_members(iris_shards),
    case Members of
        [] ->
            %% Fallback: Try connected nodes (Tests) or Legacy (Prod Config)
            Node = case nodes() of
                [Peer | _] -> Peer;
                [] -> iris_session:get_core_node(User)
            end,
            store_offline_async(Node, User, Msg),
            {noreply, State#state{routed_remote = State#state.routed_remote + 1}};
        [TargetPid | _] -> 
             %% Just pick head for now (or random) to avoid load balancer complexity here
             TargetPid ! {route_remote, User, Msg},
             {noreply, State#state{routed_remote = State#state.routed_remote + 1}}
    end.

store_offline_async(Node, User, Msg) ->
    spawn(fun() ->
        try rpc:call(Node, iris_core, store_offline, [User, Msg])
        catch _:_ -> ok
        end
    end).
