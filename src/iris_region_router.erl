-module(iris_region_router).

%% =============================================================================
%% Regional Routing: Scale Beyond Single Mnesia Cluster
%% =============================================================================
%% 
%% ARCHITECTURE:
%% 
%%   ┌─────────────────────────────────────────────────────────────────────┐
%%   │                     GLOBAL ROUTING LAYER                            │
%%   │   (This Module - Routes users to their home region)                │
%%   └─────────────────────────────────────────────────────────────────────┘
%%                       │                    │                    │
%%            ┌─────────▼─────────┐ ┌────────▼────────┐ ┌─────────▼─────────┐
%%            │   REGION: US      │ │ REGION: EU      │ │ REGION: APAC      │
%%            │   Mnesia Cluster  │ │ Mnesia Cluster  │ │ Mnesia Cluster    │
%%            │   (50 nodes max)  │ │ (50 nodes max)  │ │ (50 nodes max)    │
%%            └───────────────────┘ └─────────────────┘ └───────────────────┘
%% 
%% SCALING MATH:
%%   - 20 regions × 50 nodes/region × 2M users/node = 2 Billion users
%%   - Each user has a "home region" determined by hash(UserID)
%%   - Cross-region messages use async bridge (Kafka/direct RPC)
%% 
%% CONFIGURATION:
%%   {iris_core, [
%%       {region_id, <<"us-east-1">>},
%%       {regions, [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>]},
%%       {region_endpoints, #{
%%           <<"us-east-1">> => [{core_us_1, 'core@us-east-1.iris.io'}],
%%           <<"eu-west-1">> => [{core_eu_1, 'core@eu-west-1.iris.io'}]
%%       }}
%%   ]}
%% 
%% =============================================================================

-export([route_to_user/2, route_to_user/3]).
-export([get_home_region/1, is_local_region/1]).
-export([get_current_region/0, get_all_regions/0]).
-export([get_region_endpoint/1, set_region_endpoint/2]).
-export([route_cross_region/3]).
%% P1-H3 FIX: Health checking exports
-export([get_region_health/1, get_all_region_health/0]).
-export([probe_region/1]).

%% =============================================================================
%% API: Message Routing
%% =============================================================================

%% @doc Route a message to a user, handling cross-region if needed
-spec route_to_user(binary(), binary()) -> ok | {error, term()}.
route_to_user(UserId, Msg) ->
    route_to_user(UserId, Msg, #{}).

-spec route_to_user(binary(), binary(), map()) -> ok | {error, term()}.
route_to_user(UserId, Msg, Opts) ->
    HomeRegion = get_home_region(UserId),
    CurrentRegion = get_current_region(),
    
    case HomeRegion == CurrentRegion of
        true ->
            %% User belongs to this region - use local routing
            iris_async_router:route(UserId, Msg);
        false ->
            %% User belongs to another region - cross-region routing
            route_cross_region(HomeRegion, UserId, Msg, Opts)
    end.

%% =============================================================================
%% API: Region Management
%% =============================================================================

%% @doc Get the home region for a user (deterministic via hash)
-spec get_home_region(binary()) -> binary().
get_home_region(UserId) ->
    Regions = get_all_regions(),
    RegionCount = length(Regions),
    case RegionCount of
        0 -> get_current_region();  %% Single region mode
        1 -> hd(Regions);
        _ ->
            Index = erlang:phash2(UserId, RegionCount),
            lists:nth(Index + 1, Regions)
    end.

%% @doc Check if a region is the current local region
-spec is_local_region(binary()) -> boolean().
is_local_region(Region) ->
    Region == get_current_region().

%% @doc Get the current region ID
-spec get_current_region() -> binary().
get_current_region() ->
    case application:get_env(iris_core, region_id) of
        {ok, Region} when is_binary(Region) -> Region;
        {ok, Region} when is_list(Region) -> list_to_binary(Region);
        {ok, Region} when is_atom(Region) -> atom_to_binary(Region, utf8);
        _ -> <<"local">>  %% Default single-region mode
    end.

%% @doc Get all configured regions
-spec get_all_regions() -> [binary()].
get_all_regions() ->
    case application:get_env(iris_core, regions) of
        {ok, Regions} when is_list(Regions) ->
            [normalize_region(R) || R <- Regions];
        _ ->
            [get_current_region()]  %% Single region mode
    end.

%% @doc Get the endpoint nodes for a region
-spec get_region_endpoint(binary()) -> {ok, [node()]} | {error, not_found}.
get_region_endpoint(Region) ->
    case application:get_env(iris_core, region_endpoints) of
        {ok, Endpoints} when is_map(Endpoints) ->
            case maps:get(Region, Endpoints, undefined) of
                undefined -> {error, not_found};
                Nodes when is_list(Nodes) -> {ok, Nodes}
            end;
        _ ->
            {error, not_configured}
    end.

%% @doc Set endpoint nodes for a region (runtime configuration)
-spec set_region_endpoint(binary(), [node()]) -> ok.
set_region_endpoint(Region, Nodes) ->
    CurrentEndpoints = case application:get_env(iris_core, region_endpoints) of
        {ok, E} when is_map(E) -> E;
        _ -> #{}
    end,
    NewEndpoints = maps:put(Region, Nodes, CurrentEndpoints),
    application:set_env(iris_core, region_endpoints, NewEndpoints).

%% =============================================================================
%% API: Cross-Region Routing
%% =============================================================================

%% @doc Route a message to a user in another region
-spec route_cross_region(binary(), binary(), binary()) -> ok | {error, term()}.
route_cross_region(TargetRegion, UserId, Msg) ->
    route_cross_region(TargetRegion, UserId, Msg, #{}).

route_cross_region(TargetRegion, UserId, Msg, Opts) ->
    Strategy = maps:get(strategy, Opts, direct_rpc),
    
    case Strategy of
        direct_rpc ->
            route_via_rpc(TargetRegion, UserId, Msg);
        bridge ->
            route_via_bridge(TargetRegion, UserId, Msg);
        _ ->
            route_via_rpc(TargetRegion, UserId, Msg)
    end.

%% =============================================================================
%% P1-H3 FIX: Region Health Checking
%% =============================================================================

%% Health status for region endpoints
-define(HEALTH_TABLE, iris_region_health).
-define(HEALTH_PROBE_INTERVAL_MS, 30000).  %% Probe every 30 seconds
-define(UNHEALTHY_THRESHOLD, 3).           %% 3 consecutive failures = unhealthy

%% @doc Get health status for a specific region
-spec get_region_health(binary()) -> healthy | degraded | unhealthy | unknown.
get_region_health(Region) ->
    ensure_health_table(),
    case ets:lookup(?HEALTH_TABLE, Region) of
        [{Region, Status, _LastCheck, _FailCount}] -> Status;
        [] -> unknown
    end.

%% @doc Get health status for all regions
-spec get_all_region_health() -> map().
get_all_region_health() ->
    ensure_health_table(),
    Regions = get_all_regions(),
    maps:from_list([{R, get_region_health(R)} || R <- Regions]).

%% @doc Probe a region's health (can be called manually or by scheduler)
-spec probe_region(binary()) -> healthy | unhealthy.
probe_region(Region) ->
    ensure_health_table(),
    case get_region_endpoint(Region) of
        {ok, Nodes} when length(Nodes) > 0 ->
            %% Try to ping at least one node
            Results = [probe_node(N) || N <- Nodes],
            HealthyCount = length([R || R <- Results, R == healthy]),
            TotalCount = length(Results),
            
            {Status, FailCount} = case HealthyCount of
                0 -> 
                    %% All nodes failed
                    OldFailCount = get_fail_count(Region),
                    NewFailCount = OldFailCount + 1,
                    case NewFailCount >= ?UNHEALTHY_THRESHOLD of
                        true -> {unhealthy, NewFailCount};
                        false -> {degraded, NewFailCount}
                    end;
                N when N == TotalCount ->
                    {healthy, 0};
                _ ->
                    {degraded, 0}
            end,
            
            Now = os:system_time(millisecond),
            ets:insert(?HEALTH_TABLE, {Region, Status, Now, FailCount}),
            Status;
        _ ->
            unknown
    end.

probe_node(Node) ->
    try
        case net_adm:ping(Node) of
            pong -> healthy;
            pang -> unhealthy
        end
    catch
        _:_ -> unhealthy
    end.

get_fail_count(Region) ->
    case ets:lookup(?HEALTH_TABLE, Region) of
        [{Region, _, _, FailCount}] -> FailCount;
        [] -> 0
    end.

ensure_health_table() ->
    case ets:whereis(?HEALTH_TABLE) of
        undefined ->
            try
                ets:new(?HEALTH_TABLE, [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg -> ok  %% Already exists (race)
            end;
        _ -> ok
    end.

%% =============================================================================
%% Internal: Routing Strategies
%% =============================================================================

%% Direct RPC routing (low latency, but requires connectivity)
%% P1-H3 FIX: Integrated with health checking
route_via_rpc(TargetRegion, UserId, Msg) ->
    case get_region_endpoint(TargetRegion) of
        {ok, Nodes} when length(Nodes) > 0 ->
            %% P1-H3 FIX: Check region health first
            case get_region_health(TargetRegion) of
                unhealthy ->
                    %% Region is unhealthy - go directly to fallback
                    logger:warning("Region ~s is unhealthy, using fallback", [TargetRegion]),
                    store_cross_region_offline(TargetRegion, UserId, Msg);
                _ ->
                    %% Try nodes in order with circuit breaker
                    route_to_region_nodes(Nodes, UserId, Msg, TargetRegion)
            end;
        {error, _Reason} ->
            %% No endpoints configured - try to discover via pg
            discover_and_route(TargetRegion, UserId, Msg)
    end.

route_to_region_nodes([], _UserId, _Msg, TargetRegion) ->
    %% P1-H3 FIX: Update health status on total failure
    update_region_health_failure(TargetRegion),
    {error, all_region_nodes_failed};
route_to_region_nodes([Node | Rest], UserId, Msg, TargetRegion) ->
    case iris_circuit_breaker:call(
            Node, iris_async_router, route, [UserId, Msg]) of
        ok -> 
            %% P1-H3 FIX: Reset health on success
            update_region_health_success(TargetRegion),
            ok;
        {error, circuit_open} ->
            route_to_region_nodes(Rest, UserId, Msg, TargetRegion);
        {badrpc, _} ->
            route_to_region_nodes(Rest, UserId, Msg, TargetRegion)
    end.

update_region_health_failure(Region) ->
    ensure_health_table(),
    FailCount = get_fail_count(Region) + 1,
    Status = case FailCount >= ?UNHEALTHY_THRESHOLD of
        true -> unhealthy;
        false -> degraded
    end,
    Now = os:system_time(millisecond),
    ets:insert(?HEALTH_TABLE, {Region, Status, Now, FailCount}).

update_region_health_success(Region) ->
    ensure_health_table(),
    Now = os:system_time(millisecond),
    ets:insert(?HEALTH_TABLE, {Region, healthy, Now, 0}).

%% Discover region nodes via pg groups
discover_and_route(TargetRegion, UserId, Msg) ->
    GroupName = region_group_name(TargetRegion),
    case pg:get_members(GroupName) of
        [] ->
            %% No nodes found - store for later delivery
            logger:warning("No nodes found for region ~s, storing offline", [TargetRegion]),
            store_cross_region_offline(TargetRegion, UserId, Msg);
        [Pid | _] ->
            %% Found a node - route via it
            Node = node(Pid),
            case rpc:call(Node, iris_async_router, route, [UserId, Msg], 5000) of
                ok -> ok;
                {badrpc, Reason} ->
                    logger:error("Cross-region RPC failed: ~p", [Reason]),
                    store_cross_region_offline(TargetRegion, UserId, Msg)
            end
    end.

%% Bridge routing (async, for high-latency regions or when direct RPC fails)
route_via_bridge(TargetRegion, UserId, Msg) ->
    %% Use the bridge module for reliable async delivery
    %% This guarantees the message is durably queued before returning
    case whereis(iris_region_bridge) of
        undefined ->
            %% Bridge not started - queue directly to Mnesia and return ok
            %% The message will be picked up when bridge starts
            logger:warning("Bridge not running, storing cross-region message directly"),
            iris_region_bridge:send_cross_region(TargetRegion, UserId, Msg);
        _BridgePid ->
            %% Bridge running - use its API for durable queueing
            iris_region_bridge:send_cross_region(TargetRegion, UserId, Msg)
    end.

%% Store message for later cross-region delivery
store_cross_region_offline(TargetRegion, UserId, Msg) ->
    %% Use the bridge for reliable cross-region queueing
    %% This ensures durable storage and automatic retry
    iris_region_bridge:send_cross_region(TargetRegion, UserId, Msg).

%% =============================================================================
%% Internal: Helpers
%% =============================================================================

normalize_region(Region) when is_binary(Region) -> Region;
normalize_region(Region) when is_list(Region) -> list_to_binary(Region);
normalize_region(Region) when is_atom(Region) -> atom_to_binary(Region, utf8).

region_group_name(Region) ->
    binary_to_atom(<<"iris_region_", Region/binary>>, utf8).
