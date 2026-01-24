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
%% Internal: Routing Strategies
%% =============================================================================

%% Direct RPC routing (low latency, but requires connectivity)
route_via_rpc(TargetRegion, UserId, Msg) ->
    case get_region_endpoint(TargetRegion) of
        {ok, Nodes} when length(Nodes) > 0 ->
            %% Try nodes in order with circuit breaker
            route_to_region_nodes(Nodes, UserId, Msg);
        {error, _Reason} ->
            %% No endpoints configured - try to discover via pg
            discover_and_route(TargetRegion, UserId, Msg)
    end.

route_to_region_nodes([], _UserId, _Msg) ->
    {error, all_region_nodes_failed};
route_to_region_nodes([Node | Rest], UserId, Msg) ->
    case iris_circuit_breaker:call(
            Node, iris_async_router, route, [UserId, Msg]) of
        ok -> ok;
        {error, circuit_open} ->
            route_to_region_nodes(Rest, UserId, Msg);
        {badrpc, _} ->
            route_to_region_nodes(Rest, UserId, Msg)
    end.

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

%% Bridge routing (async, for high-latency regions)
route_via_bridge(TargetRegion, UserId, Msg) ->
    %% Check if bridge process is available
    case whereis(iris_region_bridge) of
        undefined ->
            %% No bridge - fall back to RPC
            route_via_rpc(TargetRegion, UserId, Msg);
        BridgePid ->
            %% Send to bridge for async delivery
            BridgePid ! {route, TargetRegion, UserId, Msg},
            ok
    end.

%% Store message for later cross-region delivery
store_cross_region_offline(TargetRegion, UserId, Msg) ->
    %% Store in local Mnesia with region tag
    Key = {cross_region, TargetRegion, UserId, erlang:system_time(millisecond)},
    iris_store:put(cross_region_queue, Key, Msg, #{durability => guaranteed}).

%% =============================================================================
%% Internal: Helpers
%% =============================================================================

normalize_region(Region) when is_binary(Region) -> Region;
normalize_region(Region) when is_list(Region) -> list_to_binary(Region);
normalize_region(Region) when is_atom(Region) -> atom_to_binary(Region, utf8).

region_group_name(Region) ->
    binary_to_atom(<<"iris_region_", Region/binary>>, utf8).
