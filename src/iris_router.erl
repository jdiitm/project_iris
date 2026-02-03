-module(iris_router).
-export([start_link/0, route/2, get_stats/0]).

%% Note: start_link is removed/unused as this is not a process anymore, 
%% but we keep it returning ignore or ok if supervisors call it.

start_link() -> ignore. %% Compatibility

route(User, Msg) ->
    %% PRESENCE-BASED ROUTING: First check where user IS, not where they SHOULD be
    %% This ensures messages go to the user's actual location
    case application:get_env(iris_core, regions, []) of
        [] ->
            %% Single region mode - use fast local routing
            iris_async_router:route_async(User, Msg);
        [_] ->
            %% Single region configured - use fast local routing
            iris_async_router:route_async(User, Msg);
        Regions when length(Regions) > 1 ->
            %% Multi-region mode - check presence first, then fall back to hash
            route_multiregion(User, Msg, Regions)
    end.

%% Multi-region routing: presence-based with hash fallback
route_multiregion(User, Msg, _Regions) ->
    CurrentRegion = iris_region_router:get_current_region(),
    
    %% Step 1: Check LOCAL presence first (fast path)
    case iris_core:lookup_user(User) of
        {ok, _Node, Pid} when is_pid(Pid) ->
            %% User is local - deliver directly
            logger:debug("ROUTER: User ~p found LOCAL, delivering to ~p", [User, Pid]),
            Pid ! {deliver_msg, Msg},
            ok;
        LocalResult ->
            logger:debug("ROUTER: User ~p not local (~p), checking all regions", [User, LocalResult]),
            %% Step 2: Check ALL regions for user presence
            case find_user_in_any_region(User) of
                {found, UserRegion, Pid} when is_pid(Pid) ->
                    logger:info("ROUTER: User ~p found in region ~p (pid ~p), current=~p", 
                               [User, UserRegion, Pid, CurrentRegion]),
                    %% User found in another region - deliver via their region
                    case UserRegion == CurrentRegion of
                        true ->
                            %% Same region - direct delivery (shouldn't happen often)
                            logger:debug("ROUTER: Same region, direct delivery"),
                            Pid ! {deliver_msg, Msg},
                            ok;
                        false ->
                            %% Different region - route via bridge
                            logger:info("ROUTER: Cross-region delivery via bridge to ~p", [UserRegion]),
                            iris_region_router:route_to_user(User, Msg, #{
                                strategy => bridge,
                                target_region => UserRegion
                            })
                    end;
                not_found ->
                    %% User not online anywhere - store offline in their hash-based home region
                    HomeRegion = iris_region_router:get_home_region(User),
                    logger:info("ROUTER: User ~p not found, home=~p, current=~p", 
                               [User, HomeRegion, CurrentRegion]),
                    case HomeRegion == CurrentRegion of
                        true ->
                            %% User's home is here - store offline locally
                            logger:debug("ROUTER: Storing offline locally"),
                            iris_core:store_offline_durable(User, Msg);
                        false ->
                            %% User's home is elsewhere - route to home region for offline storage
                            logger:info("ROUTER: Routing to home region ~p for offline storage", [HomeRegion]),
                            iris_region_router:route_to_user(User, Msg, #{
                                strategy => bridge,
                                target_region => HomeRegion,
                                offline => true
                            })
                    end
            end
    end.

%% Find user across all configured regions
find_user_in_any_region(User) ->
    Regions = application:get_env(iris_core, regions, []),
    RegionEndpoints = application:get_env(iris_core, region_endpoints, #{}),
    find_user_in_regions(User, Regions, RegionEndpoints).

find_user_in_regions(_User, [], _Endpoints) ->
    not_found;
find_user_in_regions(User, [Region | Rest], Endpoints) ->
    case maps:get(Region, Endpoints, []) of
        [] ->
            find_user_in_regions(User, Rest, Endpoints);
        Nodes ->
            case query_nodes_for_user(User, Nodes) of
                {ok, Pid} ->
                    {found, Region, Pid};
                not_found ->
                    find_user_in_regions(User, Rest, Endpoints)
            end
    end.

query_nodes_for_user(_User, []) ->
    not_found;
query_nodes_for_user(User, [Node | Rest]) ->
    case rpc:call(Node, iris_core, lookup_user, [User], 2000) of
        {ok, _N, Pid} when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            query_nodes_for_user(User, Rest)
    end.

get_stats() ->
    %% Delegate to async router stats
    iris_async_router:get_stats().
