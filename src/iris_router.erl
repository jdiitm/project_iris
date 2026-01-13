-module(iris_router).
-export([start_link/0, route/2, get_stats/0]).
-export([start_link/0, route/2, get_stats/0]).

-define(POOL_SIZE, erlang:system_info(schedulers)).

core_node() ->
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core"
    end,
    list_to_atom(CoreName ++ "@" ++ Host).

%% Note: start_link is removed/unused as this is not a process anymore, 
%% but we keep it returning ignore or ok if supervisors call it? 
%% Actually, iris_edge_app will invoke worker start_links. 
%% We can remove start_link here.

start_link() -> ignore. %% Compatibility

route(User, Msg) ->
    %% Switch to Async Router for Planetary Scale
    %% Sub-millisecond local lookup + Non-blocking remote delivery
    iris_async_router:route_async(User, Msg).

get_stats() ->
    %% Delegate to async router stats
    iris_async_router:get_stats().
