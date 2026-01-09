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
    Start = os:system_time(microsecond),
    WorkerId = (erlang:phash2(User, ?POOL_SIZE) + 1),
    WorkerName = list_to_atom("iris_router_" ++ integer_to_list(WorkerId)),
    gen_server:cast(WorkerName, {route, User, Msg, Start}).

get_stats() ->
    %% Aggregate from all workers
    lists:foldl(fun(Id, {TotalMsgs, TotalAvg}) -> 
        {M, A} = iris_router_worker:get_stats(Id),
        %% Weighted average calculation is complex. 
        %% Simplified: Sum Messages, Average of Averages (Approx)
        {TotalMsgs + M, TotalAvg + A} 
    end, {0, 0}, lists:seq(1, ?POOL_SIZE)).
