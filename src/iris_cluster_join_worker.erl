-module(iris_cluster_join_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

-spec start_link(cluster_join | region_wiring) -> {ok, pid()} | {error, term()}.
start_link(Task) ->
    gen_server:start_link(?MODULE, Task, []).

init(cluster_join) ->
    erlang:send_after(1000, self(), do_cluster_join),
    {ok, cluster_join};
init(region_wiring) ->
    erlang:send_after(5000, self(), do_region_wiring),
    {ok, region_wiring}.

handle_info(do_cluster_join, State) ->
    try
        iris_core_registry:join(),
        KnownPeers = application:get_env(iris_core, join_seeds, []),
        OtherPeers = [P || P <- KnownPeers, P =/= node()],
        case lists:search(fun(P) -> net_adm:ping(P) == pong end, OtherPeers) of
            {value, LivePeer} ->
                logger:info("Auto-joining cluster via ~p", [LivePeer]),
                iris_core:join_cluster(LivePeer);
            false ->
                logger:info("No cluster peers found, standalone mode")
        end
    catch Class:Reason:Stack ->
        logger:error("Cluster join worker crashed: ~p:~p~n~p",
                     [Class, Reason, Stack])
    end,
    {stop, normal, State};
handle_info(do_region_wiring, State) ->
    try
        case application:get_env(iris_core, regions, []) of
            [] -> ok;
            Regions when length(Regions) > 0 ->
                case iris_core:is_core_node(node()) of
                    true ->
                        logger:info("Regions configured, attempting to wire replication..."),
                        iris_core:init_cross_region_replication();
                    false -> ok
                end
        end
    catch Class:Reason:Stack ->
        logger:error("Region wiring worker crashed: ~p:~p~n~p",
                     [Class, Reason, Stack])
    end,
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
