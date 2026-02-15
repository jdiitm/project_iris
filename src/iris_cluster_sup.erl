-module(iris_cluster_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 60},
    Children = [
        #{id => iris_cluster_manager,
          start => {iris_cluster_manager, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_durable_batcher_sup,
          start => {iris_durable_batcher_sup, start_link, []},
          type => supervisor, restart => permanent},
        #{id => iris_core_registry,
          start => {iris_core_registry, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_status_batcher_sup,
          start => {iris_status_batcher_sup, start_link, [100]},
          type => supervisor, restart => permanent},
        #{id => iris_cluster_join_worker,
          start => {iris_cluster_join_worker, start_link, [cluster_join]},
          type => worker, restart => transient},
        #{id => iris_region_wiring_worker,
          start => {iris_cluster_join_worker, start_link, [region_wiring]},
          type => worker, restart => transient}
    ],
    {ok, {SupFlags, Children}}.
