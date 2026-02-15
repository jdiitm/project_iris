-module(iris_foundation_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 60},
    Children = [
        #{id => iris_health_handler,
          start => {iris_health_handler, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_metrics,
          start => {iris_metrics, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_mnesia_guard,
          start => {iris_mnesia_guard, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_flow_controller,
          start => {iris_flow_controller, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_dedup,
          start => {iris_dedup, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_presence,
          start => {iris_presence, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_partition_guard,
          start => {iris_partition_guard, start_link, []},
          type => worker, restart => permanent}
    ],
    {ok, {SupFlags, Children}}.
