-module(iris_messaging_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 60},
    Children = [
        #{id => iris_group,
          start => {iris_group, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_shard,
          start => {iris_shard, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_keys,
          start => {iris_keys, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_region_bridge,
          start => {iris_region_bridge, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_read_receipts,
          start => {iris_read_receipts, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_mailbox_guard,
          start => {iris_mailbox_guard, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_mailbox_monitor,
          start => {iris_mailbox_monitor, start_link, []},
          type => worker, restart => permanent},
        #{id => iris_efficiency_monitor,
          start => {iris_efficiency_monitor, start_link, []},
          type => worker, restart => permanent}
    ],
    {ok, {SupFlags, Children}}.
