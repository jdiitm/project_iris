-module(iris_router_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).

start_link(PoolSize) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSize]).

init([PoolSize]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},

    WorkerSpecs = [
        #{id => list_to_atom("iris_router_" ++ integer_to_list(I)),
          start => {iris_router_worker, start_link, [I]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [iris_router_worker]}
        || I <- lists:seq(1, PoolSize)
    ],

    {ok, {SupFlags, WorkerSpecs}}.
