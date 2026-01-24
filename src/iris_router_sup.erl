-module(iris_router_sup).
-behaviour(supervisor).

%% AUDIT FIX: Support both explicit pool size and auto-tuning
-export([start_link/0, start_link/1, init/1]).

-define(SERVER, ?MODULE).

%% Start with auto-tuned pool size
start_link() ->
    PoolSize = iris_async_router:get_pool_size(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSize]).

%% Start with explicit pool size
start_link(PoolSize) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSize]).

init([PoolSize]) ->
    logger:info("Router supervisor starting with pool size: ~p", [PoolSize]),
    
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
