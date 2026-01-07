-module(iris_status_batcher_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callback
-export([init/1]).

-define(POOL_SIZE, 100).

%%%===================================================================
%%% API
%%%===================================================================

start_link(PoolSize) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [PoolSize]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PoolSize]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% Create child specs for each batcher worker
    Children = [
        #{
            id => {iris_status_batcher, Id},
            start => {iris_status_batcher, start_link, [Id]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_status_batcher]
        } || Id <- lists:seq(0, PoolSize - 1)
    ],

    {ok, {SupFlags, Children}}.
