-module(iris_durable_batcher_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(POOL_SIZE, 8).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Create child specs for each durable batcher shard
    Children = [
        #{
            id => {iris_durable_batcher, ShardId},
            start => {iris_durable_batcher, start_link, [ShardId]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_durable_batcher]
        } || ShardId <- lists:seq(1, ?POOL_SIZE)
    ],
    
    {ok, {SupFlags, Children}}.
