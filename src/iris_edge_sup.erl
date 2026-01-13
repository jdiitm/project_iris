-module(iris_edge_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    %% Ensure PG (Default Scope) is started safely
    try pg:start_link() catch _:_ -> ok end,

    %% Get configuration
    {ok, Port} = application:get_env(iris_edge, port),
    PoolSize = erlang:system_info(schedulers),
    WsPort = Port + 1,
    
    logger:info("Iris Edge Supervisor starting (TCP: ~p, WS: ~p, Pool: ~p)", 
                [Port, WsPort, PoolSize]),
    
    %% Create ETS tables owned by THIS supervisor process (permanent)
    %% These tables survive child crashes because the supervisor owns them
    ets:new(local_presence_v2, [set, named_table, public, 
                             {read_concurrency, true}, 
                             {write_concurrency, true}]),
    
    ets:new(presence_cache, [set, named_table, public, 
                             {read_concurrency, true}, 
                             {write_concurrency, true}]),
    
    logger:info("ETS tables created (owned by supervisor)"),
    
    %% Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Child specifications
    Children = [
        %% Circuit Breaker - protects against Core node failures
        #{
            id => iris_circuit_breaker,
            start => {iris_circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_circuit_breaker]
        }
    ] ++ [
        %% ROUTER POOL (Multi-Core Optimization)
        %% Spawn 8 router shards to utilize all vCPUs
        #{
            id => list_to_atom("iris_async_router_" ++ integer_to_list(I)),
            start => {iris_async_router, start_link, [I]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_async_router]
        } || I <- lists:seq(1, 8)
    ] ++ [
        %% TCP Listener - handles raw TCP connections
        #{
            id => iris_tcp_listener,
            start => {iris_edge_listener, start_link, [Port]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_edge_listener]
        },
        
        %% WebSocket Listener - handles WS connections
        #{
            id => iris_ws_listener,
            start => {iris_edge_listener, start_link, [WsPort, iris_ws_lite]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_edge_listener]
        }
    ],

    {ok, {SupFlags, Children}}.
