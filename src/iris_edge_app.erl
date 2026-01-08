-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    logger:info("Starting Iris Edge Application..."),
    
    %% Start the root supervisor which manages all Edge components:
    %% - ETS tables (owned by supervisor)
    %% - Circuit Breaker
    %% - Router Pool Supervisor
    %% - TCP Listener
    %% - WebSocket Listener
    iris_edge_sup:start_link().

stop(_State) ->
    logger:info("Stopping Iris Edge Application..."),
    ok.
