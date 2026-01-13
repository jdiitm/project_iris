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
    %% Eager Cluster Mesh: Connect to configured Core Nodes immediately
    %% This ensures 'pg' scopes sync and the node is visible in the cluster.
    CoreNodes = application:get_env(iris_edge, core_nodes, []),
    case CoreNodes of
        [] -> 
            logger:warning("No 'core_nodes' configured! This Edge node is isolated.");
        _ ->
            logger:info("Attempting to mesh with Core Nodes: ~p", [CoreNodes]),
            lists:foreach(fun(Node) ->
                case net_adm:ping(Node) of
                    pong -> logger:info("Successfully connected to ~p", [Node]);
                    pang -> logger:warning("Failed to connect to ~p", [Node])
                end
            end, CoreNodes)
    end,

    iris_edge_sup:start_link().

stop(_State) ->
    logger:info("Stopping Iris Edge Application..."),
    ok.
