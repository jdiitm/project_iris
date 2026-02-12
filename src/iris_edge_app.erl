-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([check_mtls_enforcement/0]).

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
    logger:info("Iris Edge shutting down -- draining connections..."),
    %% Close listen sockets first (stop accepting new connections).
    %% supervisor:terminate_child handles this via iris_edge_listener:terminate/2
    %% which already closes the listen socket.
    %% Give active connections time to complete in-flight operations.
    %% Each iris_edge_conn:terminate/3 saves pending ACKs and flushes messages.
    DrainMs = application:get_env(iris_edge, shutdown_drain_ms, 5000),
    timer:sleep(DrainMs),
    logger:info("Iris Edge stopped."),
    ok.

%% @doc Check mTLS enforcement config. Exits if enforce_mtls=true but
%% ssl_dist_optfile is not set. Called from start/2.
-spec check_mtls_enforcement() -> ok.
check_mtls_enforcement() ->
    %% G1 FIX: In production, default enforce_mtls to true (NFR-15 mandatory).
    Env = application:get_env(iris_core, env, undefined),
    Default = case Env of
        production -> true;
        _          -> false
    end,
    case application:get_env(iris_core, enforce_mtls, Default) of
        true ->
            case init:get_argument(ssl_dist_optfile) of
                {ok, _} -> ok;
                error ->
                    logger:error("CRITICAL: enforce_mtls=true but ssl_dist_optfile not set"),
                    exit(mtls_not_configured)
            end;
        false ->
            logger:warning("mTLS NOT enforced (NFR-15). Set enforce_mtls=true for production."),
            ok
    end.
