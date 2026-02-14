-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([validate_production_config/0]).
%% AUDIT MITIGATION P1-3: Config validation exports (for testing)
-export([validate_num_acceptors/1, validate_rate_limits/2,
         validate_tls_cert/1, validate_replication_factor/1]).

start(_Type, _Args) ->
    logger:info("Starting Iris Edge Application..."),

    %% AUDIT MITIGATION P0-1: Fail-fast in production if critical config is missing
    validate_production_config(),

    %% AUDIT FIX: Warn if zstd NIF is not available
    case filelib:is_file("priv/iris_zstd_nif.so") of
        true -> ok;
        false ->
            logger:warning("zstd NIF not loaded (priv/iris_zstd_nif.so missing) -- "
                           "zstd compression unavailable, clients will fall back to zlib")
    end,
    
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

    %% AUDIT 3.2/6.1: Verify mTLS is configured (DRY -- delegates to iris_core)
    iris_core:check_mtls_enforcement(),

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

%% @doc AUDIT MITIGATION P0-1: Validate critical config in production mode.
%% Rejects empty core_nodes and missing JWT secret when deployment_mode=production.
-spec validate_production_config() -> ok.
validate_production_config() ->
    case application:get_env(iris_edge, deployment_mode, development) of
        production ->
            %% core_nodes MUST be set -- edge cannot route without them
            case application:get_env(iris_edge, core_nodes, []) of
                [] ->
                    logger:error("FATAL: core_nodes is empty in production mode -- "
                                 "edge node cannot route messages. Set core_nodes in config."),
                    init:stop(1),
                    exit(core_nodes_empty);
                _ -> ok
            end,
            %% JWT secret MUST be set when auth is enabled
            case application:get_env(iris_edge, auth_enabled, true) of
                true ->
                    case application:get_env(iris_edge, jwt_secret, undefined) of
                        undefined ->
                            case os:getenv("IRIS_JWT_SECRET") of
                                false ->
                                    logger:error("FATAL: auth_enabled=true but no jwt_secret configured "
                                                 "and IRIS_JWT_SECRET env var not set."),
                                    init:stop(1),
                                    exit(jwt_secret_missing);
                                _ -> ok
                            end;
                        _ -> ok
                    end;
                false -> ok
            end,
            %% AUDIT MITIGATION P1-3: Validate additional production config
            validate_extended_config(),
            ok;
        _ ->
            %% Non-production: still validate but only warn
            validate_extended_config_warn(),
            ok
    end.

%% =============================================================================
%% AUDIT MITIGATION P1-3: Extended Config Validation (Startup)
%% =============================================================================

validate_extended_config() ->
    %% Clamp num_acceptors
    Raw = application:get_env(iris_edge, num_acceptors, 500),
    Clamped = validate_num_acceptors(Raw),
    case Clamped =/= Raw of
        true ->
            logger:warning("num_acceptors clamped from ~p to ~p", [Raw, Clamped]),
            application:set_env(iris_edge, num_acceptors, Clamped);
        false -> ok
    end,
    %% Check rate limits
    Rate = application:get_env(iris_edge, rate_limit_default, 5),
    Burst = application:get_env(iris_edge, rate_burst_default, 20),
    case validate_rate_limits(Rate, Burst) of
        ok -> ok;
        {error, burst_less_than_rate} ->
            logger:error("FATAL: rate_burst_default (~p) < rate_limit_default (~p)", [Burst, Rate]),
            init:stop(1),
            exit(inverted_rate_limits)
    end,
    %% Check TLS cert files
    case application:get_env(iris_edge, tls_enabled, false) of
        true ->
            CertFile = application:get_env(iris_edge, tls_certfile, ""),
            case validate_tls_cert(CertFile) of
                ok -> ok;
                {error, cert_not_found} ->
                    logger:error("FATAL: tls_certfile ~p not found", [CertFile]),
                    init:stop(1),
                    exit(tls_cert_not_found)
            end;
        false -> ok
    end,
    ok.

validate_extended_config_warn() ->
    Raw = application:get_env(iris_edge, num_acceptors, 500),
    Clamped = validate_num_acceptors(Raw),
    case Clamped =/= Raw of
        true ->
            logger:warning("num_acceptors clamped from ~p to ~p", [Raw, Clamped]),
            application:set_env(iris_edge, num_acceptors, Clamped);
        false -> ok
    end,
    ok.

%% =============================================================================
%% AUDIT MITIGATION P1-3: Configuration Validation Functions
%% =============================================================================

%% @doc Clamp num_acceptors to safe range [1, 10000].
-spec validate_num_acceptors(integer()) -> pos_integer().
validate_num_acceptors(N) when N < 1 -> 1;
validate_num_acceptors(N) when N > 10000 -> 10000;
validate_num_acceptors(N) -> N.

%% @doc Reject rate limit configs where burst < sustained rate.
-spec validate_rate_limits(number(), number()) -> ok | {error, burst_less_than_rate}.
validate_rate_limits(Rate, Burst) when Burst >= Rate -> ok;
validate_rate_limits(_Rate, _Burst) -> {error, burst_less_than_rate}.

%% @doc Verify TLS cert file exists on disk.
-spec validate_tls_cert(string() | binary()) -> ok | {error, cert_not_found}.
validate_tls_cert(Path) ->
    case filelib:is_file(Path) of
        true -> ok;
        false -> {error, cert_not_found}
    end.

%% @doc Reject replication_factor <= 0.
-spec validate_replication_factor(integer()) -> ok | {error, invalid_replication_factor}.
validate_replication_factor(N) when is_integer(N), N > 0 -> ok;
validate_replication_factor(_) -> {error, invalid_replication_factor}.

%% AUDIT 5.2 DRY: check_mtls_enforcement/0 consolidated into iris_core.erl
