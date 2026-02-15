-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([validate_production_config/0, validate_auth_mode/0]).
-export([prep_stop/1, is_draining/0]).
%% Config validation exports (for testing)
-export([validate_num_acceptors/1, validate_rate_limits/2,
         validate_tls_cert/1, validate_replication_factor/1]).

start(_Type, _Args) ->
    logger:info("Starting Iris Edge Application..."),

    %% Fail-fast in production if critical config is missing
    validate_production_config(),

    %% Warn if zstd NIF is not available
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

    %% Verify mTLS is configured (DRY -- delegates to iris_core)
    iris_core:check_mtls_enforcement(),

    iris_edge_sup:start_link().

%% @doc Called BEFORE supervisor tree is terminated.
%% Sets draining flag (makes /ready return 503), then sleeps for drain period
%% to let in-flight operations complete and load balancer stop routing.
prep_stop(State) ->
    logger:info("Iris Edge shutting down -- draining connections..."),
    persistent_term:put(iris_edge_draining, true),
    DrainMs = application:get_env(iris_edge, shutdown_drain_ms, 5000),
    timer:sleep(DrainMs),
    logger:info("Iris Edge drain complete, proceeding with shutdown."),
    State.

stop(_State) ->
    %% Drain already happened in prep_stop/1; just log and return.
    logger:info("Iris Edge stopped."),
    ok.

%% @doc Returns true if the application is in graceful shutdown drain phase.
-spec is_draining() -> boolean().
is_draining() ->
    persistent_term:get(iris_edge_draining, false).

%% @doc Validate critical config in production mode.
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
            %% Validate auth_mode (H-3: edge nodes must not be signers)
            case validate_auth_mode() of
                ok -> ok;
                {error, signer_on_edge} ->
                    logger:error("FATAL: auth_mode=signer on edge node in production. "
                                 "Edge nodes MUST use auth_mode=verifier. "
                                 "Only auth_service nodes may be signers."),
                    init:stop(1),
                    exit(signer_on_edge_node)
            end,
            %% Validate additional production config
            validate_extended_config(),
            ok;
        _ ->
            %% Non-production: still validate but only warn
            validate_extended_config_warn(),
            ok
    end.

%% =============================================================================
%% Extended Config Validation (Startup)
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
%% Configuration Validation Functions
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

%% =============================================================================
%% Auth Mode Validation (H-3)
%% =============================================================================

%% @doc Validate auth_mode configuration.
%% In production, edge nodes MUST use auth_mode=verifier.
%% Only auth_service nodes may use auth_mode=signer.
-spec validate_auth_mode() -> ok | {error, signer_on_edge}.
validate_auth_mode() ->
    case application:get_env(iris_edge, deployment_mode, development) of
        production ->
            AuthMode = application:get_env(iris_edge, auth_mode, verifier),
            NodeRole = application:get_env(iris_edge, node_role, edge),
            case {AuthMode, NodeRole} of
                {signer, auth_service} -> ok;
                {signer, _} -> {error, signer_on_edge};
                _ -> ok
            end;
        _ ->
            ok
    end.

%% check_mtls_enforcement/0 consolidated into iris_core.erl
