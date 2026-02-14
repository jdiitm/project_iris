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
    try pg:start_link()
    catch
        error:{already_started, _} -> ok;
        Class:Reason ->
            logger:warning("pg:start_link() failed in edge_sup: ~p:~p (non-fatal)", [Class, Reason]),
            ok
    end,

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
    
    %% Per-IP connection rate limiting table (RFC Section 10)
    %% Owned by supervisor so it survives listener crashes.
    ets:new(iris_conn_rate, [public, named_table, bag,
                             {write_concurrency, true},
                             {read_concurrency, true}]),
    
    %% Session cache for connection resume (RFC Section 3.4)
    iris_session_cache:start(),
    
    logger:info("ETS tables created (owned by supervisor)"),
    
    %% Supervisor flags
    %% AUDIT MITIGATION V1: Reduced from 10 to 5 restarts per 60s.
    %% Consistent with iris_core supervisor hardening.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    
    %% Child specifications
    Children = [
        %% Health Check HTTP endpoint (/health, /ready, /metrics)
        #{
            id => iris_health_handler,
            start => {iris_health_handler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_health_handler]
        },

        %% Circuit Breaker - protects against Core node failures
        #{
            id => iris_circuit_breaker,
            start => {iris_circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_circuit_breaker]
        },

        %% Auth: JWT verification and token lifecycle
        #{
            id => iris_auth,
            start => {iris_auth, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_auth]
        },

        %% Rate Limiter: Per-user and per-IP throttling
        #{
            id => iris_rate_limiter,
            start => {iris_rate_limiter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_rate_limiter]
        },

        %% Ingress Guard: Connection-level abuse prevention
        #{
            id => iris_ingress_guard,
            start => {iris_ingress_guard, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_ingress_guard]
        },

        %% Discovery: Core node discovery via pg
        #{
            id => iris_discovery,
            start => {iris_discovery, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_discovery]
        }
    ] ++ [
        %% ROUTER POOL (Multi-Core Optimization)
        %% AUDIT FIX: Auto-tune pool size based on scheduler count
        %% Uses iris_async_router:get_pool_size() for dynamic sizing
        #{
            id => list_to_atom("iris_async_router_" ++ integer_to_list(I)),
            start => {iris_async_router, start_link, [I]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iris_async_router]
        } || I <- lists:seq(1, iris_async_router:get_pool_size())
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
