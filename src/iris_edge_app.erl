-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, Port} = application:get_env(iris_edge, port),
    io:format("Starting Edge App on port ~p...~n", [Port]),
    
    %% Start Router Pool (Size = Schedulers)
    PoolSize = erlang:system_info(schedulers),
    io:format("Starting Router Pool (Size: ~p)...~n", [PoolSize]),
    
    %% Optimization: Local Switching Cache
    %% Public named table for fast O(1) local lookup
    ets:new(local_presence, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    
    %% Optimization: Presence Cache (TTL-based)
    %% Stores {User, Status, Timestamp, InsertTime}
    ets:new(presence_cache, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    
    %% Start Circuit Breaker
    iris_circuit_breaker:start_link(),

    %% Start Router Supervisor
    {ok, _} = iris_router_sup:start_link(PoolSize),
    
    %% Start TCP Listener
    iris_edge_listener:start_link(Port),

    %% Start WebSocket Listener (Port + 1)
    WsPort = Port + 1,
    io:format("Starting WebSocket Listener on port ~p...~n", [WsPort]),
    iris_edge_listener:start_link(WsPort, iris_ws_lite).

stop(_State) ->
    ok.
