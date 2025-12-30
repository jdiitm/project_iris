-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, Port} = application:get_env(iris_edge, port),
    io:format("Starting Edge App on port ~p...~n", [Port]),
    
    %% Start Router Pool (Size = Schedulers)
    PoolSize = erlang:system_info(schedulers),
    io:format("Starting Router Pool (Size: ~p)...~n", [PoolSize]),
    lists:foreach(fun(I) -> 
        iris_router_worker:start_link(I) 
    end, lists:seq(1, PoolSize)),
    
    iris_edge_listener:start_link(Port).

stop(_State) ->
    ok.
