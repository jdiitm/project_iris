-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    %% Get port from env or default
    Port = application:get_env(iris_edge, port, 8085),
    io:format("DEBUG: Starting Edge App. Port: ~p, IsInteger: ~p~n", [Port, is_integer(Port)]),
    iris_edge_listener:start_link(Port).

stop(_State) ->
    ok.
