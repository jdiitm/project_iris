-module(iris_edge_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    %% Get postart(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(iris_edge, port),
    io:format("Starting Edge App on port ~p...~n", [Port]),
    iris_router:start_link(), %% Start the router gen_server
    iris_edge_listener:start_link(Port).

stop(_State) ->
    ok.
