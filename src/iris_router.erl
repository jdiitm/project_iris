-module(iris_router).
-export([route/2]).

-define(CORE_NODE, core_node()).

core_node() ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom("iris_core@" ++ Host).

route(ToUser, Msg) ->
    %% 1. Lookup User in Presence (RPC to Core)
    case rpc:call(?CORE_NODE, iris_core, lookup_user, [ToUser]) of
        {ok, Node, Pid} ->
            %% 2. If online, forward to Pid on that Node
            io:format("Routing msg to ~p on ~p~n", [ToUser, Node]),
            %% In a real system we might send a structured message.
            %% Here we just send the raw binary or a tuple.
            %% Using ! (bang) requires valid Pid.
            rpc:cast(Node, erlang, send, [Pid, {deliver_msg, Msg}]);
        {error, not_found} ->
            %% 3. If offline, store in RocksDB (RPC to Core)
            io:format("User ~p offline. Storing.~n", [ToUser]),
            rpc:call(?CORE_NODE, iris_core, store_offline, [ToUser, Msg]);
        {badrpc, Reason} ->
            io:format("RPC Error routing to ~p: ~p~n", [ToUser, Reason])
    end.
