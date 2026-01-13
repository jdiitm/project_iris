#!/usr/bin/env escript
%%! -name Reloader@127.0.0.1 -setcookie iris_secret

main(_) ->
    Nodes = [
        'iris_edge1@100.82.212.50',
        'iris_core1@100.95.21.52',
        'iris_core2@100.68.74.48'
    ],
    Modules = [iris_session, iris_edge_conn, iris_router_worker, iris_extreme_gen],
    
    io:format("Reloading code on ~p...~n", [Nodes]),
    
    lists:foreach(fun(Node) ->
        io:format("  Node: ~p~n", [Node]),
        case net_adm:ping(Node) of
            pong ->
                lists:foreach(fun(Mod) ->
                    rpc:call(Node, code, purge, [Mod]),
                    case rpc:call(Node, code, load_file, [Mod]) of
                        {module, Mod} -> io:format("    ~p: OK~n", [Mod]);
                        Error -> io:format("    ~p: ERROR ~p~n", [Mod, Error])
                    end
                end, Modules);
            pang ->
                io:format("    UNREACHABLE (pang)~n")
        end
    end, Nodes),
    ok.
