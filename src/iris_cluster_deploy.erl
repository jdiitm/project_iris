-module(iris_cluster_deploy).
-export([deploy/1, deploy_local/0, check_versions/1, load_modules/1]).

%% @doc Deploys the current beam files from this node to all TargetNodes.
deploy(TargetNodes) ->
    io:format("Starting Cluster Deployment...~n"),
    LocalBeams = filelib:wildcard("ebin/*.beam"),
    
    lists:foreach(fun(Node) ->
        io:format("Deploying to ~p...~n", [Node]),
        case push_code(Node, LocalBeams) of
            ok -> io:format("  -> Success.~n");
            Error -> io:format("  -> FAILED: ~p~n", [Error])
        end
    end, TargetNodes),
    io:format("Deployment Complete.~n").

%% @doc hot-loads code on the local node (simulating an upgrade).
deploy_local() ->
    deploy([node()]).

push_code(Node, Beams) ->
    %% 1. Read binary of all beams
    Modules = lists:map(fun(Path) ->
        Mod = list_to_atom(filename:basename(Path, ".beam")),
        {ok, Bin, _File} = prim_loader:get_file(Path),
        {Mod, Bin}
    end, Beams),
    
    %% 2. RPC to Target: Load Code
    rpc:call(Node, ?MODULE, load_modules, [Modules]).


load_modules(Modules) ->
    lists:foreach(fun({Mod, Bin}) ->
        %% Standard Erlang Hot Code Load Process
        code:soft_purge(Mod),
        case code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Bin) of
            {module, Mod} -> 
                %% If it's a gen_server/statem, we might need sys:change_code logic
                %% but code:load_binary handles the 'switch' if properly callbacked.
                logger:info("Hot loaded ~p", [Mod]);
            {error, Reason} ->
                logger:error("Failed to hot load ~p: ~p", [Mod, Reason])
        end
    end, Modules),
    ok.

check_versions(Nodes) ->
    lists:foreach(fun(Node) ->
        Vsns = rpc:call(Node, application, which_applications, []),
        io:format("Node ~p Versions: ~p~n", [Node, Vsns])
    end, Nodes).
