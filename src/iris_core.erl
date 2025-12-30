-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([register_user/3, lookup_user/1, store_offline/2, retrieve_offline/1]).

%% Application Callbacks
start(_StartType, _StartArgs) ->
    %% Check if we are the core node
    [Name, _Host] = string:tokens(atom_to_list(node()), "@"),
    case Name of
        "iris_core" -> init_db();
        _ -> ok %% Not core, skip DB init
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% Supervisor Callbacks
init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.

%% API

init_db() ->
    %% Ensure Mnesia is stopped for schema check (bootstrap)
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    case mnesia:create_table(presence, 
        [{ram_copies, [node()]}, 
         {attributes, [user_id, node, pid]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, presence}} -> ok;
        Err1 -> io:format("Mnesia Init Error (presence): ~p~n", [Err1])
    end,
    
    case mnesia:create_table(offline_msg, 
        [{disc_copies, [node()]}, 
         {type, bag},
         {attributes, [user_id, timestamp, msg]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, offline_msg}} -> ok;
        Err2 -> io:format("Mnesia Init Error (offline_msg): ~p~n", [Err2])
    end,
    
    io:format("Core DB Initialized. Mnesia Status: ~p~n", [mnesia:system_info(is_running)]),
    io:format("Mnesia Local Tables: ~p~n", [mnesia:system_info(local_tables)]).

register_user(User, Node, Pid) ->
    F = fun() ->
        mnesia:write({presence, User, Node, Pid})
    end,
    mnesia:activity(transaction, F).

lookup_user(User) ->
    case mnesia:dirty_read(presence, User) of
        [{presence, User, Node, Pid}] -> {ok, Node, Pid};
        [] -> {error, not_found}
    end.

store_offline(User, Msg) ->
    %% Only called on Core
    iris_offline_storage:store(User, Msg).

retrieve_offline(User) ->
    iris_offline_storage:retrieve(User).
