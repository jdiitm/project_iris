-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([register_user/3, lookup_user/1, store_offline/2, retrieve_offline/1]).
-export([get_bucket_count/1, set_bucket_count/2]).

%% Application Callbacks
start(_StartType, _StartArgs) ->
    %% Check if we are the core node
    io:format("App Start. Node: ~p~n", [node()]),
    Parts = string:tokens(atom_to_list(node()), "@"),
    io:format("Node Parts: ~p~n", [Parts]),
    case Parts of
        ["iris_core", _] -> 
            io:format("Initializing DB on Core...~n"),
            init_db();
        ["iris_core"] -> %% Handle no host case
            io:format("Initializing DB on Core (No Host)...~n"),
            init_db();
        _ -> 
            io:format("Not Core. Skipping DB init.~n"),
            ok
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
    io:format("Stopping Mnesia...~n"),
    mnesia:stop(),
    io:format("Creating Schema...~n"),
    mnesia:create_schema([node()]),
    io:format("Starting Mnesia...~n"),
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

    case mnesia:create_table(user_meta, 
        [{disc_copies, [node()]}, 
         {attributes, [user_id, bucket_count]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_meta}} -> ok;
        Err3 -> io:format("Mnesia Init Error (user_meta): ~p~n", [Err3])
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
    Count = get_bucket_count(User),
    iris_offline_storage:store(User, Msg, Count).

retrieve_offline(User) ->
    Count = get_bucket_count(User),
    iris_offline_storage:retrieve(User, Count).

get_bucket_count(User) ->
    case mnesia:dirty_read(user_meta, User) of
        [{user_meta, User, Count}] -> Count;
        [] -> 1
    end.

set_bucket_count(User, Count) ->
    F = fun() ->
        mnesia:write({user_meta, User, Count})
    end,
    mnesia:activity(transaction, F).
