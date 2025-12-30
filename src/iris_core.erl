-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([register_user/3, lookup_user/1, store_offline/2]).

%% Application Callbacks
start(_StartType, _StartArgs) ->
    %% Check if we are the core node
    NodeName = atom_to_list(node()),
    case string:str(NodeName, "iris_core") of
        0 -> ok; %% Not core, skip DB init
        _ -> init_db()
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% Supervisor Callbacks
init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.

%% API

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(presence, 
        [{ram_copies, [node()]}, 
         {attributes, [user_id, node, pid]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, presence}} -> ok;
        Err -> io:format("Mnesia Init Error: ~p~n", [Err])
    end,
    iris_rocksdb:init(),
    io:format("Core DB Initialized.~n").

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
    iris_rocksdb:store(User, Msg).
