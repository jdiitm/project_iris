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
    %% Use ETS instead of Mnesia due to missing dependency
    try ets:new(presence, [named_table, public, set, {keypos, 2}]) of
        _ -> io:format("Core DB (ETS) Initialized.~n")
    catch
        _:_ -> io:format("Core DB (ETS) already exists.~n")
    end,
    iris_rocksdb:init(),
    ok.

register_user(User, Node, Pid) ->
    ets:insert(presence, {presence, User, Node, Pid}),
    {atomic, ok}.

lookup_user(User) ->
    case ets:lookup(presence, User) of
        [{presence, User, Node, Pid}] -> {ok, Node, Pid};
        [] -> {error, not_found}
    end.

store_offline(User, Msg) ->
    %% Only called on Core
    iris_rocksdb:store(User, Msg).
