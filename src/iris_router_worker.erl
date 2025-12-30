-module(iris_router_worker).
-export([start_link/1, get_stats/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CORE_NODE, core_node()).
-define(STATS_COUNTER, iris_router_stats).
-define(IDX_MSGS, 1).
-define(IDX_TIME, 2).

core_node() ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom("iris_core@" ++ Host).

%% Start with ID (integer)
start_link(Id) ->
    Name = list_to_atom("iris_router_" ++ integer_to_list(Id)),
    gen_server:start_link({local, Name}, ?MODULE, [], []).

get_stats(Id) ->
    Name = list_to_atom("iris_router_" ++ integer_to_list(Id)),
    try gen_server:call(Name, get_stats)
    catch _:_ -> {0, 0} end.

init([]) ->
    Ref = counters:new(2, []),
    {ok, Ref}.

handle_call(get_stats, _From, Ref) ->
    Msgs = counters:get(Ref, ?IDX_MSGS),
    Time = counters:get(Ref, ?IDX_TIME),
    Avg = case Msgs of 0 -> 0; _ -> Time / Msgs end,
    {reply, {Msgs, Avg}, Ref};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast({route, User, Msg, StartTime}, Ref) ->
    case rpc:call(?CORE_NODE, iris_core, lookup_user, [User]) of
        {ok, _Node, Pid} ->
            Pid ! {deliver_msg, Msg};
        {error, not_found} ->
            rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]);
        {badrpc, Reason} ->
            io:format("RPC Error routing to ~p: ~p~n", [User, Reason])
    end,
    End = os:system_time(microsecond),
    Diff = End - StartTime,
    counters:add(Ref, ?IDX_MSGS, 1),
    counters:add(Ref, ?IDX_TIME, Diff),
    {noreply, Ref};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
