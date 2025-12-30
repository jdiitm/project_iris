-module(iris_router).
-export([start_link/0, route/2, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CORE_NODE, core_node()).
-define(SERVER, ?MODULE).
-define(STATS_COUNTER, iris_router_stats).
-define(IDX_MSGS, 1).
-define(IDX_TIME, 2). %% Total time in microseconds

core_node() ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom("iris_core@" ++ Host).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

route(User, Msg) ->
    Start = os:system_time(microsecond),
    gen_server:cast(?SERVER, {route, User, Msg, Start}).

get_stats() ->
    gen_server:call(?SERVER, get_stats).

init([]) ->
    %% Initialize counters: 2 slots (Msgs, Time)
    %% Actually counters:new returns a Ref. We need to store it.
    %% But counters are persistent if stored in persistent_term or purely functional passing?
    %% Since this is a gen_server, we can keep it in state.
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
    %% Route Logic
    case rpc:call(?CORE_NODE, iris_core, lookup_user, [User]) of
        {ok, _Node, Pid} ->
            Pid ! {deliver_msg, Msg};
        {error, not_found} ->
            %% Store offline
            rpc:call(?CORE_NODE, iris_core, store_offline, [User, Msg]);
        {badrpc, Reason} ->
            io:format("RPC Error routing to ~p: ~p~n", [User, Reason])
    end,
    %% Telemetry
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
