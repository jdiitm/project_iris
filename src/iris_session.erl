-module(iris_session).
-export([handle_packet/4, terminate/1]).

-include_lib("kernel/include/inet.hrl").

-define(CORE_NODE, core_node()).

core_node() ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom("iris_core@" ++ Host).

%% handle_packet(Packet, User, TransportPid, TransportMod) -> {ok, NewUser, Actions}
%% Actions = [ {send, Bin} | {send_batch, [Bin]} | close ]
handle_packet({login, User}, _Current, TransportPid, _Mod) ->
    io:format("User logged in: ~p~n", [User]),
    %% Register with Core
    rpc:call(?CORE_NODE, iris_core, register_user, [User, node(), TransportPid]),

    %% Optimization: Register Locally for switching
    ets:insert(local_presence, {User, TransportPid}),

    %% Retrieve Offline Messages
    OfflineMsgs = case rpc:call(?CORE_NODE, iris_core, retrieve_offline, [User]) of
         Msgs when is_list(Msgs) -> Msgs;
         Error -> 
             io:format("Error retrieving offline msgs: ~p~n", [Error]),
             []
    end,
    
    %% Actions: Ack Login + Send Offline Msgs
    Actions = [{send, <<3, "LOGIN_OK">>}] ++ [{send, Msg} || Msg <- OfflineMsgs],
    {ok, User, Actions};

handle_packet({send_message, Target, Msg}, User, _Pid, _Mod) ->
    %% io:format("Sending msg to ~p~n", [Target]),
    iris_router:route(Target, Msg),
    {ok, User, []};

handle_packet({batch_send, Target, Blob}, User, _Pid, _Mod) ->
    %% Optimized Fan-In
    Msgs = iris_proto:unpack_batch(Blob),
    rpc:call(?CORE_NODE, iris_core, store_batch, [Target, Msgs]),
    {ok, User, []};

handle_packet({get_status, TargetUser}, User, _Pid, _Mod) ->
    %% Pull-based status check with EDGE CACHING
    Now = os:system_time(seconds),
    CacheResult = ets:lookup(presence_cache, TargetUser),
    StatusTuple = case CacheResult of
        [{TargetUser, CachedStatus, CachedTime, InsertTime}] 
          when Now - InsertTime < 5 -> 
             %% Cache Hit
             {CachedStatus, CachedTime};
        [{_, _, _, _}] ->
             %% Cache EXPIRED
             fetch_and_cache(TargetUser, Now);
        [] ->
             %% Cache EMPTY
             fetch_and_cache(TargetUser, Now)
    end,
    
    {FinalState, FinalTime} = StatusTuple,
    Resp = iris_proto:encode_status(TargetUser, FinalState, FinalTime),
    {ok, User, [{send, Resp}]};

handle_packet({ack, MsgId}, User, _Pid, _Mod) ->
    {ok, User, [{ack_received, MsgId}]};

handle_packet({error, _}, User, _Pid, _Mod) ->
     io:format("Protocol Error~n"),
     {ok, User, []}. %% Or close?

fetch_and_cache(TargetUser, Now) ->
    Result = case rpc:call(?CORE_NODE, iris_core, get_status, [TargetUser]) of
        {online, true, _} -> {online, 0};
        {online, false, LS} -> {offline, LS}
    end,
    {S, T} = Result,
    ets:insert(presence_cache, {TargetUser, S, T, Now}),
    Result.

terminate(User) ->
    case User of
        undefined -> ok;
        _ -> 
            ets:delete(local_presence, User),
            rpc:cast(?CORE_NODE, iris_core, update_status, [User, offline])
    end.
