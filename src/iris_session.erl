-module(iris_session).
-export([handle_packet/4, terminate/1]).

-include_lib("kernel/include/inet.hrl").

%% Dynamic Core node discovery with failover
get_core_node() ->
    case iris_core_registry:get_core() of
        {ok, Node} -> Node;
        {error, _} -> legacy_core_node()
    end.

legacy_core_node() ->
    [NameStr, Host] = string:tokens(atom_to_list(node()), "@"),
    %% Replace iris_edgeX with iris_core, preserving any suffix like _1234567890
    CoreName = case string:str(NameStr, "iris_edge") of
        1 -> %% Starts with iris_edge
             re:replace(NameStr, "iris_edge[0-9]*", "iris_core", [{return, list}]);
        _ -> "iris_core" %% Fallback
    end,
    list_to_atom(CoreName ++ "@" ++ Host).

%% handle_packet(Packet, User, TransportPid, TransportMod) -> {ok, NewUser, Actions}
%% Actions = [ {send, Bin} | {send_batch, [Bin]} | close ]
handle_packet({login, User}, _Current, TransportPid, _Mod) ->
    io:format("User logged in: ~p~n", [User]),
    %% Register with Core
    rpc:call(get_core_node(), iris_core, register_user, [User, node(), TransportPid]),

    %% Optimization: Register Locally for switching
    ets:insert(local_presence, {User, TransportPid}),

    %% Retrieve Offline Messages
    OfflineMsgs = case rpc:call(get_core_node(), iris_core, retrieve_offline, [User]) of
         Msgs when is_list(Msgs) -> Msgs;
         Error -> 
             io:format("Error retrieving offline msgs: ~p~n", [Error]),
             []
    end,
    
    %% Actions: Ack Login first
    %% Then schedule offline messages to be delivered via reliable transport
    %% We use deliver_msg actions to ensure they go through the reliable path
    Actions = [{send, <<3, "LOGIN_OK">>}] ++ [{deliver_msg, Msg} || Msg <- OfflineMsgs],
    {ok, User, Actions};

handle_packet({send_message, Target, Msg}, User, _Pid, _Mod) ->
    %% io:format("Sending msg to ~p~n", [Target]),
    iris_router:route(Target, Msg),
    {ok, User, []};

handle_packet({batch_send, Target, Blob}, User, _Pid, _Mod) ->
    %% Optimized Fan-In
    Msgs = iris_proto:unpack_batch(Blob),
    rpc:call(get_core_node(), iris_core, store_batch, [Target, Msgs]),
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
    Result = case rpc:call(get_core_node(), iris_core, get_status, [TargetUser]) of
        {online, true, _} -> {online, 0};
        {online, false, LS} -> {offline, LS};
        {badrpc, _Reason} -> {offline, 0};  % Graceful fallback for RPC errors
        _ -> {offline, 0}  % Catch-all for unexpected responses
    end,
    {S, T} = Result,
    ets:insert(presence_cache, {TargetUser, S, T, Now}),
    Result.

terminate(User) ->
    case User of
        undefined -> ok;
        _ -> 
            ets:delete(local_presence, User),
            rpc:cast(get_core_node(), iris_core, update_status, [User, offline])
    end.
