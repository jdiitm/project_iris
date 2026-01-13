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
    %% FIXED: Scan connected nodes for actual Core IP
    Connected = nodes(connected),
    case [N || N <- Connected, string:str(atom_to_list(N), "iris_core") > 0] of
         [Core|_] -> Core;
         [] -> 
             %% Configured Nodes from sys.config
             Candidates = application:get_env(iris_edge, core_nodes, []),
             case lists:search(fun(N) -> net_adm:ping(N) == pong end, Candidates) of
                 {value, LiveCore} -> LiveCore;
                 false -> 
                     error(no_core_available)
             end
    end.

%% handle_packet(Packet, User, TransportPid, TransportMod) -> {ok, NewUser, Actions}
%% Actions = [ {send, Bin} | {send_batch, [Bin]} | close ]
handle_packet({login, User}, _Current, TransportPid, _Mod) ->
    %% io:format("User logged in: ~p~n", [User]),
    
    %% PHASE 1: LOCAL registration FIRST (sub-millisecond, never blocks)
    %% This ensures local routing works immediately
    ets:insert(local_presence_v2, {User, TransportPid}),
    
    %% Also register with new async router if available
    case whereis(iris_async_router) of
        undefined -> ok;
        _ -> iris_async_router:register_local(User, TransportPid)
    end,
    
    %% PHASE 2: Background sync to Core (async, non-blocking)
    %% This happens in a separate process to not delay login response
    spawn(fun() ->
        CoreNode = get_core_node(),
        case rpc:call(CoreNode, iris_core, register_user, [User, node(), TransportPid], 5000) of
            ok -> ok;
            {error, Reason} -> 
                %% io:format("register_user failed for ~p: ~p~n", [User, Reason]);
                ok;
            {badrpc, Reason} ->
                %% io:format("register_user RPC failed to ~p for ~p: ~p~n", [CoreNode, User, Reason])
                ok
        end
    end),

    %% PHASE 3: Retrieve offline messages (async in background)
    %% Don't block login - deliver offline msgs as they arrive
    spawn(fun() ->
        case rpc:call(get_core_node(), iris_core, retrieve_offline, [User], 5000) of
            Msgs when is_list(Msgs), length(Msgs) > 0 ->
                %% Deliver offline messages to user's socket
                [TransportPid ! {deliver_msg, Msg} || Msg <- Msgs];
            _ -> ok
        end
    end),
    
    %% Immediate response - user can start messaging NOW
    Actions = [{send, <<3, "LOGIN_OK">>}],
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
     %% io:format("Protocol Error~n"),
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
            ets:delete(local_presence_v2, User),
            rpc:cast(get_core_node(), iris_core, update_status, [User, offline])
    end.
