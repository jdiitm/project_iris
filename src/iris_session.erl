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
    %% Match both "iris_core" (Makefile) and "core_" (Docker) patterns
    IsCoreNode = fun(N) ->
        Name = atom_to_list(N),
        string:str(Name, "iris_core") > 0 orelse 
        string:prefix(Name, "core_") =/= nomatch
    end,
    case [N || N <- Connected, IsCoreNode(N)] of
         [Core|_] -> Core;
         [] -> 
             %% Configured Nodes from sys.config
             Candidates = application:get_env(iris_edge, core_nodes, []),
             case lists:search(fun(N) -> net_adm:ping(N) == pong end, Candidates) of
                 {value, LiveCore} -> LiveCore;
                 false -> 
                     %% Fallback for Tests/Single-Node: Return local node
                     %% This results in {badrpc, _} which is handled gracefully
                     node()
             end
    end.

%% handle_packet(Packet, User, TransportPid, TransportMod) -> {ok, NewUser, Actions}
%% Actions = [ {send, Bin} | {send_batch, [Bin]} | close ]
handle_packet({login, LoginData}, _Current, TransportPid, _Mod) ->
    %% Parse login data: may be just username or "username:token" format
    {User, MaybeToken} = parse_login_data(LoginData),
    
    %% AUDIT3 FIX: Protect against session memory bloat
    process_flag(max_heap_size, #{size => 100000, kill => true}), %% ~800KB limit
    
    %% Rate limiting check
    case rate_limit_check(User) of
        {deny, RetryAfter} ->
            logger:warning("Login rate limited for ~p", [User]),
            Actions = [{send, <<"RATE_LIMITED">>}, close],
            {ok, undefined, Actions};
        allow ->
            %% Optional JWT authentication
            case authenticate(User, MaybeToken) of
                ok ->
                    complete_login(User, TransportPid);
                {error, Reason} ->
                    logger:warning("Auth failed for ~p: ~p", [User, Reason]),
                    Actions = [{send, <<"AUTH_FAILED">>}, close],
                    {ok, undefined, Actions}
            end
    end;

handle_packet({send_message, Target, Msg}, User, _Pid, _Mod) ->
    iris_router:route(Target, Msg),
    {ok, User, []};

handle_packet({batch_send, Target, Blob}, User, _Pid, _Mod) ->
    Msgs = iris_proto:unpack_batch(Blob),
    %% P2-1 FIX: Use rpc:cast for fire-and-forget batch storage
    %% No need to block on batch send - offline storage is best-effort
    rpc:cast(get_core_node(), iris_core, store_batch, [Target, Msgs]),
    {ok, User, []};

handle_packet({get_status, TargetUser}, User, _Pid, _Mod) ->
    Now = os:system_time(seconds),
    CacheResult = ets:lookup(presence_cache, TargetUser),
    StatusTuple = case CacheResult of
        [{TargetUser, CachedStatus, CachedTime, InsertTime}] 
          when Now - InsertTime < 5 -> 
             {CachedStatus, CachedTime};
        [{_, _, _, _}] ->
             fetch_and_cache(TargetUser, Now);
        [] ->
             fetch_and_cache(TargetUser, Now)
    end,
    
    {FinalState, FinalTime} = StatusTuple,
    Resp = iris_proto:encode_status(TargetUser, FinalState, FinalTime),
    {ok, User, [{send, Resp}]};

handle_packet({ack, MsgId}, User, _Pid, _Mod) ->
    {ok, User, [{ack_received, MsgId}]};

handle_packet({error, _}, User, _Pid, _Mod) ->
     {ok, User, []}.

%% =============================================================================
%% Internal: Login helpers
%% =============================================================================

complete_login(User, TransportPid) ->
    %% PHASE 1: LOCAL registration FIRST (sub-millisecond, never blocks)
    ets:insert(local_presence_v2, {User, TransportPid}),
    
    %% Also register with new async router if available
    case whereis(iris_async_router_1) of
        undefined -> ok;
        _ -> iris_async_router:register_local(User, TransportPid)
    end,
    
    %% PHASE 2: Async Core registration (eventual consistency acceptable)
    %% Local ETS registration (Phase 1) handles immediate routing
    %% AUDIT FIX: Reduces worst-case login time from 10s to 5s
    CoreNode = get_core_node(),
    spawn(fun() ->
        case rpc:call(CoreNode, iris_core, register_user, [User, node(), TransportPid], 5000) of
            ok -> ok;
            {badrpc, Reason} -> 
                logger:warning("Async Core registration failed for ~p on ~p: ~p", [User, CoreNode, Reason]);
            {error, Reason} ->
                logger:warning("Async Core registration error for ~p: ~p", [User, Reason])
        end
    end),

    %% PHASE 3: Retrieve offline messages SYNCHRONOUSLY (RFC FR-2 compliance)
    %% Messages MUST be delivered when recipient connects
    OfflineActions = case rpc:call(get_core_node(), iris_core, retrieve_offline, [User], 5000) of
        Msgs when is_list(Msgs), length(Msgs) > 0 ->
            %% Encode each offline message as reliable message and include in response
            lists:map(fun(Msg) ->
                MsgId = iris_proto:generate_msg_id(),
                {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
            end, Msgs);
        _ -> []
    end,
    
    %% Response: LOGIN_OK followed by any offline messages
    {ok, User, [{send, <<3, "LOGIN_OK">>} | OfflineActions]}.

parse_login_data(Data) ->
    case binary:split(Data, <<":">>) of
        [User, Token] -> {User, Token};
        [User] -> {User, undefined}
    end.

rate_limit_check(User) ->
    case whereis(iris_rate_limiter) of
        undefined -> allow;
        _ -> iris_rate_limiter:check(User)
    end.

authenticate(_User, undefined) ->
    %% No token provided - check if auth is required
    case whereis(iris_auth) of
        undefined -> ok;
        _ ->
            case iris_auth:is_auth_enabled() of
                false -> ok;
                true -> {error, token_required}
            end
    end;
authenticate(User, Token) ->
    case whereis(iris_auth) of
        undefined -> ok;
        _ ->
            case iris_auth:is_auth_enabled() of
                false -> ok;
                true ->
                    case iris_auth:validate_token(Token) of
                        {ok, Claims} ->
                            %% Verify token subject matches claimed user
                            case maps:get(<<"sub">>, Claims, undefined) of
                                User -> ok;
                                _ -> {error, user_mismatch}
                            end;
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

%% =============================================================================
%% Internal: Status helpers
%% =============================================================================

fetch_and_cache(TargetUser, Now) ->
    %% P2-1 FIX: Use async fetch with fallback for status
    %% Status queries are non-critical - return cached/default on timeout
    Result = try
        case rpc:call(get_core_node(), iris_core, get_status, [TargetUser], 1000) of
            {online, true, _} -> {online, 0};
            {online, false, LS} -> {offline, LS};
            {badrpc, _Reason} -> {offline, 0};
            _ -> {offline, 0}
        end
    catch
        _:_ -> {offline, 0}  %% Timeout or error - return safe default
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
