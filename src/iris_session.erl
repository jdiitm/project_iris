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

handle_packet({send_message, Target, Msg}, User, _Pid, _Mod) when User =/= undefined ->
    %% VIOLATION-4 FIX: Rate limit check on message send
    case check_message_rate(User) of
        allow ->
            iris_router:route(Target, Msg),
            {ok, User, []};
        {deny, RetryAfter} ->
            logger:warning("Message rate limited for ~p", [User]),
            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
    end;

handle_packet({send_message, _Target, _Msg}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, []};

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

%% =============================================================================
%% Typing Indicators (RFC FR-8: Real-time, best-effort)
%% =============================================================================
%% Fire-and-forget: relay to recipient if online, discard if offline.
%% No durability required - typing is transient state.

handle_packet({typing_start, Target}, User, _Pid, _Mod) when User =/= undefined ->
    %% Relay typing indicator to target if they're online
    relay_typing_indicator(Target, User, true),
    {ok, User, []};

handle_packet({typing_stop, Target}, User, _Pid, _Mod) when User =/= undefined ->
    %% Relay typing stop to target if they're online
    relay_typing_indicator(Target, User, false),
    {ok, User, []};

handle_packet({typing_start, _Target}, undefined, _Pid, _Mod) ->
    %% Not logged in - ignore
    {ok, undefined, []};

handle_packet({typing_stop, _Target}, undefined, _Pid, _Mod) ->
    %% Not logged in - ignore
    {ok, undefined, []};

%% =============================================================================
%% Read Receipts (RFC FR-4: Optional, real-time)
%% =============================================================================
%% Best-effort: relay to original sender if online, discard if offline.
%% No durability required - read status is non-critical metadata.

handle_packet({read_receipt, MsgId, OriginalSender}, User, _Pid, _Mod) when User =/= undefined ->
    %% Relay read receipt to original sender
    iris_read_receipts:relay_read_receipt(MsgId, User, OriginalSender),
    {ok, User, []};

handle_packet({read_receipt, _MsgId, _OriginalSender}, undefined, _Pid, _Mod) ->
    %% Not logged in - ignore
    {ok, undefined, []};

%% =============================================================================
%% E2EE Key Bundle Operations (RFC-001-AMENDMENT-001, FR-13, FR-14)
%% =============================================================================
%% Server never has access to plaintext - only routes encrypted messages

handle_packet({upload_prekeys, Bundle}, User, _Pid, _Mod) when User =/= undefined ->
    %% Upload user's key bundle (identity key, signed prekey, one-time prekeys)
    case whereis(iris_keys) of
        undefined ->
            %% Keys module not running
            {ok, User, [{send, <<16#22, 0:32>>}]};  %% Empty response indicating error
        _ ->
            case iris_keys:upload_bundle(User, Bundle) of
                ok ->
                    Response = iris_proto:encode_prekey_response(#{status => <<"ok">>}),
                    {ok, User, [{send, Response}]};
                {error, Reason} ->
                    logger:warning("E2EE key upload failed for ~p: ~p", [User, Reason]),
                    Response = iris_proto:encode_prekey_response(#{status => <<"error">>, reason => atom_to_binary(Reason, utf8)}),
                    {ok, User, [{send, Response}]}
            end
    end;

handle_packet({upload_prekeys, _Bundle}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, [{send, <<16#22, 0:32>>}]};

handle_packet({fetch_prekeys, TargetUser}, User, _Pid, _Mod) when User =/= undefined ->
    %% Fetch another user's key bundle for X3DH key exchange
    case whereis(iris_keys) of
        undefined ->
            %% Keys module not running
            {ok, User, [{send, <<16#22, 0:32>>}]};
        _ ->
            case iris_keys:fetch_bundle(TargetUser) of
                {ok, Bundle} ->
                    Response = iris_proto:encode_prekey_response(Bundle),
                    {ok, User, [{send, Response}]};
                {error, not_found} ->
                    %% User has no keys registered
                    Response = iris_proto:encode_prekey_response(#{status => <<"not_found">>}),
                    {ok, User, [{send, Response}]};
                {error, Reason} ->
                    logger:warning("E2EE key fetch failed for ~p: ~p", [TargetUser, Reason]),
                    Response = iris_proto:encode_prekey_response(#{status => <<"error">>}),
                    {ok, User, [{send, Response}]}
            end
    end;

handle_packet({fetch_prekeys, _TargetUser}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, [{send, <<16#22, 0:32>>}]};

handle_packet({e2ee_msg, Recipient, Ciphertext, Header}, User, _Pid, _Mod) when User =/= undefined ->
    %% Route E2EE message to recipient (server never decrypts)
    %% VIOLATION-4 FIX: Rate limit check on message send
    case check_message_rate(User) of
        allow ->
            %% Encode delivery packet with sender info
            DeliveryPacket = iris_proto:encode_e2ee_delivery(User, {Header, Ciphertext}),
            %% Route to recipient using async router
            iris_router:route(Recipient, DeliveryPacket),
            {ok, User, []};
        {deny, RetryAfter} ->
            logger:warning("E2EE message rate limited for ~p", [User]),
            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
    end;

handle_packet({e2ee_msg, _Recipient, _Ciphertext, _Header}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, []};

%% =============================================================================
%% Group Messaging Operations (RFC-001-AMENDMENT-001, FR-17 to FR-23)
%% =============================================================================

handle_packet({group_create, GroupName}, User, _Pid, _Mod) when User =/= undefined ->
    %% Create a new group with User as admin
    case whereis(iris_group) of
        undefined ->
            %% Group module not running
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        _ ->
            case iris_group:create_group(GroupName, User) of
                {ok, GroupId} ->
                    %% Send group_join notification back to creator
                    JoinPacket = iris_proto:encode_group_join(GroupId, User),
                    {ok, User, [{send, JoinPacket}]};
                {error, Reason} ->
                    logger:warning("Group creation failed for ~p: ~p", [User, Reason]),
                    {ok, User, [{send, encode_error(Reason)}]}
            end
    end;

handle_packet({group_create, _GroupName}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({group_leave, GroupId}, User, _Pid, _Mod) when User =/= undefined ->
    %% Leave a group
    case whereis(iris_group) of
        undefined ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        _ ->
            case iris_group:remove_member(GroupId, User, User) of
                ok ->
                    {ok, User, [{send, <<16#32, "OK">>}]};
                {error, Reason} ->
                    logger:warning("Group leave failed for ~p from ~p: ~p", [User, GroupId, Reason]),
                    {ok, User, [{send, encode_error(Reason)}]}
            end
    end;

handle_packet({group_leave, _GroupId}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({group_msg, GroupId, Ciphertext, Header}, User, _Pid, _Mod) when User =/= undefined ->
    %% Route encrypted group message to all members
    %% Rate limit check
    case check_message_rate(User) of
        allow ->
            case whereis(iris_group) of
                undefined ->
                    {ok, User, [{send, encode_error(group_service_unavailable)}]};
                _ ->
                    case iris_group:is_member(GroupId, User) of
                        false ->
                            {ok, User, [{send, encode_error(not_member)}]};
                        true ->
                            %% Fan out to all group members
                            case iris_group:get_members(GroupId) of
                                {ok, Members} ->
                                    %% Encode the message once
                                    DeliveryPacket = iris_proto:encode_group_msg(GroupId, 
                                        maps:put(<<"sender">>, User, Header), Ciphertext),
                                    %% Send to all members except sender
                                    lists:foreach(fun(#{user_id := MemberId}) ->
                                        if MemberId =/= User ->
                                            iris_router:route(MemberId, DeliveryPacket);
                                        true -> ok
                                        end
                                    end, Members),
                                    {ok, User, []};
                                {error, _Reason} ->
                                    {ok, User, [{send, encode_error(group_not_found)}]}
                            end
                    end
            end;
        {deny, RetryAfter} ->
            logger:warning("Group message rate limited for ~p", [User]),
            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
    end;

handle_packet({group_msg, _GroupId, _Ciphertext, _Header}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({group_roster, GroupId}, User, _Pid, _Mod) when User =/= undefined ->
    %% Request group roster (member list)
    case whereis(iris_group) of
        undefined ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        _ ->
            case iris_group:is_member(GroupId, User) of
                false ->
                    {ok, User, [{send, encode_error(not_member)}]};
                true ->
                    case iris_group:get_members(GroupId) of
                        {ok, Members} ->
                            MemberIds = [M || #{user_id := M} <- Members],
                            Response = iris_proto:encode_group_roster_response(GroupId, MemberIds),
                            {ok, User, [{send, Response}]};
                        {error, Reason} ->
                            {ok, User, [{send, encode_error(Reason)}]}
                    end
            end
    end;

handle_packet({group_roster, _GroupId}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({sender_key_dist, GroupId, KeyData}, User, _Pid, _Mod) when User =/= undefined ->
    %% Distribute sender key to group
    case whereis(iris_group) of
        undefined ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        _ ->
            case iris_group:is_member(GroupId, User) of
                false ->
                    {ok, User, [{send, encode_error(not_member)}]};
                true ->
                    %% Store sender key and broadcast to members
                    KeyId = crypto:strong_rand_bytes(8),
                    KeyIdHex = binary_to_list(base16_encode(KeyId)),
                    ok = iris_group:store_sender_key(GroupId, User, list_to_binary(KeyIdHex), KeyData),
                    
                    %% Notify all other members of the new sender key
                    case iris_group:get_members(GroupId) of
                        {ok, Members} ->
                            DistPacket = iris_proto:encode_sender_key_dist(GroupId, KeyData),
                            lists:foreach(fun(#{user_id := MemberId}) ->
                                if MemberId =/= User ->
                                    iris_router:route(MemberId, DistPacket);
                                true -> ok
                                end
                            end, Members);
                        _ -> ok
                    end,
                    {ok, User, []}
            end
    end;

handle_packet({sender_key_dist, _GroupId, _KeyData}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({error, _}, User, _Pid, _Mod) ->
     {ok, User, []}.

%% =============================================================================
%% Internal: Login helpers
%% =============================================================================

complete_login(User, TransportPid) ->
    %% PHASE 1: LOCAL registration FIRST (sub-millisecond, never blocks)
    %% Single source of truth: direct ETS insert
    %% All router shards read from the same public ETS table (local_presence_v2)
    true = ets:insert(local_presence_v2, {User, TransportPid}),
    
    %% REMOVED: Redundant iris_async_router:register_local call
    %% Both would insert to same table anyway - the async router reads directly from local_presence_v2
    
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

%% =============================================================================
%% Internal: Rate Limiting (VIOLATION-4 FIX)
%% =============================================================================
%% Rate limit on message sending, not just login

check_message_rate(User) ->
    case whereis(iris_rate_limiter) of
        undefined -> allow;
        _ -> iris_rate_limiter:check(User)
    end.

encode_rate_limited(RetryAfter) ->
    %% Error response with retry-after hint
    <<16#FF, RetryAfter:32>>.

encode_error(Reason) when is_atom(Reason) ->
    ReasonBin = atom_to_binary(Reason, utf8),
    <<16#FE, (byte_size(ReasonBin)):16, ReasonBin/binary>>;
encode_error(Reason) when is_binary(Reason) ->
    <<16#FE, (byte_size(Reason)):16, Reason/binary>>;
encode_error(_Reason) ->
    <<16#FE, 5:16, "error">>.

%% Simple base16 encoding (hex)
base16_encode(<<>>) -> <<>>;
base16_encode(<<N:4, Rest/bitstring>>) ->
    Char = if N < 10 -> $0 + N; true -> $a + N - 10 end,
    RestEncoded = base16_encode(Rest),
    <<Char, RestEncoded/binary>>.

%% =============================================================================
%% Internal: Typing indicator relay (RFC FR-8)
%% =============================================================================
%% Best-effort relay: send to recipient if online, discard if offline.
%% No durability required - typing state is transient.

relay_typing_indicator(Target, Sender, IsTyping) ->
    %% Look up target in local presence first (fast path)
    case ets:lookup(local_presence_v2, Target) of
        [{Target, Pid}] when is_pid(Pid) ->
            %% Target is on this node - send directly
            TypingPacket = iris_proto:encode_typing_relay(Sender, IsTyping),
            Pid ! {deliver_typing, TypingPacket},
            ok;
        [] ->
            %% Target not on this node - check Core for remote routing
            %% Fire-and-forget: don't wait for result
            spawn(fun() ->
                case rpc:call(get_core_node(), iris_core, lookup_user, [Target], 1000) of
                    {online, TargetNode, TargetPid} when is_pid(TargetPid) ->
                        %% Send to remote node
                        TypingPacket = iris_proto:encode_typing_relay(Sender, IsTyping),
                        catch rpc:cast(TargetNode, erlang, send, [TargetPid, {deliver_typing, TypingPacket}]);
                    _ ->
                        %% Target offline - discard typing indicator (expected behavior)
                        ok
                end
            end),
            ok
    end.

terminate(User) ->
    case User of
        undefined -> ok;
        _ -> 
            %% FIXED: Only delete if THIS process owns the entry (lock-free check)
            %% This prevents new logins from having their entry deleted by old connections
            %% Race scenario without fix:
            %%   T0: Conn1 login "alice" -> ETS: {alice, Pid1}
            %%   T1: Conn1 close() scheduled
            %%   T2: Conn2 login "alice" -> ETS: {alice, Pid2}
            %%   T3: Conn1 terminate() -> ets:delete(alice) -> DELETES Pid2's entry!
            %% With fix: T3 checks ownership and skips delete since Pid2 != Pid1
            Self = self(),
            case ets:lookup(local_presence_v2, User) of
                [{User, Self}] ->
                    %% We own it - safe to delete
                    ets:delete(local_presence_v2, User),
                    rpc:cast(get_core_node(), iris_core, update_status, [User, offline]);
                [{User, _OtherPid}] ->
                    %% Different process owns it (new login happened) - don't delete
                    ok;
                [] ->
                    %% Already deleted - nothing to do
                    ok
            end
    end.
