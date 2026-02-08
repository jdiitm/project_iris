-module(iris_session).
-export([handle_packet/4, terminate/1]).
-export([validate_e2ee_header/1]).  %% Exported for TDD (audit finding 1)
-export([group_fanout_recipients/3]).  %% Exported for TDD (audit finding 3)

-include_lib("kernel/include/inet.hrl").

%% =============================================================================
%% HOT-001 FIX: Paginated Offline Delivery Constants
%% =============================================================================
-define(OFFLINE_PAGE_SIZE, 500).      %% Messages per page
-define(OFFLINE_INLINE_LIMIT, 1000).  %% Deliver all if under this limit

%% =============================================================================
%% RFC Section 7.4: Graceful Degradation Levels
%% =============================================================================
%% Under overload, features are disabled in this order:
%% 1. Typing indicators (FR-8) - disabled FIRST (at LEVEL_SLOW)
%% 2. Presence updates (FR-6, FR-7) - disabled SECOND (at LEVEL_SHED)
%% 3. Read receipts (FR-4) - disabled THIRD (at LEVEL_SHED)
%% 4. Message delivery - NEVER disabled
-define(LEVEL_NORMAL, 1).
-define(LEVEL_SLOW, 2).
-define(LEVEL_SHED, 3).
-define(LEVEL_CRITICAL, 4).

%% RFC Section 11.1: Version/Capability Negotiation
-define(SERVER_VERSIONS, [1]).
-define(SERVER_CAPABILITIES, [<<"zstd">>, <<"zlib">>, <<"e2ee">>, <<"groups">>]).

%% @doc Check if a feature should be degraded based on current load level.
%% Returns true if the feature should be skipped (degraded).
-spec should_degrade(atom()) -> boolean().
should_degrade(Feature) ->
    Level = get_flow_level(),
    case Feature of
        typing ->
            %% Typing disabled at SLOW or higher (first to degrade)
            Level >= ?LEVEL_SLOW;
        presence ->
            %% Presence disabled at SHED or higher (second to degrade)
            Level >= ?LEVEL_SHED;
        read_receipt ->
            %% Read receipts disabled at SHED or higher (third to degrade)
            Level >= ?LEVEL_SHED;
        message ->
            %% Messages NEVER disabled (RFC 7.4 requirement)
            false
    end.

%% @doc Get current flow controller level (lockfree via ETS).
-spec get_flow_level() -> integer().
get_flow_level() ->
    try
        case ets:lookup(iris_flow_controller_ets, level) of
            [{level, L}] -> L;
            [] -> ?LEVEL_NORMAL
        end
    catch
        error:badarg ->
            %% ETS table doesn't exist yet - assume normal
            ?LEVEL_NORMAL
    end.

%% @doc Track a request for flow controller rate calculation (RFC 7.4).
%% This is lockfree - directly increments ETS counter without gen_server call.
%% Enables throughput-based degradation: high message rates trigger SLOW/SHED levels,
%% causing typing/presence to be dropped while keeping message delivery working.
-spec track_request(binary()) -> ok.
track_request(User) ->
    try
        %% Consistent hash to shard for write distribution
        Shard = erlang:phash2(User, 16),  %% 16 shards
        ets:update_counter(iris_flow_controller_ets, {admitted, Shard}, 1, {{admitted, Shard}, 0}),
        ok
    catch
        error:badarg ->
            %% ETS table doesn't exist yet - ignore
            ok
    end.

%% Generate a unique session ID for connection resume (RFC Section 3.4)
generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

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

%% Check if iris_group service is available (on core node or locally)
is_group_service_available() ->
    %% First check local (for single-node/test setups)
    case whereis(iris_group) of
        Pid when is_pid(Pid) -> true;
        undefined ->
            %% Try core node via RPC
            CoreNode = get_core_node(),
            case rpc:call(CoreNode, erlang, whereis, [iris_group], 2000) of
                Pid when is_pid(Pid) -> true;
                _ -> false
            end
    end.

%% Call iris_group function, routing to core node if needed
call_iris_group(Function, Args) ->
    %% First try local (for single-node/test setups)
    case whereis(iris_group) of
        Pid when is_pid(Pid) ->
            apply(iris_group, Function, Args);
        undefined ->
            %% Route to core node
            CoreNode = get_core_node(),
            case rpc:call(CoreNode, iris_group, Function, Args, 5000) of
                {badrpc, Reason} ->
                    logger:warning("Group RPC failed: ~p", [Reason]),
                    {error, group_service_unavailable};
                Result ->
                    Result
            end
    end.

%% handle_packet(Packet, User, TransportPid, TransportMod) -> {ok, NewUser, Actions}
%% Actions = [ {send, Bin} | {send_batch, [Bin]} | close ]
handle_packet({login, LoginData}, _Current, TransportPid, _Mod) ->
    %% RFC NFR-31: Span instrumentation for login
    iris_trace:new_span(<<"session.login">>),
    %% Parse login data: may be just username or "username:token" format
    {User, MaybeToken} = parse_login_data(LoginData),
    
    %% AUDIT3 FIX: Protect against session memory bloat
    %% NFR-26 FIX: Increased from 100000 to 1000000 words to support 1000-member groups
    %% 1000000 words = ~8MB, sufficient for large group roster/fanout operations
    process_flag(max_heap_size, #{size => 1000000, kill => true}), %% ~8MB limit
    
    %% RFC Section 10.1: Check failed-login rate limit first
    Result = case iris_auth:check_login_rate(User) of
        {error, rate_limited} ->
            logger:warning("Failed-login rate limited for ~p (10/hour)", [User]),
            Actions = [{send, <<"LOGIN_RATE_LIMITED">>}, close],
            {ok, undefined, Actions};
        ok ->
            %% Per-message rate limiting check
            case rate_limit_check(User) of
                {deny, _RetryAfter} ->
                    logger:warning("Login rate limited for ~p", [User]),
                    Actions2 = [{send, <<"RATE_LIMITED">>}, close],
                    {ok, undefined, Actions2};
                allow ->
                    %% Optional JWT authentication
                    case authenticate(User, MaybeToken) of
                        ok ->
                            complete_login(User, TransportPid);
                        {error, Reason} ->
                            %% RFC Section 10.1: Record failed login attempt
                            iris_auth:record_failed_login(User),
                            logger:warning("Auth failed for ~p: ~p", [User, Reason]),
                            Actions3 = [{send, <<"AUTH_FAILED">>}, close],
                            {ok, undefined, Actions3}
                    end
            end
    end,
    iris_trace:end_span(<<"session.login">>),
    Result;

handle_packet({send_message, _Target, _Msg}, User, _Pid, _Mod) when User =/= undefined ->
    %% =============================================================================
    %% RFC-001-AMENDMENT-001 Section 7: v1.0 REJECTS plaintext messages (opcode 0x02)
    %% =============================================================================
    %% Clients MUST use E2EE (opcode 0x23) or CBOR (opcode 0x10) for all messages.
    %% This is a BREAKING CHANGE required for v1.0 compliance.
    %% Deprecation schedule:
    %%   v0.9: Emit warning (DONE)
    %%   v1.0: Reject with error (THIS CODE)
    %%   v1.1: Remove opcode from protocol spec
    logger:warning("RFC VIOLATION: Rejected plaintext message (0x02) from ~p. Use E2EE (0x23) or CBOR (0x10)", [User]),
    {ok, User, [{send, encode_error(e2ee_required)}]};

handle_packet({send_message, _Target, _Msg}, undefined, _Pid, _Mod) ->
    %% Not logged in AND using deprecated plaintext - reject silently
    {ok, undefined, []};

%% =============================================================================
%% RFC FR-5: Sequence-numbered messages for FIFO ordering
%% =============================================================================
%% AUDIT FIX: Client includes sequence number to guarantee ordering even under
%% parallel processing. The sequence number is used as the storage timestamp.
%% 
%% NOTE: Dedup (RFC NFR-11) is handled on the CORE side in iris_core:store_offline_durable
%% because that's where messages are persisted. Edge nodes don't have iris_dedup running.
handle_packet({send_seq, Target, SeqNo, Msg}, User, _Pid, _Mod) when User =/= undefined ->
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.send_seq">>),
    %% RFC NFR-32: Count incoming message
    iris_metrics:msg_in(),
    %% RFC 7.4 FIX: Track request for flow controller rate calculation
    %% This enables throughput-based degradation (typing/presence shed under load)
    track_request(User),
    Result = case check_message_rate(User) of
        allow ->
            %% Route with sequence number preserved as ordering key
            %% Dedup happens on core when message is stored
            iris_router:route_sequenced(Target, Msg, SeqNo),
            iris_metrics:msg_out(),
            {ok, User, []};
        {deny, RetryAfter} ->
            logger:warning("Message rate limited for ~p", [User]),
            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
    end,
    iris_trace:end_span(<<"session.send_seq">>),
    Result;

handle_packet({send_seq, _Target, _SeqNo, _Msg}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, []};

handle_packet({batch_send, Target, Blob}, User, _Pid, _Mod) ->
    Msgs = iris_proto:unpack_batch(Blob),
    %% P2-1 FIX: Use rpc:cast for fire-and-forget batch storage
    %% No need to block on batch send - offline storage is best-effort
    rpc:cast(get_core_node(), iris_core, store_batch, [Target, Msgs]),
    {ok, User, []};

handle_packet({get_status, TargetUser}, User, _Pid, _Mod) ->
    %% RFC 7.4: Skip presence queries when under heavy load (second to degrade)
    case should_degrade(presence) of
        true ->
            %% Return stale/unknown status instead of making expensive queries
            Resp = iris_proto:encode_status(TargetUser, offline, 0),
            {ok, User, [{send, Resp}]};
        false ->
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
            {ok, User, [{send, Resp}]}
    end;

handle_packet({ack, MsgId}, User, _Pid, _Mod) ->
    %% RFC NFR-32: Count ACK sent
    iris_metrics:ack_sent(),
    {ok, User, [{ack_received, MsgId}]};

%% =============================================================================
%% Typing Indicators (RFC FR-8: Real-time, best-effort)
%% =============================================================================
%% Fire-and-forget: relay to recipient if online, discard if offline.
%% No durability required - typing is transient state.
%% RFC Section 7.4: FIRST feature to degrade under load.

handle_packet({typing_start, Target}, User, _Pid, _Mod) when User =/= undefined ->
    %% RFC 7.4: Skip typing indicators when under load (first to degrade)
    case should_degrade(typing) of
        true ->
            %% Silently drop - typing is non-critical
            {ok, User, []};
        false ->
            relay_typing_indicator(Target, User, true),
            {ok, User, []}
    end;

handle_packet({typing_stop, Target}, User, _Pid, _Mod) when User =/= undefined ->
    %% RFC 7.4: Skip typing indicators when under load (first to degrade)
    case should_degrade(typing) of
        true ->
            {ok, User, []};
        false ->
            relay_typing_indicator(Target, User, false),
            {ok, User, []}
    end;

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
%% RFC Section 7.4: THIRD feature to degrade under load.

handle_packet({read_receipt, MsgId, OriginalSender}, User, _Pid, _Mod) when User =/= undefined ->
    %% RFC 7.4: Skip read receipts when under heavy load (third to degrade)
    case should_degrade(read_receipt) of
        true ->
            %% Silently drop - read receipts are non-critical
            {ok, User, []};
        false ->
            iris_read_receipts:relay_read_receipt(MsgId, User, OriginalSender),
            {ok, User, []}
    end;

handle_packet({read_receipt, _MsgId, _OriginalSender}, undefined, _Pid, _Mod) ->
    %% Not logged in - ignore
    {ok, undefined, []};

%% =============================================================================
%% HOT-001 FIX: Paginated Offline Message Retrieval
%% =============================================================================
%% For users with large offline queues (1000+ messages), messages are delivered
%% in pages. Client requests additional pages with {get_offline_page, Cursor}.

handle_packet({get_offline_page, Cursor}, User, _Pid, _Mod) when User =/= undefined ->
    %% Client requesting next page of offline messages
    CoreNode = get_core_node(),
    case rpc:call(CoreNode, iris_core, retrieve_offline_paginated, 
                  [User, ?OFFLINE_PAGE_SIZE, Cursor], 5000) of
        {Msgs, NextCursor} when is_list(Msgs), length(Msgs) > 0 ->
            %% Encode messages
            MsgActions = lists:map(fun(Msg) ->
                MsgId = iris_proto:generate_msg_id(),
                {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
            end, Msgs),
            
            %% Confirm delivery of previous page (delete from storage)
            PrevCursor = max(0, Cursor - ?OFFLINE_PAGE_SIZE),
            spawn(fun() ->
                rpc:call(CoreNode, iris_core, delete_offline_confirmed, 
                         [User, {PrevCursor, Cursor}], 5000)
            end),
            
            %% Add continuation indicator if more pages exist
            case NextCursor of
                done ->
                    {ok, User, MsgActions};
                _ ->
                    Remaining = 0, %% Unknown at this point
                    MoreIndicator = encode_offline_more(NextCursor, Remaining),
                    {ok, User, MsgActions ++ [{send, MoreIndicator}]}
            end;
        _ ->
            {ok, User, []}
    end;

handle_packet({get_offline_page, _Cursor}, undefined, _Pid, _Mod) ->
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
            %% GAP-13: Use fetch_bundle/3 to record requester as contact
            case iris_keys:fetch_bundle(TargetUser, true, User) of
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
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.e2ee_msg">>),
    %% RFC NFR-32: Count incoming message
    iris_metrics:msg_in(),
    %% Route E2EE message to recipient (server never decrypts)
    %% RFC Section 8 / NFR-18: Validate payload size before routing (GAP-7 fix)
    E2eeResult = case iris_limits:validate_payload(Ciphertext) of
        {error, payload_too_large} ->
            {ok, User, [{send, encode_error(payload_too_large)}]};
        ok ->
            %% NFR-18: Validate E2EE header fields before routing
            case validate_e2ee_header(Header) of
                ok ->
                    %% VIOLATION-4 FIX: Rate limit check on message send
                    case check_message_rate(User) of
                        allow ->
                            %% Encode delivery packet with sender info
                            DeliveryPacket = iris_proto:encode_e2ee_delivery(User, {Header, Ciphertext}),
                            %% Route to recipient using async router
                            iris_router:route(Recipient, DeliveryPacket),
                            iris_metrics:msg_out(),
                            {ok, User, []};
                        {deny, RetryAfter} ->
                            logger:warning("E2EE message rate limited for ~p", [User]),
                            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
                    end;
                {error, Reason} ->
                    logger:warning("E2EE header validation failed for ~p: ~p", [User, Reason]),
                    {ok, User, [{send, encode_error(invalid_e2ee_header)}]}
            end
    end,
    iris_trace:end_span(<<"session.e2ee_msg">>),
    E2eeResult;

handle_packet({e2ee_msg, _Recipient, _Ciphertext, _Header}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, []};

%% =============================================================================
%% Group Messaging Operations (RFC-001-AMENDMENT-001, FR-17 to FR-23)
%% =============================================================================

handle_packet({group_create, GroupName}, User, _Pid, _Mod) when User =/= undefined ->
    %% Create a new group with User as admin
    case is_group_service_available() of
        false ->
            %% Group module not running
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        true ->
            case call_iris_group(create_group, [GroupName, User]) of
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

%% =============================================================================
%% Group Join/Add Member (RFC-001-AMENDMENT-001, FR-18)
%% =============================================================================
%% Packet 0x31 can be sent by client to add a member to a group.
%% Only group admins can add members.

handle_packet({group_join, GroupId, MemberName}, User, _Pid, _Mod) when User =/= undefined ->
    %% Add a member to a group (must be admin)
    case is_group_service_available() of
        false ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        true ->
            case call_iris_group(add_member, [GroupId, MemberName, User]) of
                ok ->
                    %% Send join notification back to confirm
                    JoinPacket = iris_proto:encode_group_join(GroupId, MemberName),
                    {ok, User, [{send, JoinPacket}]};
                {error, Reason} ->
                    logger:warning("Group add_member failed: ~p adding ~p to ~p: ~p", 
                                 [User, MemberName, GroupId, Reason]),
                    {ok, User, [{send, encode_error(Reason)}]}
            end
    end;

handle_packet({group_join, _GroupId, _MemberName}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({group_leave, GroupId}, User, _Pid, _Mod) when User =/= undefined ->
    %% Leave a group
    case is_group_service_available() of
        false ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        true ->
            case call_iris_group(remove_member, [GroupId, User, User]) of
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
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.group_msg">>),
    %% RFC NFR-32: Count incoming message
    iris_metrics:msg_in(),
    %% Route encrypted group message to all members
    %% RFC Section 8: Validate payload size (GAP-7 fix)
    GrpResult = case iris_limits:validate_payload(Ciphertext) of
        {error, payload_too_large} ->
            {ok, User, [{send, encode_error(payload_too_large)}]};
        ok ->
            %% Rate limit check
            case check_message_rate(User) of
                allow ->
                    case is_group_service_available() of
                        false ->
                            {ok, User, [{send, encode_error(group_service_unavailable)}]};
                        true ->
                            case call_iris_group(is_member, [GroupId, User]) of
                                false ->
                                    {ok, User, [{send, encode_error(not_member)}]};
                                true ->
                                    %% Fan out to all group members
                                    case call_iris_group(get_members, [GroupId]) of
                                        {ok, Members} ->
                                            %% Encode the message once
                                            DeliveryPacket = iris_proto:encode_group_msg(GroupId, 
                                                maps:put(<<"sender">>, User, Header), Ciphertext),
                                            %% Audit Finding 3: Re-check membership to close TOCTOU window
                                            Recipients = group_fanout_recipients(GroupId, User, Members),
                                            lists:foreach(fun(#{user_id := MemberId}) ->
                                                iris_router:route(MemberId, DeliveryPacket),
                                                iris_metrics:msg_out()
                                            end, Recipients),
                                            {ok, User, []};
                                        {error, _Reason} ->
                                            {ok, User, [{send, encode_error(group_not_found)}]}
                                    end;
                                {error, _} ->
                                    {ok, User, [{send, encode_error(group_service_unavailable)}]}
                            end
                    end;
                {deny, RetryAfter} ->
                    logger:warning("Group message rate limited for ~p", [User]),
                    {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
            end
    end,
    iris_trace:end_span(<<"session.group_msg">>),
    GrpResult;

handle_packet({group_msg, _GroupId, _Ciphertext, _Header}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({group_roster, GroupId}, User, _Pid, _Mod) when User =/= undefined ->
    %% Request group roster (member list)
    case is_group_service_available() of
        false ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        true ->
            case call_iris_group(is_member, [GroupId, User]) of
                false ->
                    {ok, User, [{send, encode_error(not_member)}]};
                true ->
                    case call_iris_group(get_members, [GroupId]) of
                        {ok, Members} ->
                            MemberIds = [M || #{user_id := M} <- Members],
                            Response = iris_proto:encode_group_roster_response(GroupId, MemberIds),
                            {ok, User, [{send, Response}]};
                        {error, Reason} ->
                            {ok, User, [{send, encode_error(Reason)}]}
                    end;
                {error, _} ->
                    {ok, User, [{send, encode_error(group_service_unavailable)}]}
            end
    end;

handle_packet({group_roster, _GroupId}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

handle_packet({sender_key_dist, GroupId, KeyData}, User, _Pid, _Mod) when User =/= undefined ->
    %% Distribute sender key to group
    case is_group_service_available() of
        false ->
            {ok, User, [{send, encode_error(group_service_unavailable)}]};
        true ->
            case call_iris_group(is_member, [GroupId, User]) of
                false ->
                    {ok, User, [{send, encode_error(not_member)}]};
                true ->
                    %% Store sender key and broadcast to members
                    KeyId = crypto:strong_rand_bytes(8),
                    KeyIdHex = binary_to_list(base16_encode(KeyId)),
                    call_iris_group(store_sender_key, [GroupId, User, list_to_binary(KeyIdHex), KeyData]),
                    
                    %% Notify all other members of the new sender key
                    case call_iris_group(get_members, [GroupId]) of
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
                    {ok, User, []};
                {error, _} ->
                    {ok, User, [{send, encode_error(group_service_unavailable)}]}
            end
    end;

handle_packet({sender_key_dist, _GroupId, _KeyData}, undefined, _Pid, _Mod) ->
    {ok, undefined, []};

%% =============================================================================
%% Control Opcodes (PROTOCOL_V1_FREEZE v1.1)
%% =============================================================================

%% =============================================================================
%% Version/Capability Negotiation (0x0C): RFC Section 11.1
%% =============================================================================
%% Client sends supported versions and capabilities after LOGIN.
%% Server responds with negotiated (intersection) version and capabilities.
%% Supported server capabilities: "zstd", "zlib", "e2ee", "groups"

handle_packet({version_negotiate, ClientVersions, ClientCapabilities}, User, _Pid, _Mod) ->
    %% Negotiate version: pick highest version supported by both
    NegotiatedVersion = negotiate_version(ClientVersions, ?SERVER_VERSIONS),
    %% Negotiate capabilities: intersection of client and server
    NegotiatedCaps = iris_compression:negotiate(ClientCapabilities, ?SERVER_CAPABILITIES),
    Response = iris_proto:encode_version_response(NegotiatedVersion, NegotiatedCaps),
    logger:info("Version negotiated: v~p, caps=~p for user ~p",
                [NegotiatedVersion, NegotiatedCaps, User]),
    {ok, User, [{send, Response}, {set_capabilities, NegotiatedCaps}]};

%% PING (0x08): Client keepalive heartbeat - respond with PONG
handle_packet(ping, User, _Pid, _Mod) ->
    Pong = iris_proto:encode_pong(),
    {ok, User, [{send, Pong}]};

%% PONG (0x09): Server keepalive response - no action needed
handle_packet(pong, User, _Pid, _Mod) ->
    {ok, User, []};

%% RESUME (0x0A): Connection resume (RFC Section 3.4)
%% Lookup session in cache; if valid, replay missed messages.
%% If expired/unknown, send NACK so client does full login.
handle_packet({resume, SessionId, LastSeqNo}, User, _Pid, _Mod) ->
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.resume">>),
    ResumeResult = case iris_session_cache:get_messages_after(SessionId, LastSeqNo) of
        {ok, Messages} ->
            %% Replay missed messages as a batch
            ReplayActions = [{send, MsgBin} || {_Seq, MsgBin} <- Messages],
            logger:info("RESUME: Replaying ~p messages for ~p (session ~p, after seq ~p)",
                       [length(Messages), User, SessionId, LastSeqNo]),
            {ok, User, ReplayActions};
        {error, _Reason} ->
            %% Session expired or not found -- NACK
            logger:info("RESUME NACK: Session ~p not found/expired for ~p", [SessionId, User]),
            NackPayload = encode_error(<<"RESUME_NACK">>),
            {ok, User, [{send, NackPayload}]}
    end,
    iris_trace:end_span(<<"session.resume">>),
    ResumeResult;

%% TOKEN_REFRESH (0x0B): Token refresh flow (RFC FR-11a)
%% Refresh tokens live on Core (mnesia). Validate+rotate via RPC to Core,
%% then create access token locally on Edge (iris_auth gen_server runs here).
handle_packet({token_refresh, RefreshToken}, User, _Pid, _Mod) ->
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.token_refresh">>),
    CoreNode = get_core_node(),
    %% Step 1: Validate and rotate on Core (mnesia-only, no gen_server needed)
    TrResult = iris_circuit_breaker:call(CoreNode, iris_auth, validate_and_rotate_refresh, [RefreshToken]),
    TrResponse = case TrResult of
        {ok, _UserId, NewRefresh} ->
            %% Step 2: Create access token locally on Edge
            case catch iris_auth:create_token(User) of
                {ok, NewAccess} ->
                    AccessBin = ensure_binary(NewAccess),
                    RefreshBin = ensure_binary(NewRefresh),
                    Response = <<16#0B,
                                 (byte_size(AccessBin)):16, AccessBin/binary,
                                 (byte_size(RefreshBin)):16, RefreshBin/binary>>,
                    logger:info("TOKEN_REFRESH: Issued new token pair for ~p", [User]),
                    {ok, User, [{send, Response}]};
                _ ->
                    %% Access token creation failed -- return explicit error
                    logger:error("TOKEN_REFRESH: Access token creation failed for ~p", [User]),
                    {ok, User, [{send, encode_error(<<"token_creation_failed">>)}]}
            end;
        {error, token_reused} ->
            logger:warning("TOKEN_REFRESH: Reuse detected for ~p, revoking family", [User]),
            {ok, User, [{send, encode_error(token_reused)}]};
        {error, Reason} ->
            logger:warning("TOKEN_REFRESH: Failed for ~p: ~p", [User, Reason]),
            {ok, User, [{send, encode_error(Reason)}]};
        {badrpc, Reason} ->
            logger:warning("TOKEN_REFRESH: RPC failed for ~p: ~p", [User, Reason]),
            {ok, User, [{send, encode_error(service_unavailable)}]}
    end,
    iris_trace:end_span(<<"session.token_refresh">>),
    TrResponse;

%% =============================================================================
%% CBOR Message (0x10): RFC-001-AMENDMENT-001 extensible message format
%% =============================================================================
%% RFC Section 1.2: If the CBOR map includes an "idempotency_key" field,
%% server MUST validate it as UUIDv7 (16 bytes, version=7, variant=0b10).
handle_packet({cbor_msg, Target, Map}, User, _Pid, _Mod) when User =/= undefined ->
    %% RFC NFR-31: Span instrumentation
    iris_trace:new_span(<<"session.cbor_msg">>),
    %% RFC NFR-32: Count incoming message
    iris_metrics:msg_in(),
    track_request(User),
    CborResult = case check_message_rate(User) of
        allow ->
            %% RFC Section 1.2: Validate idempotency_key if present
            case validate_cbor_idempotency_key(Map) of
                ok ->
                    %% Route as CBOR delivery to target
                    DeliveryPacket = iris_proto:encode_cbor_msg(User, Map),
                    iris_router:route(Target, DeliveryPacket),
                    iris_metrics:msg_out(),
                    {ok, User, []};
                {error, invalid_idempotency_key} ->
                    logger:warning("CBOR msg rejected: invalid idempotency_key from ~p", [User]),
                    {ok, User, [{send, encode_error(invalid_idempotency_key)}]}
            end;
        {deny, RetryAfter} ->
            logger:warning("CBOR message rate limited for ~p", [User]),
            {ok, User, [{send, encode_rate_limited(RetryAfter)}]}
    end,
    iris_trace:end_span(<<"session.cbor_msg">>),
    CborResult;

handle_packet({cbor_msg, _Target, _Map}, undefined, _Pid, _Mod) ->
    %% Not logged in - reject
    {ok, undefined, []};

handle_packet({error, _}, User, _Pid, _Mod) ->
     {ok, User, []}.

%% =============================================================================
%% Internal: Traced RPC (RFC NFR-30: Every RPC MUST propagate trace_id)
%% =============================================================================

%% @doc RPC wrapper that propagates trace context across Edge->Core boundary.
%% Injects current trace context into the RPC arguments, and the core-side
%% entry point can extract it using iris_trace:extract/1.
-spec traced_rpc(node(), module(), atom(), list()) -> term().
traced_rpc(Node, Mod, Fun, Args) ->
    traced_rpc(Node, Mod, Fun, Args, 5000).

-spec traced_rpc(node(), module(), atom(), list(), timeout()) -> term().
traced_rpc(Node, Mod, Fun, Args, Timeout) ->
    %% Inject trace context as last argument (map)
    TraceCtx = iris_trace:inject(#{}),
    case TraceCtx of
        #{<<"trace_id">> := _} ->
            %% Pass trace context alongside args via a wrapper call
            rpc:call(Node, iris_trace, execute_with_context,
                     [TraceCtx, Mod, Fun, Args], Timeout);
        _ ->
            %% No active trace - plain RPC
            rpc:call(Node, Mod, Fun, Args, Timeout)
    end.

%% =============================================================================
%% Internal: Version negotiation helper (RFC Section 11.1)
%% =============================================================================

negotiate_version(ClientVersions, ServerVersions) when is_list(ClientVersions), is_list(ServerVersions) ->
    Common = [V || V <- ClientVersions, lists:member(V, ServerVersions)],
    case Common of
        [] -> hd(ServerVersions);  %% Fallback to server's primary version
        _ -> lists:max(Common)
    end;
negotiate_version(_, ServerVersions) ->
    hd(ServerVersions).

%% =============================================================================
%% Internal: UUIDv7 idempotency key validation (RFC Section 1.2)
%% =============================================================================

%% @doc Validate idempotency_key in CBOR message map if present.
%% If the key is absent, validation passes (backwards compatibility).
%% If present, it MUST be valid UUIDv7.
-spec validate_cbor_idempotency_key(map()) -> ok | {error, invalid_idempotency_key}.
validate_cbor_idempotency_key(Map) when is_map(Map) ->
    case maps:get(<<"idempotency_key">>, Map, undefined) of
        undefined -> ok;  %% Field not present - allow (backwards compat)
        Key -> iris_uuid:validate_idempotency_key(Key)
    end.

%% =============================================================================
%% Internal: Group Fan-out Recipient Filtering (Audit Finding 3: TOCTOU Fix)
%% =============================================================================

%% @doc Filter group member list to valid recipients for fan-out.
%% Re-checks membership for each recipient to close the TOCTOU window
%% between get_members and the actual routing.
%% Excludes the sender and any members removed since the snapshot.
-spec group_fanout_recipients(binary(), binary(), [map()]) -> [map()].
group_fanout_recipients(GroupId, Sender, Members) ->
    lists:filter(fun(#{user_id := MemberId}) ->
        MemberId =/= Sender andalso
        call_iris_group(is_member, [GroupId, MemberId]) =:= true
    end, Members).

%% =============================================================================
%% Internal: E2EE Header Validation (RFC-001-AMENDMENT-001 Section 4.1, NFR-18)
%% =============================================================================

%% @doc Validate E2EE message header contains required fields with correct sizes.
%% Required: ik (identity key, 32 bytes), ek (ephemeral key, 32 bytes).
%% The server cannot decrypt but CAN validate structural integrity and key sizes.
%% Audit Finding 1: Prevent trivially empty/garbage headers that bypass E2EE.
-define(MIN_E2EE_KEY_LEN, 32).  %% X25519 public key size

-spec validate_e2ee_header(term()) -> ok | {error, term()}.
validate_e2ee_header(Header) when is_map(Header) ->
    RequiredKeys = [<<"ik">>, <<"ek">>],
    Missing = [K || K <- RequiredKeys, not maps:is_key(K, Header)],
    case Missing of
        [] ->
            %% Validate key field types and minimum lengths
            validate_e2ee_key_fields(Header);
        _ -> {error, {missing_e2ee_fields, Missing}}
    end;
validate_e2ee_header(_) ->
    %% Non-map header MUST be rejected (NFR-18).
    %% E2EE headers are always CBOR maps per RFC-001-AMENDMENT-001 Section 4.1.
    %% A non-map value cannot contain required keys (ik, ek).
    {error, invalid_header_type}.

%% @doc Validate ik and ek fields are binaries of at least 32 bytes.
validate_e2ee_key_fields(Header) ->
    IK = maps:get(<<"ik">>, Header),
    EK = maps:get(<<"ek">>, Header),
    case {is_binary(IK), is_binary(EK)} of
        {false, _} -> {error, {invalid_e2ee_key, ik, not_binary}};
        {_, false} -> {error, {invalid_e2ee_key, ek, not_binary}};
        {true, true} ->
            case {byte_size(IK) >= ?MIN_E2EE_KEY_LEN, byte_size(EK) >= ?MIN_E2EE_KEY_LEN} of
                {false, _} -> {error, {e2ee_key_too_short, ik, byte_size(IK)}};
                {_, false} -> {error, {e2ee_key_too_short, ek, byte_size(EK)}};
                {true, true} -> ok
            end
    end.

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
        case traced_rpc(CoreNode, iris_core, register_user, [User, node(), TransportPid]) of
            ok -> ok;
            {badrpc, Reason} -> 
                logger:warning("Async Core registration failed for ~p on ~p: ~p", [User, CoreNode, Reason]);
            {error, Reason} ->
                logger:warning("Async Core registration error for ~p: ~p", [User, Reason])
        end
    end),

    %% PHASE 3: Retrieve offline messages (HOT-001 FIX: Paginated for large queues)
    %% Messages MUST be delivered when recipient connects (RFC FR-2 compliance)
    %% But for celebrity accounts with 1M+ messages, we stream in pages to prevent OOM
    OfflineActions = deliver_offline_messages(User),
    
    %% Generate session_id for connection resume (RFC Section 3.4)
    SessionId = generate_session_id(),
    iris_session_cache:store(SessionId, User),
    
    %% Generate refresh token (RFC FR-11a) -- best-effort via Core RPC
    RefreshTokenPart = case catch iris_circuit_breaker:call(CoreNode, iris_auth, create_refresh_token, [User]) of
        {ok, RT} ->
            RTBin = ensure_binary(RT),
            <<(byte_size(RTBin)):16, RTBin/binary>>;
        _ ->
            <<0:16>>  %% No refresh token available
    end,
    
    %% Response: LOGIN_OK + SessionIdLen(16) + SessionId + RefreshTokenLen(16) + RefreshToken
    SidLen = byte_size(SessionId),
    LoginOkPayload = <<3, "LOGIN_OK", SidLen:16, SessionId/binary, RefreshTokenPart/binary>>,
    {ok, User, [{send, LoginOkPayload}, {set_session_id, SessionId} | OfflineActions]}.

%% =============================================================================
%% HOT-001 FIX: Paginated Offline Delivery for Celebrity Hotspots
%% For normal users (<1000 offline messages): deliver all at once
%% For hot users (>=1000 messages): deliver first page + OFFLINE_MORE indicator
%% Client must request subsequent pages via {get_offline_page, Cursor}

deliver_offline_messages(User) ->
    CoreNode = get_core_node(),
    %% First, check queue depth to decide delivery strategy (with failover)
    QueueDepth = get_offline_queue_depth_with_failover(User, CoreNode),
    
    case QueueDepth of
        N when is_integer(N), N > 0, N =< ?OFFLINE_INLINE_LIMIT ->
            %% Small queue - deliver all at once (original behavior)
            deliver_all_offline(User, CoreNode);
        N when is_integer(N), N > ?OFFLINE_INLINE_LIMIT ->
            %% Large queue - deliver first page + continuation indicator
            logger:info("HOT-001: User ~p has ~p offline messages, using paginated delivery", [User, N]),
            deliver_offline_page(User, CoreNode, 0, N);
        unknown_try_anyway ->
            %% Failed to get queue depth - try direct retrieval anyway (failover path)
            logger:warning("Queue depth check failed for ~p, trying direct retrieval", [User]),
            deliver_all_offline(User, CoreNode);
        _ ->
            %% Confirmed no messages
            []
    end.

%% Get offline queue depth with failover to other cores
get_offline_queue_depth_with_failover(User, PrimaryCore) ->
    case catch rpc:call(PrimaryCore, iris_core, get_offline_queue_depth, [User], 2000) of
        N when is_integer(N) -> 
            N;
        {badrpc, _Reason} ->
            %% Primary failed - try other cores
            AllCores = application:get_env(iris_edge, core_nodes, []),
            OtherCores = [C || C <- AllCores, C =/= PrimaryCore],
            get_queue_depth_from_any(User, OtherCores);
        _ -> 
            0
    end.

get_queue_depth_from_any(_User, []) ->
    %% All cores failed - return special marker to try retrieval anyway
    unknown_try_anyway;
get_queue_depth_from_any(User, [Core | Rest]) ->
    case net_adm:ping(Core) of
        pong ->
            case catch rpc:call(Core, iris_core, get_offline_queue_depth, [User], 2000) of
                N when is_integer(N) -> N;
                _ -> get_queue_depth_from_any(User, Rest)
            end;
        pang ->
            get_queue_depth_from_any(User, Rest)
    end.

deliver_all_offline(User, CoreNode) ->
    %% Try primary core node first
    case rpc:call(CoreNode, iris_core, retrieve_offline, [User], 5000) of
        Msgs when is_list(Msgs), length(Msgs) > 0 ->
            lists:map(fun(Msg) ->
                MsgId = iris_proto:generate_msg_id(),
                {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
            end, Msgs);
        {badrpc, Reason} ->
            %% Primary core failed - try other cores (multimaster failover)
            logger:warning("Offline retrieval from ~p failed: ~p, trying other cores", [CoreNode, Reason]),
            deliver_offline_failover(User, CoreNode);
        [] ->
            %% Primary returned empty - but other cores might have messages
            %% (Mnesia sync might be pending after partition heal)
            logger:debug("No offline messages on primary ~p, trying other cores", [CoreNode]),
            deliver_offline_failover(User, CoreNode);
        _ -> 
            []
    end.

%% Failover: Try other core nodes if primary is down (multimaster durability)
deliver_offline_failover(User, FailedCore) ->
    %% Get all configured core nodes
    AllCores = application:get_env(iris_edge, core_nodes, []),
    %% Filter out the failed core and try remaining ones
    OtherCores = [C || C <- AllCores, C =/= FailedCore],
    deliver_offline_from_any(User, OtherCores).

deliver_offline_from_any(_User, []) ->
    logger:error("All core nodes failed for offline retrieval"),
    [];
deliver_offline_from_any(User, [Core | Rest]) ->
    case net_adm:ping(Core) of
        pong ->
            case rpc:call(Core, iris_core, retrieve_offline, [User], 5000) of
                Msgs when is_list(Msgs), length(Msgs) > 0 ->
                    logger:info("Retrieved ~p offline messages from failover core ~p", [length(Msgs), Core]),
                    lists:map(fun(Msg) ->
                        MsgId = iris_proto:generate_msg_id(),
                        {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
                    end, Msgs);
                {badrpc, Reason} ->
                    logger:warning("Failover core ~p also failed: ~p", [Core, Reason]),
                    deliver_offline_from_any(User, Rest);
                _ ->
                    %% No messages on this core, try next
                    deliver_offline_from_any(User, Rest)
            end;
        pang ->
            %% Core unreachable, try next
            deliver_offline_from_any(User, Rest)
    end.

deliver_offline_page(User, CoreNode, Cursor, TotalCount) ->
    case rpc:call(CoreNode, iris_core, retrieve_offline_paginated, 
                  [User, ?OFFLINE_PAGE_SIZE, Cursor], 5000) of
        {Msgs, NextCursor} when is_list(Msgs), length(Msgs) > 0 ->
            %% Encode messages
            MsgActions = lists:map(fun(Msg) ->
                MsgId = iris_proto:generate_msg_id(),
                {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
            end, Msgs),
            
            %% Add continuation indicator if more pages exist
            case NextCursor of
                done ->
                    %% Last page - just send messages
                    MsgActions;
                _ ->
                    %% More pages - append OFFLINE_MORE indicator
                    %% Client should send {get_offline_page, NextCursor} to continue
                    Remaining = TotalCount - (Cursor + length(Msgs)),
                    MoreIndicator = encode_offline_more(NextCursor, Remaining),
                    MsgActions ++ [{send, MoreIndicator}]
            end;
        {badrpc, Reason} ->
            %% Primary core failed - try other cores (multimaster failover)
            logger:warning("Paginated offline retrieval from ~p failed: ~p, trying other cores", [CoreNode, Reason]),
            deliver_offline_page_failover(User, CoreNode, Cursor, TotalCount);
        {[], done} ->
            %% Primary returned empty - but other cores might have messages
            logger:debug("No paginated offline messages on primary ~p, trying other cores", [CoreNode]),
            deliver_offline_page_failover(User, CoreNode, Cursor, TotalCount);
        _ -> []
    end.

%% Failover for paginated offline delivery
deliver_offline_page_failover(User, FailedCore, Cursor, TotalCount) ->
    AllCores = application:get_env(iris_edge, core_nodes, []),
    OtherCores = [C || C <- AllCores, C =/= FailedCore],
    deliver_page_from_any(User, OtherCores, Cursor, TotalCount).

deliver_page_from_any(_User, [], _Cursor, _TotalCount) ->
    logger:error("All core nodes failed for paginated offline retrieval"),
    [];
deliver_page_from_any(User, [Core | Rest], Cursor, TotalCount) ->
    case net_adm:ping(Core) of
        pong ->
            case rpc:call(Core, iris_core, retrieve_offline_paginated,
                          [User, ?OFFLINE_PAGE_SIZE, Cursor], 5000) of
                {Msgs, NextCursor} when is_list(Msgs), length(Msgs) > 0 ->
                    logger:info("Retrieved ~p offline messages from failover core ~p", [length(Msgs), Core]),
                    MsgActions = lists:map(fun(Msg) ->
                        MsgId = iris_proto:generate_msg_id(),
                        {send, iris_proto:encode_reliable_msg(MsgId, Msg)}
                    end, Msgs),
                    case NextCursor of
                        done -> MsgActions;
                        _ ->
                            Remaining = TotalCount - (Cursor + length(Msgs)),
                            MoreIndicator = encode_offline_more(NextCursor, Remaining),
                            MsgActions ++ [{send, MoreIndicator}]
                    end;
                {badrpc, _Reason} ->
                    deliver_page_from_any(User, Rest, Cursor, TotalCount);
                _ ->
                    deliver_page_from_any(User, Rest, Cursor, TotalCount)
            end;
        pang ->
            deliver_page_from_any(User, Rest, Cursor, TotalCount)
    end.

%% Encode indicator that more offline messages are available
%% Format: [opcode=0x80, cursor:32, remaining:32]
encode_offline_more(NextCursor, Remaining) ->
    <<16#80, NextCursor:32, Remaining:32>>.

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
%% RFC 7.4 FIX: Also track request for flow controller rate-based degradation

check_message_rate(User) ->
    %% Track request for flow controller (rate-based degradation)
    iris_flow_controller:track_request(User),
    case whereis(iris_rate_limiter) of
        undefined -> allow;
        _ -> iris_rate_limiter:check(User)
    end.

encode_rate_limited(RetryAfter) ->
    %% Error response with retry-after hint
    <<16#FF, RetryAfter:32>>.

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).

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
                    rpc:cast(get_core_node(), iris_core, update_status, [User, offline]),
                    ok;  %% FIX: Explicit ok return (rpc:cast returns true)
                [{User, _OtherPid}] ->
                    %% Different process owns it (new login happened) - don't delete
                    ok;
                [] ->
                    %% Already deleted - nothing to do
                    ok
            end
    end.
