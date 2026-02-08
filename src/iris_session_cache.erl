-module(iris_session_cache).

%% =============================================================================
%% FM-3: Session Cache for Connection Resume (RFC-001 v4.0 Section 3.4)
%%
%% ETS-backed session cache with TTL for connection resume.
%% Stores session state (user, sequence counter, queued messages)
%% that persists for 5 minutes after disconnect to allow resume.
%%
%% RFC-001 v4.0 Section 3.4: "<=100K sessions per edge node"
%% Enforced via LRU eviction when MAX_SESSIONS is reached.
%% =============================================================================

-export([start/0, stop/0]).
-export([store/2, lookup/1, remove/1]).
-export([next_seq/1, queue_message/3, get_messages_after/2]).
-export([get_ttl/0, get_count/0, get_max_sessions/0]).

-define(SESSION_TABLE, iris_session_cache_data).
-define(MESSAGE_TABLE, iris_session_cache_msgs).
-define(TTL_SECONDS, 300).  %% 5 minutes
-define(MAX_SESSIONS, 100000).  %% RFC-001 v4.0 Section 3.4: <=100K per edge

%% @doc Start the session cache (create ETS tables).
start() ->
    case ets:info(?SESSION_TABLE) of
        undefined ->
            ets:new(?SESSION_TABLE, [named_table, public, set,
                                     {read_concurrency, true}]);
        _ -> ok
    end,
    case ets:info(?MESSAGE_TABLE) of
        undefined ->
            ets:new(?MESSAGE_TABLE, [named_table, public, ordered_set,
                                     {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

%% @doc Stop the session cache (delete ETS tables).
stop() ->
    catch ets:delete(?SESSION_TABLE),
    catch ets:delete(?MESSAGE_TABLE),
    ok.

%% @doc Store a session with user_id. Sets sequence counter to 0.
%% RFC-001 v4.0 Section 3.4: Enforces <=100K sessions per edge node.
%% If at capacity, evicts the oldest session (LRU) before inserting.
-spec store(binary(), binary()) -> ok.
store(SessionId, UserId) ->
    Now = os:system_time(second),
    %% Enforce MAX_SESSIONS limit (RFC 3.4)
    maybe_evict_oldest(),
    ets:insert(?SESSION_TABLE, {SessionId, UserId, 0, Now}),
    ok.

%% @doc Lookup a session. Returns {ok, #{user_id, seq, created_at}} or {error, not_found}.
-spec lookup(binary()) -> {ok, map()} | {error, not_found | expired}.
lookup(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{SessionId, UserId, Seq, CreatedAt}] ->
            Now = os:system_time(second),
            case (Now - CreatedAt) > ?TTL_SECONDS of
                true ->
                    %% Expired -- clean up
                    remove(SessionId),
                    {error, not_found};
                false ->
                    {ok, #{user_id => UserId, seq => Seq, created_at => CreatedAt}}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Remove a session.
-spec remove(binary()) -> ok.
remove(SessionId) ->
    ets:delete(?SESSION_TABLE, SessionId),
    %% Clean up queued messages
    cleanup_messages(SessionId),
    ok.

%% @doc Get and increment sequence number for a session.
-spec next_seq(binary()) -> non_neg_integer().
next_seq(SessionId) ->
    ets:update_counter(?SESSION_TABLE, SessionId, {3, 1}).

%% @doc Queue a message for potential replay.
-spec queue_message(binary(), non_neg_integer(), binary()) -> ok.
queue_message(SessionId, SeqNo, Message) ->
    ets:insert(?MESSAGE_TABLE, {{SessionId, SeqNo}, Message}),
    ok.

%% @doc Get messages with sequence > LastSeq for a session.
-spec get_messages_after(binary(), non_neg_integer()) -> {ok, [{non_neg_integer(), binary()}]} | {error, not_found}.
get_messages_after(SessionId, LastSeq) ->
    case lookup(SessionId) of
        {error, _} = Err -> Err;
        {ok, _} ->
            %% Collect messages with seq > LastSeq
            Messages = collect_messages(SessionId, LastSeq),
            {ok, Messages}
    end.

%% @doc Get TTL in seconds.
-spec get_ttl() -> non_neg_integer().
get_ttl() ->
    ?TTL_SECONDS.

%% @doc Get current session count.
-spec get_count() -> non_neg_integer().
get_count() ->
    case ets:info(?SESSION_TABLE, size) of
        undefined -> 0;
        N -> N
    end.

%% @doc Get maximum sessions allowed (RFC 3.4).
-spec get_max_sessions() -> non_neg_integer().
get_max_sessions() ->
    ?MAX_SESSIONS.

%% =============================================================================
%% Internal
%% =============================================================================

collect_messages(SessionId, LastSeq) ->
    %% Use match to get all messages for this session
    Pattern = {{SessionId, '$1'}, '$2'},
    Guards = [{'>', '$1', LastSeq}],
    Result = ets:select(?MESSAGE_TABLE, [{Pattern, Guards, [{{'$1', '$2'}}]}]),
    lists:sort(fun({SeqA, _}, {SeqB, _}) -> SeqA =< SeqB end, Result).

cleanup_messages(SessionId) ->
    %% Delete all messages for this session
    Pattern = {{SessionId, '_'}, '_'},
    ets:match_delete(?MESSAGE_TABLE, Pattern),
    ok.

%% @doc Evict oldest session if at capacity (RFC 3.4: <=100K).
%% Uses created_at timestamp for LRU ordering.
maybe_evict_oldest() ->
    case ets:info(?SESSION_TABLE, size) of
        undefined -> ok;
        Size when Size < ?MAX_SESSIONS -> ok;
        _Size ->
            %% At capacity -- evict oldest (lowest created_at)
            evict_oldest_session()
    end.

evict_oldest_session() ->
    %% Fold over ETS to find session with lowest created_at
    %% For production at 100K entries, a secondary index would be faster,
    %% but eviction is rare (only when at exact limit) and amortized.
    case ets:first(?SESSION_TABLE) of
        '$end_of_table' -> ok;
        FirstKey ->
            {OldestKey, _OldestTime} = ets:foldl(
                fun({SessId, _User, _Seq, CreatedAt}, {BestId, BestTime}) ->
                    if CreatedAt < BestTime -> {SessId, CreatedAt};
                       true -> {BestId, BestTime}
                    end
                end,
                {FirstKey, case ets:lookup(?SESSION_TABLE, FirstKey) of
                    [{_, _, _, T}] -> T;
                    _ -> erlang:system_time(second)
                end},
                ?SESSION_TABLE
            ),
            remove(OldestKey)
    end.
