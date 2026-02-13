-module(iris_user_safety).

%% =============================================================================
%% RL-3: User Block/Report (RFC-001 v4.0 FR-8b)
%%
%% Provides user-to-user blocking and reporting. Blocked users cannot
%% send messages to the blocker. Reports are stored for admin review.
%%
%% AUDIT P0-5: All writes use mnesia:transaction for durability.
%% AUDIT 7.4: Input validation on user ID size (max 128 bytes).
%% =============================================================================

-export([block_user/2, unblock_user/2, is_blocked/2]).
-export([check_can_message/2, get_blocked/1]).
-export([report_user/3]).

-define(BLOCKS_TABLE, user_blocks).
-define(REPORTS_TABLE, user_reports).
-define(MAX_USER_ID_SIZE, 128).

%% @doc Block a user. Blocker will no longer receive messages from Blocked.
-spec block_user(binary(), binary()) -> ok | {error, invalid_user_id}.
block_user(Blocker, Blocked) when is_binary(Blocker), byte_size(Blocker) =< ?MAX_USER_ID_SIZE,
                                  is_binary(Blocked), byte_size(Blocked) =< ?MAX_USER_ID_SIZE ->
    Key = {Blocker, Blocked},
    Now = os:system_time(second),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write({?BLOCKS_TABLE, Key, Blocker, Blocked, Now})
    end),
    ok;
block_user(_, _) ->
    {error, invalid_user_id}.

%% @doc Unblock a previously blocked user.
-spec unblock_user(binary(), binary()) -> ok | {error, invalid_user_id}.
unblock_user(Blocker, Blocked) when is_binary(Blocker), byte_size(Blocker) =< ?MAX_USER_ID_SIZE,
                                    is_binary(Blocked), byte_size(Blocked) =< ?MAX_USER_ID_SIZE ->
    Key = {Blocker, Blocked},
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:delete({?BLOCKS_TABLE, Key})
    end),
    ok;
unblock_user(_, _) ->
    {error, invalid_user_id}.

%% @doc Check if Blocker has blocked Blocked.
-spec is_blocked(binary(), binary()) -> boolean().
is_blocked(Blocker, Blocked) ->
    Key = {Blocker, Blocked},
    case mnesia:dirty_read(?BLOCKS_TABLE, Key) of
        [_|_] -> true;
        [] -> false
    end.

%% @doc Check if Sender can message Recipient (not blocked).
-spec check_can_message(binary(), binary()) -> ok | {error, blocked}.
check_can_message(Sender, Recipient) ->
    case is_blocked(Recipient, Sender) of
        true -> {error, blocked};
        false -> ok
    end.

%% @doc Get list of users blocked by UserId.
-spec get_blocked(binary()) -> [binary()].
get_blocked(UserId) ->
    try
        Records = mnesia:dirty_match_object(?BLOCKS_TABLE,
            {?BLOCKS_TABLE, '_', UserId, '_', '_'}),
        [Blocked || {?BLOCKS_TABLE, _Key, _Blocker, Blocked, _At} <- Records]
    catch
        Class:Reason ->
            %% AUDIT P2-6: Log errors instead of silently returning empty list
            logger:warning("get_blocked failed for ~p: ~p:~p", [UserId, Class, Reason]),
            []
    end.

%% @doc Report a user. Reports are stored for admin review.
-spec report_user(binary(), binary(), binary()) -> ok | {error, invalid_user_id}.
report_user(Reporter, Reported, Reason) when is_binary(Reporter), byte_size(Reporter) =< ?MAX_USER_ID_SIZE,
                                              is_binary(Reported), byte_size(Reported) =< ?MAX_USER_ID_SIZE ->
    Key = {Reporter, Reported, os:system_time(millisecond)},
    Now = os:system_time(second),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write({?REPORTS_TABLE, Key, Reporter, Reported, Reason, Now})
    end),
    ok;
report_user(_, _, _) ->
    {error, invalid_user_id}.
