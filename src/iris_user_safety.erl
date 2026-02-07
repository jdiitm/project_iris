-module(iris_user_safety).

%% =============================================================================
%% RL-3: User Block/Report (RFC-001 v4.0 FR-8b)
%%
%% Provides user-to-user blocking and reporting. Blocked users cannot
%% send messages to the blocker. Reports are stored for admin review.
%% =============================================================================

-export([block_user/2, unblock_user/2, is_blocked/2]).
-export([check_can_message/2, get_blocked/1]).
-export([report_user/3]).

-define(BLOCKS_TABLE, user_blocks).
-define(REPORTS_TABLE, user_reports).

%% @doc Block a user. Blocker will no longer receive messages from Blocked.
-spec block_user(binary(), binary()) -> ok.
block_user(Blocker, Blocked) ->
    Key = {Blocker, Blocked},
    Now = os:system_time(second),
    mnesia:dirty_write(?BLOCKS_TABLE, {?BLOCKS_TABLE, Key, Blocker, Blocked, Now}),
    ok.

%% @doc Unblock a previously blocked user.
-spec unblock_user(binary(), binary()) -> ok.
unblock_user(Blocker, Blocked) ->
    Key = {Blocker, Blocked},
    mnesia:dirty_delete(?BLOCKS_TABLE, Key),
    ok.

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
        _:_ -> []
    end.

%% @doc Report a user. Reports are stored for admin review.
-spec report_user(binary(), binary(), binary()) -> ok.
report_user(Reporter, Reported, Reason) ->
    Key = {Reporter, Reported, os:system_time(millisecond)},
    Now = os:system_time(second),
    mnesia:dirty_write(?REPORTS_TABLE, {?REPORTS_TABLE, Key, Reporter, Reported, Reason, Now}),
    ok.
