%%%-------------------------------------------------------------------
%%% @doc Operational Hard Limits (RFC-001 v3.0 Section 8)
%%%
%%% These limits are enforced to maintain system stability at 5B DAU scale.
%%% All limits are centralized here for consistency and auditability.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_limits).

%% Limit accessors
-export([
    max_e2ee_group_members/0,
    max_broadcast_group_members/0,
    max_payload_size/0,
    max_rate_sustained/0,
    max_rate_burst/0,
    max_rate_burst_window/0,
    max_fanout_rate/0,
    max_inbox_size/0,
    max_username_length/0,
    max_group_name_length/0,
    max_groups_per_user/0,
    dedup_window_hot_ms/0,
    dedup_window_days/0
]).

%% Validation functions
-export([
    validate_payload/1,
    validate_username/1,
    validate_group_size/2,
    validate_rate/2
]).

%% =============================================================================
%% Operational Hard Limits (RFC-001 v3.0 Section 8)
%% =============================================================================

%% @doc Maximum members in an E2EE group.
%% Rationale: N² complexity for Sender Key distribution
-spec max_e2ee_group_members() -> pos_integer().
max_e2ee_group_members() -> 256.

%% @doc Maximum members in a broadcast group (server-side fan-out, no E2EE).
-spec max_broadcast_group_members() -> pos_integer().
max_broadcast_group_members() -> 10000.

%% @doc Maximum payload size in bytes.
%% Rationale: Large media → Blob Store with URL + decrypt key
-spec max_payload_size() -> pos_integer().
max_payload_size() -> 65536.  %% 64 KB

%% @doc Sustained rate limit (messages per second per user).
-spec max_rate_sustained() -> pos_integer().
max_rate_sustained() -> 5.

%% @doc Burst rate limit (messages per second per user during burst).
-spec max_rate_burst() -> pos_integer().
max_rate_burst() -> 20.

%% @doc Burst window in seconds.
-spec max_rate_burst_window() -> pos_integer().
max_rate_burst_window() -> 10.

%% @doc Maximum fan-out rate (inboxes per second per worker).
%% Rationale: Core worker throughput limit
-spec max_fanout_rate() -> pos_integer().
max_fanout_rate() -> 1000.

%% @doc Maximum messages in an inbox before archival.
%% Oldest messages archived to cold storage.
-spec max_inbox_size() -> pos_integer().
max_inbox_size() -> 10000.

%% @doc Maximum username length in bytes.
-spec max_username_length() -> pos_integer().
max_username_length() -> 256.

%% @doc Maximum group name length in bytes.
-spec max_group_name_length() -> pos_integer().
max_group_name_length() -> 256.

%% @doc Maximum groups a user can belong to.
-spec max_groups_per_user() -> pos_integer().
max_groups_per_user() -> 100.

%% @doc Hot tier dedup window (ETS cache) in milliseconds.
-spec dedup_window_hot_ms() -> pos_integer().
dedup_window_hot_ms() -> 300000.  %% 5 minutes

%% @doc Warm tier dedup window in days (bloom filter).
-spec dedup_window_days() -> pos_integer().
dedup_window_days() -> 7.

%% =============================================================================
%% Validation Functions
%% =============================================================================

%% @doc Validate payload size.
-spec validate_payload(binary() | pos_integer()) -> ok | {error, payload_too_large}.
validate_payload(Payload) when is_binary(Payload) ->
    validate_payload(byte_size(Payload));
validate_payload(Size) when is_integer(Size), Size =< 65536 ->
    ok;
validate_payload(_Size) ->
    {error, payload_too_large}.

%% @doc Validate username length.
-spec validate_username(binary() | list()) -> ok | {error, username_too_long}.
validate_username(Username) when is_binary(Username) ->
    case byte_size(Username) =< max_username_length() of
        true -> ok;
        false -> {error, username_too_long}
    end;
validate_username(Username) when is_list(Username) ->
    validate_username(iolist_to_binary(Username)).

%% @doc Validate group size based on type (e2ee or broadcast).
-spec validate_group_size(e2ee | broadcast, pos_integer()) -> 
    ok | {error, group_full}.
validate_group_size(e2ee, Size) when Size < 256 -> ok;
validate_group_size(broadcast, Size) when Size < 10000 -> ok;
validate_group_size(_, _) -> {error, group_full}.

%% @doc Check if user is within rate limit.
%% Returns ok if allowed, {error, rate_limited} if exceeded.
%% RateState is a map with keys: count, window_start, burst_count, burst_start
-spec validate_rate(map(), non_neg_integer()) -> 
    {ok, map()} | {error, rate_limited}.
validate_rate(RateState, Now) ->
    %% Check sustained rate (5/sec over rolling window)
    SustainedWindow = maps:get(sustained_window, RateState, Now - 1000),
    SustainedCount = maps:get(sustained_count, RateState, 0),
    
    %% Check burst rate (20/sec for 10s)
    BurstStart = maps:get(burst_start, RateState, Now),
    BurstCount = maps:get(burst_count, RateState, 0),
    
    %% Reset windows if expired
    {NewSustainedCount, NewSustainedWindow} = 
        case Now - SustainedWindow > 1000 of
            true -> {0, Now};
            false -> {SustainedCount, SustainedWindow}
        end,
    
    {NewBurstCount, NewBurstStart} =
        case Now - BurstStart > 10000 of  %% 10 second burst window
            true -> {0, Now};
            false -> {BurstCount, BurstStart}
        end,
    
    %% Check limits
    case {NewSustainedCount < max_rate_sustained(), 
          NewBurstCount < max_rate_burst()} of
        {true, true} ->
            %% Allowed - update counters
            NewState = RateState#{
                sustained_count => NewSustainedCount + 1,
                sustained_window => NewSustainedWindow,
                burst_count => NewBurstCount + 1,
                burst_start => NewBurstStart
            },
            {ok, NewState};
        _ ->
            {error, rate_limited}
    end.
